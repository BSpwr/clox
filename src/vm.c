#include "vm.h"

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "memory.h"
#include "object.h"
#include "time.h"
#include "value.h"

VM vm;

static Value clockNative(int argCount, Value* args) {
    return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

void initVM() {
    resetStack();
    vm.objects      = NULL;
    vm.openUpvalues = NULL;

    vm.bytesAllocated = 0;
    vm.nextGC         = 1024 * 1024;

    vm.grayCount    = 0;
    vm.grayCapacity = 0;
    vm.grayStack    = NULL;

    initTable(&vm.globals);
    initTable(&vm.strings);

    vm.initString = NULL;
    vm.initString = copyString("init", 4);

    defineNative("clock", clockNative);
}

static void resetStack() {
    vm.stackTop   = vm.stack;
    vm.frameCount = 0;
}

static void runtimeError(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    for (int i = vm.frameCount - 1; i >= 0; i--) {
        CallFrame*   frame    = &vm.frames[i];
        ObjFunction* function = frame->closure->function;
        // -1 because the IP is sittting on the next instruction to be executed.
        size_t instruction = frame->ip - function->chunk.code - 1;
        int    line        = function->chunk.lines[instruction];
        fprintf(stderr, "[line %d] in ", line);
        function->name == NULL ? fprintf(stderr, "script\n")
                               : fprintf(stderr, "%s()\n", function->name->chars);
    }

    resetStack();
}

static void defineNative(const char* name, NativeFn function) {
    /* You’re probably wondering why we push and pop the name and function on the
    stack. That looks weird, right? This is the kind of stuff you have to worry about
    when garbage collection gets involved. Both copyString() and
    newNative() dynamically allocate memory. That means once we have a GC,
    they can potentially trigger a collection. If that happens, we need to ensure the
    collector knows we’re not done with the name and ObjFunction so that it
    doesn’t free them out from under us. Storing them on the value stack
    accomplishes that. */
    push(OBJ_VAL(copyString(name, (int)strlen(name))));
    push(OBJ_VAL(newNative(function)));
    tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
    pop();
    pop();
}

void freeVM() {
    freeTable(&vm.globals);
    freeTable(&vm.strings);
    vm.initString = NULL;
    freeObjects();
}

void push(Value value) { *vm.stackTop++ = value; }

Value pop() { return *--vm.stackTop; }

Value peek(int distance) { return vm.stackTop[-distance - 1]; }

static bool call(ObjClosure* closure, int argCount) {
    if (argCount != closure->function->arity) {
        runtimeError("Expected %d arguments, but got %d.", closure->function->arity, argCount);
        return false;
    }

    if (vm.frameCount == FRAMES_MAX) {
        runtimeError("Stack overflow.");
        return false;
    }

    CallFrame* frame = &vm.frames[vm.frameCount++];
    frame->closure   = closure;
    frame->ip        = closure->function->chunk.code;
    // -1 to skip over local slot zero, which contains function being called.
    frame->slots = vm.stackTop - argCount - 1;
    return true;
}

static bool callValue(Value callee, int argCount) {
    if (IS_OBJ(callee)) {
        switch (OBJ_TYPE(callee)) {
            case OBJ_BOUND_METHOD: {
                ObjBoundMethod* bound = AS_BOUND_METHOD(callee);
                // The -argCount skips past the arguments and the - 1 adjusts for the fact that
                // stackTop points just past the last used stack slot.
                vm.stackTop[-argCount - 1] = bound->receiver;
                return call(bound->method, argCount);
            }
            case OBJ_CLASS: {
                ObjClass* klass            = AS_CLASS(callee);
                vm.stackTop[-argCount - 1] = OBJ_VAL(newInstance(klass));
                Value initializer;
                if (tableGet(&klass->methods, vm.initString, &initializer)) {
                    return call(AS_CLOSURE(initializer), argCount);
                } else if (argCount != 0) {
                    runtimeError("Expected 0 arguments but got %d.", argCount);
                    return false;
                }
                return true;
            }
            case OBJ_CLOSURE: return call(AS_CLOSURE(callee), argCount);
            case OBJ_NATIVE: {
                NativeFn native = AS_NATIVE(callee);
                Value    result = native(argCount, vm.stackTop - argCount);
                vm.stackTop -= argCount + 1;
                push(result);
                return true;
            }
            default: break;  // Non-callable object type.
        }
    }

    runtimeError("Can only call functions and classes.");
    return false;
}

static bool invokeFromClass(ObjClass* klass, ObjString* name, int argCount) {
    Value method;
    if (!tableGet(&klass->methods, name, &method)) {
        runtimeError("Undefined property '%s'.", name->chars);
        return false;
    }

    return call(AS_CLOSURE(method), argCount);
}

static bool invoke(ObjString* name, int argCount) {
    Value receiver = peek(argCount);

    if (!IS_INSTANCE(receiver)) {
        runtimeError("Only instances have methods.");
        return false;
    }

    ObjInstance* instance = AS_INSTANCE(receiver);

    // Before looking up a method on the instance’s class, we look
    // for a field with the same name. If we find a field, then we store it on the stack in
    // place of the receiver, under the argument list.
    Value value;
    if (tableGet(&instance->fields, name, &value)) {
        vm.stackTop[-argCount - 1] = value;
        return callValue(value, argCount);
    }

    return invokeFromClass(instance->klass, name, argCount);
}

static bool bindMethod(ObjClass* klass, ObjString* name) {
    Value method;
    if (!tableGet(&klass->methods, name, &method)) {
        runtimeError("Undefined property '%s'", name->chars);
        return false;
    }

    ObjBoundMethod* bound = newBoundMethod(peek(0), AS_CLOSURE(method));

    pop(); // Receiver.
    push(OBJ_VAL(bound));
    return true;
}

static ObjUpvalue* captureUpvalue(Value* local) {
    ObjUpvalue* prevUpvalue = NULL;
    ObjUpvalue* upvalue     = vm.openUpvalues;

    // We start at the head of the list, which is the upvalue closest to the top of the stack.
    // Walk through the list, using a pointer comparison to iterate past every upvalue pointing to
    // slots above the one we're looking for.
    while (upvalue != NULL && upvalue->location > local) {
        prevUpvalue = upvalue;
        upvalue     = upvalue->next;
    }

    // The local slot we stopped at is the slot we're looking for.
    // We found an existing upvalue capturing the variable, so reuse it.
    if (upvalue != NULL && upvalue->location == local) {
        return upvalue;
    }

    // We found an upvalue whose local slot is below the one we're looking for.
    // Since the list is sorted, that means we've gone past the slot we are closing over
    // and thus there must not be an existing upvalue for it.
    ObjUpvalue* createdUpvalue = newUpvalue(local);
    createdUpvalue->next       = upvalue;

    if (prevUpvalue == NULL) {
        vm.openUpvalues = createdUpvalue;
    } else {
        prevUpvalue->next = createdUpvalue;
    }

    return createdUpvalue;
}

static void closeUpvalues(Value* last) {
    // Closes every open upvalue that points to that slot or any slot above it on the stack.
    while (vm.openUpvalues != NULL && vm.openUpvalues->location >= last) {
        ObjUpvalue* upvalue = vm.openUpvalues;
        upvalue->closed     = *upvalue->location;
        upvalue->location   = &upvalue->closed;
        vm.openUpvalues     = upvalue->next;
    }
}

static void defineMethod(ObjString* name) {
    Value     method = peek(0);
    ObjClass* klass  = AS_CLASS(peek(1));
    tableSet(&klass->methods, name, method);
    pop();
}

static bool isFalsey(Value value) { return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value)); }

static void concatenate() {
    // Concatenating two strings requires allocating a new character array on the heap, which can in
    // turn trigger a GC. Since we’ve already popped the operand strings by that point, they can
    // potentially be missed by the mark phase and get swept away. Instead of popping them off the
    // stack eagerly, we peek them, so they remain on the stack while the string is created. Once
    // that’s done, we can safely pop them off and replace them with the result.
    ObjString* b = AS_STRING(peek(0));
    ObjString* a = AS_STRING(peek(1));

    int   length = a->length + b->length;
    char* chars  = ALLOCATE(char, length + 1);
    memcpy(chars, a->chars, a->length);
    memcpy(chars + a->length, b->chars, b->length);
    chars[length] = '\0';

    ObjString* result = takeString(chars, length);
    pop();
    pop();
    push(OBJ_VAL(result));
}

static InterpretResult run() {
    CallFrame* frame = &vm.frames[vm.frameCount - 1];

#define READ_BYTE()     (*frame->ip++)
#define READ_SHORT()    (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))
#define READ_CONSTANT() (frame->closure->function->chunk.constants.values[READ_BYTE()])
#define READ_STRING()   AS_STRING(READ_CONSTANT())
#define BINARY_OP(valueType, op)                          \
    do {                                                  \
        if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
            runtimeError("Operands must be numbers.");    \
            return INTERPRET_RUNTIME_ERROR;               \
        }                                                 \
        double b = AS_NUMBER(pop());                      \
        double a = AS_NUMBER(pop());                      \
        push(valueType(a op b));                          \
    } while (false)

    for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
        printf("          ");
        for (Value* value = vm.stack; value < vm.stackTop; value++) {
            printf("[ ");
            printValue(*value);
            printf(" ]");
        }
        printf("\n");
        disassembleInstruction(&frame->closure->function->chunk,
                               (int)(frame->ip - frame->closure->function->chunk.code));
#endif
        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                push(constant);
                break;
            }
            case OP_NIL: push(NIL_VAL); break;
            case OP_TRUE: push(BOOL_VAL(true)); break;
            case OP_FALSE: push(BOOL_VAL(false)); break;
            case OP_POP: pop(); break;

            case OP_GET_LOCAL: {
                uint8_t slot = READ_BYTE();
                push(frame->slots[slot]);
                break;
            }

            case OP_SET_LOCAL: {
                uint8_t slot       = READ_BYTE();
                frame->slots[slot] = peek(0);
                break;
            }

            case OP_GET_GLOBAL: {
                ObjString* name = READ_STRING();
                Value      value;
                if (!tableGet(&vm.globals, name, &value)) {
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(value);
                break;
            }

            case OP_DEFINE_GLOBAL: {
                ObjString* name = READ_STRING();
                tableSet(&vm.globals, name, peek(0));
                pop();
                break;
            }

            case OP_SET_GLOBAL: {
                ObjString* name = READ_STRING();
                if (tableSet(&vm.globals, name, peek(0))) {
                    // The key already exists, cannot implicity define variables.
                    tableDelete(&vm.globals, name);
                    runtimeError("Undefined variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                // Do not pop the resuling value off the stack, since assignment is an expression
                // and may be nested inside another expression.
                break;
            }

            case OP_GET_UPVALUE: {
                uint8_t slot = READ_BYTE();
                push(*frame->closure->upvalues[slot]->location);
                break;
            }

            case OP_SET_UPVALUE: {
                uint8_t slot                              = READ_BYTE();
                *frame->closure->upvalues[slot]->location = peek(0);
                break;
            }

            case OP_GET_PROPERTY: {
                if (!IS_INSTANCE(peek(0))) {
                    runtimeError("Only instances have properties.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjInstance* instance = AS_INSTANCE(peek(0));
                ObjString*   name     = READ_STRING();

                Value value;
                if (tableGet(&instance->fields, name, &value)) {
                    pop();  // Instance.
                    push(value);
                    break;
                }

                if (!bindMethod(instance->klass, name)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case OP_SET_PROPERTY: {
                if (!IS_INSTANCE(peek(1))) {
                    runtimeError("Only instances have fields.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjInstance* instance = AS_INSTANCE(peek(1));
                tableSet(&instance->fields, READ_STRING(), peek(0));

                // A setter is itself an expression whose result is the assigned value,
                // so we need to leave that value on the stack.
                Value value = pop();
                pop();
                push(value);
                break;
            }

            case OP_GET_SUPER: {
                ObjString* name = READ_STRING();
                ObjClass* superclass = AS_CLASS(pop());
                if (!bindMethod(superclass, name)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }

            case OP_EQUAL: {
                Value a = pop();
                Value b = pop();
                push(BOOL_VAL(valuesEqual(a, b)));
                break;
            }

            case OP_GREATER: BINARY_OP(BOOL_VAL, >); break;
            case OP_LESS: BINARY_OP(BOOL_VAL, <); break;
            case OP_ADD: {
                if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
                    concatenate();
                } else if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
                    double b = AS_NUMBER(peek(0));
                    double a = AS_NUMBER(peek(1));
                    push(NUMBER_VAL(a + b));
                } else {
                    runtimeError("Operands must be two numbers or two strings.");
                }
                break;
            }
            case OP_SUBTRACT: BINARY_OP(NUMBER_VAL, -); break;
            case OP_MULTIPLY: BINARY_OP(NUMBER_VAL, *); break;
            case OP_DIVIDE: BINARY_OP(NUMBER_VAL, /); break;
            case OP_NOT: push(BOOL_VAL(isFalsey(pop()))); break;
            case OP_NEGATE: {
                if (!IS_NUMBER(peek(0))) {
                    runtimeError("Operand must be a number.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                push(NUMBER_VAL(-AS_NUMBER(pop())));
                break;
            }

            case OP_PRINT: {
                printValue(pop());
                printf("\n");
                break;
            }

            case OP_JUMP: {
                uint16_t offset = READ_SHORT();
                frame->ip += offset;
                break;
            }

            case OP_JUMP_IF_FALSE: {
                uint16_t offset = READ_SHORT();
                if (isFalsey(peek(0))) frame->ip += offset;
                break;
            }

            case OP_LOOP: {
                uint16_t offset = READ_SHORT();
                frame->ip -= offset;
                break;
            }

            case OP_CALL: {
                int argCount = READ_BYTE();
                if (!callValue(peek(argCount), argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                /* If callValue() is successful, there will be a new frame on the CallFrame stack
                for the called function. The run() function has its own cached pointer to the
                current frame, so we need to update that */
                frame = &vm.frames[vm.frameCount - 1];
                break;
                /* Since the bytecode dispatch loop reads from that frame variable, when the VM
                goes to execute the next instruction, it will read the ip from the newly called
                function’s CallFrame and jump to its code. */
            }

            case OP_INVOKE: {
                ObjString* method   = READ_STRING();
                int        argCount = READ_BYTE();
                if (!invoke(method, argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }

            case OP_SUPER_INVOKE: {
                ObjString* method = READ_STRING();
                int argCount = READ_BYTE();
                ObjClass* superclass = AS_CLASS(pop());
                if (!invokeFromClass(superclass, method, argCount)) {
                    return INTERPRET_RUNTIME_ERROR;
                }
                frame = &vm.frames[vm.frameCount - 1];
                break;
            }

            case OP_CLOSURE: {
                ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
                ObjClosure*  closure  = newClosure(function);
                push(OBJ_VAL(closure));
                for (int i = 0; i < closure->upvalueCount; i++) {
                    uint8_t isLocal = READ_BYTE();
                    uint8_t index   = READ_BYTE();
                    if (isLocal) {
                        closure->upvalues[i] = captureUpvalue(frame->slots + index);
                    } else {
                        closure->upvalues[i] = frame->closure->upvalues[index];
                    }
                }
                break;
            }

            case OP_CLOSE_UPVALUE: {
                closeUpvalues(vm.stackTop - 1);
                pop();
                break;
            }

            case OP_RETURN: {
                Value result = pop();  // Pop return value.

                closeUpvalues(frame->slots);  // Close every remaining open upvalue owned by the
                                              // returning function.
                vm.frameCount--;

                // If it's the last callframe, we're done with top level execution.
                if (vm.frameCount == 0) {
                    pop();
                    return INTERPRET_OK;
                }

                vm.stackTop = frame->slots;             // Discard call frame.
                push(result);                           // Push return value.
                frame = &vm.frames[vm.frameCount - 1];  // Update cached pointer to current frame.
                break;
            }

            case OP_CLASS: {
                push(OBJ_VAL(newClass(READ_STRING())));
                break;
            }

            case OP_INHERIT: {
                Value superclass = peek(1);
                if (!IS_CLASS(superclass)) {
                    runtimeError("Superclass must be a class.");
                    return INTERPRET_RUNTIME_ERROR;
                }

                ObjClass* subclass = AS_CLASS(peek(0));
                tableAddAll(&AS_CLASS(superclass)->methods, &subclass->methods);
                pop(); // Subclass.
                break;
            }

            case OP_METHOD: {
                defineMethod(READ_STRING());
                break;
            }
        }
    }
#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
}

InterpretResult interpret(const char* source) {
    ObjFunction* function = compile(source);
    if (function == NULL) return INTERPRET_COMPILE_ERROR;

    push(OBJ_VAL(function));
    ObjClosure* closure = newClosure(function);
    pop();
    push(OBJ_VAL(closure));
    callValue(OBJ_VAL(closure), 0);

    return run();
}