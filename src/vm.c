#include "vm.h"

#include <stdio.h>

#include "common.h"
#include "debug.h"
#include "compiler.h"

VM vm;

void initVM() { resetStack(); }

static void resetStack() { vm.stackTop = vm.stack; }

void freeVM() {}

InterpretResult interpret(const char* source) {
    compile(source);
    return INTERPRET_OK;
}

static InterpretResult run() {
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
#define BINARY_OP(op) \
    do { \
        Value b = pop(); \
        Value a = pop(); \
        push(a op b); \
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
        disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
#endif
        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_CONSTANT: {
                Value constant = READ_CONSTANT();
                push(constant);
                break;
            }
            case OP_ADD: {
                BINARY_OP(+);
                break;
            }
            case OP_SUBTRACT: {
                BINARY_OP(-);
                break;
            }
            case OP_MULTIPLY: {
                BINARY_OP(*);
                break;
            }
            case OP_DIVIDE: {
                BINARY_OP(/);
                break;
            }
            case OP_NEGATE: {
                push(-pop());
                break;
            }
            case OP_RETURN: {
                printValue(pop());
                printf("\n");
                return INTERPRET_OK;
            }
        }
    }
#undef READ_BYTE
#undef READ_CONSTANT
}

void push(Value value) { *vm.stackTop++ = value; }

Value pop() { return *--vm.stackTop; }
