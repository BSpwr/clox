#include "memory.h"

#include "chunk.h"
#include "compiler.h"
#include "object.h"
#include "vm.h"

#ifdef DEBUG_LOG_GC
#include <stdio.h>

#include "debug.h"
#endif

void* reallocate(void* pointer, size_t oldSize, size_t newSize) {
    vm.bytesAllocated += newSize - oldSize;

    if (newSize > oldSize) {
#ifdef DEBUG_STRESS_GC
        collectGarbage();
#endif

        if (vm.bytesAllocated > vm.nextGC) {
            collectGarbage();
        }
    }
    if (newSize == 0) {
        free(pointer);
        return NULL;
    }

    void* result = realloc(pointer, newSize);
    if (result == NULL) exit(1);
    return result;
}

static void markRoots() {
    // Most roots are local variables or temporaries sitting right in the VM's stack
    // So start by walking that.
    for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
        markValue(*slot);
    }

    // Most function call state lives in the value stack, but the VM maintains a separate stack of
    // CallFrames. Each CallFrame contains a pointer to the closure being called. The VM uses those
    // pointers to access constants and upvalues, so those closures need to be kept around too.
    for (int i = 0; i < vm.frameCount; i++) {
        markObject((Obj*)vm.frames[i].closure);
    }

    // The open upvalue list is another set of values that the VM can directly reach.
    for (ObjUpvalue* upvalue = vm.openUpvalues; upvalue != NULL; upvalue = upvalue->next) {
        markObject((Obj*)upvalue);
    }

    // The other main sources of roots are the global variables.
    // Those live in a hash table owned by the VM.
    markTable(&vm.globals);

    // The compiler itself periodically grabs memory from the heap for literals and the constant
    // table. If the GC runs while we’re in the middle of compiling, then any values the
    // compiler directly accesses need to be treated as roots too.
    markCompilerRoots();
}

static void blackenObject(Obj* object);
static void traceReferences() {
    while (vm.grayCount > 0) {
        Obj* object = vm.grayStack[--vm.grayCount];
        blackenObject(object);
    }
}

static void sweep() {
    Obj* previous = NULL;
    Obj* object   = vm.objects;
    while (object != NULL) {
        if (object->isMarked) {
            object->isMarked = false;
            previous         = object;
            object           = object->next;
        } else {
            Obj* unreached = object;

            object = object->next;
            if (previous != NULL) {
                previous->next = object;
            } else {
                vm.objects = object;
            }

            freeObject(unreached);
        }
    }
}

void markObject(Obj* object) {
    if (object == NULL) return;
    if (object->isMarked) return;

#ifdef DEBUG_LOG_GC
    printf("%p mark ", (void*)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif

    object->isMarked = true;

    // We’ll create a separate worklist to keep track of all of the gray objects.
    // When an object turns gray, in addition to setting the mark field,
    // we’ll also add it to the worklist.
    if (vm.grayCapacity < vm.grayCount + 1) {
        vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
        vm.grayStack    = realloc(vm.grayStack, sizeof(Obj*) * vm.grayCapacity);
        // If we can’t create or grow the gray stack, then we can’t finish the garbage collection.
        if (vm.grayStack == NULL) exit(1);
    }

    vm.grayStack[vm.grayCount++] = object;
}

void markValue(Value value) {
    if (!IS_OBJ(value)) return;
    markObject(AS_OBJ(value));
}

static void markArray(ValueArray* array) {
    for (int i = 0; i < array->count; i++) {
        markValue(array->values[i]);
    }
}

static void blackenObject(Obj* object) {
#ifdef DEBUG_LOG_GC
    printf("%p blacken ", (void*)object);
    printValue(OBJ_VAL(object));
    printf("\n");
#endif
    switch (object->type) {
        case OBJ_NATIVE: break;
        case OBJ_STRING: break;
        case OBJ_UPVALUE: {
            // Since the value is no longer on the stack,
            // we need to make sure we trace the reference to it from the upvalue.
            markValue(((ObjUpvalue*)object)->closed);
            break;
        }
        case OBJ_FUNCTION: {
            // Each function has a reference to an ObjString containing the function’s name.
            // The function has a constant table packed full of references to other objects.
            ObjFunction* function = (ObjFunction*)object;
            markObject((Obj*)function->name);
            markArray(&function->chunk.constants);
            break;
        }
        case OBJ_CLOSURE: {
            // Each closure has a reference to the bare function it wraps,
            // as well as an array of pointers to the upvalues it captures.
            ObjClosure* closure = (ObjClosure*)object;
            markObject((Obj*)closure->function);
            for (int i = 0; i < closure->upvalueCount; i++) {
                markObject((Obj*)closure->upvalues[i]);
            }
            break;
        }
    }
}

void collectGarbage() {
#ifdef DEBUG_LOG_GC
    printf("-- gc begin\n");
    size_t before = vm.bytesAllocated;
#endif

    markRoots();
    traceReferences();
    /* To remove references to unreachable strings, we need to know which strings are
    unreachable. We don’t know that until after the mark phase has completed. But
    we can’t wait until after the sweep phase is done because by then the
    objects—and their mark bits—are no longer around to check. So the right time
    is exactly between the marking and sweeping phases */
    tableRemoveWhite(&vm.strings);
    sweep();

    vm.nextGC = vm.bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
    printf("-- gc end\n");
    printf("   collected %ld bytes from (%ld to %ld) next at %ld\n",
            before - vm.bytesAllocated, before, vm.bytesAllocated, vm.nextGC);
#endif
}

void freeObject(Obj* object) {
#ifdef DEBUG_LOG_GC
    printf("%p free type %d\n", (void*)object, object->type);
#endif

    switch (object->type) {
        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*)object;
            FREE_ARRAY(ObjUpvalue*, closure->upvalues, closure->upvalueCount);
            FREE(ObjClosure, object);
            break;
        }
        case OBJ_FUNCTION: {
            ObjFunction* function = (ObjFunction*)(object);
            freeChunk(&function->chunk);
            FREE(ObjFunction, object);
            break;
        }
        case OBJ_NATIVE: FREE(ObjNative, object); break;
        case OBJ_STRING: {
            ObjString* string = (ObjString*)(object);
            FREE_ARRAY(char, string->chars, string->length + 1);
            FREE(ObjString, object);
            break;
        }
        case OBJ_UPVALUE: FREE(ObjUpvalue, object); break;
    }
}

void freeObjects() {
    Obj* object = vm.objects;
    while (object->next != NULL) {
        Obj* next = object->next;
        freeObject(object);
        object = next;
    }

    free(vm.grayStack);
}
