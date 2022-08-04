#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "object.h"
#include "table.h"

#define FRAMES_MAX 64
#define STACK_MAX  (FRAMES_MAX * UINT8_COUNT)

typedef struct CallFrame {
    ObjClosure* closure;
    uint8_t*    ip;
    Value*      slots;  // Points into the VM's value stack at the first slot this closure can use.
} CallFrame;

typedef struct VM {
    CallFrame frames[FRAMES_MAX];
    int       frameCount;
    Value     stack[STACK_MAX];
    Value*    stackTop;

    Table       globals;
    Table       strings;
    ObjString*  initString;
    ObjUpvalue* openUpvalues;

    size_t bytesAllocated;
    size_t nextGC;

    Obj* objects;

    int   grayCount;
    int   grayCapacity;
    Obj** grayStack;
} VM;

extern VM vm;

typedef enum InterpretResult {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
} InterpretResult;

void            initVM();
static void     resetStack();
static void     defineNative(const char* name, NativeFn function);
void            freeVM();
InterpretResult interpret(const char* source);
void            push(Value value);
Value           pop();
Value           peek(int distance);

#endif