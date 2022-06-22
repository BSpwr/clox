#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "table.h"

#define STACK_MAX 256

typedef struct VM {
    Chunk* chunk;
    uint8_t* ip;
    Value stack[STACK_MAX];
    Value* stackTop;

    Table globals;
    Table strings;

    Obj* objects;
} VM;

extern VM vm;

typedef enum InterpretResult {
    INTERPRET_OK,
    INTERPRET_COMPILE_ERROR,
    INTERPRET_RUNTIME_ERROR,
} InterpretResult;

void initVM();
static void resetStack();
void freeVM();
InterpretResult interpret(const char* source);
static InterpretResult run();
void push(Value value);
Value pop();
Value peek(int distance);

#endif