#include "compiler.h"

#include <stdio.h>
#include <string.h>

#include "chunk.h"
#include "common.h"
#include "object.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct Parser {
    Token current;
    Token previous;
    bool  hadError;
    bool  panicMode;
} Parser;

typedef enum Precedence {
    PREC_NONE,
    PREC_ASSIGNMENT,  // =
    PREC_OR,          // or
    PREC_AND,         // and
    PREC_EQUALITY,    // == !=
    PREC_COMPARISON,  // < > <= >=
    PREC_TERM,        // + -
    PREC_FACTOR,      // * /
    PREC_UNARY,       // ! -
    PREC_CALL,        // . ()
    PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool canAssign);

typedef struct ParseRule {
    ParseFn    prefix;
    ParseFn    infix;
    Precedence precedence;
} ParseRule;

typedef struct Local {
    Token name;
    int   depth;
} Local;

typedef struct Compiler {
    Local locals[UINT8_COUNT];
    int   localCount;
    int   scopeDepth;
} Compiler;

Parser parser;

Compiler* current = NULL;

Chunk* compilingChunk;

static Chunk* currentChunk() { return compilingChunk; }

// ---- START FORWARD DECLARATIONS ---- //
// Error handling
static void errorAt(Token* token, const char* message);
static void error(const char* message);
static void errorAtCurrent(const char* message);

// Parser helpers
static void advance();
static void consume(TokenType type, const char* message);
static bool check(TokenType type);
static bool match(TokenType type);

// Emit bytecode helpers
static void emitByte(uint8_t byte);
static void emitBytes(uint8_t byte1, uint8_t byte2);
static void emitReturn();
static void emitConstant(Value value);

// Variable/constant/scope helpers
static uint8_t makeConstant(Value value);
static uint8_t identifierConstant(Token* name);
static void    declareVariable();
static bool    identifiersEqual(Token* a, Token* b);
static int resolveLocal(Compiler* compiler, Token* name);
static void    addLocal(Token name);
static void    namedVariable(Token name, bool canAssign);
static void    defineVariable(uint8_t global);
static void markInitialized();
static uint8_t parseVariable(const char* errorMessage);
static void    beginScope();
static void    endScope();

// Driving functions
static void initCompiler(Compiler* compiler);
bool        compile(const char* source, Chunk* chunk);
static void endCompiler();

// Recursive descent
static void expression();
static void statement();
static void declaration();
static void varDeclaration();
static void expressionStatement();
static void printStatement();
static void synchronize();
static void block();

// Pratt parsing
ParseRule         rules[];
static ParseRule* getRule(TokenType operatorType);
static void       parsePrecedence(Precedence precedence);
static void       binary(bool canAssign);
static void       unary(bool canAssign);
static void       grouping(bool canAssign);
static void       number(bool canAssign);
static void       string(bool canAssign);
static void       literal(bool canAssign);
static void       variable(bool canAssign);
// ---- END FORWARD DECLARATIONS ---- //

static void errorAt(Token* token, const char* message) {
    if (parser.panicMode) return;
    parser.panicMode = true;

    fprintf(stderr, "[line %d] Error\n", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        // Nothing.
    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

static void error(const char* message) { errorAt(&parser.previous, message); }

static void errorAtCurrent(const char* message) { errorAt(&parser.current, message); }

static void advance() {
    parser.previous = parser.current;

    for (;;) {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR) break;

        errorAtCurrent(parser.current.start);
    }
}

static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

static bool check(TokenType type) { return parser.current.type == type; }

static bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}

static void emitByte(uint8_t byte) { writeChunk(currentChunk(), byte, parser.previous.line); }

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static void emitReturn() { emitByte(OP_RETURN); }

// Adds a constant to the chunk's dynamic value array, and returns the index to constant
static uint8_t makeConstant(Value value) {
    int constant = addConstant(currentChunk(), value);
    if (constant > UINT8_MAX) {
        error("Too many constants in one chunk.");
        return 0;
    }

    return (uint8_t)constant;
}

static void emitConstant(Value value) { emitBytes(OP_CONSTANT, makeConstant(value)); }

static void initCompiler(Compiler* compiler) {
    compiler->localCount = 0;
    compiler->scopeDepth = 0;
    current              = compiler;
}

static void endCompiler() {
    emitReturn();
#ifdef DEBUG_PRINT_CODE
    if (!parser.hadError) {
        disassembleChunk(currentChunk(), "code");
    }
#endif
}

static void beginScope() { current->scopeDepth++; }

static void endScope() {
    current->scopeDepth--;

    while (current->localCount >= 0 &&
           current->locals[current->localCount - 1].depth > current->scopeDepth) {
        emitByte(OP_POP);
        current->localCount--;
    }
}

static void binary(bool canAssign) {
    // Remember the operator.
    TokenType operatorType = parser.previous.type;

    // Compile the right operand.
    ParseRule* rule = getRule(operatorType);
    parsePrecedence((Precedence)(rule->precedence + 1));

    // Emit the operator instruction.
    switch (operatorType) {
        case TOKEN_BANG_EQUAL: emitBytes(OP_EQUAL, OP_NOT); break;
        case TOKEN_EQUAL_EQUAL: emitByte(OP_EQUAL); break;
        case TOKEN_GREATER: emitByte(OP_GREATER); break;
        case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break;
        case TOKEN_LESS: emitByte(OP_LESS); break;
        case TOKEN_LESS_EQUAL: emitBytes(OP_GREATER, OP_NOT); break;
        case TOKEN_PLUS: emitByte(OP_ADD); break;
        case TOKEN_MINUS: emitByte(OP_SUBTRACT); break;
        case TOKEN_STAR: emitByte(OP_MULTIPLY); break;
        case TOKEN_SLASH: emitByte(OP_DIVIDE); break;
        default: return;  // Unreachable.
    }
}

static void unary(bool canAssign) {
    TokenType operatorType = parser.previous.type;

    // Compile the operand.
    parsePrecedence(PREC_UNARY);

    // Emit the operator instruction.
    switch (operatorType) {
        case TOKEN_BANG: emitByte(OP_NOT); break;
        case TOKEN_MINUS: emitByte(OP_NEGATE); break;
        default: return;  // Unreachable.
    }
}

static void grouping(bool canAssign) {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(bool canAssign) {
    double value = strtod(parser.previous.start, NULL);
    emitConstant(NUMBER_VAL(value));
}

static void string(bool canAssign) {
    emitConstant(OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

static void literal(bool canAssign) {
    switch (parser.previous.type) {
        case TOKEN_FALSE: emitByte(OP_FALSE); break;
        case TOKEN_NIL: emitByte(OP_NIL); break;
        case TOKEN_TRUE: emitByte(OP_TRUE); break;
        default: return;  // Unreachable.
    }
}

static void parsePrecedence(Precedence precedence) {
    advance();
    ParseFn prefixRule = getRule(parser.previous.type)->prefix;
    if (prefixRule == NULL) {
        error("Expect expression.");
        return;
    }

    /* If the variable happens to be the right-hand side of an infix operator,
    or the operand of a unary operator,
    then that containing expression is too high precedence to permit the '='.
    To fix this, variable() should only look for and consume the '='
    if it's in the context of a low precedence expression. The code that knows the current
    precedence is, logically enough, parsePrecedence() . The variable()
    function doesn’t need to know the actual level. It just cares that the precedence
    is low enough to allow assignment, so we pass that fact in as a Boolean.
    Since assignment is the lowest precedence expression, the only time we allow an
    assignment is when parsing an assignment expression or top-level expression
    like in an expression statement. */

    /* If the variable is nested inside some expression with
    higher precedence, canAssign will be false and this will ignore the = even
    if there is one there. Then this returns and eventually makes its way back to
    parsePrecedence(). */
    bool canAssign = precedence <= PREC_ASSIGNMENT;
    prefixRule(canAssign);

    /* If the next token is too low precedence, or isn’t an infix operator at all, we’re
    done. We’ve parsed as much expression as we can. Otherwise, we consume the
    operator and hand off control to the infix parser we found. It consumes
    whatever other tokens it needs (usually the right operand) and returns back to
    parsePrecedence() . Then we loop back around and see if the next token is
    also a valid infix operator that can take the entire preceding expression as its
    operand. We keep looping like that, crunching through infix operators and their
    operands until we hit a token that isn’t an infix operator or is too low
    precedence and stop. */

    // (lower precedence things can contain higher precedence, ex. assignment can have infix
    // operators in it)
    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule(canAssign);
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        error("Invalid assignment target.");
    }
}

// Take the given token and add it's lexeme to the chunk's constant table as a string,
// return the index.
static uint8_t identifierConstant(Token* name) {
    return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

/* This is the point where the compiler records the existence of the variable. We
only do this for locals, so if we’re in the top level global scope, we just bail out.
Because global variables are late bound, the compiler doesn’t keep track of
which declarations for them it has seen. */
static void declareVariable() {
    // Global variables are implicitly declared.
    if (current->scopeDepth == 0) return;

    Token* name = &parser.previous;
    // Look through the local variable stack to ensure that variables
    // are not being redefined in the same scope.
    for (int i = current->localCount - 1; i >= 0; i--) {
        Local* local = &current->locals[i];

        // This is a stack, if top of stack has lower scope depth than new variable
        // to be inserted, then it is not a redefinition and we can break out early.
        if (local->depth != -1 && local->depth < current->scopeDepth) {
            break;
        }
        
        // If the variable was declared but not assigned to before (depth == -1)
        // and the scope depth and name matches, that is invalid.
        if (identifiersEqual(name, &local->name)) {
            error("Already variable with this name in this scope.");
        }
    }

    addLocal(*name);
}

static bool identifiersEqual(Token* a, Token* b) {
    if (a->length != b->length) return false;
    return memcmp(a->start, b->start, a->length) == 0;
}

static int resolveLocal(Compiler* compiler, Token* name) {
    for (int i = compiler->localCount - 1; i >= 0; i--) {
        Local* local = &compiler->locals[i];
        if (identifiersEqual(name, &local->name)) {
            if (local->depth == -1) {
                error("Can't read local variable in it's own initializer.");
            }
            return i;
        }
    }

    return -1;
}

/* This creates a new Local and appends it to the compiler’s array of variables. It
stores the variable’s name and the depth of the scope that owns the variable. */
static void addLocal(Token name) {
    if (current->localCount == UINT8_COUNT) {
        error("Too many local variables in function.");
        return;
    }
    Local* local = &current->locals[current->localCount++];
    local->name  = name;
    local->depth = -1;
}

static void namedVariable(Token name, bool canAssign) {
    uint8_t getOp, setOp;
    int arg = resolveLocal(current, &name);
    if (arg != -1) {
        getOp = OP_GET_LOCAL;
        setOp = OP_SET_LOCAL;
    } else {
        arg = identifierConstant(&name);
        getOp = OP_GET_GLOBAL;
        setOp = OP_SET_GLOBAL;
    }

    if (canAssign && match(TOKEN_EQUAL)) {
        expression();
        emitBytes(setOp, (uint8_t)arg);
    } else {
        emitBytes(getOp, (uint8_t)arg);
    }
}

static void variable(bool canAssign) { namedVariable(parser.previous, canAssign); }

static uint8_t parseVariable(const char* errorMessage) {
    consume(TOKEN_IDENTIFIER, errorMessage);

    declareVariable();
    /* At runtime, locals aren’t looked up
    by name. There’s no need to stuff the variable’s name into the constant table so
    if the declaration is inside a local scope we return a dummy table index instead. */
    if (current->scopeDepth > 0) return 0;

    return identifierConstant(&parser.previous);
}

static void defineVariable(uint8_t global) {
    if (current->scopeDepth > 0) {markInitialized(); return;}
    emitBytes(OP_DEFINE_GLOBAL, global);
}

static void markInitialized() {
    current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void expression() { parsePrecedence(PREC_ASSIGNMENT); }

static void varDeclaration() {
    uint8_t slot = parseVariable("Expect variable name.");

    if (match(TOKEN_EQUAL)) {
        expression();
    } else {
        emitByte(OP_NIL);
    }
    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

    defineVariable(slot);
}

static void expressionStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    emitByte(OP_POP);
}

static void printStatement() {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after value.");
    emitByte(OP_PRINT);
}

static void synchronize() {
    parser.panicMode = false;

    while (parser.current.type != TOKEN_EOF) {
        if (parser.previous.type == TOKEN_SEMICOLON) return;

        switch (parser.current.type) {
            case TOKEN_CLASS:
            case TOKEN_FUN:
            case TOKEN_VAR:
            case TOKEN_FOR:
            case TOKEN_IF:
            case TOKEN_WHILE:
            case TOKEN_PRINT:
            case TOKEN_RETURN: return;
            default:  // Do nothing.
                break;
        }

        advance();
    }
}

static void declaration() {
    if (match(TOKEN_VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    if (parser.panicMode) synchronize();
}

static void block() {
    while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
        declaration();
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void statement() {
    if (match(TOKEN_PRINT)) {
        printStatement();
    } else if (match(TOKEN_LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else
        expressionStatement();
}

ParseRule rules[] = {
    [TOKEN_LEFT_PAREN]    = {grouping, NULL,   PREC_NONE      },
    [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE      },
    [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE      },
    [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE      },
    [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE      },
    [TOKEN_DOT]           = {NULL,     NULL,   PREC_NONE      },
    [TOKEN_MINUS]         = {unary,    binary, PREC_TERM      },
    [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM      },
    [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE      },
    [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR    },
    [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR    },
    [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE      },
    [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY  },
    [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE      },
    [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY  },
    [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE      },
    [TOKEN_STRING]        = {string,   NULL,   PREC_NONE      },
    [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE      },
    [TOKEN_AND]           = {NULL,     NULL,   PREC_NONE      },
    [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE      },
    [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE      },
    [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE      },
    [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE      },
    [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE      },
    [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE      },
    [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE      },
    [TOKEN_OR]            = {NULL,     NULL,   PREC_NONE      },
    [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE      },
    [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE      },
    [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE      },
    [TOKEN_THIS]          = {NULL,     NULL,   PREC_NONE      },
    [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE      },
    [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE      },
    [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE      },
    [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE      },
    [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE      },
};

static ParseRule* getRule(TokenType operatorType) { return &rules[operatorType]; }

bool compile(const char* source, Chunk* chunk) {
    initScanner(source);

    Compiler compiler;
    initCompiler(&compiler);

    compilingChunk = chunk;

    parser.hadError  = false;
    parser.panicMode = false;

    advance();

    while (!match(TOKEN_EOF)) {
        declaration();
    }

    endCompiler();
    return !parser.hadError;
}