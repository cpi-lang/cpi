#ifndef NODE_H
#define NODE_H

#define COLORED_OUTOUT true

#include <iostream>
#include <unordered_map>
#include <vector>

#include <cppformat/format.h>

#include "lexer.h"

using namespace std;

// Node Stuff
struct Node;
struct NodeMetadata;
struct Scope;

struct None;
struct Symbol;
struct Constant;
struct Module;
struct Declaration;
struct Parameter;
struct Argument;
struct Assignment;
struct TypeTuple;
struct TypeFunction;
struct TypePointer;
struct TypeConstant;
struct Apply;
struct TypeDefinition;
struct Rec;
struct Function;
struct Ret;
struct Import;
struct Label;
struct Jump;
struct Require;
struct Typeof;
struct Dereference;
struct AddressOf;
struct If;
struct Do;
struct While;
struct Defer;
struct BinOp;
struct Pipe;
struct Neg;
struct Cast;
struct Heap;
struct Sizeof;
struct Assert;
struct Not;
struct Question;
struct ValueTuple;
struct Dot;
struct ParseError;
struct Unit;
struct StringLiteral;
struct Character;
struct Spread;

struct IfBranch;

// NONE
Node* none(Region r = Region{});

enum class NodeType
{
    NONE,
    SYMBOL,
    CONSTANT,
    MODULE,
    SCOPE,
    DECLARATION,
    PARAMETER,
    ARGUMENT,
    ASSIGNMENT,
    TYPETUPLE,
    TYPEFUNCTION,
    TYPEPOINTER,
    TYPECONSTANT,
    APPLY,
    TYPEDEFINITION,
    REC,
    FUNCTION,
    RET,
    IMPORT,
    REQUIRE,
    TYPEOF,
    DEREFERENCE,
    ADDRESSOF,
    IF,
    DO,
    WHILE,
    DEFER,
    BINOP,
    PIPE,
    QUESTION,
    VALUETUPLE,
    DOT,
    NEG,
    NOT,
    CAST,
    HEAP,
    SIZEOF,
    ASSERT,
    PARSEERROR,
    UNIT,
    STRINGLITERAL,
    CHARACTER,
    SPREAD,
    LABEL,
    JUMP
};

void reportError(ParseError e);

enum class ParseErrorType
{
    ERROR,
    WARNING
};

typedef struct ParseError
{
    string error;
    Region region;
    ParseErrorType errorType = ParseErrorType::ERROR;

    vector<string> notes;

    ParseError(string error, Region region) : error(error), region(region) { }
    ParseError(string error, Region region, ParseErrorType errorType) :
        error(error),
        region(region),
        errorType(errorType) { }
} ParseError;

string TYPETOSTRING(NodeType type);
string OPTOSTRING(OpType type);

typedef union NodeData
{
    None *none;
    Symbol *symbol;
    Constant *constant;
    Module *module;
    Declaration *declaration;
    Parameter* parameter;
    Argument* argument;
    Assignment *assignment;
    TypeTuple *typeTuple;
    TypeFunction *typeFunction;
    TypePointer *typePointer;
    TypeConstant *typeConstant;
    Apply *apply;
    TypeDefinition *typeDefinition;
    Rec *rec;
    Function *function;
    Ret *ret;
    Import *import;
    Require *require;
    Typeof *typeof_;
    Dereference *dereference;
    AddressOf *addressOf;
    If *if_;
    Do *do_;
    While *while_;
    Defer *defer;
    BinOp *binop;
    Pipe *pipe;
    ValueTuple *valueTuple;
    Dot *dot;
    Neg *neg;
    Not *not_;
    Question *question;
    Cast *cast;
    Heap *heap;
    Sizeof *sizeof_;
    Assert *assert_;
    ParseError *parseError;
    Unit *unit;
    StringLiteral *stringLiteral;
    Character *character;
    Spread *spread;
    Label *label;
    Jump *jump;

    NodeData(None* n) : none(n) { }
    NodeData(Symbol* s) : symbol(s) { }
    NodeData(Constant* c) : constant(c) { }
    NodeData(Module* m) : module(m) { }
    NodeData(Declaration* d) : declaration(d) { }
    NodeData(Parameter* p) : parameter(p) { }
    NodeData(Argument* a) : argument(a) { }
    NodeData(Assignment* a) : assignment(a) { }
    NodeData(TypeTuple* t) : typeTuple(t) { }
    NodeData(TypeFunction* t) : typeFunction(t) { }
    NodeData(TypePointer* t) : typePointer(t) { }
    NodeData(TypeConstant* c) : typeConstant(c) { }
    NodeData(Apply* a) : apply(a) { }
    NodeData(TypeDefinition* t) : typeDefinition(t) { }
    NodeData(Rec* r) : rec(r) { }
    NodeData(Function* f) : function(f) { }
    NodeData(Ret* r) : ret(r) { }
    NodeData(Import* i) : import(i) { }
    NodeData(Require* r) : require(r) { }
    NodeData(Typeof* t) : typeof_(t) { }
    NodeData(Dereference* d) : dereference(d) { }
    NodeData(AddressOf* a) : addressOf(a) { }
    NodeData(If* i) : if_(i) { }
    NodeData(Do *d) : do_(d) { }
    NodeData(While* w) : while_(w) { }
    NodeData(Defer* d) : defer(d) { }
    NodeData(BinOp* b) : binop(b) { }
    NodeData(Pipe* p) : pipe(p) { }
    NodeData(ValueTuple* v) : valueTuple(v) { }
    NodeData(Dot* d) : dot(d) { }
    NodeData(Neg* n) : neg(n) { }
    NodeData(Cast* c) : cast(c) { }
    NodeData(Heap *h) : heap(h) { }
    NodeData(Sizeof* s) : sizeof_(s) { }
    NodeData(Assert* a) : assert_(a) { }
    NodeData(Not* n) : not_(n) { }
    NodeData(Question *q) : question(q) { }
    NodeData(ParseError* e) : parseError(e) { }
    NodeData(Unit* u) : unit(u) { }
    NodeData(StringLiteral* s) : stringLiteral(s) { }
    NodeData(Character *c) : character(c) { }
    NodeData(Spread* s) : spread(s) { }
    NodeData(Label *l) : label(l) { }
    NodeData(Jump *j) : jump(j) { }
} NodeData;

typedef struct Scope
{
    unordered_map<string, Node*> values;
    vector<Defer *> deferreds;
    Scope* parent;
    bool isModuleLevel = false;
    bool importsNonStatic = true;

    Scope() : values({}), parent(nullptr) {}
    Scope(unordered_map<string, Node*> _values) :
        values(_values),
        parent(nullptr) { }
    Scope(unordered_map<string, Node*> _values, Scope* _parent) :
        values(_values),
        parent(_parent) { }

    Node* resolve(string s, Region r, bool childImportsNonStatic = true);
} Scope;

typedef struct NodeMetadata
{
    Scope *scope = new Scope();
    vector<Node *> annotations = {};
    void *llvm = nullptr;
    Region region;
} NodeMetadata;

typedef struct Node
{
    NodeType type;
    NodeData data;
    NodeMetadata metadata = {};

    Node();
    Node(Node *n);
    Node(NodeType _type, NodeData _data);
    Node(string s);
    Node(Symbol *s);
    Node(Constant *c);
    Node(Declaration *d);
    Node(Parameter *p);
    Node(Argument *a);
    Node(Module *m);
    Node(Assignment *a);
    Node(TypeTuple *t);
    Node(TypeFunction *t);
    Node(TypePointer *t);
    Node(TypeConstant *c);
    Node(Apply *a);
    Node(TypeDefinition *t);
    Node(Rec *r);
    Node(Function *f);
    Node(Ret *r);
    Node(Import *i);
    Node(Require *r);
    Node(Typeof *t);
    Node(Dereference *d);
    Node(AddressOf *a);
    Node(If *i);
    Node(Do *d);
    Node(While *w);
    Node(Defer *d);
    Node(BinOp *b);
    Node(Pipe *p);
    Node(ValueTuple *v);
    Node(Dot *d);
    Node(Neg *n);
    Node(Cast *c);
    Node(Heap *h);
    Node(Sizeof *s);
    Node(Assert *a);
    Node(Not *n);
    Node(Label *l);
    Node(Jump *j);
    Node(Question *q);
    Node(ParseError *e);
    Node(Unit* u);
    Node(StringLiteral *s);
    Node(Character *c);
    Node(Spread *s);

    void insertIntoScope(string key, Node *value)
    {
        // if key already exists then there's an error
        auto found = metadata.scope->values.find(key);
        if (found != metadata.scope->values.end())
        {
            auto error = ParseError{fmt::format("redeclaration of symbol {}", key), value->metadata.region};
            error.notes.push_back(fmt::format("previous declaration was here: {}", found->second->metadata.region));
        }

        metadata.scope->values[key] = value;
    }

    void setParentScope(Node *parent)
    {
        // functions get their own scopes
        metadata.scope->parent = parent->metadata.scope;

        // sanity check -- make sure scope's parent or grandparent are not equal to the scope itself.
        if (metadata.scope == metadata.scope->parent)
        {
            assert(false);
        }
        if (metadata.scope->parent != nullptr
            && metadata.scope->parent == metadata.scope->parent->parent)
        {
            assert(false);
        }

        if (type == NodeType::FUNCTION)
        {
            // nothing directly inside a function is at module level
            metadata.scope->isModuleLevel = false;

            // can only import static things from outside this function
            metadata.scope->importsNonStatic = false;

            return;
        }

        metadata.scope->isModuleLevel = parent->metadata.scope->isModuleLevel;
    }
} Node;

typedef struct None { } None;

typedef struct Symbol
{
    string value;
    Node* resolved;

    // TODO(chad): @HACK
    bool needs_resolution;

    Symbol();
    Symbol(string _value);
    Symbol(string _value, Node* resolved);
    Symbol(Symbol* s);
    Symbol(Symbol* s, Node* resolved);
    ~Symbol();
} Symbol;

typedef struct Spread
{
    Node* value;
} Spread;

enum class ConstantType
{
    None,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    BOOL,
    CHAR,
    STRING,
    NIL,
    TUPLE
};

typedef struct ValueTuple
{
    vector<Node*> values; // should always be PARAMETER
} ValueTuple;

typedef union ConstantData
{
    long intvalue;
    double floatvalue;
    bool _bool;
    string _string;
    char _char;
    void *nil;
    ValueTuple tuple;

    ConstantData() { }
    ConstantData(const ConstantData& cd)
    {
        memcpy(this, (void *) &cd, sizeof(ConstantData));
    }
    ~ConstantData() { }
} ConstantData;

typedef struct Constant
{
    ConstantType type;
    ConstantData data;
} Constant;

typedef struct Module
{
    string name;
    vector<Node*> statements;
} Module;

enum class DeclarationType
{
    IMMUTABLE,
    MUTABLE
};

typedef struct Declaration
{
    string name;
    Node* type;
    Node* initialValue;
    DeclarationType declarationType;
    bool external;
    bool global;
} Declaration;

typedef struct Parameter
{
    string name;
    Node* type;
    Node* initialValue;
    unsigned int order;

    Parameter();

    template<typename U, typename V>
    Parameter(string t, U u, V v, unsigned int order);
} Parameter;

typedef struct Argument
{
    string name;
    Node* value;
} Argument;

typedef struct Assignment
{
    Node* lhs;
    Node* rhs;
} Assignment;

typedef struct TypeTuple
{
    vector<Node*> types;
    void* opaque;
    bool isAnonymous;

    TypeTuple();
    TypeTuple(vector<Node*> _types);
    TypeTuple(initializer_list<Node*> _types);
} TypeTuple;

typedef struct TypeFunction
{
    TypeTuple input;
    Node* output;
} TypeFunction;

typedef struct TypePointer
{
    Node* type;
} TypePointer;

typedef struct TypeConstant
{
    ConstantType type;
} TypeConstant;

typedef struct Apply
{
    Node *operator_;
    vector<Node*> operands; // should always be NodeType::Argument
} Apply;

typedef struct TypeDefinition
{
    string name;
    Node* type;
    void* resolved;
} TypeDefinition;

typedef struct Rec
{
    Node *resolved;
} Rec;

typedef struct Function
{
    vector<Node*> parameters; // should always be NodeType::Parameter
    vector<Node*> block;

    Node *type;
} Function;

typedef struct Ret
{
    Node* value;
} Ret;

typedef struct Require
{
    string fileName;
    Node* resolved;
} Require;

typedef struct Import
{
    Node* value;
} Import;

typedef struct Label
{
    string name;

    // todo(chad): keep track of this externally
    bool created;
} Label;

typedef struct Jump
{
    string name;
    Node *resolved;
} Jump;

typedef struct Typeof
{
    Node* value;
    Node* resolved;

    // we reuse the same struct for the 'typeof' operator and the or 'rettype' operator,
    // which gets the return type of a function
    bool is_rt;

    Typeof();
    Typeof(Node* _value, bool is_rt) : value(_value), is_rt(is_rt) { }
    Typeof(Node* _value, bool is_rt, Node* _resolved) : value(_value), is_rt(is_rt), resolved(_resolved) { }
} Typeof;

typedef struct Dereference
{
    Node* value;
} Dereference;

typedef struct AddressOf
{
    Node* value;
} AddressOf;

typedef struct IfBranch
{
    Node* condition;
    vector<Node*> statements;

    IfBranch() : condition(none()) { }
} IfBranch;

typedef struct If
{
    IfBranch* if_branch;
    vector<IfBranch*> elif_branches;
    IfBranch* else_branch;

    If() :
        if_branch(new IfBranch{}),
        else_branch(new IfBranch{}) { }

    If(IfBranch* if_branch, vector<IfBranch*> elif_branches, IfBranch* else_branch) :
        if_branch(if_branch),
        elif_branches(elif_branches),
        else_branch(else_branch) { }
} If;

typedef struct Do
{
    Node *identifier;
    vector<Node *> statements;
} Do;

typedef struct While
{
    Node* condition;
    vector<Node*> statements;
} While;

typedef struct Defer
{
    vector<Node*> statements;
} Defer;

typedef struct BinOp
{
    OpType type;
    Node* lhs;
    Node* rhs;

    // if true, the doing '+=' instead of '+' for example
    bool is_eq;
} BinOp;

typedef struct Pipe
{
    Node* args;
    Node* func;

    Node* resolved; // resolved apply
} Pipe;

typedef struct Dot
{
    Node* lhs;
    Node* rhs;

    Node* resolved;
} Dot;

typedef struct Neg
{
    Node* value;
} Neg;

typedef struct Cast
{
    Node* type;
    Node* value;
} Cast;

typedef struct Heap
{
    Node *value;
} Heap;

typedef struct Sizeof
{
    Node* type;
} Sizeof;

typedef struct Assert
{
    Node* value;
} Assert;

typedef struct Not
{
    Node* value;
} Not;

typedef struct Question
{
    Node *value;
} Question;

typedef struct Unit
{
    vector<Node*> modules;
} Unit;

typedef struct StringLiteral
{
    string s;
    Node *resolved;
} StringLiteral;

typedef struct Character
{
    char c;
} Character;

// SYMBOL
Node* symbol(string value, Lexer* l);
Node* symbol(string value, Region r);

// Constant
Node* constInt(ConstantType type, long value, Lexer* l);
Node* constInt(ConstantType type, long value, Region r);

Node* constFloat(ConstantType type, double value, Lexer* l);
Node* constFloat(ConstantType type, double value, Region r);

Node* constBool(bool value, Lexer* l);
Node* constBool(bool value, Region r);

Node *constChar(char c, Region r);

Node* constString(string value, Lexer* l);
Node* constString(string value, Region r);

Node* constNil(Lexer* l);
Node *constNil(Region r);

Node *constTuple(ValueTuple v, Lexer *l);
Node *constTuple(ValueTuple v, Region r);

// MODULE
Node* module(string t, vector<Node*> _statements, Lexer* l);
Node* module(string t, vector<Node*> _statements, Region r);

// DECLARATION
Node* declaration(string name, Node *type, DeclarationType dc, Lexer* l, Node* v = none());
Node* declaration(string name, Node *type, DeclarationType dc, Region r, Node* v = none());

// PARAMETER
template<typename U, typename V>
Parameter::Parameter(string t, U u, V v, unsigned int _order) :
    name(t),
    type(new Node{u}),
    initialValue(new Node{v}),
    order(_order) { }

template<typename U, typename V = Node*>
Node* parameter(string t, U u, V v, unsigned int order, Lexer* l)
{
    auto ret = new Node{new Parameter{t, u, v, order}};
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
template<typename T, typename U, typename V = Node*>
Node* parameter(T name, U type, V initialValue, unsigned int order, Region r)
{
    auto ret = new Node{new Parameter{name, type, initialValue, order}};
    ret->metadata.region = r;
    return ret;
}

// Argument
Node* argument(string name, Node* value, Lexer* l);
Node* argument(string name, Node* value, Region r);

// ASSIGNMENT
Node* assignment(Node *lhs, Node *rhs, Lexer* l);
Node* assignment(Node *lhs, Node *rhs, Region r);

// TYPE TUPLE
Node* typeTuple(vector<Node*> types, Lexer* l);
Node* typeTuple(vector<Node*> types, Region r);

// TYPE FUNCTION
Node* typeFunction(vector<Node*> input, Node* output, Lexer* l);
Node* typeFunction(vector<Node*> input, Node* output, Region r);

// TYPE POINTER
Node* typePointer(Node* type, Lexer* l);
Node* typePointer(Node* type, Region r);

// TYPE CONSTANT
Node* typeConstant(ConstantType type, Lexer* l);
Node* typeConstant(ConstantType type, Region r);

// Constant
Node *constant(ConstantType type, ConstantData data, Region r);

// APPLY
Node* apply(Node* function, vector<Node*> parameters, Lexer* l);
Node* apply(Node* function, vector<Node*> parameters, Region r);

// TYPE DEFINITION
Node* typeDefinition(string name, Node* type, Lexer* l);
Node* typeDefinition(string name, Node* type, Region r);

// FUNCTION
Node* function_(vector<Node*> parameters, vector<Node*> statements, Lexer* l);
Node* function_(vector<Node*> parameters, vector<Node*> statements, Region r);

// REC
Node* rec(Lexer* l);
Node* rec(Region r);

// RET
Node* ret(Node* s, Lexer* l);
Node* ret(Node* s, Region r);

// IMPORT
Node* import(Node* n, Lexer* l);
Node* import(Node* n, Region r);

// Label
Node *label(string s, Lexer *l);

// Jump
Node *jump(string s, Lexer *l);

// REQUIRE
Node* require(string fileName, Region r);
Node* require(string fileName, Lexer* l);

// Typeof
Node* typeof_(Node* n, bool is_rt, Lexer* l);
Node* typeof_(Node* n, bool is_rt, Region r);

// DEREFERENCE
Node* dereference(Node* n, Lexer* l);
Node* dereference(Node* n, Region r);

// ADDRESS OF
Node* addressOf(Node* n, Lexer* l);
Node* addressOf(Node* n, Region r);

// If
Node* if_(If if_, Lexer* l);
Node* if_(If if_, Region r);

// Do
Node* do_(Do do_, Lexer* l);
Node* do_(Do do_, Region r);

// While
Node* while_(Node* condition, vector<Node*> statements, Lexer* l);
Node* while_(Node* condition, vector<Node*> statements, Region r);

// Defer
Node* defer(vector<Node*> statements, Lexer* l);
Node* defer(vector<Node*> statements, Region r);

// Value Tuple
Node* valueTuple(vector<Node*> values, Lexer* l);
Node* valueTuple(vector<Node*> values, Region r);

// BinOp
Node* binop(Tok type, Node* lhs, Node* rhs, bool is_eq, Lexer* l);
Node* binop(Tok type, Node* lhs, Node* rhs, bool is_eq, Region r);

// Pipe
Node* pipe(Node* args, Node* func, Lexer* l);
Node* pipe(Node* args, Node* func, Region r);

// Dot
Node* dot(Node* lhs, Node* rhs, Lexer* l);
Node* dot(Node* lhs, Node* rhs, Region r);

// Neg
Node* neg(Node* value, Lexer* l);
Node* neg(Node* value, Region r);

// Cast
Node* cast(Node* type, Node* value, Lexer* l);
Node* cast(Node* type, Node* value, Region r);

// Heap
Node* heap(Node* value, Lexer* l);
Node* heap(Node* value, Region r);

// Sizeof
Node* sizeof_(Node* type, Lexer* l);
Node* sizeof_(Node* type, Region r);

// Assert
Node* assert_(Node* rvalue, Lexer* l);
Node* assert_(Node* rvalue, Region r);

// Not
Node* not_(Node* value, Lexer* l);

// Question
Node *question(Node *value, Lexer *l);

// ParseError
Node* parseError(string message, Region region);
Node* parseError(string message, Lexer *l);

// Unit
Node* unit(vector<Node*> modules, Lexer* l);
Node* unit(vector<Node*> modules, Region r);

// StringLiteral
Node* stringLiteral(string s, Lexer* l);
Node* stringLiteral(string s, Region r);

// Character
Node *character(char c, Lexer *l);
Node* character(char c, Region r);

// Spread
Node* spread(Node* v, Lexer* l);
Node* spread(Node* v, Region r);

// todo(chad): might want to turn this into a generic nodeToString at some point, for printing out nodes for error messages
string typeDescription(Node* n);

#endif
