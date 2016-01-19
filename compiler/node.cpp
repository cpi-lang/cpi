#include "node.h"

// NODE
Node::Node() : type(NodeType::NONE), data(new None{}), metadata(NodeMetadata()) { }
Node::Node(Node* n) : type(n->type), data(n->data), metadata(n->metadata) { }
Node::Node(NodeType _type, NodeData _data) : type(_type), data(_data), metadata(NodeMetadata()) { }
Node::Node(Symbol* s) : type(NodeType::SYMBOL), data(s), metadata(NodeMetadata()) { }
Node::Node(Constant* c) : type(NodeType::CONSTANT), data(c), metadata(NodeMetadata()) { }
Node::Node(string s) : type(NodeType::SYMBOL), data(new Symbol{s}), metadata(NodeMetadata()) { }
Node::Node(Declaration* d) : type(NodeType::DECLARATION), data(d), metadata(NodeMetadata()) { }
Node::Node(Parameter* p) : type(NodeType::PARAMETER), data(p), metadata(NodeMetadata()) { }
Node::Node(Argument* a) : type(NodeType::ARGUMENT), data(a), metadata(NodeMetadata()) { }
Node::Node(Module* m) : type(NodeType::MODULE), data(m), metadata(NodeMetadata()) { }
Node::Node(Assignment* a) : type(NodeType::ASSIGNMENT), data(a), metadata(NodeMetadata()) { }
Node::Node(TypeTuple* t) : type(NodeType::TYPETUPLE), data(t), metadata(NodeMetadata()) { }
Node::Node(TypeFunction* t) : type(NodeType::TYPEFUNCTION), data(t), metadata(NodeMetadata()) { }
Node::Node(TypePointer* t) : type(NodeType::TYPEPOINTER), data(t), metadata(NodeMetadata()) { }
Node::Node(TypeConstant* t) : type(NodeType::TYPECONSTANT), data(t), metadata(NodeMetadata()) { }
Node::Node(Apply* a) : type(NodeType::APPLY), data(a), metadata(NodeMetadata()) { }
Node::Node(TypeDefinition* t) : type(NodeType::TYPEDEFINITION), data(t), metadata(NodeMetadata()) { }
Node::Node(Rec* r) : type(NodeType::REC), data(r), metadata(NodeMetadata()) { }
Node::Node(Function* f) : type(NodeType::FUNCTION), data(f), metadata(NodeMetadata()) { }
Node::Node(Ret* r) : type(NodeType::RET), data(r), metadata(NodeMetadata()) { }
Node::Node(Import* i) : type(NodeType::IMPORT), data(i), metadata(NodeMetadata()) { }
Node::Node(Require* r) : type(NodeType::REQUIRE), data(r), metadata(NodeMetadata()) { }
Node::Node(Typeof* t) : type(NodeType::TYPEOF), data(t), metadata(NodeMetadata()) { }
Node::Node(Dereference* d) : type(NodeType::DEREFERENCE), data(d), metadata(NodeMetadata()) { }
Node::Node(AddressOf* a) : type(NodeType::ADDRESSOF), data(a), metadata(NodeMetadata()) { }
Node::Node(If* i) : type(NodeType::IF), data(i), metadata(NodeMetadata()) { }
Node::Node(Do* d) : type(NodeType::DO), data(d), metadata(NodeMetadata()) { }
Node::Node(While* w) : type(NodeType::WHILE), data(w), metadata(NodeMetadata()) { }
Node::Node(Defer* d) : type(NodeType::DEFER), data(d), metadata(NodeMetadata()) { }
Node::Node(BinOp* b) : type(NodeType::BINOP), data(b), metadata(NodeMetadata()) { }
Node::Node(Pipe* p) : type(NodeType::PIPE), data(p), metadata(NodeMetadata()) { }
Node::Node(ValueTuple* v) : type(NodeType::VALUETUPLE), data(v), metadata(NodeMetadata()) { }
Node::Node(Dot* d) : type(NodeType::DOT), data(d), metadata(NodeMetadata()) { }
Node::Node(Neg* n) : type(NodeType::NEG), data(n), metadata(NodeMetadata()) { }
Node::Node(Cast* c) : type(NodeType::CAST), data(c), metadata(NodeMetadata()) { }
Node::Node(Heap* h) : type(NodeType::HEAP), data(h), metadata(NodeMetadata()) { }
Node::Node(Sizeof* s) : type(NodeType::SIZEOF), data(s), metadata(NodeMetadata()) { }
Node::Node(Assert* a) : type(NodeType::ASSERT), data(a), metadata(NodeMetadata()) { }
Node::Node(Not* n) : type(NodeType::NOT), data(n), metadata(NodeMetadata()) { }
Node::Node(Label *l) : type(NodeType::LABEL), data(l), metadata(NodeMetadata()) { }
Node::Node(Jump *j) : type(NodeType::JUMP), data(j), metadata(NodeMetadata()) { }
Node::Node(Question *q) : type(NodeType::QUESTION), data(q), metadata(NodeMetadata()) { }
Node::Node(ParseError* e) : type(NodeType::PARSEERROR), data(e), metadata(NodeMetadata()) { }
Node::Node(Unit* u) : type(NodeType::UNIT), data(u), metadata(NodeMetadata()) { }
Node::Node(StringLiteral* s) : type(NodeType::STRINGLITERAL), data(s), metadata(NodeMetadata()) { }
Node::Node(Character *c) : type(NodeType::CHARACTER), data(c), metadata(NodeMetadata()) { }
Node::Node(Spread* s) : type(NodeType::SPREAD), data(s), metadata(NodeMetadata()) { }

bool isStatic(Node *n)
{
    if (n->type == NodeType::MODULE
        || n->type == NodeType::TYPEDEFINITION)
    {
        return true;
    }

    return n->type == NodeType::DECLARATION
           && (n->data.declaration->external
               || n->data.declaration->global);

}

// Scope
Node *Scope::resolve(string s, Region r, bool childImportsNonStatic)
{
    auto found = values.find(s);
    if (found != values.end())
    {
        if (childImportsNonStatic)
        {
            return found->second;
        }
        else if (isStatic(found->second))
        {
            return found->second;
        }
        return none();
    }

    // Try parent scope
    if (parent == nullptr)
    {
        return none(r);
    }

    return parent->resolve(s, r, importsNonStatic && childImportsNonStatic);
}

Node* node()
{
    return new Node{};
}

// NONE
Node* none(Region r)
{
    auto ret = new Node{};
    ret->metadata.region = r;
    return ret;
}

// SYMBOL
Symbol::Symbol() : resolved(nullptr), needs_resolution(true) { }
Symbol::Symbol(string _value) : value(_value), resolved(nullptr), needs_resolution(true) { }
Symbol::Symbol(string _value, Node* resolved) : value(_value), resolved(resolved), needs_resolution(true) { }
Symbol::Symbol(Symbol* s) : value(s->value), resolved(nullptr), needs_resolution(true) { }
Symbol::Symbol(Symbol* s, Node* resolved) : value(s->value), resolved(resolved), needs_resolution(true) { }
Symbol::~Symbol() { }

Node* symbol(string value, Lexer* l)
{
    auto region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return symbol(value, region);
}
Node* symbol(string value, Region r)
{
    auto ret = new Node(new Symbol{value, none(r)});
    ret->metadata.region = r;
    return ret;
}

// CONSTANT
Node* constInt(ConstantType type, long value, Lexer* l)
{
    assert(
        type == ConstantType::I8
        || type == ConstantType::I16
        || type == ConstantType::I32
        || type == ConstantType::I64
        || type == ConstantType::U8
        || type == ConstantType::U16
        || type == ConstantType::U32
        || type == ConstantType::U64
    );

    auto constant = new Constant();
    constant->type = ConstantType::I32;
    constant->data.intvalue = value;
    auto ret = new Node(constant);
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* constInt(ConstantType type, long value, Region r)
{
    assert(
        type == ConstantType::I8
        || type == ConstantType::I16
        || type == ConstantType::I32
        || type == ConstantType::I64
        || type == ConstantType::U8
        || type == ConstantType::U16
        || type == ConstantType::U32
        || type == ConstantType::U64
    );

    auto constant = new Constant();
    constant->type = ConstantType::I32;
    constant->data.intvalue = value;
    auto ret = new Node(constant);
    ret->metadata.region = r;
    return ret;
}
Node* constFloat(ConstantType type, double value, Lexer* l)
{
    assert(type == ConstantType::F32 || type == ConstantType::F64);
    auto constant = new Constant();
    constant->type = ConstantType::F32;
    constant->data.floatvalue = value;
    auto ret = new Node(constant);
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* constFloat(ConstantType type, double value, Region r)
{
    assert(type == ConstantType::F32 || type == ConstantType::F64);
    auto constant = new Constant();
    constant->type = ConstantType::F32;
    constant->data.floatvalue = value;
    auto ret = new Node(constant);
    ret->metadata.region = r;
    return ret;
}
Node* constBool(bool value, Lexer* l)
{
    auto constant = new Constant();
    constant->type = ConstantType::BOOL;
    constant->data._bool = value;
    auto ret = new Node(constant);
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* constBool(bool value, Region r)
{
    auto constant = new Constant();
    constant->type = ConstantType::BOOL;
    constant->data._bool = value;
    auto ret = new Node(constant);
    ret->metadata.region = r;
    return ret;
}
Node* constString(string value, Lexer* l)
{
    auto constant = new Constant();
    constant->type = ConstantType::STRING;
    constant->data._string = value;
    auto ret = new Node(constant);
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* constString(string value, Region r)
{
    auto constant = new Constant();
    constant->type = ConstantType::STRING;
    constant->data._string = value;
    auto ret = new Node(constant);
    ret->metadata.region = r;
    return ret;
}
Node* constNil(Lexer* l)
{
    auto constant = new Constant();
    constant->type = ConstantType::NIL;
    auto ret = new Node(constant);
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node *constNil(Region r)
{
    auto constant = new Constant();
    constant->type = ConstantType::NIL;
    auto ret = new Node(constant);
    ret->metadata.region = r;
    return ret;
}
Node *constChar(char c, Region r)
{
    auto constant = new Constant();
    constant->type = ConstantType::CHAR;
    constant->data._char = c;
    auto ret = new Node(constant);
    ret->metadata.region = r;
    return ret;
}

Node *constTuple(ValueTuple v, Lexer *l)
{
    auto constant = new Constant();
    constant->type = ConstantType::TUPLE;
    constant->data.tuple = v;
    auto ret = new Node(constant);
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node *constTuple(ValueTuple v, Region r)
{
    auto constant = new Constant();
    constant->type = ConstantType::TUPLE;
    constant->data.tuple = v;
    auto ret = new Node(constant);
    ret->metadata.region = r;
    return ret;
}

// PARAMETER
Parameter::Parameter() { }

// ARGUMENT
Node* argument(string name, Node* value, Lexer* l)
{
    auto ret = new Node(new Argument{name, value});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* argument(string name, Node* value, Region r)
{
    auto ret = new Node(new Argument{name, value});
    ret->metadata.region = r;
    return ret;
}

// TYPE TUPLE
TypeTuple::TypeTuple() : opaque(nullptr), isAnonymous(true) { }
TypeTuple::TypeTuple(initializer_list<Node*> _types) : types(_types), opaque(nullptr), isAnonymous(true) { }
TypeTuple::TypeTuple(vector<Node*> _types) : types(_types), opaque(nullptr), isAnonymous(true) { }

Node* typeTuple(vector<Node*> types, Lexer* l)
{
    auto ret = new Node(new TypeTuple{types});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* typeTuple(vector<Node*> types, Region r)
{
    auto ret = new Node(new TypeTuple{types});
    ret->metadata.region = r;
    return ret;
}

// TYPE FUNCTION
Node* typeFunction(vector<Node*> input, Node* output, Lexer* l)
{
    auto region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return typeFunction(input, output, region);
}
Node* typeFunction(vector<Node*> input, Node* output, Region r)
{
    for (auto i = 0; i < input.size(); i++)
    {
        if (input[i]->type != NodeType::PARAMETER)
        {
            input[i] = parameter("", input[i], none(r), (unsigned int) i, r);
        }
    }

    auto ret = new Node(new TypeFunction{TypeTuple(input), output});
    ret->metadata.region = r;
    return ret;
}

// TYPE POINTER
Node* typePointer(Node* type, Lexer* l)
{
    auto ret = new Node(new TypePointer{type});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* typePointer(Node* type, Region r)
{
    auto ret = new Node(new TypePointer{type});
    ret->metadata.region = r;
    return ret;
}

// TYPE CONSTANT
Node* typeConstant(ConstantType type, Lexer* l)
{
    auto ret = new Node(new TypeConstant{type});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* typeConstant(ConstantType type, Region r)
{
    auto ret = new Node(new TypeConstant{type});
    ret->metadata.region = r;
    return ret;
}

// Constant
Node *constant(ConstantType type, ConstantData data, Region r)
{
    auto ret = new Node(new Constant{type, data});
    ret->metadata.region = r;
    return ret;
}

// DECLARATION
Node* declaration(string name, Node *type, DeclarationType dc, Lexer* l, Node* v)
{
    auto ret = new Node{new Declaration{name, type, v, dc, false, false}};
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* declaration(string name, Node *type, DeclarationType dc, Region r, Node* v)
{
    auto ret = new Node{new Declaration{name, type, v, dc, false, false}};
    ret->metadata.region = r;
    return ret;
}

// ASSIGNMENT
Node* assignment(Node *lhs, Node *rhs, Lexer* l)
{
    auto ret = new Node{new Assignment{lhs, rhs}};
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
};
Node* assignment(Node *lhs, Node *rhs, Region r)
{
    auto ret = new Node{new Assignment{lhs, rhs}};
    ret->metadata.region = r;
    return ret;
};

// APPLY
Node* apply(Node* function, vector<Node*> parameters, Lexer* l)
{
    auto ret = new Node(new Apply{function, parameters});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* apply(Node* function, vector<Node*> parameters, Region r)
{
    auto ret = new Node(new Apply{function, parameters});
    ret->metadata.region = r;
    return ret;
}

// FUNCTION
Node* function_(vector<Node*> parameters, vector<Node*> statements, Lexer* l)
{
    auto region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return function_(parameters, statements, region);
}
Node* function_(vector<Node*> parameters, vector<Node*> statements, Region r)
{
    auto ret = new Node(new Function{parameters, statements, none(r)});
    ret->metadata.region = r;
    ret->metadata.scope->isModuleLevel = false;
    return ret;
}

// REC
Node* rec(Lexer* l)
{
    auto ret = new Node(new Rec{});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* rec(Region r)
{
    auto ret = new Node(new Rec{});
    ret->metadata.region = r;
    return ret;
}

// RET
Node* ret(Node* s, Lexer* l)
{
    auto ret = new Node(new Ret{s});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* ret(Node* s, Region r)
{
    auto ret = new Node(new Ret{s});
    ret->metadata.region = r;
    return ret;
}

// REQUIRE
Node* require(string fileName, Region r)
{
    auto ret = new Node(new Require{fileName, nullptr});
    ret->metadata.region = r;
    return ret;
}
Node* require(string fileName, Lexer* l)
{
    auto ret = new Node(new Require{fileName, nullptr});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}

// IMPORT
Node* import(Node* n, Lexer* l)
{
    auto ret = new Node(new Import{n});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* import(Node* n, Region r)
{
    auto ret = new Node(new Import{n});
    ret->metadata.region = r;
    return ret;
}

// Label
Node *label(string s, Lexer *l)
{
    auto ret = new Node(new Label{s, false});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}

// Jump
Node *jump(string s, Lexer *l)
{
    auto ret = new Node(new Jump{s});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}

// TYPEOF
Typeof::Typeof() : value(node()), resolved(node()) { }

Node* typeof_(Node* n, bool is_rt, Lexer* l)
{
    auto ret = new Node(new Typeof{n, is_rt, none()});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}

Node* typeof_(Node* n, bool is_rt, Region r)
{
    auto ret = new Node(new Typeof{n, is_rt, none()});
    ret->metadata.region = r;
    return ret;
}

// DEREFERENCE
Node* dereference(Node* n, Lexer* l)
{
    auto ret = new Node(new Dereference{n});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* dereference(Node* n, Region r)
{
    auto ret = new Node(new Dereference{n});
    ret->metadata.region = r;
    return ret;
}

// ADDRESS OF
Node* addressOf(Node* n, Lexer* l)
{
    auto ret = new Node(new AddressOf{n});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* addressOf(Node* n, Region r)
{
    auto ret = new Node(new AddressOf{n});
    ret->metadata.region = r;
    return ret;
}

// If
Node* if_(If if_, Lexer* l)
{
    auto ret = new Node(new If{if_.if_branch, if_.elif_branches, if_.else_branch});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* if_(If if_, Region r)
{
    auto ret = new Node(new If{if_.if_branch, if_.elif_branches, if_.else_branch});
    ret->metadata.region = r;
    return ret;
}

// Do
Node *do_(Do do_, Lexer* l)
{
    auto ret = new Node(new Do{do_.identifier, do_.statements});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node *do_(Do do_, Region r)
{
    auto ret = new Node(new Do{do_.identifier, do_.statements});
    ret->metadata.region = r;
    return ret;
}

// Value Tuple
Node* valueTuple(vector<Node*> values, Lexer* l)
{
    auto ret = new Node(new ValueTuple{values});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* valueTuple(vector<Node*> values, Region r)
{
    auto ret = new Node(new ValueTuple{values});
    ret->metadata.region = r;
    return ret;
}

// While
Node* while_(Node* condition, vector<Node*> statements, Lexer* l)
{
    auto ret = new Node(new While{condition, statements});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* while_(Node* condition, vector<Node*> statements, Region r)
{
    auto ret = new Node(new While{condition, statements});
    ret->metadata.region = r;
    return ret;
}

// Defer
Node* defer(vector<Node*> statements, Lexer* l)
{
    auto ret = new Node(new Defer{statements});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* defer(vector<Node*> statements, Region r)
{
    auto ret = new Node(new Defer{statements});
    ret->metadata.region = r;
    return ret;
}

// Pipe
Node* pipe(Node* args, Node* func, Lexer* l)
{
    auto region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return pipe(args, func, region);
}
Node* pipe(Node* args, Node* func, Region r)
{
    auto ret = new Node(new Pipe{args, func, none(r)});
    ret->metadata.region = r;
    return ret;
}

// BinOp
Node* binop(Tok type, Node* lhs, Node* rhs, bool is_eq, Lexer* l)
{
    auto region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return binop(type, lhs, rhs, is_eq, region);
}
Node* binop(Tok type, Node* lhs, Node* rhs, bool is_eq, Region r)
{
    auto ret = new Node(new BinOp{toOp(type), lhs, rhs, is_eq});
    ret->metadata.region = r;
    return ret;
}

// Dot
Node* dot(Node* lhs, Node* rhs, Lexer* l)
{
    auto region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return dot(lhs, rhs, region);
}
Node* dot(Node* lhs, Node* rhs, Region r)
{
    auto ret = new Node(new Dot{lhs, rhs, none(r)});
    ret->metadata.region = r;
    return ret;
}

// Neg
Node* neg(Node* value, Lexer* l)
{
    auto ret = new Node(new Neg{value});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* neg(Node* value, Region r)
{
    auto ret = new Node(new Neg{value});
    ret->metadata.region = r;
    return ret;
}

// Cast
Node* cast(Node* type, Node* value, Lexer* l)
{
    auto ret = new Node(new Cast{type, value});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* cast(Node* type, Node* value, Region r)
{
    auto ret = new Node(new Cast{type, value});
    ret->metadata.region = r;
    return ret;
}

// Heap
Node* heap(Node* value, Lexer* l)
{
    auto ret = new Node(new Heap{value});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* heap(Node* value, Region r)
{
    auto ret = new Node(new Heap{value});
    ret->metadata.region = r;
    return ret;
}

// Siezof
Node* sizeof_(Node* type, Lexer* l)
{
    auto ret = new Node(new Sizeof{type});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* sizeof_(Node* type, Region r)
{
    auto ret = new Node(new Sizeof{type});
    ret->metadata.region = r;
    return ret;
}

// Assert
Node* assert_(Node* rvalue, Lexer* l)
{
    auto ret = new Node(new Assert{rvalue});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* assert_(Node* rvalue, Region r)
{
    auto ret = new Node(new Assert{rvalue});
    ret->metadata.region = r;
    return ret;
}

// Not
Node* not_(Node* value, Lexer* l)
{
    auto ret = new Node(new Not{value});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}

Node *question(Node *value, Lexer *l)
{
    auto ret = new Node(new Question{value});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}

// ParseError
Node* parseError(string message, Region region)
{
    auto ret = new Node(new ParseError{message, region});
    ret->metadata.region = region;
    return ret;
}
Node* parseError(string message, Lexer *l)
{
    auto region = Region{l->fileName, l->saved.top(), l->lastLoc};
    auto ret = new Node(new ParseError{message, region});
    ret->metadata.region = region;
    return ret;
}

// Unit
Node* unit(vector<Node*> modules, Lexer* l)
{
    auto ret = new Node(new Unit{modules});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    ret->metadata.scope->isModuleLevel = true;
    return ret;
}
Node* unit(vector<Node*> modules, Region r)
{
    auto ret = new Node(new Unit{modules});
    ret->metadata.scope->isModuleLevel = true;
    ret->metadata.region = r;
    return ret;
}

// Module
Node* module(string t, vector<Node*> _statements, Lexer* l)
{
    auto ret = new Node{new Module{t, _statements}};
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    ret->metadata.scope->isModuleLevel = true;
    return ret;
}
Node* module(string t, vector<Node*> _statements, Region r)
{
    auto ret = new Node{new Module{t, _statements}};
    ret->metadata.region = r;
    ret->metadata.scope->isModuleLevel = true;
    return ret;
}

// TypeDefinition
Node* typeDefinition(string name, Node* type, Lexer* l)
{
    auto ret = new Node(new TypeDefinition{name, type, nullptr});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* typeDefinition(string name, Node* type, Region r)
{
    auto ret = new Node(new TypeDefinition{name, type, nullptr});
    ret->metadata.region = r;
    return ret;
}

// StringLiteral
Node* stringLiteral(string s, Lexer* l)
{
    auto ret = new Node(new StringLiteral{s});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* stringLiteral(string s, Region r)
{
    auto ret = new Node(new StringLiteral{s});
    ret->metadata.region = r;
    return ret;
}

// Character
Node* character(char c, Lexer* l)
{
    auto ret = new Node(new Character{c});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* character(char c, Region r)
{
    auto ret = new Node(new Character{c});
    ret->metadata.region = r;
    return ret;
}

// Spread
Node* spread(Node* v, Lexer* l)
{
    auto ret = new Node(new Spread{v});
    ret->metadata.region = Region{l->fileName, l->saved.top(), l->lastLoc};
    return ret;
}
Node* spread(Node* v, Region r)
{
    auto ret = new Node(new Spread{v});
    ret->metadata.region = r;
    return ret;
}

string TYPETOSTRING(NodeType type)
{
    switch (type)
    {
        case NodeType::NONE:
            return "NONE";
        case NodeType::UNIT:
            return "UNIT";
        case NodeType::SYMBOL:
            return "symbol";
        case NodeType::MODULE:
            return "module";
        case NodeType::DECLARATION:
            return "declaration";
        case NodeType::PARAMETER:
            return "parameter";
        case NodeType::ARGUMENT:
            return "argument";
        case NodeType::ASSIGNMENT:
            return "assignment";
        case NodeType::TYPETUPLE:
            return "struct type";
        case NodeType::TYPEFUNCTION:
            return "type function";
        case NodeType::TYPEPOINTER:
            return "pointer type";
        case NodeType::TYPECONSTANT:
            return "constant type";
        case NodeType::APPLY:
            return "application";
        case NodeType::TYPEDEFINITION:
            return "type definition";
        case NodeType::RET:
            return "ret";
        case NodeType::FUNCTION:
            return "function";
        case NodeType::IMPORT:
            return "import";
        case NodeType::REQUIRE:
            return "require";
        case NodeType::CONSTANT:
            return "constant";
        case NodeType::SCOPE:
            return "scope";
        case NodeType::IF:
            return "if";
        case NodeType::BINOP:
            return "binop";
        case NodeType::DOT:
            return ".";
        case NodeType::STRINGLITERAL:
            return "string literal";
        case NodeType::PARSEERROR:
            return "unknown";
        default:
            assert(false);
    }

    assert(false);
    return "";
}

void reportError(ParseError e)
{
    cout << e.region;
#if COLORED_OUTPUT
    cout << KRED;
#endif
    cout << " error: ";
#if COLORED_OUTPUT
    cout << KNRM;
#endif
    cout << e.error << endl;
    for (auto note : e.notes)
    {
        cout << KMAG << "NOTE: " << KNRM << note << endl;
    }
}

// todo(chad): long-term, modify the print visitor to take a string buffer and a node
// and fill that string buffer from the node.  Then re-use that logic here.
string typeDescription(Node* n)
{
    auto ntype = n->type;

    if (ntype == NodeType::PARAMETER)
    {
        n = n->data.parameter->type;
        ntype = n->type;
    }

    switch (ntype)
    {
        case NodeType::SYMBOL:
            return n->data.symbol->value;
        case NodeType::TYPECONSTANT:
        {
            switch (n->data.typeConstant->type)
            {
                case ConstantType::I8:
                    return "i8";
                case ConstantType::I16:
                    return "i16";
                case ConstantType::I32:
                    return "i32";
                case ConstantType::I64:
                    return "i64";
                case ConstantType::U8:
                    return "u8";
                case ConstantType::U16:
                    return "u16";
                case ConstantType::U32:
                    return "u32";
                case ConstantType::U64:
                    return "u64";
                case ConstantType::F32:
                    return "f32";
                case ConstantType::F64:
                    return "f64";
                case ConstantType::BOOL:
                    return "bool";
                case ConstantType::STRING:
                    return "string";
                case ConstantType::CHAR:
                    return "char";
                case ConstantType::NIL:
                    return "nil";
                default:
                    assert(false);
                    break;
            }
        }
        case NodeType::TYPEPOINTER:
            return fmt::format("a pointer to {0}", typeDescription(n->data.typePointer->type));
        case NodeType::TYPEFUNCTION:
            return fmt::format("a function type returning {0}", typeDescription(n->data.typeFunction->output));
        case NodeType::TYPEOF:
            return typeDescription(n->data.typeof_->resolved);
        case NodeType::MODULE:
            return "a module";
        case NodeType::TYPEDEFINITION:
            return n->data.typeDefinition->name;
        case NodeType::TYPETUPLE:
            return "tuple type";
        case NodeType::NONE:
            return "none/void";
        default:
            assert(false);
    }

    assert(false);
    return "";
}
