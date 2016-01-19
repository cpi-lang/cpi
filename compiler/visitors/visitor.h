#ifndef VISITOR_H
#define VISITOR_H

#include <iostream>
#include <stack>
#include <vector>

#include <cppformat/format.h>
#include "json.hpp"

#include "../node.h"

using namespace std;

class Visitor
{
public:
    stack<Node*> nodes;
    vector<ParseError> errors;
    unordered_map<Node*, void *> visited;

    Node* typeOf(Node* n);
    bool typesMatch(Node* t1, Node* t2);

    virtual void visitNone(None n) { };
    virtual void visitSymbol(Symbol* s)
    {
        if (s->resolved != nullptr)
        {
            visit(s->resolved);
        }
    };
    virtual void visitConstant(Constant* c) { };
    virtual void visitModule(Module* m)
    {
        visitMany(m->statements);
    }
    virtual void visitDeclaration(Declaration* d)
    {
        visit(d->type);
        visit(d->initialValue);
    }
    virtual void visitParameter(Parameter* p)
    {
        visit(p->type);
        visit(p->initialValue);
    }
    virtual void visitArgument(Argument* a)
    {
        visit(a->value);
    }
    virtual void visitAssignment(Assignment* a)
    {
        visit(a->lhs);
        visit(a->rhs);
    }
    virtual void visitTypeTuple(TypeTuple* t)
    {
        visitMany(t->types);
    }
    virtual void visitTypeFunction(TypeFunction t)
    {
        visitTypeTuple(&t.input);
        visit(t.output);
    }
    virtual void visitTypePointer(TypePointer* t)
    {
        visit(t->type);
    }
    virtual void visitTypeConstant(TypeConstant* t) { }
    virtual void visitApply(Apply* a)
    {
        visitMany(a->operands);
        visit(a->operator_);
    }
    virtual void visitTypeDefinition(TypeDefinition* t)
    {
        visit(t->type);
    }
    virtual void visitRec(Rec* r)
    {
        visit(r->resolved);
    }
    virtual void visitFunction(Function* f)
    {
        for (auto p : f->parameters)
        {
            if (p->type != NodeType::PARAMETER)
            {
                errors.push_back(ParseError{
                    "expected function parameter. Maybe you meant to create an anonymous tuple with #{...} instead?",
                    nodes.top()->metadata.region
                });
                return;
            }
            else
            {
                visit(p->data.parameter->type);
                visit(p->data.parameter->initialValue);
            }
        }

        visitMany(f->block);
    }
    virtual void visitRet(Ret* r)
    {
        visit(r->value);
    }
    virtual void visitTypeof(Typeof* t)
    {
        visit(t->value);
    }
    virtual void visitDereference(Dereference* d)
    {
        visit(d->value);
    }
    virtual void visitAddressOf(AddressOf* a)
    {
        visit(a->value);
    }
    virtual void visitIf(If* i)
    {
        visit(i->if_branch->condition);
        visitMany(i->if_branch->statements);

        for (auto br : i->elif_branches)
        {
            visit(br->condition);
            visitMany(br->statements);
        }

        visit(i->else_branch->condition);
        visitMany(i->else_branch->statements);
    }
    virtual void visitDo(Do *d)
    {
        visitMany(d->statements);
    }
    virtual void visitWhile(While* w)
    {
        visit(w->condition);
        visitMany(w->statements);
    }
    virtual void visitSpread(Spread* s)
    {
        visit(s->value);
    }
    virtual void visitPipe(Pipe* s)
    {
        visit(s->resolved);
    }
    virtual void visitBinOp(BinOp* b)
    {
        visit(b->lhs);
        visit(b->rhs);
    }
    virtual void visitDot(Dot* d)
    {
        visit(d->lhs);
        visit(d->rhs);
    }
    virtual void visitValueTuple(ValueTuple* v)
    {
        visitMany(v->values);
    }
    virtual void visitNeg(Neg* n)
    {
        visit(n->value);
    }
    virtual void visitCast(Cast* c)
    {
        visit(c->value);
    }
    virtual void visitHeap(Heap* h)
    {
        visit(h->value);
    }
    virtual void visitSizeof(Sizeof* s)
    {
        visit(s->type);
    }
    virtual void visitDefer(Defer* d)
    {
        visitMany(d->statements);
    }
    virtual void visitAssert(Assert* a)
    {
        visit(a->value);
    }
    virtual void visitNot(Not* n)
    {
        visit(n->value);
    }
    virtual void visitLabel(Label *l) { }
    virtual void visitJump(Jump *j) { }
    virtual void visitQuestion(Question *q)
    {
        visit(q->value);
    }
    virtual void visitImport(Import* i) { }
    virtual void visitRequire(Require* r)
    {
        visit(r->resolved);
    }
    virtual void visitUnit(Unit* u)
    {
        visitMany(u->modules);
    }
    virtual void visitStringLiteral(StringLiteral* s) { }
    virtual void visitCharacter(Character* c) { }
    virtual void visitParseError(ParseError* p)
    {
        reportError(*p);
        exit(-1);
    }

    void visit_base(Node *n);
    void forceVisit(Node *n);
    virtual void visit(Node* n);

    void visitMany(vector<Node*> t);

    Node* sanitize(Node* n);
    Node *sanitizeType(Node* n);
    Node *sanitizeTypeHelper(Node *n, unordered_map<Node *, bool> *visited);

    Node *getStringType();
};

class PrintVisitor : public Visitor
{
public:
    int indentationLevel;

    void visitNone(None n);
    void visitUnit(Unit* u);
    void visitSymbol(Symbol* s);
    void visitConstant(Constant* c);
    void visitModule(Module* m);
    void visitDeclaration(Declaration* d);
    void visitParameter(Parameter* p);
    void visitArgument(Argument* a);
    void visitAssignment(Assignment* a);
    void visitTypeTuple(TypeTuple* t);
    void visitTypeFunction(TypeFunction t);
    void visitTypeConstant(TypeConstant *t);
    void visitTypePointer(TypePointer* t);
    void visitApply(Apply* a);
    void visitTypeDefinition(TypeDefinition* t);
    void visitRec(Rec* r);
    void visitFunction(Function* f);
    void visitRet(Ret* r);
    void visitTypeof(Typeof* t);
    void visitDereference(Dereference* d);
    void visitAddressOf(AddressOf* a);
    void visitIf(If* i);
    void visitDo(Do *d);
    void visitWhile(While* w);
    void visitDefer(Defer* d);
    void visitValueTuple(ValueTuple* v);
    void visitBinOp(BinOp* b);
    void visitDot(Dot* d);
    void visitNeg(Neg* n);
    void visitCast(Cast* c);
    void visitHeap(Heap* h);
    void visitSizeof(Sizeof* s);
    void visitAssert(Assert* a);
    void visitNot(Not* n);
    void visitQuestion(Question *q);
    void visitImport(Import* i);
    void visitRequire(Require* r);
    void visitStringLiteral(StringLiteral* s);
    void visitPipe(Pipe* p);

    // helpers
    void indent() { for (int i = 0; i < indentationLevel; i++) { cout << "\t"; } }

    void terminate() { cout << ";"; }
};

class JsonPrintVisitor : public Visitor
{
private:
    nlohmann::json visitRetOutput(Node *n);
    nlohmann::json visitManyRetOutput(vector<Node *> n);

public:
    nlohmann::json output;

    nlohmann::json metadata(Node *n);

    void visitNone(None n);
    void visitUnit(Unit* u);
    void visitSymbol(Symbol* s);
    void visitModule(Module* m);
    void visitDeclaration(Declaration* d);
    void visitParameter(Parameter* p);
    void visitArgument(Argument* a);
    void visitAssignment(Assignment* a);
    void visitTypeTuple(TypeTuple* t);
    void visitTypeFunction(TypeFunction t);
    void visitTypeConstant(TypeConstant *t);
    void visitTypePointer(TypePointer* t);
    void visitApply(Apply* a);
    void visitTypeDefinition(TypeDefinition* t);
    void visitRec(Rec* r);
    void visitFunction(Function* f);
    void visitRet(Ret* r);
    void visitTypeof(Typeof* t);
    void visitDereference(Dereference* d);
    void visitAddressOf(AddressOf* a);
    void visitIf(If* i);
    void visitDo(Do *d);
    void visitWhile(While* w);
    void visitDefer(Defer* d);
    void visitValueTuple(ValueTuple* v);
    void visitBinOp(BinOp* b);
    void visitDot(Dot* d);
    void visitNeg(Neg* n);
    void visitCast(Cast* c);
    void visitHeap(Heap* h);
    void visitSizeof(Sizeof* s);
    void visitAssert(Assert* a);
    void visitNot(Not* n);
    void visitQuestion(Question *q);
    void visitImport(Import* i);
    void visitRequire(Require* r);
    void visitStringLiteral(StringLiteral* s);
    void visitPipe(Pipe* p);
    void visitCharacter(Character *c);
    void visitSpread(Spread *s);
    void visitConstant(Constant *c);

    void visit(Node *n)
    {
        output = {{"kind", "none"}};
        Visitor::visit(n);
//        output["metadtata"] = metadata(n);
    }
};

class RequireVisitor : public Visitor
{
public:
    unordered_map<string, Node*> requireds;

    void visitRequire(Require* r);
};

class SymbolsVisitor : public Visitor
{
public:
    stack<Scope*> currentParentScopes;
    stack<Node*> rec;

    void visitSymbol(Symbol *s);
    void visitUnit(Unit *u);
    void visitModule(Module *m);
    void visitDeclaration(Declaration *d);
    void visitParameter(Parameter *p);
    void visitArgument(Argument *a);
    void visitAssignment(Assignment *a);
    void visitTypeTuple(TypeTuple *t);
    void visitTypeFunction(TypeFunction t);
    void visitTypePointer(TypePointer *t);
    void visitApply(Apply *a);
    void visitTypeDefinition(TypeDefinition *t);
    void visitFunction(Function *f);
    void visitRet(Ret *r);
    void visitRec(Rec *r);
    void visitTypeof(Typeof *t);
    void visitDereference(Dereference *d);
    void visitAddressOf(AddressOf *a);
    void visitIf(If *i);
    void visitWhile(While *w);
    void visitDo(Do *d);
    void visitDefer(Defer *d);
    void visitValueTuple(ValueTuple *v);
    void visitBinOp(BinOp *b);
    void visitDot(Dot *d);
    void visitNeg(Neg *n);
    void visitCast(Cast *c);
    void visitHeap(Heap *h);
    void visitSizeof(Sizeof *s);
    void visitAssert(Assert *a);
    void visitNot(Not *n);
    void visitQuestion(Question *q);
    void visitImport(Import *i);
    void visitSpread(Spread *s);
    void visitJump(Jump *j);
    void visitPipe(Pipe *p);
    void visitRequire(Require *r);
    void visitLabel(Label *l);

    void addStringType(Module *m)
    {
        m->statements.push_back(getStringType());
    }
};

class TypePropagationVisitor : public Visitor
{
public:
    stack<Scope*> currentParentScopes;

    void visitDeclaration(Declaration* d);
    void visitDot(Dot* d);
    void visitParameter(Parameter* p);
    void visitFunction(Function* f);
    void visitBinOp(BinOp* b);
    void visitTypeof(Typeof* t);
    void visitApply(Apply* a);
    void visitPipe(Pipe* p);
    void visitAssignment(Assignment* a);
    void visitIf(If* i);
    void visitWhile(While* w);
    void visitModule(Module* m);
    void visitRequire(Require* r);
    void visitRec(Rec *r);
    void visitCast(Cast *c);
    void visitDereference(Dereference *d);
    void visitValueTuple(ValueTuple *v);
    void visitConstant(Constant *c);
    void visitQuestion(Question *q);

    void locateModulesWithType(Node *type, Scope *s, vector<Node *> *modules);
};

Scope* copyScope(Scope* s);

Node* resolveDot(Dot* d);

bool isNumericType(string s);

bool isBuiltinType(string s);

Node *sanitize(Node *n);

typedef struct Ordering
{
    int given_index;
    int target_index;
} Ordering;
vector<Ordering> get_ordering(vector<Node*> expectedTypes,
                              vector<string> givenArgs,
                              vector<ParseError> *errorsList,
                              Region callSite);

#endif
