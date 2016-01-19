#include <iostream>
#include <stack>
#include <fstream>

#include "lexer.h"
#include "parser.h"

using namespace std;
using nlohmann::json;

Node *parse_unit(Lexer* lexer)
{
    lexer->save();
    vector<Node *> modules;

    while (lexer->front.tok != Tok::END)
    {
        if (lexer->front.tok == Tok::REQUIRE)
        {
            lexer->popFront(); // 'require'
            assert(lexer->front.tok == Tok::STRINGLITERAL);
            auto requireStmt = require(lexer->front.value, lexer);
            lexer->popFront(); // file name
            modules.push_back(requireStmt);

            if (lexer->front.tok == Tok::SEMICOLON) { lexer->popFront(); }
        }
        else
        {
            vector<Node *> annotations = {};
            while (lexer->front.tok == Tok::ANNOTATION)
            {
                lexer->popFront(); // '@'
                annotations.push_back(parse_rvalue_simple(lexer));
            }

            auto module = parse_module(lexer);
            module->metadata.annotations = annotations;
            modules.push_back(module);

            if (module->type == NodeType::PARSEERROR)
            {
                auto ret = unit(modules, lexer);
                lexer->revert();
                return ret;
            }
        }
    }

    auto ret = unit(modules, lexer);
    lexer->revert();
    return ret;
}

vector<Node *> parseMany(json j)
{
    vector<Node *> r;
    for (auto c : j)
    {
        r.push_back(parse_json_node(c));
    }
    return r;
}

Node *parse_json_node(json j)
{
    auto jKind = j["kind"];
    if (!jKind.is_string())
    {
        return parseError(fmt::format("unrecognized json node {}", jKind.dump()), Region{});
    }

    string kind = jKind;
    if (kind == "unit")
    {
        return unit(parseMany(j["children"]), Region{});
    }
    else if (kind == "module")
    {
        return module(j["name"], parseMany(j["children"]), Region{});
    }
    else if (kind == "none")
    {
        return none();
    }
    else if (kind == "symbol")
    {
        return symbol(j["value"], Region{});
    }
    else if (kind == "declaration")
    {
        auto d = declaration(j["name"],
                           parse_json_node(j["type"]),
                           j["declarationType"] == "mutable" ? DeclarationType::MUTABLE : DeclarationType::IMMUTABLE,
                           Region{},
                           parse_json_node(j["initialValue"]));

        d->data.declaration->external = j["external"];
        d->data.declaration->global = j["global"];
        return d;
    }
    else if (kind == "parameter")
    {
        return parameter(j["name"],
                         parse_json_node(j["type"]),
                         parse_json_node(j["initialValue"]),
                         j["order"],
                         Region{});
    }
    else if (kind == "argument")
    {
        return argument(j["name"], parse_json_node(j["value"]), Region{});
    }
    else if (kind == "assignment")
    {
        return assignment(parse_json_node(j["lhs"]), parse_json_node(j["rhs"]), Region{});
    }
    else if (kind == "typeTuple")
    {
        auto ret = typeTuple(parseMany(j["types"]), Region{});
        ret->data.typeTuple->isAnonymous = j["isAnonymous"];
        return ret;
    }
    else if (kind == "typeFunction")
    {
        return typeFunction(parseMany(j["input"]), parse_json_node(j["output"]), Region{});
    }
    else if (kind == "typePointer")
    {
        return typePointer(parse_json_node(j["type"]), Region{});
    }
    else if (kind == "typeBasic")
    {
        string ct = j["type"];

        if (ct == "nil")
        {
            return typeConstant(ConstantType::NIL, Region{});
        }
        if (ct == "char")
        {
            return typeConstant(ConstantType::CHAR, Region{});
        }
        if (ct == "bool")
        {
            return typeConstant(ConstantType::BOOL, Region{});
        }
        if (ct == "i8")
        {
            return typeConstant(ConstantType::I8, Region{});
        }
        if (ct == "i16")
        {
            return typeConstant(ConstantType::I16, Region{});
        }
        if (ct == "i32")
        {
            return typeConstant(ConstantType::I32, Region{});
        }
        if (ct == "i64")
        {
            return typeConstant(ConstantType::I64, Region{});
        }
        if (ct == "u8")
        {
            return typeConstant(ConstantType::U8, Region{});
        }
        if (ct == "u16")
        {
            return typeConstant(ConstantType::U16, Region{});
        }
        if (ct == "u32")
        {
            return typeConstant(ConstantType::U32, Region{});
        }
        if (ct == "u64")
        {
            return typeConstant(ConstantType::U64, Region{});
        }
        if (ct == "f32")
        {
            return typeConstant(ConstantType::F32, Region{});
        }
        if (ct == "f64")
        {
            return typeConstant(ConstantType::F64, Region{});
        }
        if (ct == "none")
        {
            return typeConstant(ConstantType::None, Region{});
        }
    }
    else if (kind == "apply")
    {
        return apply(parse_json_node(j["operator"]), parseMany(j["operands"]), Region{});
    }
    else if (kind == "typeDefinition")
    {
        return typeDefinition(j["name"], parse_json_node(j["type"]), Region{});
    }
    else if (kind == "rec")
    {
        return rec(Region{});
    }
    else if (kind == "function")
    {
        return function_(parseMany(j["parameters"]), parseMany(j["statements"]), Region{});
    }
    else if (kind == "ret")
    {
        return ret(parse_json_node(j["value"]), Region{});
    }
    else if (kind == "import")
    {
        return import(parse_json_node(j["value"]), Region{});
    }
    else if (kind == "require")
    {
        return require(j["fileName"], Region{});
    }
    else if (kind == "typeof")
    {
        return typeof_(parse_json_node(j["value"]), false, Region{});
    }
    else if (kind == "rettype")
    {
        return typeof_(parse_json_node(j["value"]), true, Region{});
    }
    else if (kind == "dereference")
    {
        return dereference(parse_json_node(j["value"]), Region{});
    }
    else if (kind == "addressOf")
    {
        return addressOf(parse_json_node(j["value"]), Region{});
    }
    else if (kind == "if")
    {
        auto if_branch = new IfBranch();
        if_branch->condition = parse_json_node(j["ifBranch"]["condition"]);
        if_branch->statements = parseMany(j["ifBranch"]["statements"]);

        vector<IfBranch *> elif_branches;
        for (auto eb : j["elifBranches"])
        {
            auto elif_branch = new IfBranch();
            elif_branch->condition = parse_json_node(eb["condition"]);
            elif_branch->statements = parseMany(eb["statements"]);

            elif_branches.push_back(elif_branch);
        }

        auto else_branch = new IfBranch();
        else_branch->condition = parse_json_node(j["elseBranch"]["condition"]);
        else_branch->statements = parseMany(j["elseBranch"]["statements"]);

        If i;
        i.if_branch = if_branch;
        i.elif_branches = elif_branches;
        i.else_branch = else_branch;

        return if_(i, Region{});
    }
    else if (kind == "do")
    {
        return do_(Do{parse_json_node(j["outputIdentifier"]), parseMany(j["statements"])}, Region{});
    }
    else if (kind == "while")
    {
        return while_(parse_json_node(j["condition"]), parseMany(j["statements"]), Region{});
    }
    else if (kind == "defer")
    {
        return defer(parseMany(j["statements"]), Region{});
    }
    else if (kind == "binOp")
    {
        string op = j["op"];

        if (op == "==")
        {
            return binop(Tok::EQEQ,
                         parse_json_node(j["lhs"]),
                         parse_json_node(j["rhs"]),
                         false,
                         Region{});
        }
        if (op == "!=")
        {
            return binop(Tok::NEQ,
                         parse_json_node(j["lhs"]),
                         parse_json_node(j["rhs"]),
                         false,
                         Region{});
        }
        if (op == "+")
        {
            return binop(Tok::PLUS,
                         parse_json_node(j["lhs"]),
                         parse_json_node(j["rhs"]),
                         false,
                         Region{});
        }
        if (op == "-")
        {
            return binop(Tok::MINUS,
                         parse_json_node(j["lhs"]),
                         parse_json_node(j["rhs"]),
                         false,
                         Region{});
        }
        if (op == "|")
        {
            return binop(Tok::PIPE,
                         parse_json_node(j["lhs"]),
                         parse_json_node(j["rhs"]),
                         false,
                         Region{});
        }
        if (op == "*")
        {
            return binop(Tok::STAR,
                         parse_json_node(j["lhs"]),
                         parse_json_node(j["rhs"]),
                         false,
                         Region{});
        }
        if (op == "<")
        {
            return binop(Tok::LT,
                         parse_json_node(j["lhs"]),
                         parse_json_node(j["rhs"]),
                         false,
                         Region{});
        }
        if (op == "<=")
        {
            return binop(Tok::LTE,
                         parse_json_node(j["lhs"]),
                         parse_json_node(j["rhs"]),
                         true,
                         Region{});
        }
        if (op == ">")
        {
            return binop(Tok::GT,
                         parse_json_node(j["lhs"]),
                         parse_json_node(j["rhs"]),
                         false,
                         Region{});
        }
        if (op == ">=")
        {
            return binop(Tok::GTE,
                         parse_json_node(j["lhs"]),
                         parse_json_node(j["rhs"]),
                         true,
                         Region{});
        }
        if (op == "/")
        {
            return binop(Tok::FSLASH,
                         parse_json_node(j["lhs"]),
                         parse_json_node(j["rhs"]),
                         false,
                         Region{});
        }
        if (op == "or")
        {
            return binop(Tok::OR,
                         parse_json_node(j["lhs"]),
                         parse_json_node(j["rhs"]),
                         false,
                         Region{});
        }
        if (op == "mod")
        {
            return binop(Tok::MOD,
                         parse_json_node(j["lhs"]),
                         parse_json_node(j["rhs"]),
                         false,
                         Region{});
        }
        return parseError(fmt::format("unsupported binary op {}", op), Region{});
    }
    else if (kind == "pipe")
    {
        return pipe(parse_json_node(j["operands"]), parse_json_node(j["operator"]), Region{});
    }
    else if (kind == "neg")
    {
        return neg(parse_json_node(j["value"]), Region{});
    }
    else if (kind == "cast")
    {
        return cast(parse_json_node(j["type"]), parse_json_node(j["value"]), Region{});
    }
    else if (kind == "heap")
    {
        return heap(parse_json_node(j["value"]), Region{});
    }
    else if (kind == "sizeof")
    {
        return sizeof_(parse_json_node(j["type"]), Region{});
    }
    else if (kind == "assert")
    {
        return assert_(parse_json_node(j["value"]), Region{});
    }
    else if (kind == "not")
    {
        return assert_(parse_json_node(j["value"]), Region{});
    }
    else if (kind == "tuple")
    {
        return valueTuple(parseMany(j["values"]), Region{});
    }
    else if (kind == "dot")
    {
        return dot(parse_json_node(j["lhs"]), parse_json_node(j["rhs"]), Region{});
    }
    else if (kind == "stringLiteral")
    {
        return stringLiteral(j["value"], Region{});
    }
    else if (kind == "character")
    {
        string value = j["value"];
        char c = value[0];

        return character(c, Region{});
    }
    else if (kind == "spread")
    {
        return spread(parse_json_node(j["value"]), Region{});
    }
    else if (kind == "constant")
    {
        string ct = j["type"];
        auto value = j["value"];

        if (ct == "nil")
        {
            return constNil(Region{});
        }
        if (ct == "char")
        {
            string stringValue = value;
            return constChar(stringValue[0], Region{});
        }
        if (ct == "bool")
        {
            return constBool((bool) value, Region{});
        }
        if (ct == "i8")
        {
            return constInt(ConstantType::I8, (int) value, Region{});
        }
        if (ct == "i16")
        {
            return constInt(ConstantType::I16, (int) value, Region{});
        }
        if (ct == "i32")
        {
            return constInt(ConstantType::I32, (int) value, Region{});
        }
        if (ct == "i64")
        {
            return constInt(ConstantType::I64, (int) value, Region{});
        }
        if (ct == "u8")
        {
            return constInt(ConstantType::U8, (int) value, Region{});
        }
        if (ct == "u16")
        {
            return constInt(ConstantType::U16, (int) value, Region{});
        }
        if (ct == "u32")
        {
            return constInt(ConstantType::U32, (int) value, Region{});
        }
        if (ct == "u64")
        {
            return constInt(ConstantType::U64, (int) value, Region{});
        }
        if (ct == "f32")
        {
            return constFloat(ConstantType::F32, (float) value, Region{});
        }
        if (ct == "f64")
        {
            return constFloat(ConstantType::F64, (float) value, Region{});
        }
        if (ct == "none")
        {
            return constNil(Region{});
        }
        if (ct == "tuple")
        {
            auto tupleValues = parseMany(j["values"]);
            return constTuple(ValueTuple{tupleValues}, Region{});
        }
    }

    assert(false);
}

Node *parse_unit_from_json(string jsonSrc)
{
    auto j = json::parse(jsonSrc);
    return parse_json_node(j);
}

Node *parse_module(Lexer* lexer)
{
    lexer->save(true);
    if (lexer->front.tok != Tok::MODULE)
    {
        auto ret = parseError("expected 'module', 'import', or 'require' statement", lexer->front.region);
        lexer->revert();
        return ret;
    }
    lexer->popFront(); // 'module'

    if (lexer->front.tok != Tok::STR)
    {
        auto ret = parseError("expected identifier", lexer->front.region);
        lexer->revert();
        return ret;
    }
    auto moduleName = lexer->front.value;
    lexer->popFront(); // moduleName

    if (lexer->front.tok != Tok::LCURLY)
    {
        lexer->revert();
        return parseError("expected '{'", lexer->front.region);
    }
    lexer->popFront(); // '{'

    vector<Node *> statements;
    while (lexer->front.tok != Tok::RCURLY)
    {
        auto newStatement = parse_statement(lexer);
        if (newStatement->type == NodeType::PARSEERROR)
        {
            lexer->revert();
            return newStatement;
        }
        if (newStatement->type == NodeType::DECLARATION)
        {
            newStatement->data.declaration->external = true;
        }
        statements.push_back(newStatement);
    }

    if (lexer->front.tok != Tok::RCURLY)
    {
        auto ret = parseError("expected '}", lexer->front.region);
        lexer->revert();
        return ret;
    }
    lexer->popFront(); // '}'

    auto ret = module(moduleName, statements, lexer);

    lexer->revert();

    return ret;
}

Node *parse_statement(Lexer* lexer)
{
    lexer->save();

    vector<Node *> annotations = {};

    while (lexer->front.tok == Tok::ANNOTATION)
    {
        lexer->popFront(); // '@'
        annotations.push_back(parse_rvalue_simple(lexer));
    }

    if (lexer->front.tok == Tok::HASH && lexer->buffer.tok == Tok::STR)
    {
        lexer->popFront(); // '#'
        auto label_name = lexer->front.value;
        lexer->popFront();
        return label(label_name, lexer);
    }

    if (lexer->front.tok == Tok::SKINNYARROW)
    {
        lexer->popFront(); // '->'
        if (lexer->front.tok != Tok::STR)
        {
            lexer->revert();
            return parseError("expected identifier", lexer->front.region);
        }
        auto label_name = lexer->front.value;
        lexer->popFront();
        return jump(label_name, lexer);
    }

    if (lexer->front.tok == Tok::TYPE)
    {
        // type
        lexer->popFront(); // 'type'

        // name
        if (lexer->front.tok != Tok::STR)
        {
            lexer->revert();
            return parseError("expected identifier", lexer->front.region);
        }
        auto typeName = lexer->front.value;
        lexer->popFront();

        // eq
        if (lexer->front.tok != Tok::EQ)
        {
            lexer->revert();
            return parseError("expected '='", lexer->front.region);
        }
        lexer->popFront(); // '='

        // type
        Node *type = parse_type(lexer);
        if (type->type == NodeType::TYPETUPLE)
        {
            type->data.typeTuple->isAnonymous = false;
        }
        auto td = typeDefinition(typeName, type, lexer);

        // ';'
        if (lexer->front.tok == Tok::SEMICOLON)
        {
            lexer->popFront();
        }

        lexer->revert();
        td->metadata.annotations = annotations;
        return td;
    }

    // require
    if (lexer->front.tok == Tok::REQUIRE)
    {
        lexer->popFront(); // 'require'
        assert(lexer->front.tok == Tok::STRINGLITERAL);
        auto requireStmt = require(lexer->front.value, lexer);
        lexer->popFront(); // file name
        if (lexer->front.tok == Tok::SEMICOLON) { lexer->popFront(); }
        requireStmt->metadata.annotations = annotations;
        return requireStmt;
    }

    // module
    if (lexer->front.tok == Tok::MODULE)
    {
        auto ret = parse_module(lexer);
        lexer->revert();
        ret->metadata.annotations = annotations;
        return ret;
    }

    // ret
    if (lexer->front.tok == Tok::RET)
    {
        lexer->popFront(); // 'ret'

        auto rvalue = parse_rvalue(lexer);
        if (rvalue->type == NodeType::PARSEERROR)
        {
            lexer->revert();
            rvalue->metadata.annotations = annotations;
            return rvalue;
        }

        if (lexer->front.tok == Tok::SEMICOLON)
        {
            lexer->popFront(); // ';'
        }

        auto bret = ret(rvalue, lexer);
        lexer->revert();
        bret->metadata.annotations = annotations;
        return bret;
    }

    // import
    if (lexer->front.tok == Tok::IMPORT)
    {
        auto ret = parse_import(lexer);
        lexer->revert();
        ret->metadata.annotations = annotations;
        return ret;
    }

    // while
    if (lexer->front.tok == Tok::WHILE)
    {
        lexer->popFront(); // 'while'

        auto condition = parse_rvalue(lexer);

        if (lexer->front.tok != Tok::LCURLY)
        {
            lexer->revert();
            return parseError("expected '{'", lexer->front.region);
        }
        lexer->popFront(); // '{'

        vector<Node *> statements;
        while (lexer->front.tok != Tok::RCURLY)
        {
            statements.push_back(parse_statement(lexer));
        }

        if (lexer->front.tok != Tok::RCURLY)
        {
            lexer->revert();
            return parseError("expected '}'", lexer->front.region);
        }
        lexer->popFront(); // '}'

        auto ret = while_(condition, statements, lexer);
        lexer->revert();
        ret->metadata.annotations = annotations;
        return ret;
    }

    // defer
    if (lexer->front.tok == Tok::DEFER)
    {
        lexer->popFront(); // 'defer'

        if (lexer->front.tok != Tok::LCURLY)
        {
            lexer->revert();
            return parseError("expected '{'", lexer->front.region);
        }
        lexer->popFront(); // '{'

        vector<Node *> statements;
        while (lexer->front.tok != Tok::RCURLY)
        {
            statements.push_back(parse_statement(lexer));
        }

        if (lexer->front.tok != Tok::RCURLY)
        {
            lexer->revert();
            return parseError("expected '}'", lexer->front.region);
        }
        lexer->popFront(); // '}'

        auto ret = defer(statements, lexer);
        lexer->revert();
        ret->metadata.annotations = annotations;
        return ret;
    }

    // if
    if (lexer->front.tok == Tok::IF)
    {
        auto if_stmt = If{};

        lexer->popFront(); // 'if'

        if_stmt.if_branch->condition = parse_rvalue(lexer);

        if (lexer->front.tok != Tok::LCURLY)
        {
            lexer->revert();
            return parseError("expected '{'", lexer->front.region);
        }
        lexer->popFront(); // '{'

        while (lexer->front.tok != Tok::RCURLY)
        {
            if_stmt.if_branch->statements.push_back(parse_statement(lexer));
        }

        if (lexer->front.tok != Tok::RCURLY)
        {
            lexer->revert();
            return parseError("expected '}'", lexer->front.region);
        }
        lexer->popFront(); // '}'

        // possible elif / else
        while (lexer->front.tok == Tok::ELIF)
        {
            lexer->popFront(); // 'elif'

            auto elifBranch = new IfBranch{};
            elifBranch->condition = parse_rvalue(lexer);

            if (lexer->front.tok != Tok::LCURLY)
            {
                lexer->revert();
                return parseError("expected '{'", lexer->front.region);
            }
            lexer->popFront(); // '{'

            while (lexer->front.tok != Tok::RCURLY)
            {
                elifBranch->statements.push_back(parse_statement(lexer));
            }

            if (lexer->front.tok != Tok::RCURLY)
            {
                lexer->revert();
                return parseError("expected '}'", lexer->front.region);
            }
            lexer->popFront(); // '}'

            if_stmt.elif_branches.push_back(elifBranch);
        }
        if (lexer->front.tok == Tok::ELSE)
        {
            lexer->popFront(); // 'else'

            if (lexer->front.tok != Tok::LCURLY)
            {
                lexer->revert();
                return parseError("expected '{'", lexer->front.region);
            }
            lexer->popFront(); // '{'

            while (lexer->front.tok != Tok::RCURLY)
            {
                if_stmt.else_branch->statements.push_back(parse_statement(lexer));
            }

            if (lexer->front.tok != Tok::RCURLY)
            {
                lexer->revert();
                return parseError("expected '}'", lexer->front.region);
            }
            lexer->popFront(); // '}'
        }

        auto ret = if_(if_stmt, lexer);
        lexer->revert();
        ret->metadata.annotations = annotations;
        return ret;
    }

    // do
    if (lexer->front.tok == Tok::DO)
    {
        auto ret = parse_do(lexer);
        ret->metadata.annotations = annotations;
        return ret;
    }

    // variable name
    auto varName = parse_rvalue(lexer);

    Node *type = none();
    Node *rvalue = none();

    // ':', '::'
    if (lexer->front.tok == Tok::COLON
        || lexer->front.tok == Tok::COLONCOLON)
    {
        DeclarationType dc;
        if (lexer->front.tok == Tok::COLON)
        {
            dc = DeclarationType::IMMUTABLE;
        }
        else
        {
            dc = DeclarationType::MUTABLE;
        }

        lexer->popFront(); // ':', '::'
        type = parse_type(lexer);

        // ';'
        if (lexer->front.tok == Tok::SEMICOLON)
        {
            lexer->popFront();
            assert(varName->type == NodeType::SYMBOL);
            auto ret = declaration(varName->data.symbol->value, type, dc, lexer, rvalue);
            lexer->revert();
            ret->metadata.annotations = annotations;
            return ret;
        }

        if (lexer->front.tok != Tok::EQ)
        {
            auto ret = declaration(varName->data.symbol->value, type, dc, lexer, rvalue);
            lexer->revert();
            ret->metadata.annotations = annotations;
            return ret;
        }

        lexer->popFront(); // '='

        rvalue = parse_rvalue(lexer);
        if (rvalue->type == NodeType::PARSEERROR)
        {
            lexer->revert();
            rvalue->metadata.annotations = annotations;
            return rvalue;
        }

        // ';'
        if (lexer->front.tok == Tok::SEMICOLON)
        {
            lexer->popFront(); // ';'
        }

        // return
        assert(varName->type == NodeType::SYMBOL);
        auto ret = declaration(varName->data.symbol->value, type, dc, lexer, rvalue);
        lexer->revert();
        ret->metadata.annotations = annotations;
        return ret;
    }

    // ':=', '::='
    if (lexer->front.tok == Tok::COLONEQ
        || lexer->front.tok == Tok::COLONCOLONEQ)
    {
        DeclarationType dc;
        if (lexer->front.tok == Tok::COLONEQ)
        {
            dc = DeclarationType::IMMUTABLE;
        }
        else
        {
            dc = DeclarationType::MUTABLE;
        }

        lexer->popFront(); // ':=', '::='

        rvalue = parse_rvalue(lexer);
        if (rvalue->type == NodeType::PARSEERROR)
        {
            lexer->revert();
            return rvalue;
        }

        // ';'
        if (lexer->front.tok == Tok::SEMICOLON)
        {
            lexer->popFront(); // ';'
        }

        // return
        assert(varName->type == NodeType::SYMBOL);
        auto ret = declaration(varName->data.symbol->value, type, dc, lexer, rvalue);
        lexer->revert();
        ret->metadata.annotations = annotations;
        return ret;
    }

    // '='
    if (lexer->front.tok == Tok::EQ)
    {
        lexer->popFront(); // '='

        rvalue = parse_rvalue(lexer);

        // ';'
        if (lexer->front.tok == Tok::SEMICOLON)
        {
            lexer->popFront(); // ';'
        }

        // return
        auto ret = assignment(varName, rvalue, lexer);
        lexer->revert();
        ret->metadata.annotations = annotations;
        return ret;
    }

    if (lexer->front.tok == Tok::SEMICOLON)
    {
        lexer->popFront(); // ';'
    }

    lexer->revert();
    varName->metadata.annotations = annotations;
    return varName;

    assert(false);
}

Node *parse_do(Lexer *lexer)
{
    auto do_stmt = Do{};

    lexer->popFront(); // 'do'

    if (lexer->front.tok == Tok::LPAREN)
    {
        lexer->popFront(); // '('

        if (lexer->front.tok != Tok::STR)
        {
            lexer->revert();
            return parseError("expected single identifier", lexer->front.region);
        }

        do_stmt.identifier = symbol(lexer->front.value, lexer);
        lexer->popFront(); // symbol

        if (lexer->front.tok != Tok::RPAREN)
        {
            lexer->revert();
            return parseError("expected ')'", lexer->front.region);
        }
        lexer->popFront(); // ')'
    }

    if (lexer->front.tok != Tok::LCURLY)
    {
        lexer->revert();
        return parseError("expected '{'", lexer->front.region);
    }
    lexer->popFront(); // '{'

    while (lexer->front.tok != Tok::RCURLY)
    {
        do_stmt.statements.push_back(parse_statement(lexer));
    }

    if (lexer->front.tok != Tok::RCURLY)
    {
        lexer->revert();
        return parseError("expected '}'", lexer->front.region);
    }
    lexer->popFront(); // '}'

    auto ret = do_(do_stmt, lexer);
    lexer->revert();
    return ret;
}

Node *parse_rvalue(Lexer* lexer)
{
    lexer->save();
    stack<Node *> shuntingYardTmp;
    stack<Node *> shuntingYardOutput;

    auto primary = parse_rvalue_simple(lexer);
    if (primary->type == NodeType::PARSEERROR)
    {
        lexer->revert();
        return primary;
    }
    shuntingYardOutput.push(primary);

    while (isBinaryOp(lexer->front.tok))
    {
        auto op = lexer->front;
        lexer->popFront();
        while (!shuntingYardTmp.empty()
               && precedence(shuntingYardTmp.top()->data.binop->type) <= precedence(toOp(op.tok)))
        {
            // pop off the last two elements from output, create binary op from them
            auto rhs = shuntingYardOutput.top();
            shuntingYardOutput.pop();

            auto lhs = shuntingYardOutput.top();
            shuntingYardOutput.pop();

            Node *shuntingYardNext;
            if (shuntingYardTmp.top()->data.binop->type == OpType::PIPE)
            {
                shuntingYardNext = pipe(lhs, rhs, lexer);
            }
            else if (shuntingYardTmp.top()->data.binop->type == OpType::DOTDOTPIPE)
            {
                // push special pipe type with spread
                shuntingYardNext = pipe(spread(lhs, lexer), rhs, lexer);
            }
            else
            {
                auto type = (Tok) shuntingYardTmp.top()->data.binop->type;
                shuntingYardNext = binop(type, lhs, rhs, shuntingYardTmp.top()->data.binop->is_eq, lexer);
            }
            shuntingYardOutput.push(shuntingYardNext);
            shuntingYardTmp.pop();
        }

        // push binary op
        shuntingYardTmp.push(binop(op.tok, none(), none(), isBinaryEq(op.tok), lexer));

        // push next rvalue
        shuntingYardOutput.push(parse_rvalue_simple(lexer));
    }

    while (!shuntingYardTmp.empty())
    {
        // pop off the last two elements from output, create a binary op from them
        auto rhs = shuntingYardOutput.top();
        shuntingYardOutput.pop();

        auto lhs = shuntingYardOutput.top();
        shuntingYardOutput.pop();

        Node *shuntingYardNext;
        if (shuntingYardTmp.top()->data.binop->type == OpType::PIPE)
        {
            // push special pipe type
            shuntingYardNext = pipe(lhs, rhs, lexer);
        }
        else if (shuntingYardTmp.top()->data.binop->type == OpType::DOTDOTPIPE)
        {
            // push special pipe type with spread
            shuntingYardNext = pipe(spread(lhs, lexer), rhs, lexer);
        }
        else
        {
            auto type = (Tok) shuntingYardTmp.top()->data.binop->type;
            shuntingYardNext = binop(type, lhs, rhs, shuntingYardTmp.top()->data.binop->is_eq, lexer);
        }
        shuntingYardOutput.push(shuntingYardNext);
        shuntingYardTmp.pop();
    }

    assert(shuntingYardOutput.size() == 1);
    lexer->revert();
    return shuntingYardOutput.top();
}

Node *parse_import(Lexer* lexer)
{
    lexer->save();
    assert(lexer->front.tok == Tok::IMPORT);
    lexer->popFront(); // 'import'

    auto itemToImport = parse_rvalue_simple(lexer);

    if (lexer->front.tok == Tok::SEMICOLON)
    {
        lexer->popFront();
    }

    auto ret = import(itemToImport, lexer);
    lexer->revert();
    return ret;
}

Node *parse_type(Lexer* lexer)
{
    lexer->save();
    auto firstType = parse_type_simple(lexer);

    // if right arrow, then it is a function type
    if (lexer->front.tok == Tok::RIGHTARROW)
    {
        lexer->popFront(); // '=>'
        auto returnType = parse_type(lexer);
        auto ret = typeFunction(getTypeTuple(firstType), returnType, lexer);
        lexer->revert();
        return ret;
    }

    lexer->revert();
    return firstType;
}

Node *postfix(Node *n, Lexer *lexer)
{
    if (lexer->front.tok != Tok::QUESTION)
    {
        return n;
    }

    lexer->popFront(); // '?'
    return question(n, lexer);
}

Node *parse_rvalue_simple(Lexer* lexer)
{
    lexer->save();
    auto primary = none(Region{lexer->fileName, lexer->saved.top(), lexer->lastLoc});

    vector<Node *> annotations = {};

    while (lexer->front.tok == Tok::ANNOTATION)
    {
        lexer->popFront(); // '@'
        annotations.push_back(parse_rvalue_simple(lexer));
    }

    auto t = lexer->front;

    if (lexer->front.tok == Tok::STR && (lexer->front.value == "true" || lexer->front.value == "false"))
    {
        if (lexer->front.value == "true")
        {
            lexer->popFront();
            auto ret = constBool(true, lexer);
            lexer->revert();
            ret->metadata.annotations = annotations;
            return postfix(ret, lexer);
        }

        lexer->popFront();
        auto ret = constBool(false, lexer);
        lexer->revert();
        ret->metadata.annotations = annotations;
        return postfix(ret, lexer);
    }
    if (lexer->front.tok == Tok::NOT)
    {
        lexer->popFront(); // '!'
        auto ret = not_(parse_rvalue_simple(lexer), lexer);
        lexer->revert();
        ret->metadata.annotations = annotations;
        return postfix(ret, lexer);
    }
    if (lexer->front.tok == Tok::DOTDOT)
    {
        lexer->popFront(); // '..'
        auto ret = spread(parse_rvalue_simple(lexer), lexer);
        lexer->revert();
        ret->metadata.annotations = annotations;
        return postfix(ret, lexer);
    }
    if (lexer->front.tok == Tok::MINUS)
    {
        lexer->popFront(); // '-'
        auto ret = neg(parse_rvalue_simple(lexer), lexer);
        lexer->revert();
        ret->metadata.annotations = annotations;
        return postfix(ret, lexer);
    }
    if (lexer->front.tok == Tok::NIL)
    {
        lexer->popFront(); // 'nil'
        auto ret = constNil(lexer);
        lexer->revert();
        ret->metadata.annotations = annotations;
        return postfix(ret, lexer);
    }
    if (lexer->front.tok == Tok::HEAP)
    {
        lexer->popFront(); // 'heap'

        auto rvalue = parse_rvalue(lexer);
        auto ret = heap(rvalue, lexer);
        ret->metadata.annotations = annotations;
        return postfix(ret, lexer);
    }
    if (lexer->front.tok == Tok::CAST)
    {
        lexer->popFront(); // 'cast'

        if (lexer->front.tok != Tok::LPAREN)
        {
            lexer->revert();
            return parseError("expected '('", lexer->front.region);
        }
        lexer->popFront(); // '('

        auto type = parse_type(lexer);

        if (lexer->front.tok != Tok::RPAREN)
        {
            lexer->revert();
            return parseError("expected ')'", lexer->front.region);
        }
        lexer->popFront(); // ')'

        auto rvalue = parse_rvalue_simple(lexer);

        auto ret = cast(type, rvalue, lexer);
        lexer->revert();
        ret->metadata.annotations = annotations;
        return postfix(ret, lexer);
    }
    if (lexer->front.tok == Tok::SIZEOF)
    {
        lexer->popFront(); // 'sizeof'

        if (lexer->front.tok != Tok::LPAREN)
        {
            lexer->revert();
            return parseError("expected '('", lexer->front.region);
        }
        lexer->popFront(); // '('

        auto type = parse_type(lexer);

        if (lexer->front.tok != Tok::RPAREN)
        {
            lexer->revert();
            return parseError("expected ')'", lexer->front.region);
        }
        lexer->popFront(); // ')'

        auto ret = sizeof_(type, lexer);
        lexer->revert();
        ret->metadata.annotations = annotations;
        return postfix(ret, lexer);
    }
    if (lexer->front.tok == Tok::ASSERT)
    {
        lexer->popFront(); // 'assert'

        auto rvalue = parse_rvalue(lexer);

        auto ret = assert_(rvalue, lexer);
        lexer->revert();
        ret->metadata.annotations = annotations;
        return postfix(ret, lexer);
    }
    else if (lexer->front.tok == Tok::STAR)
    {
        lexer->popFront(); // '*'
        auto rvalue = parse_rvalue_simple(lexer);
        auto ret = dereference(rvalue, lexer);
        lexer->revert();
        ret->metadata.annotations = annotations;
        return postfix(ret, lexer);
    }
    else if (lexer->front.tok == Tok::DO)
    {
        auto ret = parse_do(lexer);
        ret->metadata.annotations = annotations;
        return postfix(ret, lexer);
    }
    else if (lexer->front.tok == Tok::AMPERSAND)
    {
        lexer->popFront(); // '&'
        auto rvalue = parse_rvalue(lexer);
        auto ret = addressOf(rvalue, lexer);
        lexer->revert();
        ret->metadata.annotations = annotations;
        return postfix(ret, lexer);
    }

    else if (lexer->front.tok == Tok::LPAREN)
    {
        lexer->popFront(); // '('

        primary = parse_rvalue(lexer);

        if (lexer->front.tok != Tok::RPAREN)
        {
            primary = parseError("expected ')'", lexer->front.region);
        }
        lexer->popFront(); // ')'
    }

    else if(lexer->front.tok == Tok::STRINGLITERAL)
    {
        t = lexer->front;
        lexer->popFront();
        primary = stringLiteral(t.value, lexer);
    }
    else if (lexer->front.tok == Tok::CHARACTER)
    {
        t = lexer->front;
        lexer->popFront();

        // todo(chad): utf-8
        primary = character(t.value[0], lexer);
    }

    else if (lexer->front.tok == Tok::REC)
    {
        lexer->popFront();
        primary = rec(lexer);
    }

    else if (lexer->front.tok == Tok::TRUE)
    {
        lexer->popFront(); // 'true'
        primary = constBool(true, lexer);
    }
    else if (lexer->front.tok == Tok::FALSE)
    {
        lexer->popFront(); // 'false'
        primary = constBool(false, lexer);
    }

    else if (lexer->front.tok == Tok::STR)
    {
        t = lexer->front;
        lexer->popFront();
        primary = symbol(t.value, lexer);

        if (primary->type == NodeType::NONE)
        {
            primary = typeTuple({}, lexer);
        }

        if (lexer->front.tok == Tok::RIGHTARROW)
        {
            lexer->popFront(); // '=>'

            Node *functionNode;

            assert(primary->type == NodeType::SYMBOL);
            auto primaryName = primary->data.symbol->value;

            if (lexer->front.tok == Tok::LCURLY)
            {
                lexer->popFront(); // '{'

                vector<Node *> statements;
                while (lexer->front.tok != Tok::RCURLY)
                {
                    lexer->popFront(); // '{'

                    while (lexer->front.tok != Tok::RCURLY)
                    {
                        statements.push_back(parse_statement(lexer));
                    }

                    if (lexer->front.tok != Tok::RCURLY)
                    {
                        lexer->revert();
                        return parseError("expected '}'", t.region);
                    }
                    lexer->popFront(); // '}'

                    functionNode = function_({ parameter(primaryName, none(), none(), 0, lexer) }, statements, lexer);
                }
            }
            else
            {
                functionNode = function_({ parameter(primaryName, none(), none(), 0, lexer) }, { ret(parse_rvalue(lexer), lexer) }, lexer);
            }

            lexer->revert();
            functionNode->metadata.annotations = annotations;
            return functionNode;
        }
    }

    else if (lexer->front.tok == Tok::NUM)
    {
        primary = getConstFromTok(lexer, t);
    }

    else if (lexer->front.tok == Tok::HASH)
    {
        t = lexer->front;
        lexer->popFront(); // '#'

        if (lexer->front.tok != Tok::LCURLY)
        {
            primary = parseError("expected '{'", t.region);
        }
        lexer->popFront(); // '{'

        auto valueTupleArguments = parse_value_tuple_arguments(lexer);

        if (!valueTupleArguments.empty() && valueTupleArguments[0]->type == NodeType::PARSEERROR)
        {
            primary = valueTupleArguments[0];
        }
        if (lexer->front.tok != Tok::RCURLY)
        {
            primary = parseError("expected '}'", t.region);
        }
        else
        {
            lexer->popFront(); // '}'

            if (lexer->constantContext.top())
            {
                primary = constTuple(ValueTuple{valueTupleArguments}, lexer);
            }
            else
            {
                primary = valueTuple(valueTupleArguments, lexer);
            }
        }
    }

    else if (lexer->front.tok == Tok::LCURLY)
    {
        auto inputType = parse_type_simple(lexer);

        if (lexer->front.tok != Tok::RIGHTARROW)
        {
            primary = parseError("expected '=>'", t.region);
        }
        lexer->popFront(); // '=>'

        vector<Node *> rv;
        if (lexer->front.tok == Tok::LCURLY)
        {
            lexer->popFront(); // '{'

            while (lexer->front.tok != Tok::RCURLY)
            {
                auto newStatement = parse_statement(lexer);
                if (newStatement->type == NodeType::PARSEERROR)
                {
                    lexer->revert();
                    newStatement->metadata.annotations = annotations;
                    return postfix(newStatement, lexer);
                }
                rv.push_back(newStatement);
            }

            if (lexer->front.tok != Tok::RCURLY)
            {
                primary = parseError("expected '}'", t.region);
            }
            lexer->popFront();
        }
        else
        {
            rv.push_back(ret(parse_rvalue(lexer), lexer));
        }

        auto inputTypes = getTypeTuple(inputType);
        auto ret = function_(inputTypes, rv, lexer);
        lexer->revert();
        ret->metadata.annotations = annotations;
        return postfix(ret, lexer);
    }
    else
    {
        lexer->revert();
        return parseError(fmt::format("expected identifier, not {}", lexer->front.value), t.region);
    }

    while (lexer->front.tok == Tok::DOT || lexer->front.tok == Tok::LPAREN)
    {
        // dot
        if (lexer->front.tok == Tok::DOT)
        {
            lexer->popFront(); // '.'

            if (lexer->front.tok == Tok::NUM)
            {
                primary = dot(primary,
                              constInt(ConstantType::I32, parse_int(lexer->front.value), lexer),
                              lexer);
                lexer->popFront();
            }
            else if (lexer->front.tok != Tok::STR)
            {
                t = lexer->front;
                lexer->popFront();
                lexer->revert();
                return parseError(fmt::format("expected identifier, not '{}'", t.value), t.region);
            }
            else
            {
                primary = dot(primary, symbol(lexer->front.value, lexer), lexer);
                lexer->popFront();
            }
        }

        // optional (multiple) function calls
        if (lexer->front.tok == Tok::LPAREN)
        {
            if (lexer->front.tok != Tok::LPAREN)
            {
                lexer->revert();
                primary->metadata.annotations = annotations;
                return postfix(primary, lexer);
            }

            lexer->popFront();

            vector<Node *> arguments;

            while (lexer->front.tok != Tok::RPAREN)
            {
                auto name = string("");

                if (lexer->buffer.tok == Tok::COLON)
                {
                    name = lexer->front.value;
                    lexer->popFront(); // name
                    lexer->popFront(); // ':'
                }

                auto value = parse_rvalue(lexer);
                arguments.push_back(argument(name, value, lexer));

                if (lexer->front.tok == Tok::RPAREN)
                {
                    continue;
                }

                if (lexer->front.tok != Tok::COMMA)
                {
                    lexer->revert();
                    return parseError("expected ','", lexer->front.region);
                }
                lexer->popFront(); // ','
            }

            if (lexer->front.tok != Tok::RPAREN)
            {
                lexer->revert();
                return parseError("expected ')'", lexer->front.region);
            }
            lexer->popFront(); // ')'

            primary = apply(primary, arguments, lexer);
        }
    }

    lexer->revert();
    primary->metadata.annotations = annotations;
    return postfix(primary, lexer);
}

Node *parse_type_simple(Lexer* lexer)
{
    lexer->save();
    Node *simple_type = nullptr;

    if (lexer->front.tok == Tok::REC)
    {
        lexer->popFront();
        return rec(lexer);
    }
    else if (lexer->front.tok == Tok::LPAREN)
    {
        lexer->popFront(); // '('

        simple_type = parse_type(lexer);

        if (lexer->front.tok != Tok::RPAREN)
        {
            return parseError("expected ')'", lexer->front.region);
        }
        lexer->popFront(); // ')'
    }
    if (lexer->front.tok == Tok::STAR)
    {
        lexer->popFront(); // '*'
        simple_type = typePointer(parse_type_simple(lexer), lexer);
    }
    if (lexer->front.tok == Tok::TYPEOF)
    {
        lexer->popFront(); // 'typeof'
        auto typ = typeof_(parse_rvalue_simple(lexer), false, lexer);
        simple_type = typ;
    }
    if (lexer->front.tok == Tok::RETTYPE)
    {
        lexer->popFront(); // 'rt'
        auto typ = typeof_(parse_rvalue_simple(lexer), true, lexer);
        simple_type = typ;
    }

    if (lexer->front.tok == Tok::ENUM)
    {
        lexer->popFront(); // 'enum'

        if (lexer->front.tok != Tok::LCURLY)
        {
           return parseError("expected '{'", lexer->front.region);
        }
        lexer->popFront(); // '{'

        auto fields = parse_type_tuple_arguments(lexer);

        if (lexer->front.tok != Tok::RCURLY)
        {
            return parseError("expected '}'", lexer->front.region);
        }
        lexer->popFront(); // '}'
    }

    if (lexer->front.tok != Tok::LCURLY && lexer->front.tok != Tok::STR)
    {
        lexer->revert();
        if (simple_type == nullptr)
        {
            return parseError("could not parse type", lexer);
        }
        return simple_type;
    }

    else if (lexer->front.tok != Tok::LCURLY)
    {
        assert(lexer->front.tok == Tok::STR);
        auto typeName = lexer->front.value;
        lexer->popFront();
        simple_type = symbol(typeName, lexer);
    }

    else
    {
        auto t = lexer->front;
        lexer->popFront(); // '{'

        auto types = parse_type_tuple_arguments(lexer);

        if (lexer->front.tok != Tok::RCURLY)
        {
            lexer->revert();
            return parseError("expected '}'", Region{t.region.fileName, t.region.start, lexer->front.region.end});
        }
        lexer->popFront(); // '}'

        simple_type = typeTuple(types, lexer);
    }

    while (lexer->front.tok == Tok::DOT)
    {
        lexer->popFront(); // '.'

        if (lexer->front.tok != Tok::STR)
        {
            auto t = lexer->front;
            lexer->popFront();
            lexer->revert();
            return parseError(fmt::format("expected identifier, not {}", t.value), t.region);
        }
        auto memberName = lexer->front;
        lexer->popFront();

        simple_type = dot(simple_type, symbol(memberName.value, lexer), lexer);
    }

    lexer->revert();
    return simple_type;
}

int parse_binary_int(string s)
{
    auto power = s.length() - 3;
    auto value = 0;
    for (int i = 2; i < s.length(); i++)
    {
        if (s[i] == '1') { value += pow(2, power); }
        power -= 1;
    }
    return value;
}

int hex_char_to_int(char s)
{
    switch (s)
    {
        case '0': return 0;
        case '1': return 1;
        case '2': return 2;
        case '3': return 3;
        case '4': return 4;
        case '5': return 5;
        case '6': return 6;
        case '7': return 7;
        case '8': return 8;
        case '9': return 9;
        case 'A': return 10;
        case 'B': return 11;
        case 'C': return 12;
        case 'D': return 13;
        case 'E': return 14;
        case 'F': return 15;
        default: return 0;
    }
}

int parse_hex_int(string s)
{
    auto power = s.length() - 3;
    auto value = 0;
    for (int i = 2; i < s.length(); i++)
    {
        auto intvalue = hex_char_to_int(s[i]);
        value += intvalue * pow(16, power);
        power -= 1;
    }
    return value;
}

int parse_int(string s)
{
    istringstream buffer(s);
    int value;
    buffer >> value;
    return value;
}

float parse_float(string s)
{
    istringstream buffer(s);
    float value;
    buffer >> value;
    return value;
}

vector<Node *> parse_type_tuple_arguments(Lexer *lexer)
{
    lexer->save();
    vector<Node *> types;

    while (lexer->front.tok != Tok::RCURLY)
    {
        // if there's a ':' next, parsing something like "a : i32" or "a : i32 = 3"
        if (lexer->buffer.tok == Tok::COLON)
        {
            if (lexer->front.tok != Tok::STR)
            {
                types.push_back(parseError(fmt::format("expected identifier, not {}", lexer->front.value), lexer->front.region));
                lexer->popFront();
            }

            auto name = lexer->front.value;
            lexer->popFront(); // name
            lexer->popFront(); // ':'
            auto type = parse_type(lexer);

            if (type == nullptr)
            {
                return { parseError("could not parse type", lexer->front.region) };
            }

            auto rvalue = none();
            if (lexer->front.tok == Tok::EQ)
            {
                lexer->popFront(); // '='
                rvalue = parse_rvalue(lexer);
            }
            types.push_back(parameter(name, type, rvalue, (unsigned int) types.size(), lexer));

            if (lexer->front.tok == Tok::RCURLY)
            {
                continue;
            }

            if (lexer->front.tok != Tok::COMMA)
            {
                lexer->revert();
                return { parseError("expected ','", lexer->front.region) };
            }
            lexer->popFront(); // ','

            continue;
        }

        // if there's a ':=' next, parsing something like "a := 3"
        else if (lexer->buffer.tok == Tok::COLONEQ)
        {
            auto name = lexer->front.value;
            lexer->popFront(); // type name
            lexer->popFront(); // ':='
            auto defaultValue = parse_rvalue(lexer);
            types.push_back(parameter(name, none(), defaultValue, (unsigned int) types.size(), lexer));

            if (lexer->front.tok == Tok::COMMA)
            {
                lexer->popFront(); // ','
            }

            continue;
        }

        // assume we must be parsing a type.
        // either something like `*i32` or `*i32 = &3`
        auto type = parse_type(lexer);
        auto defaultValue = none();

        if (lexer->front.tok == Tok::COMMA)
        {
            lexer->popFront(); // ','
            types.push_back(parameter("", type, defaultValue, (unsigned int) types.size(), lexer));
            continue;
        }
        else if (lexer->front.tok == Tok::EQ)
        {
            lexer->popFront(); // '='
            defaultValue = parse_rvalue(lexer);
            types.push_back(parameter("", type, defaultValue, (unsigned int) types.size(), lexer));
            continue;
        }
        else if (lexer->front.tok == Tok::RCURLY)
        {
            auto name = lexer->front.value;
            types.push_back(parameter("", type, defaultValue, (unsigned int) types.size(), lexer));
            break;
        }
        else
        {
            // error
            lexer->popFront();
            return { parseError(fmt::format("unexpected token {}", lexer->front.value), lexer->front.region) };
        }
    }

    lexer->revert();
    return types;
}

vector<Node *> parse_value_tuple_arguments(Lexer *lexer)
{
    lexer->save();
    vector<Node *> types;

    while (lexer->front.tok != Tok::RCURLY)
    {
        // if there's a ':' next, parsing something like "a : 3"
        if (lexer->buffer.tok == Tok::COLON)
        {
            if (lexer->front.tok != Tok::STR)
            {
                lexer->popFront();
                return { parseError("unexpected", lexer->front.region) };
            }
            else
            {
                auto name = lexer->front.value;
                lexer->popFront(); // name
                lexer->popFront(); // ':'
                auto rvalue = parse_rvalue(lexer);
                types.push_back(parameter(name, none(), rvalue, (unsigned int) types.size(), lexer));

                if (lexer->front.tok == Tok::RCURLY)
                {
                    continue;
                }

                if (lexer->front.tok != Tok::COMMA)
                {
                    lexer->revert();
                    return { parseError("expected ','", lexer->front.region) };
                }
                lexer->popFront(); // ','
            }
        }

        // if there's a ',' next, parsing something like "3, 17 + foo()"
        else
        {
            auto rvalue = parse_rvalue(lexer);
            types.push_back(parameter("", none(), rvalue, (unsigned int) types.size(), lexer));

            if (lexer->front.tok == Tok::RCURLY)
            {
                continue;
            }

            if (lexer->front.tok != Tok::COMMA)
            {
                lexer->revert();
                return { parseError("expected ','", lexer->front.region) };
            }
            lexer->popFront(); // ','
        }
    }

    lexer->revert();
    return types;
}

vector<Node *> getTypeTuple(Node *n)
{
    if (n->type == NodeType::TYPETUPLE)
    {
        return n->data.typeTuple->types;
    }
    return { n };
}

Node *getConstFromTok(Lexer* l, Token t)
{
    l->popFront(); // number

    auto isFloat = false;
    for (auto i = 0; i < t.value.size(); i++)
    {
        if (t.value[i] == '.')
        {
            isFloat = true;
        }
    }
    if (isFloat)
    {
        return constFloat(ConstantType::F32, parse_float(t.value), l);
    }

    auto isBinary = false;
    for (auto i = 0; i < t.value.size(); i++)
    {
        if (t.value[i] == 'b')
        {
            isBinary = true;
        }
    }
    if (isBinary)
    {
        return constInt(ConstantType::I32, parse_binary_int(t.value), l);
    }

    auto isHex = false;
    for (auto i = 0; i < t.value.size(); i++)
    {
        if (t.value[i] == 'x')
        {
            isHex = true;
        }
    }
    if (isHex)
    {
        return constInt(ConstantType::I32, parse_hex_int(t.value), l);
    }

    return constInt(ConstantType::I32, parse_int(t.value), l);
}

string readFile(string fileName)
{
    if (fileName.length() > 0 && fileName[0] != '/')
    {
        char currentPath[FILENAME_MAX];
        fileName = string(getcwd(currentPath, sizeof(currentPath))) + "/" + fileName;
    }

    ifstream ifs(fileName);
    string content((istreambuf_iterator<char>(ifs)), (istreambuf_iterator<char>()));
    return content;
}
