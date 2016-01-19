#include <vector>
#include <iostream>
#include "lexer.h"

OpType toOp(Tok tok)
{
    switch (tok)
    {
        case Tok::LT:
            return OpType::LT;
        case Tok::LTE:
            return OpType::LTE;
        case Tok::GT:
            return OpType::GT;
        case Tok::GTE:
            return OpType::GTE;
        case Tok::PLUS:
        case Tok::PLUSEQ:
            return OpType::PLUS;
        case Tok::MINUS:
        case Tok::SUBEQ:
            return OpType::MINUS;
        case Tok::PIPE:
            return OpType::PIPE;
        case Tok::DOTDOTPIPE:
            return OpType::DOTDOTPIPE;
        case Tok::STAR:
        case Tok::MULEQ:
            return OpType::STAR;
        case Tok::FSLASH:
        case Tok::DIVEQ:
            return OpType::FSLASH;
        case Tok::EQEQ:
            return OpType::EQEQ;
        case Tok::OR:
        case Tok::OREQ:
            return OpType::OR;
        case Tok::AND:
        case Tok::ANDEQ:
            return OpType::AND;
        case Tok::BITAND:
        case Tok::BITANDEQ:
            return OpType::BITAND;
        case Tok::BITOR:
        case Tok::BITOREQ:
            return OpType::BITOR;
        case Tok::MOD:
        case Tok::MODEQ:
            return OpType::MOD;
        case Tok::SHL:
        case Tok::SHLEQ:
            return OpType::SHL;
        case Tok::SHR:
        case Tok::SHREQ:
            return OpType::SHR;
        case Tok::ASHR:
        case Tok::ASHREQ:
            return OpType::ASHR;
        case Tok::NEQ:
            return OpType::NEQ;
        default:
            return OpType::NONE;
    }
}

bool isBinaryOp(Tok op)
{
    vector<Tok> tokens = {
        Tok::PLUS,
        Tok::PLUSEQ,
        Tok::MINUS,
        Tok::SUBEQ,
        Tok::STAR,
        Tok::MULEQ,
        Tok::FSLASH,
        Tok::DIVEQ,
        Tok::OREQ,
        Tok::ANDEQ,
        Tok::BITANDEQ,
        Tok::BITOREQ,
        Tok::MODEQ,
        Tok::SHL,
        Tok::SHLEQ,
        Tok::SHR,
        Tok::SHREQ,
        Tok::ASHR,
        Tok::ASHREQ,
        Tok::EQEQ,
        Tok::NEQ,
        Tok::LT,
        Tok::LTE,
        Tok::GT,
        Tok::GTE,
        Tok::AND,
        Tok::BITAND,
        Tok::BITOR,
        Tok::OR,
        Tok::XOR,
        Tok::MOD,
        Tok::PIPE,
        Tok::DOTDOTPIPE
    };

    for (auto tok : tokens)
    {
        if (tok == op) { return true; }
    }

    return false;
}

bool isBinaryEq(Tok op)
{
    vector<Tok> tokens = {
        Tok::PLUSEQ,
        Tok::SUBEQ,
        Tok::MULEQ,
        Tok::DIVEQ,
        Tok::ANDEQ,
        Tok::BITANDEQ,
        Tok::BITOREQ,
        Tok::OREQ,
        Tok::MODEQ,
        Tok::SHLEQ,
        Tok::SHREQ,
        Tok::ASHREQ
    };

    for (auto tok : tokens)
    {
        if (tok == op) { return true; }
    }

    return false;
}

map<Tok, string> specialTokens()
{
    return map<Tok, string>
    {
        { Tok::LPAREN, "(" },
        { Tok::RPAREN, ")" },
        { Tok::LCURLY, "{" },
        { Tok::RCURLY, "}" },
        { Tok::HASH, "#" },
        { Tok::PLUSEQ, "+=" },
        { Tok::SUBEQ, "-=" },
        { Tok::MULEQ, "*=" },
        { Tok::DIVEQ, "/=" },
        { Tok::OREQ, "or=" },
        { Tok::ANDEQ, "and=" },
        { Tok::BITANDEQ, "bitand=" },
        { Tok::SHREQ, "shr=" },
        { Tok::ASHREQ, "ashr=" },
        { Tok::SHLEQ, "shl=" },
        { Tok::BITOREQ, "bitor=" },
        { Tok::MODEQ, "mod=" },
        { Tok::COMMENT, "--" },
        { Tok::PLUS, "+" },
        { Tok::MINUS, "-" },
        { Tok::PIPE, "|" },
        { Tok::DOTDOTPIPE, "..|" },
        { Tok::STAR, "*" },
        { Tok::AMPERSAND, "&" },
        { Tok::NEQ, "!=" },
        { Tok::FSLASH, "/" },
        { Tok::EQEQ, "==" },
        { Tok::LTE, "<=" },
        { Tok::LT, "<" },
        { Tok::GTE, ">=" },
        { Tok::GT, ">" },
        { Tok::OR, "or" },
        { Tok::AND, "and" },
        { Tok::BITAND, "bitand" },
        { Tok::BITOR, "bitor" },
        { Tok::SHR, "shr" },
        { Tok::ASHR, "ashr" },
        { Tok::SHL, "shl" },
        { Tok::RIGHTARROW, "=>" },
        { Tok::SKINNYARROW, "->" },
        { Tok::COLONEQ, ":=" },
        { Tok::COLONCOLONEQ, "::=" },
        { Tok::COLON, ":" },
        { Tok::COLONCOLON, "::" },
        { Tok::EQ, "=" },
        { Tok::COMMA, "," },
        { Tok::SEMICOLON, ";" },
        { Tok::DOT, "." },
        { Tok::DOTDOT, ".." },
        { Tok::NOT, "!" },
        { Tok::QUESTION, "?" },
        { Tok::ANNOTATION, "@" },
    };
}

map<Tok, string> keywords()
{
    return map<Tok, string>
    {
        { Tok::MOD,        "mod" },
        { Tok::RET,        "ret" },
        { Tok::IF,         "if" },
        { Tok::DO,         "do" },
        { Tok::ELIF,       "elif" },
        { Tok::ELSE,       "else" },
        { Tok::WHILE,      "while" },
        { Tok::DEFER,      "defer" },
        { Tok::TYPEOF,     "typeof" },
        { Tok::RETTYPE,    "rt" },
        { Tok::TYPE,       "type" },
        { Tok::MODULE,     "module" },
        { Tok::IMPORT,     "import" },
        { Tok::REQUIRE,    "require" },
        { Tok::CAST,       "cast" },
        { Tok::HEAP,       "heap" },
        { Tok::SIZEOF,     "sizeof" },
        { Tok::ASSERT,     "assert" },
        { Tok::NIL,        "nil" },
        { Tok::TRUE,       "true" },
        { Tok::FALSE,      "false" },
        { Tok::REC,        "rec" },
        { Tok::ENUM,       "enum" }
    };
}

// lower precedence is stronger
int precedence(OpType op)
{
    switch (op)
    {
        case OpType::PIPE:
        case OpType::DOTDOTPIPE:
            return 1;
        case OpType::LT:
        case OpType::GT:
        case OpType::LTE:
        case OpType::GTE:
            return 2;
        case OpType::MOD:
        case OpType::STAR:
        case OpType::FSLASH:
            return 3;
        case OpType::PLUS:
        case OpType::MINUS:
            return 4;
        case OpType::EQEQ:
        case OpType::NEQ:
            return 5;
        case OpType::XOR:
        case OpType::OR:
        case OpType::AND:
            return 6;
        default:
            assert(false);
    }
}

bool isIdChar(char c)
{
    return isalpha(c) || isnumber(c) || c == '_';
}

pair<Tok, string> getSpecial(string s)
{
    vector<pair<Tok, string>> specials;
    for (const auto& t : specialTokens())
    {
        auto token = t.second;
        if (s.length() >= token.length() && s.substr(0, token.length()).compare(token) == 0)
        {
            specials.push_back(t);
        }
    }

    for (const auto& t : keywords())
    {
        auto keyword = t.second;
        bool isKeyword = s.length() >= keyword.length() && s.substr(0, keyword.length()).compare(keyword) == 0;
        if (isKeyword)
        {
            if (s.length() > keyword.length())
            {
                auto nextChar = s[keyword.length()];
                if (!isIdChar(nextChar))
                {
                    specials.push_back(t);
                }
            }
        }
    }

    if (!specials.empty())
    {
        auto max_size = 0;
        auto max_idx = 0;
        for (auto i = 0; i < specials.size(); i++)
        {
            if (specials[i].second.size() > max_size)
            {
                max_size = (int) specials[i].second.size();
                max_idx = i;
            }
        }
        return specials[max_idx];
    }

    return pair<Tok, string>(Tok::NONE, "");
}

Token Lexer::computeFront()
{
    // eof?
    if (s.length() == 0)
    {
        return Token(Tok::END, "", Region(fileName, loc, loc));
    }

    // eat newlines and whitespace
    while (s.length() > 0 && (s[0] == '\n' || isspace(s[0])))
    {
        // eat newlines
        if (s.length() > 0 && s[0] == '\n')
        {
            loc.line += 1;
            loc.col = START_LINE;
            s = s.substr(1, s.size());
        }

        // eat whitespace
        if (s.length() > 0 && isspace(s[0]) && s[0] != '\n')
        {
            unsigned long whitespaceLength = 1;
            auto slice = s.substr(1, s.length());
            while (slice.length() > 0 && isspace(slice[0]) && slice[0] != '\n')
            {
                slice = slice.substr(1, slice.length());
                whitespaceLength += 1;
            }

            eat((int) whitespaceLength);
        }
    }

    // check for comment
    if (s.length() >= 2 && s[0] == '-' && s[1] == '-')
    {
        auto i = 0;
        while (s.length() > 0 && s[i] != '\n')
        {
            i += 1;
        }

        // todo(chad): incorporate this comment into the ast instead of just throwing it away?
        auto ret = Token(Tok::COMMENT, s.substr(0, (unsigned long) i), Region(fileName, loc.line, loc.col, loc.col + i));
        eat(i);
        return computeFront();
    }

    // string literal
    while (s.length() > 0 && s[0] == '"')
    {
        if (s.length() == 1)
        {
            return Token(Tok::ERR, "cannot end with '\"'", Region(fileName, loc.line, loc.col, loc.col + 1));
        }

        auto strLitIdx = 1;
        while (strLitIdx != s.length() && s[strLitIdx] != '"')
        {
            strLitIdx += 1;
        }
        auto strLit = s.substr(1, (unsigned long) strLitIdx - 1);

        auto ret = Token(Tok::STRINGLITERAL, strLit, Region(fileName, loc.line, loc.col, loc.col + strLitIdx));
        eat(strLitIdx + 1);
        return ret;
    }

    // character literal
    while (s.length() > 0 && s[0] == '\'')
    {
        if (s.length() == 1)
        {
            return Token(Tok::ERR, "cannot end with \"\'\"", Region(fileName, loc.line, loc.col, loc.col + 1));
        }

        auto strLitIdx = 1;
        while (strLitIdx != s.length() && s[strLitIdx] != '\'')
        {
            strLitIdx += 1;
        }
        auto strLit = s.substr(1, (unsigned long) strLitIdx - 1);

        auto ret = Token(Tok::CHARACTER, strLit, Region(fileName, loc.line, loc.col, loc.col + strLitIdx));
        eat(strLitIdx + 1);
        return ret;
    }

    if (s.length() > 0 && s[0] == '`')
    {
        if (s.length() == 1)
        {
            return Token(Tok::ERR, "cannot end with '`'", Region(fileName, loc.line, loc.col, loc.col + 1));
        }

        auto strLitIdx = 1;
        while (strLitIdx != s.length() && s[strLitIdx] != '`')
        {
            strLitIdx += 1;
        }
        auto strLit = s.substr(1, (unsigned long) strLitIdx - 1);

        auto ret = Token(Tok::STR, strLit, Region(fileName, loc.line, loc.col, loc.col + strLitIdx));
        eat(strLitIdx + 1);
        return ret;
    }

    // special character
    bool pass = false;
    while (s.length() > 0 && !pass)
    {
        auto special = getSpecial(s);
        if (special.first != Tok::NONE)
        {
            // token?
            auto ret = Token(special.first, special.second, Region(fileName,
                                                                   loc.line,
                                                                   loc.col,
                                                                   loc.col + special.second.length()));
            eat((int) special.second.length());
            return ret;
        }
        else
        {
            pass = true;
        }
    }

    // numeric data
    if (s.length() > 0 && isnumber(s[0]))
    {
        auto i = 0;
        auto numDots = 0;
        string num_to_parse = "";
        while (s.length() >= i && (isnumber(s[i])
                                   || s[i] == '.'
                                   || s[i] == '_'
                                   || s[i] == 'b'
                                   || s[i] == 'x'
                                   || s[i] == 'A'
                                   || s[i] == 'B'
                                   || s[i] == 'C'
                                   || s[i] == 'D'
                                   || s[i] == 'E'
                                   || s[i] == 'F'
        ))
        {
            if (s[i] != '_')
            {
                num_to_parse += s[i];
            }
            i += 1;
        }

        auto ret = Token(Tok::NUM, num_to_parse, Region(fileName, loc.line, loc.col, loc.col + i));
        eat(i);
        return ret;
    }

    // generic alphanumeric identifier
    if (s.length() > 0 && isIdChar(s[0]))
    {
        auto i = 0;
        while (s.length() >= i && isIdChar(s[i]))
        {
            i += 1;
        }

        auto ret = Token(Tok::STR, s.substr(0, (unsigned long) i), Region(fileName,
                                                                          loc.line,
                                                                          loc.col,
                                                                          loc.col + i));
        eat(i);
        return ret;
    }

    // eof?
    if (s.length() == 0)
    {
        return Token(Tok::END, "", Region(fileName, loc, loc));
    }

    auto ret = Token(Tok::ERR, fmt::format("unexpected token {}", s[0]), Region(fileName,
                                                                                loc.line,
                                                                                loc.col,
                                                                                loc.col + 1));
    eat(1);
    return ret;
}

void Lexer::popFront()
{
    // save
    lastLoc = front.region.end;
    front = buffer;

    // update front
    buffer = computeFront();
}

void Lexer::eat(int length)
{
    assert(length <= s.size());
    s = s.substr((unsigned long) length, s.size());
    loc.col += length;
}

void Lexer::save(bool _constantContext)
{
    saved.push(front.region.start);
    constantContext.push(_constantContext);
}

void Lexer::revert()
{
    saved.pop();
    constantContext.pop();
}

Lexer* lex(string program, string fileName)
{
    auto lexer = new Lexer(program, fileName);
    lexer->popFront(); // pop initial 'NONE' token
    lexer->popFront(); // fill buffer
    return lexer;
}

void lexer_debug(string program, string fileName)
{
    auto lexer = new Lexer(program, fileName);
    lexer->popFront(); // pop initial 'NONE' token
    lexer->popFront(); // fill buffer

    while (lexer->front.tok != Tok::END)
    {
        cout << lexer->front.value << "  " << lexer->front.region << endl;
        lexer->popFront();
    }
}
