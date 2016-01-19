#ifndef LEXER_H
#define LEXER_H

#include <cppformat/format.h>
#include <stack>

#define START_LINE 1
#define START_COL 1

// colors for terminal output
#define KNRM  "\x1b[0m"
#define KRED  "\x1b[31;1m"
#define KGRN  "\x1b[32;1m"
#define KYEL  "\x1b[33;1m"
#define KBLU  "\x1b[34;1m"
#define KMAG  "\x1b[35;1m"
#define KCYN  "\x1b[36;1m"
#define KWHT  "\x1b[37;1m"

using namespace std;

struct Location
{
    unsigned long line;
    unsigned long col;

    Location() : line(START_LINE), col(START_COL) { }

    Location(unsigned long _line, unsigned long _col) :
        line(_line),
        col(_col) { }

    friend std::ostream &operator<<(std::ostream &os, const Location &location)
    {
        return os << location.line << ":" << location.col;
    }
};

struct Region
{
    string fileName;
    Location start;
    Location end;

    Region() : start(Location()), end(Location()) { }
    Region(string fileName, unsigned long line, unsigned long startCol, unsigned long endCol) :
        fileName(fileName),
        start(Location(line, startCol)),
        end(Location(line, endCol)) { }
    Region(string fileName, Location start, Location end) : fileName(fileName), start(start), end(end) { }

    friend std::ostream &operator<<(std::ostream &os, const Region &region)
    {
        if (region.start.line == region.end.line)
        {
            return os << region.fileName << " " << region.start << "-" << region.end.col;
        }

        return os << region.fileName << " " << region.start << "-" << region.end;
    }
};

enum class Tok
{
    NONE = 0,
    EQEQ = 1,
    NEQ = 2,
    LT = 3,
    LTE = 4,
    GT = 5,
    GTE = 6,
    AND = 7,
    OR = 8,
    XOR = 9,
    MOD = 10,
    STAR = 11,
    FSLASH = 12,
    PLUS = 13,
    MINUS = 14,
    PIPE = 15,
    DOTDOTPIPE = 16,
    BITAND = 17,
    BITOR = 18,
    SHL = 19,
    SHR = 20,
    ASHR = 21,

    LPAREN,
    RPAREN,
    LCURLY,
    RCURLY,
    HASH,
    SKINNYARROW,
    COMMENT,
    NIL,
    REC,
    AMPERSAND,
    PLUSEQ,
    SUBEQ,
    MULEQ,
    DIVEQ,
    OREQ,
    ANDEQ,
    BITANDEQ,
    BITOREQ,
    SHLEQ,
    SHREQ,
    ASHREQ,
    MODEQ,
    RIGHTARROW,
    COLONEQ,
    COLON,
    COLONCOLONEQ,
    COLONCOLON,
    EQ,
    TYPE,
    MODULE,
    IMPORT,
    COMMA,
    RET,
    SEMICOLON,
    STR,
    NUM,
    DOTDOT,
    NOT,
    QUESTION,
    DOT,
    IF,
    ELIF,
    ELSE,
    WHILE,
    DEFER,
    END,
    ERR,
    TYPEOF,
    RETTYPE,
    ENUM,
    REQUIRE,
    STRINGLITERAL,
    CHARACTER,
    ANNOTATION,
    CAST,
    HEAP,
    SIZEOF,
    ASSERT,
    TRUE,
    FALSE,
    DO
};

enum class OpType
{
    NONE = 0,
    EQEQ = 1,
    NEQ = 2,
    LT = 3,
    LTE = 4,
    GT = 5,
    GTE = 6,
    AND = 7,
    OR = 8,
    XOR = 9,
    MOD = 10,
    STAR = 11,
    FSLASH = 12,
    PLUS = 13,
    MINUS = 14,
    PIPE = 15,
    DOTDOTPIPE = 16,
    BITAND = 17,
    BITOR = 18,
    SHL = 19,
    SHR = 20,
    ASHR = 21
};

OpType toOp(Tok tok);

// Tok helpers
string TokToString(Tok tok);
int precedence(OpType op);
bool isBinaryOp(Tok op);
bool isBinaryEq(Tok op);

map<Tok, string> specialTokens();
map<Tok, string> keywords();
pair<Tok, string> getSpecial(string s);

bool isIdChar(char c);

struct Token
{
    Tok tok;
    string value;
    Region region;

    Token() : tok(Tok::NONE), value(""), region(Region()) { }
    Token(Tok _tok, string _value, Region _region) : tok(_tok), value(_value), region(_region) { }
};

struct Lexer
{
    string s;

    string fileName;

    Token front;
    Token buffer;

    Location lastLoc;
    Location loc;

    stack<Location> saved;
    stack<bool> constantContext;

    Lexer(string _s, string fileName) :
        s(_s),
        fileName(fileName),
        loc(Location()),
        front(Token()),
        buffer(Token()),
        lastLoc(Location()) { }

    Token computeFront();
    void popFront();
    void eat(int length);
    void save(bool constantContext = false);
    void revert();
};

Lexer* lex(string program, string fileName);
void lexer_debug(string program);

#endif
