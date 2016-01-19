#ifndef PARSER_H
#define PARSER_H

#include <cppformat/format.h>
#include <json.hpp>

#include "node.h"

using namespace std;

Node *parse_unit(Lexer* lexer);
Node *parse_unit_from_json(string jsonSrc);
Node *parse_module(Lexer* lexer);
Node *parse_statement(Lexer* lexer);
Node *parse_rvalue(Lexer* lexer);
Node *parse_import(Lexer* lexer);
Node *parse_type(Lexer* lexer);
Node *parse_type_simple(Lexer* lexer);
Node *parse_rvalue_simple(Lexer* lexer);
Node *parse_do(Lexer *lexer);
int parse_int(string s);
float parse_float(string s);

vector<Node*> parse_type_tuple_arguments(Lexer *lexer);
vector<Node*> parse_value_tuple_arguments(Lexer *lexer);

Node* getConstFromTok(Lexer* l, Token t);

vector<Node*> getTypeTuple(Node* n);

string readFile(string fileName);
Node *parse_json_node(nlohmann::json j);

#endif
