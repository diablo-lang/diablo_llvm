%{
    open Ast
%}

%token <int> INT
%token <string> ID
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LANGLE
%token RANGLE
%token COMMA
%token SEMICOLON
%token EQUAL
%token PLUS
%token MINUS
%token MULT
%token DIV
%token REM
%token AND
%token OR
%token BANG
%token LET
%token TRUE
%token FALSE
%token FUNCTION
%token IF
%token ELSE
%token MAIN
%token EOF

%right EQUAL
%left PLUS MINUS LANGLE RANGLE
%left MULT DIV REM
%left AND OR
%nonassoc BANG

%start <Ast.program> program

%%

program:
    | function_defns=list(function_defn); main=main_expr; EOF { Program(function_defns, main) }
    ;

block:
    | LBRACE; exprs=separated_list(SEMICOLON, expr); RBRACE { Block(exprs) }
    ;

expr:
    | LPAREN; e=expr; RPAREN {e}
    | i=INT { Integer i }
    | id=ID { Identifier id }
    | TRUE { Boolean true }
    | FALSE { Boolean false }
    | op=un_op e=expr { UnOp(op, e) }
    | e1=expr op=bin_op e2=expr { BinOp(op, e1, e2) }
    | LET; id=ID; EQUAL; e=expr { Let(id, e) }
    | IF; cond_expr=expr; then_block=block; ELSE; else_block=block { If(cond_expr, then_block, else_block) }
    ;

param:
    | name=ID; { Param(name) }
    ;

params:
    | LPAREN; params=separated_list(COMMA, param); RPAREN { params }
    ;

function_defn:
    | FUNCTION; name=ID; params=params; body=block; { Function(name, params, body) }
    ;

main_expr:
    | MAIN; LPAREN; RPAREN; body=block; { body }
    ;

%inline un_op:
    | BANG { UnOpNot }
    | MINUS { UnOpNegate }
    ;

%inline bin_op:
    | PLUS { BinOpPlus }
    | MINUS { BinOpMinus }
    | MULT { BinOpMult }
    | DIV { BinOpDiv }
    | REM { BinOpRem }
    | LANGLE { BinOpLessThan }
    | RANGLE { BinOpGreaterThan }
    | LANGLE EQUAL { BinOpLessThanEqual }
    | RANGLE EQUAL { BinOpGreaterThanEqual }
    | AND { BinOpAnd }
    | OR { BinOpOr }
    | EQUAL EQUAL { BinOpEqual }
    | BANG EQUAL { BinOpNotEqual }
    ;
