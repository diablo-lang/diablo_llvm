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
%token IF
%token ELSE
%token EOF

%right EQUAL
%left PLUS MINUS LANGLE RANGLE
%left MULT DIV REM
%left AND OR
%nonassoc BANG

%start <Ast.expr> program

%%

program:
    | e = expr; EOF { e }
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
