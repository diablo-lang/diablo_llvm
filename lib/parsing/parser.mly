%{
    open Ast.Ast_types
    open Parsed_ast
%}

%token <int> INT
%token <string> ID
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LANGLE
%token RANGLE
%token RARROW
%token NEWLINE
%token RETURN
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
%token TYPE_INT
%token TYPE_BOOL
%token TYPE_VOID
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

%start <Parsed_ast.program> program

%%

program:
    | list(NEWLINE); function_defns=list(function_defn); main=main_block; EOF { Program(function_defns, main) }
    ;

expr_terminator:
    | t=SEMICOLON { t }
    | t=NEWLINE { t }
    ;

block:
    | LBRACE; exprs=separated_list_with_optional_trailing_sep(expr_terminator, expr); RBRACE { Block(exprs) }
    ;

expr:
    | LPAREN; e=expr; RPAREN { e }
    | i=INT { Integer i }
    | id=ID { Identifier id }
    | TRUE { Boolean true }
    | FALSE { Boolean false }
    | op=un_op e=expr { UnOp(op, e) }
    | e1=expr op=bin_op e2=expr { BinOp(op, e1, e2) }
    | LET; id=ID; EQUAL; e=expr { Let(id, e) }
    | IF; cond_expr=expr; then_expr=block; ELSE; else_expr=block { If(cond_expr, then_expr, else_expr) }
    | fn=ID; fn_args=args { Call(fn, fn_args) }
    | RETURN; e=expr { e }
    ;

param:
    | param_type=diablo_type; name=ID; { TParam(param_type, name) }
    ;

params:
    | LPAREN; params=separated_list(COMMA, param); RPAREN { params }
    ;

args:
    | LPAREN; args=separated_list(COMMA, expr); RPAREN { args }

function_defn:
    | FUNCTION; name=ID; params=params; RARROW; return_type=diablo_type; body=block; list(NEWLINE) { TFunction(name, params, return_type, body) }
    ;

diablo_type:
    | TYPE_INT { TInt }
    | TYPE_BOOL { TBool }
    | TYPE_VOID { TVoid }
    ;

main_block:
    | MAIN; LPAREN; RPAREN; body=block; list(NEWLINE) { body }
    ;

separated_list_with_optional_trailing_sep(X, Y) :
    | list(NEWLINE); xs=Y; list(NEWLINE) { xs }
    | { [] }
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
