{
    open Parser

    exception SyntaxError of string
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let identifier = (alpha) (alpha|digit|'_')*
(* TODO: Disambiguate unary '-' op from binary '-' op in typing phase *)
(* https://discuss.ocaml.org/t/bug-with-ocamllex/13413/9 *)
let integer = '-'? digit+
let newline = '\r' | '\n' | "\r\n"
let whitespace = [' ' '\t']+

let string_literal = "\"" (('\"' | '\\' | ['\x20'-'\x7e'])*) "\"" 

rule read_token =
    parse
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | "[" { LBRACKET }
    | "]" { RBRACKET }
    | "<" { LANGLE }
    | ">" { RANGLE }
    | "," { COMMA }
    | "->" { RARROW }
    | ":" { COLON }
    | ";" { SEMICOLON }
    | "=" { EQUAL }
    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { MULT }
    | "/" { DIV }
    | "%" { REM }
    | "and" { AND }
    | "or" { OR }
    | "!" { BANG }
    | "fn" { FUNCTION }
    | "nil" { UNIT }
    | "return" { RETURN }
    | "int" { TYPE_INT }
    | "bool" { TYPE_BOOL }
    | "unit" { TYPE_UNIT }
    | "str" { TYPE_STRING }
    | "true" { TRUE }
    | "false" { FALSE }
    | "if" { IF }
    | "else" { ELSE }
    | "module" { MODULE }
    | "import" { IMPORT }
    | string_literal { STRING_LITERAL (Lexing.lexeme lexbuf) }
    | whitespace { read_token lexbuf }
    | "#" { read_comment lexbuf }
    | integer { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | identifier { ID (Lexing.lexeme lexbuf) }
    | newline { Lexing.new_line lexbuf; read_token lexbuf }
    | eof { EOF }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and read_comment =
    parse
    | newline { Lexing.new_line lexbuf; read_token lexbuf }
    | eof { EOF }
    | _ { read_comment lexbuf }
