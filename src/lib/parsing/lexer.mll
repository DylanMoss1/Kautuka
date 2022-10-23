{
    open Parser
    open Lexing

    exception LexerError of string

    let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = pos.pos_lnum + 1
      }
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let int = '-'? digit+
let id = ((alpha) (alpha|digit|'_')*) | "_"
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read_token = parse
  | whitespace                       { read_token lexbuf }
  | '('                              { LPAREN }
  | ')'                              { RPAREN }
  | '{'                              { LBRACE }
  | '}'                              { RBRACE }
  | "int"                            { T_INT }
  | "bool"                           { T_BOOL }
  | "string"                         { T_STRING }
  | "+"                              { PLUS }
  | "-"                              { MINUS }
  | "*"                              { MULT }
  | "/"                              { DIV }
  | "="                              { EQUALS }
  | "=="                             { EQ }
  | "!="                             { NE }
  | "<"                              { LT }
  | "<="                             { LE }
  | ">"                              { GT }
  | ">="                             { GE }
  | "&&"                             { AND }
  | "||"                             { OR }
  | "package"                        { PACKAGE }
  | "val"                            { VALUE }
  | ','                              { COMMA }
  | int as i                         { INT (int_of_string i) }
  | id as s                          { ID (s)}
  | newline { next_line lexbuf; read_token lexbuf }
  | eof                              { EOF }
  | _ {raise (LexerError ("Lexer Error - Illegal character: " ^ Lexing.lexeme lexbuf)) }