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
  | newline { next_line lexbuf; read_token lexbuf }
  | '('                              { LPAREN }
  | ')'                              { RPAREN }
  | '{'                              { LBRACE }
  | '}'                              { RBRACE }
  | "="                              { EQUALS }
  | ":="                             { DECL }
  | "int"                            { T_INT }
  | "bool"                           { T_BOOL }
  | "string"                         { T_STRING }
  | int as i                         { INT (int_of_string i) }
  | "true"                           { BOOL (true) }
  | "false"                          { BOOL (false) }
  | '"'                              { read_string (Buffer.create 256) lexbuf }
  | "func"                           { FUNC }
  | ','                              { COMMA }
  | "var"                            { VAR }
  | "package"                        { PACKAGE }
  | id as s                          { ID (s) }
  | eof                              { EOF }
  (*
  | ":="                             { DECL }
  | "+"                              { PLUS }
  | "-"                              { MINUS }
  | "*"                              { MULT }
  | "/"                              { DIV }
  | "%"                              { MOD }
  | "=="                             { EQ }
  | "!="                             { NE }
  | "<"                              { LT }
  | "<="                             { LE }
  | ">"                              { GT }
  | ">="                             { GE }
  | "&&"                             { AND }
  | "||"                             { OR }
  | "!"                              { NOT }
  | "</>"                            { FRAGMENT }
  | "if"                             { IF }
  | "else"                           { ELSE }
  | "while"                          { WHILE }
  | "for"                            { FOR }
  | "range"                          { RANGE }
  | ";"                              { SEMICOLON }
  | _ {raise (LexerError ("Lexer Error - Illegal character: " ^ Lexing.lexeme lexbuf)) }
*)

  and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (LexerError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (LexerError ("String is not terminated")) }