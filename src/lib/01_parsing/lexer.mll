{
    open Parser
    open Lexing

    exception Lexer_error of string

    let next_line lexbuf =
      let pos = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <-
        { pos with pos_bol = lexbuf.lex_curr_pos;
                  pos_lnum = pos.pos_lnum + 1
        }
}

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let int = '-'? digit+
let id = ((alpha) (alpha|digit|'_')*) | "_"
let func_call_id = id whitespace ['(']

rule read_token = parse
  | whitespace                       { read_token lexbuf }
  | newline                          { next_line lexbuf; read_token lexbuf }
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
  | ";"                              { SEMICOLON }
  | "var"                            { VAR }
  | "package"                        { PACKAGE }
  | "</>"                            { IGNORE }
  | "<!>"                            { FORCEPAR }
  | "<$>"                            { FORCESEQ }
  | "if"                             { IF }
  | "else"                           { ELSE }
  | "else if"                        { ELIF }
  | "while"                          { WHILE }
  | "for"                            { FOR }
  | "range"                          { RANGE }
  | ";"                              { SEMICOLON }
  | "break"                          { BREAK }
  | "continue"                       { CONTINUE }
  | "print"                          { PRINT }
  | "input"                          { INPUT }
  | "open"                           { OPEN }
  | "read"                           { READ }
  | "write"                          { WRITE }
  | "append"                         { APPEND }
  | "++"                             { INCREMENT }
  | "--"                             { DECREMENT }
  | "+"                              { PLUS }
  | "-"                              { MINUS }
  | "*"                              { MULT }
  | "=="                             { EQ }
  | "!="                             { NE }
  | "<"                              { LT }
  | "<="                             { LE }
  | ">"                              { GT }
  | ">="                             { GE }
  | "&&"                             { AND }
  | "||"                             { OR }
  | "!"                              { NOT }
  | id as s                          { ID (s) }
  | eof                              { EOF }
  | _ { raise (Lexer_error ("Lexer Error - Illegal character: " ^ Lexing.lexeme lexbuf)) }

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
  | _ { raise (Lexer_error ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Lexer_error ("String is not terminated")) }