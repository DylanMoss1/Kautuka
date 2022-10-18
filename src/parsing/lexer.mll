{
    open Lexing
    
    exception SyntaxError of string

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
let id = (alpha) (alpha|digit|'_')*
let whitespace = [' ' '\t']+ 
let newline = '\r' | '\n' | "\r\n"

rule read_token = parse
    | "+" { PLUS }
    | whitespace { read_token lexbuf }
    | int { INT (int_of_string (Lexing.lexeme lexbuf))}
    | newline { next_line lexbuf; read_token lexbuf }
    | eof { EOF }
    | _ {raise (SyntaxError ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }