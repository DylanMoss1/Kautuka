open Core
open Lexing
open Parser
open Format
open Parser_types

let pprint_tokens ppf = function
  | LPAREN -> Fmt.pf ppf "LPAREN@."
  | RPAREN -> Fmt.pf ppf "RPAREN@."
  | LBRACE -> Fmt.pf ppf "LBRACE@."
  | RBRACE -> Fmt.pf ppf "RBRACE@."
  | EQUALS -> Fmt.pf ppf "EQUALS@."
  | DECL -> Fmt.pf ppf "DECL@."
  | T_INT -> Fmt.pf ppf "T_INT@."
  | T_BOOL -> Fmt.pf ppf "T_BOOL@."
  | T_STRING -> Fmt.pf ppf "T_STRING@."
  | INT i -> Fmt.pf ppf "INT(%d)@." i
  | BOOL true -> Fmt.pf ppf "BOOL(true)@."
  | BOOL false -> Fmt.pf ppf "BOOL(false)@."
  | STRING s -> Fmt.pf ppf "STRING(%s)@." s
  | FUNC -> Fmt.pf ppf "FUNC@."
  | COMMA -> Fmt.pf ppf "COMMA@."
  | VAR -> Fmt.pf ppf "VAR@."
  | PACKAGE -> Fmt.pf ppf "PACKAGE@."
  | IGNORE -> Fmt.pf ppf "IGNORE@."
  | FORCEPAR -> Fmt.pf ppf "FORCEPAR@."
  | FORCESEQ -> Fmt.pf ppf "FORCESEQ@."
  | IF -> Fmt.pf ppf "IF@."
  | ELSE -> Fmt.pf ppf "ELSE@."
  | ELIF -> Fmt.pf ppf "ELIF@."
  | WHILE -> Fmt.pf ppf "WHILE@."
  | FOR -> Fmt.pf ppf "FOR@."
  | RANGE -> Fmt.pf ppf "RANGE@."
  | SEMICOLON -> Fmt.pf ppf "SEMICOLON@."
  | BREAK -> Fmt.pf ppf "BREAK@."
  | CONTINUE -> Fmt.pf ppf "CONTINUE@."
  | PRINT -> Fmt.pf ppf "PRINT@."
  | INPUT -> Fmt.pf ppf "INPUT@."
  | OPEN -> Fmt.pf ppf "OPEN@."
  | READ -> Fmt.pf ppf "READ@."
  | WRITE -> Fmt.pf ppf "WRITE@."
  | APPEND -> Fmt.pf ppf "APPEND@."
  | INCREMENT -> Fmt.pf ppf "INCREMENT@."
  | DECREMENT -> Fmt.pf ppf "DECREMENT@."
  | PLUS -> Fmt.pf ppf "PLUS@."
  | MINUS -> Fmt.pf ppf "MINUS@."
  | MULT -> Fmt.pf ppf "MULT@."
  | DIV -> Fmt.pf ppf "DIV@."
  | MOD -> Fmt.pf ppf "MOD@."
  | EQ -> Fmt.pf ppf "EQ@."
  | NE -> Fmt.pf ppf "NE@."
  | LT -> Fmt.pf ppf "LT@."
  | LE -> Fmt.pf ppf "LE@."
  | GT -> Fmt.pf ppf "GT@."
  | GE -> Fmt.pf ppf "GE@."
  | AND -> Fmt.pf ppf "AND@."
  | OR -> Fmt.pf ppf "OR@."
  | NOT -> Fmt.pf ppf "NOT@."
  | ID s -> Fmt.pf ppf "ID(%s)@." s
  | EOF -> Fmt.pf ppf "EOF@."


type error =
  [ `Lexer_error of string
  | `Parser_error of string
  ]

let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Fmt.str "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)


let pprint_lexbuf lexbuf =
  while lexbuf.lex_eof_reached do
    pprint_tokens std_formatter (Lexer.read_token lexbuf)
  done


let init_pos file_name =
  { pos_fname = file_name; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }


let parse ~debug input_program =
  if debug
  then (
    print_endline "LEXER DEBUG INFO:\n";
    let lexbuf = Lexing.from_string input_program in
    pprint_lexbuf lexbuf;
    print_endline "");
  let lexbuf = Lexing.from_string input_program in
  try Ok (Parsed_ast.create (Parser.program Lexer.read_token lexbuf)) with
  | Lexer.Lexer_error msg ->
    let error_msg = Fmt.str "%s: %s@." (print_error_position lexbuf) msg in
    Result.Error (`Lexer_error error_msg)
  | Parser.Error ->
    let error_msg =
      Fmt.str "%s: Parsing Error@." (print_error_position lexbuf)
    in
    Result.Error (`Parser_error error_msg)
