open Core
open Parser

let string_of_token = function
  | NEWLINE -> Fmt.str "NEWLINE"
  | LPAREN -> Fmt.str "LPAREN"
  | RPAREN -> Fmt.str "RPAREN"
  | LBRACE -> Fmt.str "LBRACE"
  | RBRACE -> Fmt.str "RBRACE"
  | EQUALS -> Fmt.str "EQUALS"
  | DECL -> Fmt.str "DECL"
  | T_INT -> Fmt.str "T_INT"
  | T_BOOL -> Fmt.str "T_BOOL"
  | T_STRING -> Fmt.str "T_STRING"
  | INT i -> Fmt.str "INT(%d)" i
  | BOOL true -> Fmt.str "BOOL(true)"
  | BOOL false -> Fmt.str "BOOL(false)"
  | STRING s -> Fmt.str "STRING(%s)" s
  | FUNC -> Fmt.str "FUNC"
  | COMMA -> Fmt.str "COMMA"
  | VAR -> Fmt.str "VAR"
  | PACKAGE -> Fmt.str "PACKAGE"
  | IGNORE -> Fmt.str "IGNORE"
  | FORCEPAR -> Fmt.str "FORCEPAR"
  | FORCESEQ -> Fmt.str "FORCESEQ"
  | IF -> Fmt.str "IF"
  | ELSE -> Fmt.str "ELSE"
  | ELIF -> Fmt.str "ELIF"
  | FOR -> Fmt.str "FOR"
  | RANGE -> Fmt.str "RANGE"
  | SEMICOLON -> Fmt.str "SEMICOLON"
  | BREAK -> Fmt.str "BREAK"
  | CONTINUE -> Fmt.str "CONTINUE"
  | RETURN -> Fmt.str "RETURN"
  | PRINT -> Fmt.str "PRINT"
  | INPUT -> Fmt.str "INPUT"
  | OPEN -> Fmt.str "OPEN"
  | READ -> Fmt.str "READ"
  | WRITE -> Fmt.str "WRITE"
  | APPEND -> Fmt.str "APPEND"
  | INCREMENT -> Fmt.str "INCREMENT"
  | DECREMENT -> Fmt.str "DECREMENT"
  | PLUS -> Fmt.str "PLUS"
  | PLUS_EQUALS -> Fmt.str "PLUS_EQUALS"
  | MINUS -> Fmt.str "MINUS"
  | MULT -> Fmt.str "MULT"
  | EQ -> Fmt.str "EQ"
  | NE -> Fmt.str "NE"
  | LT -> Fmt.str "LT"
  | LE -> Fmt.str "LE"
  | GT -> Fmt.str "GT"
  | GE -> Fmt.str "GE"
  | AND -> Fmt.str "AND"
  | OR -> Fmt.str "OR"
  | NOT -> Fmt.str "NOT"
  | UNDERSCORE -> Fmt.str "UNDERSCORE"
  | ID s -> Fmt.str "ID(%s)" s
  | EOF -> Fmt.str "EOF"


let string_of_token_list token_list =
  let rec string_of_token_list_inner token_list acc =
    match token_list with
    | token :: token_list ->
      string_of_token_list_inner token_list (acc @ [ string_of_token token ])
    | [] -> acc
  in
  String.concat ~sep:"\n" (string_of_token_list_inner token_list [])
