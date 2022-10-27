open Core
open Ast.Ast_pprint

let () =
  In_channel.read_all "./files/kau_program.kau"
  |> Lexing.from_string
  |> Parsing.Parser.program Parsing.Lexer.read_token
  |> fun ast_program ->
  Out_channel.write_all "./files/compiled_program.go"
    ~data:(string_of_program ast_program)
