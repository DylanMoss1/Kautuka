open Core
open Parsing
open Ast.Pprint_ast

let () =
  In_channel.read_all "./files/kau_program.kau"
  |> Lexing.from_string
  |> Parser.program Lexer.read_token
  |> fun ast_program ->
  Out_channel.write_all "./files/compiled_program.go"
    ~data:(string_of_program ast_program)
