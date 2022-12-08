open Core
open Parsing
open Annotated_ast

let usage_msg = "x [--debug]"
let debug = ref false
let anon_fun _ = ()
let speclist = [ "--debug", Arg.Set debug, "Output intermediary steps" ]

let () =
  Arg.parse speclist anon_fun usage_msg;
  print_endline "\n";
  In_channel.read_all "./files/kau_program.kau"
  |> Lex_and_parse.parse ~debug: !debug
  |> fun parse_result ->
  match parse_result with
  | Ok ast ->
    Out_channel.write_all
      "./files/compiled_program.go"
      ~data:(Init_ast.string_of_ast (Init_ast.annotated_ast_of_ast ast))
  | Error error ->
    (match error with
    | `Lexer_error msg -> print_endline msg
    | `Parser_error msg -> print_endline msg)
