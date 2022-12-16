open! Core
open Parsing
open! Ast
open Parser_types
open Preperation

let usage_msg = "x [--debug]"
let debug = ref false
let anon_fun _ = ()
let speclist = [ "--debug", Arg.Set debug, "Output intermediary steps" ]

let () =
  Arg.parse speclist anon_fun usage_msg;
  print_endline "\n";
  In_channel.read_all "./files/kau_program.kau"
  |> Lex_and_parse.parse ~debug:!debug
  |> Result.map ~f:Imports.Import_pipeline.pipeline_program
  |> fun parse_result ->
  match parse_result with
  | Ok ast ->
    Out_channel.write_all
      "./files/compiled_program.go"
      ~data:(Parsed_ast.string_of_t ast)
  | Error error ->
    (match error with
    | `Lexer_error msg -> print_endline msg
    | `Parser_error msg -> print_endline msg)
