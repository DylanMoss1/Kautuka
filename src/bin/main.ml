open Core
open Parsing
open Ast

let usage_msg = "x [--debug]"
let debug = ref false
let anon_fun _ = ()
let speclist = [ "--debug", Arg.Set debug, "Output intermediary steps" ]

let () =
  Arg.parse speclist anon_fun usage_msg;
  print_endline "\n";
  In_channel.read_all "./files/kau_program.kau"
  |> Lexing.from_string
  |> Lex_and_parse.parse
  |> fun parse_result ->
  match parse_result with
  | Ok ast ->
    Out_channel.write_all
      "./files/compiled_program.go"
      ~data:(Init_ast.string_of_ast ast)
  | Error error ->
    (match error with
    | `Lexer_error msg -> print_endline msg
    | `Parser_error msg -> print_endline msg)

(* let () =
  Arg.parse speclist anon_fun usage_msg;
  print_endline "\n";
  if !debug
  then
    In_channel.read_all "./files/kau_program.kau"
    |> Lexing.from_string
    |> (fun x ->
         print_endline "LEXER DEBUG INFO:\n";
         Lex_and_parse.debug x;
         set_position x (lexeme_start_p x);
         print_endline "";
         x)
    |> Lex_and_parse.parse
    (* |> Result.map ~f:parallelise_program *)
    |> fun ast_program ->
    match ast_program with
    | Ok ast_program ->
      Out_channel.write_all
        "./files/compiled_program.go"
        ~data:( ast_program)
    | Error error ->
      (match error with
      | `Lexer_error msg -> print_endline msg
      | `Parser_error msg -> print_endline msg)
  else
    In_channel.read_all "./files/kau_program.kau"
    |> Lexing.from_string
    |> Lex_and_parse.parse
    |> fun ast_program ->
    match ast_program with
    | Ok ast_program ->
      Out_channel.write_all
        "./files/compiled_program.go"
        ~data:(Ast.Pprint_ast.string_of_program ast_program)
    | Error error ->
      (match error with
      | `Lexer_error msg -> print_endline msg
      | `Parser_error msg -> print_endline msg) *)
