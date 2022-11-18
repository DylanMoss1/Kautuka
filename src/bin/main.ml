open Core
open Parsing

(* let ( /> ) x f = f x; x    *)
(*
   let () =
     In_channel.read_all "./files/kau_program.kau"
     |> Lexing.from_string
     /> pprint_tokens
     |> Parser.program Lexer.read_token
     |> fun ast_program ->
     Out_channel.write_all "./files/compiled_program.go"
       ~data:(string_of_program ast_program) *)

let usage_msg = "x [--debug]"
let debug = ref false 

let anon_fun _ = () 

let speclist = [("--debug", Arg.Set debug, "Output intermediary steps")]

let () = 
  Arg.parse speclist anon_fun usage_msg;
  print_endline "\n";

  if !debug then 

    In_channel.read_all "./files/kau_program.kau"
    |> Lexing.from_string |> Lex_and_parse.debug

  else 

    In_channel.read_all "./files/kau_program.kau"
    |> Lexing.from_string |> Lex_and_parse.parse
    |> fun ast_program ->
    match ast_program with
    | Ok ast_program ->
        Out_channel.write_all "./files/compiled_program.go"
          ~data:(Ast.Pprint_ast.string_of_program ast_program)
    | Error error -> (
        match error with
        | `Lexer_error msg -> print_endline msg
        | `Parser_error msg -> print_endline msg)
