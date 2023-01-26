open! Core
open Lexing
open Parsing
open Preperation
open Side_effect_system

let usage_msg = "x [--debug]"
let debug = ref false
let anon_fun _ = ()
let speclist = [ "--debug", Arg.Set debug, "Output intermediary steps" ]

let token_list_of_lexbuf lexbuf =
  let rec lexbuf_to_string_inner lexbuf acc =
    if lexbuf.lex_eof_reached
    then lexbuf_to_string_inner lexbuf (acc @ [ Lexer.read_token lexbuf ])
    else acc
  in
  lexbuf_to_string_inner lexbuf []


let () =
  Arg.parse speclist anon_fun usage_msg;
  print_endline "\n";
  In_channel.read_all "./files/kau_program.kau"
  |> Lexing.from_string
  |> (fun lexbuf ->
       let () =
         Out_channel.write_all
           "./files/01_parsing_lexer_tokens"
           ~data:(Tokens.string_of_token_list (token_list_of_lexbuf lexbuf))
       in
       lexbuf)
  |> (fun lexbuf ->
       Parser_types.Parsed_ast.create (Parser.program Lexer.read_token lexbuf))
  |> (fun parsed_ast ->
       Parser_types.Parsed_ast.output_to_debug_file
         "01_parsing_parsed_ast"
         parsed_ast;
       parsed_ast)
  |> Import.Import_ast_pipeline.pipeline_ast
       ~debug_file:(Some "02_preperation_import")
  |> Alpha_conversion.Alpha_conversion_ast_pipeline.pipeline_ast
       ~debug_file:(Some "03_side-effect-system_alpha-conversion")
  |> Side_effect_tracking.Side_effect_ast_pipeline.pipeline_ast
       ~debug_file:(Some "03_side-effect-system_side-effect-tracking")
  |> fun _ -> ()
