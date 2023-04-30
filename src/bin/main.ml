open! Core
open Lexing
open Parsing
open Preperation
open Side_effect_system
open Cost_analysis
open Parallelisation

let usage_msg = "x [--debug]"
let debug = ref false
let sequential = ref false
let anon_fun _ = ()

let speclist =
  [ "--debug", Arg.Set debug, "Output intermediary steps"
  ; "--seq", Arg.Set sequential, "Disable automatic parallelisation"
  ]


let token_list_of_lexbuf lexbuf =
  let rec lexbuf_to_string_inner lexbuf acc =
    if lexbuf.lex_eof_reached
    then lexbuf_to_string_inner lexbuf (acc @ [ Lexer.read_token lexbuf ])
    else acc
  in
  lexbuf_to_string_inner lexbuf []


let print_error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Fmt.str "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)


let () =
  Arg.parse speclist anon_fun usage_msg;
  print_endline "\n";
  In_channel.read_all "./files/kau_program.kau"
  |> fun program ->
  Out_channel.write_all
    "./files/intermediary_steps/01_parsing_lexer"
    ~data:
      (Tokens.string_of_token_list
         (token_list_of_lexbuf (Lexing.from_string program)));
  let lexbuf = Lexing.from_string program in
  try
    Parser_types.Parsed_ast.create (Parser.program Lexer.read_token lexbuf)
    |> (fun parsed_ast ->
         Parser_types.Parsed_ast.output_to_debug_file
           "01_parsing_parser.go"
           parsed_ast;
         parsed_ast)
    |> Import.Import_ast_pipeline.pipeline_ast
         ~debug_file:(Some "02_preperation_import")
    |> Alpha_conversion.Alpha_conversion_ast_pipeline.pipeline_ast
         ~debug_file:(Some "02_preparation_alpha-conversion")
    |> File_tracking.File_tracking_ast_pipeline.pipeline_ast
         ~debug_file:(Some "03_side-effect-system_file-tracking")
    |> Side_effect_tracking.Side_effect_ast_pipeline.pipeline_ast
         ~debug_file:(Some "03_side-effect-system_side-effect-tracking")
    |> Type_cost.Type_cost_ast_pipeline.pipeline_ast
         ~debug_file:(Some "04_cost-analysis__type-cost")
    |> Runtime_cost.Cost_tracking_ast_pipeline.pipeline_ast
         ~debug_file:(Some "04_cost-analysis_cost-tracking")
    |> Reorder_and_parallelise.pipeline_ast
         ~debug_file:(Some "05_parallelisation_reorder-and-parallelise")
    |> Translate_to_go.pipeline_ast ~sequential:!sequential
  with
  | Lexer.Lexer_error msg ->
    print_endline
      (Fmt.str "Lexer Error: %s: %s" (print_error_position lexbuf) msg)
  | Parser.Error ->
    print_endline (Fmt.str "Parsing Error: %s" (print_error_position lexbuf))
