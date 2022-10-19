open Core

let () = In_channel.read_all "./files/kau_program.kau" 
  |> Lexing.from_string 
  |> Parsing.Parser.program Parsing.Lexer.token 
  |> fun ast_block -> Out_channel.write_all "./files/compiled_program.go" ~data:(Go_translation.ast_block_to_go ast_block)