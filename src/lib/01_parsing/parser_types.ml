open! Core
open Ast.Annotated_ast

type block_type =
  | Default
  | Ignore
  | Force_par
  | Force_seq

let string_of_block_type = function
  | Default -> "Default"
  | Ignore -> "Ignore"
  | Force_par -> "Force_par"
  | Force_seq -> "Force_seq"


module Import_empty_annotation = struct
  type t = unit

  let string_of_t _ = ""
  let create = ()
end

module Block_Annotation = struct
  type t = block_type

  let string_of_t = string_of_block_type
  let create block_type = block_type
end

module Var_name_annotation = struct
  type t = { name : string }

  let string_of_t t = t.name
  let create name = { name }
end

module Expr_empty_annotation = struct
  type t = unit

  let string_of_t _ = ""
  let create = ()
end

module Parsed_ast =
  Annotated_ast (Block_Annotation) (Var_name_annotation)
    (Import_empty_annotation)
    (Expr_empty_annotation)
