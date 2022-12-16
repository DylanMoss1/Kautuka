open! Core
open Ast_types
open Ast_to_string
open! Core

module type Type_annotation = sig
  type t

  val string_of_t : t -> string
end

module Annotated_ast
    (Block_annotation : Type_annotation)
    (Import_annotation : Type_annotation) =
struct
  type block_annotation = Block_annotation.t
  type import_annotation = Import_annotation.t
  type t = (block_annotation, import_annotation) program

  let string_of_t =
    string_of_program
      ~string_of_block_annotation:Block_annotation.string_of_t
      ~string_of_import_annotation:Import_annotation.string_of_t


  let create (x : (block_annotation, import_annotation) program) : t = x

  let create_block_annotation (block_annotation : Block_annotation.t)
      : block_annotation
    =
    block_annotation


  let create_import_annotation (import_annotation : Import_annotation.t)
      : import_annotation
    =
    import_annotation
end