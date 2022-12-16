module type Type_annotation = sig
  type t

  val string_of_t : t -> string
end

module Annotated_ast : functor
  (Block_annotation : Type_annotation)
  (Import_annotation : Type_annotation)
  -> sig
  type block_annotation = Block_annotation.t
  type import_annotation = Import_annotation.t
  type t = (block_annotation, import_annotation) Ast_types.program

  val string_of_t
    :  (Block_annotation.t, Import_annotation.t) Ast_types.program
    -> string

  val create : (block_annotation, import_annotation) Ast_types.program -> t
  val create_block_annotation : Block_annotation.t -> block_annotation
  val create_import_annotation : Import_annotation.t -> import_annotation
end
