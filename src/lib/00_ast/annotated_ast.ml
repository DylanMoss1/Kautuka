open! Core
open Ast_types
open Ast_to_string
open! Core

module type Annotation = sig
  type t

  val string_of_t : t -> string
end

module type Annotated_ast = sig
  type block_annot
  type var_annot
  type import_annot
  type expr_annot
  type t

  val string_of_t : t -> string
  val create : (block_annot, var_annot, import_annot, expr_annot) program -> t
end

module Make_annotated_ast
    (Block_annotation : Annotation)
    (Var_annotation : Annotation)
    (Import_annotation : Annotation)
    (Expr_annotation : Annotation) =
struct
  type block_annot = Block_annotation.t
  type var_annot = Var_annotation.t
  type import_annot = Import_annotation.t
  type expr_annot = Expr_annotation.t
  type t = (block_annot, var_annot, import_annot, expr_annot) program

  let string_of_t t =
    string_of_program
      ~string_of_block_annot:Block_annotation.string_of_t
      ~string_of_var_annot:Var_annotation.string_of_t
      ~string_of_import_annot:Import_annotation.string_of_t
      t


  let create (x : (block_annot, var_annot, import_annot, expr_annot) program)
      : t
    =
    x


  let create_block_annot (block_annot : Block_annotation.t) : block_annot =
    block_annot


  let create_var_annot (var_annot : Var_annotation.t) : var_annot = var_annot

  let create_import_annot (import_annot : Import_annotation.t) : import_annot =
    import_annot


  let create_expr_annot (expr_annot : Expr_annotation.t) : expr_annot =
    expr_annot


  let string_of_block_annot = Block_annotation.string_of_t
  let string_of_var_annot = Var_annotation.string_of_t
  let string_of_import_annot = Import_annotation.string_of_t
  let string_of_expr_annot = Expr_annotation.string_of_t
end
