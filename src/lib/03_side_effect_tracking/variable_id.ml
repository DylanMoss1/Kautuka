open! Core
open Util
open Util.Item
open Util.Context
open Ast.Annotated_ast
open Ast.Ast_pipeline
open Parsing.Parser_types
open Preperation.Import

type alpha_var =
  { name : string
  ; alpha : Alpha.t
  }
[@@deriving compare, sexp_of, of_sexp]

module Alpha_var_annotation = struct
  type t = alpha_var [@@deriving compare, sexp_of, of_sexp]

  let string_of_t t = t.name
end

module Alpha_var_context = Make_context (String_item) (Alpha)

module Alpha_var_ast =
  Annotated_ast (Block_Annotation) (Alpha_var_annotation) (Import_annotation)
    (Expr_empty_annotation)

module Alpha_var_ast_mapping = struct
  include
    Default_ast_mapping (Import_ast) (Alpha_var_ast) (Alpha_var_context)
      (Empty_result)

  exception Unbound_var of string

  let var env (var : old_var_annot) ~var_effect =
    match var_effect with
    | Init ->
      (* Raises an exception if var exists in the inner-most scope *)
      let (_ : 'a option) =
        Alpha_var_context.get_value_outside_scope var.name env
      in
      let alpha = Alpha.create in
      add_to_env var.name alpha env, { name = var.name; alpha }, empty_result
    | Read | Write ->
      (match Alpha_var_context.get_value var.name env with
      | Some alpha -> env, { name = var.name; alpha }, empty_result
      | None -> raise (Unbound_var var.name))
end

module Alpha_var_ast_pipeline = Ast_pipeline (Alpha_var_ast_mapping)
