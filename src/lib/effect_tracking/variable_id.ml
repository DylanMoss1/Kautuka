open! Core
open Util
open Util.Environment_
open Ast.Annotated_ast
open Ast_pipeline
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

module String_item = struct
  include String

  let string_of_t t = t
  let create x = x
end

module Alpha_var_environment = Environment_ (String_item) (Alpha)

module Alpha_var_ast =
  Annotated_ast (Block_type_annotation) (Alpha_var_annotation)
    (Import_some_annotation)

module Unknown_alpha_var_ast_mapping = struct
  type old_block_annot = Import_ast.block_annot
  type old_var_annot = Import_ast.var_annot
  type old_import_annot = Import_ast.import_annot
  type new_block_annot = Alpha_var_ast.block_annot
  type new_var_annot = Alpha_var_ast.var_annot
  type new_import_annot = Alpha_var_ast.import_annot
  type env = Alpha_var_environment.t
  type env_key = String_item.t
  type env_value = Alpha.t

  let empty_env = Alpha_var_environment.empty
  let add_to_env = Alpha_var_environment.add_new_item
  let add_new_scope = Alpha_var_environment.add_new_scope
  let remove_scope = Alpha_var_environment.remove_scope
  let get_value = Alpha_var_environment.get_value
  let get_value_outside_scope = Alpha_var_environment.get_value_outside_scope

  include No_result
end

module Alpha_var_ast_mapping = struct
  include Default_ast_mapping (Unknown_alpha_var_ast_mapping)

  (* HANDLE EXCEPTIONS : FROM ENVIRONMENT AS WELL *)

  exception Unbound_var of string

  let var env (var : old_var_annot) ~var_effect =
    match var_effect with
    | Init ->
      let _ = Alpha_var_environment.get_value_outside_scope var.name env in
      let alpha = Alpha.create () in
      add_to_env var.name alpha env, { name = var.name; alpha }, empty_result ()
    | Read | Write ->
      (match Alpha_var_environment.get_value var.name env with
      | Some alpha -> env, { name = var.name; alpha }, empty_result ()
      | None -> raise (Unbound_var var.name))
end

module Alpha_var_ast_pipeline = Ast_pipeline (Alpha_var_ast_mapping)
