open! Core
open Util
open Util.Environment_
open Ast.Annotated_ast
open Ast_pipeline
open Parsing.Parser_types
open Preperation.Import

type var_uuid =
  { name : string
  ; uuid : Uuid.t
  }
[@@deriving compare, sexp_of, of_sexp]

module Var_uuid_annotation = struct
  type t = var_uuid [@@deriving compare, sexp_of, of_sexp]

  let string_of_t t = t.name
end

module String_item = struct
  include String

  let string_of_t t = t
  let create x = x
end

module Var_uuid_environment = Environment_ (String_item) (Uuid)

module Variable_uuid_ast =
  Annotated_ast (Block_type_annotation) (Var_uuid_annotation)
    (Import_some_annotation)

module Unknown_variable_uuid_ast_mapping = struct
  type old_block_annot = Import_ast.block_annot
  type old_var_annot = Import_ast.var_annot
  type old_import_annot = Import_ast.import_annot
  type new_block_annot = Variable_uuid_ast.block_annot
  type new_var_annot = Variable_uuid_ast.var_annot
  type new_import_annot = Variable_uuid_ast.import_annot
  type env = Var_uuid_environment.t
  type env_key = String_item.t
  type env_value = Uuid.t

  let empty_env = Var_uuid_environment.empty
  let add_to_env = Var_uuid_environment.add_new_item
  let add_new_scope = Var_uuid_environment.add_new_scope
  let remove_scope = Var_uuid_environment.remove_scope
  let get_value = Var_uuid_environment.get_value
  let get_value_outside_scope = Var_uuid_environment.get_value_outside_scope

  include No_result
end

module Variable_uuid_ast_mapping = struct
  include Default_ast_mapping (Unknown_variable_uuid_ast_mapping)

  (* HANDLE EXCEPTIONS : FROM ENVIRONMENT AS WELL *)

  exception Unbound_var of string

  let new_var env (var : old_var_annot) =
    let _ = Var_uuid_environment.get_value_outside_scope var.name env in
    let uuid = Uuid.create () in
    add_to_env var.name uuid env, { name = var.name; uuid }, empty_result ()


  let existing_var env (var : old_var_annot) =
    match Var_uuid_environment.get_value var.name env with
    | Some uuid -> env, { name = var.name; uuid }, empty_result ()
    | None -> raise (Unbound_var var.name)
end

module Variable_uuid_ast_pipeline = Ast_pipeline (Variable_uuid_ast_mapping)
