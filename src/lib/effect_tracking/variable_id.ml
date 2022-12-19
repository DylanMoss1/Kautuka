open! Core
open Util
open Util.Environment_
open Util.Item
open Ast.Ast_types
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
  let create name uuid = { name; uuid }
end

module String_item = struct
  include String

  let string_of_t t = t
  let create t = t
end

module Var_uuid_environment = Environment_ (String_item) (Var_uuid_annotation)

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
  type env_value = Var_uuid_annotation.t

  let empty_env = Var_uuid_environment.empty
  let add_to_env = Var_uuid_environment.add_new_item
  let add_new_scope = Var_uuid_environment.add_new_scope
  let remove_scope = Var_uuid_environment.remove_scope
  let get_value = Var_uuid_environment.get_value
  let get_value_outside_scope = Var_uuid_environment.get_value_outside_scope

  include No_result
end

module Uuid_ast_mapping = struct
  include Default_ast_mapping (Unknown_variable_uuid_ast_mapping)

  (* HANDLE EXCEPTIONS *)

  exception Unbound_var of string

  let var env var =
    match Var_uuid_environment.get_value (String_item.create var.name) env with
    | Some uuid -> env, { var with uuid }, empty_result ()
    | None -> raise (Unbound_var var.name)


  let block env block = add_new_scope env, block, empty_result ()

  let var_statement env = function 
  | 



    (* let _ = Var_uuid_environment.get_value_outside_scope
        (String_item.create var.name)
        env
  in 
    with
    | Some uuid -> env, { var with uuid }, empty_result ()
    | None -> raise (Unbound_var var.name) *)

  (* env.add_to_env var.name  *)
end

(* open! Core
open Util.Extended_set
open Ast.Ast_types
open Ast.Annotated_ast
open Ast_pipeline
open Parsing.Parser_types

module Variable_ast = Annotated_ast (Block_type_annotation) (Import_set)

module Unknown_import_ast_mapping = struct
  type result = Import_ast.import_annotation
  type old_block_annot = Parsed_ast.block_annotation
  type old_import_annot = Parsed_ast.import_annotation
  type new_block_annot = Import_ast.block_annotation
  type new_import_annot = Import_ast.import_annotation

  let collect_results = Import_set.union_of_list
  let empty_result () = Import_set.empty
end

module Import_ast_mapping = struct
  include Default_ast_mapping (Unknown_import_ast_mapping)

  let func_call func_call (result : result) =
    match func_call with
    | User_func user_func -> User_func user_func, result
    | Print expr -> Print expr, Import_set.add result I_Fmt
    | Input -> Input, Import_set.add result I_Fmt
    | Open expr -> Open expr, Import_set.add result I_Fmt
    | Read expr -> Read expr, Import_set.add result I_Fmt
    | Write write_template -> Write write_template, Import_set.add result I_Fmt
    | Append write_template ->
      Append write_template, Import_set.add result I_Fmt


  let program new_package _ new_global_vars new_funcs result =
    ( { package = new_package
      ; imports = result
      ; global_vars = new_global_vars
      ; funcs = new_funcs
      }
    , result )
end

module Import_ast_pipeline = Ast_pipeline (Import_ast_mapping) *)
