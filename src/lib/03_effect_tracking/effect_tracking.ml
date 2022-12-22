open! Core
open Util.Extended_set
open Util
open Util.Environment_
open Ast.Ast_types
open Ast.Annotated_ast
open Ast.Ast_pipeline
open Parsing.Parser_types
open Preperation.Import
open Variable_id

type 'a var_type =
  | Func_var of 'a var
  | Value_var of 'a var

type effect_read_write =
  | Read
  | Write
[@@deriving of_sexp, sexp_of, compare]

type effect_type =
  | Console
  | File
  | Var_mut of Alpha.t
[@@deriving of_sexp, sexp_of, compare]

type effect = effect_read_write * effect_type
[@@deriving of_sexp, sexp_of, compare]

module Effect = struct
  type t = effect [@@deriving of_sexp, sexp_of, compare]

  let string_of_effect_type = function
    | Console -> "console"
    | File -> "file"
    | Var_mut uuid -> "var-mut:" ^ Alpha.string_of_t uuid


  let string_of_t = function
    | Read, effect_type -> "Read_" ^ string_of_effect_type effect_type
    | Write, effect_type -> "Write_" ^ string_of_effect_type effect_type
end

module Effect_set = Make_extended_set (Effect)

type block_effect =
  { block_type : block_type
  ; effect_set : Effect_set.t
  }

module Block_effect_annotation = struct
  type t = block_effect

  let string_of_block_type = function
    | Default -> "Default"
    | Ignore -> "Ignore"
    | Force_par -> "Force_par"
    | Force_seq -> "Force_seq"


  let string_of_t t =
    Fmt.str
      "[%s, %s]"
      (string_of_block_type t.block_type)
      (Effect_set.string_of_t t.effect_set)
end

module Effect_tracking_ast =
  Annotated_ast (Block_effect_annotation) (Alpha_var_annotation)
    (Import_some_annotation)

module Func_effect_environment = Environment_ (Alpha) (Effect_set)

module Unknown_effect_tracking_ast_mapping = struct
  type old_block_annot = Alpha_var_ast.block_annot
  type old_var_annot = Alpha_var_ast.var_annot
  type old_import_annot = Alpha_var_ast.import_annot
  type new_block_annot = Effect_tracking_ast.block_annot
  type new_var_annot = Effect_tracking_ast.var_annot
  type new_import_annot = Effect_tracking_ast.import_annot
  type result = Effect_set.t
  type env = Func_effect_environment.t
  type env_key = Alpha.t
  type env_value = Effect_set.t

  let collect_results = Effect_set.union_of_list
  let union_results = Effect_set.union
  let empty_result () = Effect_set.empty
  let empty_env = Func_effect_environment.empty
  let add_to_env = Func_effect_environment.add_new_item
  let add_new_scope = Func_effect_environment.add_new_scope
  let remove_scope = Func_effect_environment.remove_scope
  let get_value = Func_effect_environment.get_value
  let get_value_outside_scope = Func_effect_environment.get_value_outside_scope
end

module Effect_tracking_ast_mapping = struct
  include Default_ast_mapping (Unknown_effect_tracking_ast_mapping)

  exception Func_called_before_defined of string

  let add_result = Effect_set.add
  let new_effect effect = add_result (empty_result ()) effect

  let var env var ~var_effect =
    match var_effect with
    | Init -> ignore_leaf env var
    | Read -> env, var, new_effect (Read, Var_mut var.alpha)
    | Write -> env, var, new_effect (Write, Var_mut var.alpha)


  let func_call env func_call result =
    match func_call with
    | User_func user_func ->
      ( env
      , User_func user_func
      , union_results
          result
          (match get_value user_func.name.alpha env with
          | Some effect_set -> effect_set
          | None -> raise (Func_called_before_defined user_func.name.name)) )
    | Print expr -> env, Print expr, add_result result (Write, Console)
    | Input -> env, Input, add_result result (Write, Console)
    | Open expr -> ignore_branch env (Open expr) result
    | Read var -> env, Read var, add_result result (Read, File)
    | Write write_template ->
      env, Write write_template, add_result result (Write, File)
    | Append write_template ->
      env, Append write_template, add_result result (Write, File)


  let block ~env ~new_contents ~old_annotations ~result =
    ( env
    , { contents = new_contents
      ; annotations = { block_type = old_annotations; effect_set = result }
      }
    , result )


  let func env (func : ('a, new_var_annot) func) result =
    add_to_env func.name.alpha result env, func, result
end

module Effect_tracking_ast_pipeline = Ast_pipeline (Effect_tracking_ast_mapping)
