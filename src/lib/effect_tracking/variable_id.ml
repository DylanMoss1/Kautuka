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
  let create name uuid = { name; uuid }
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

  (* HANDLE EXCEPTIONS *)

  exception Unbound_var of string

  (* let create_new_uuid ~env ~var ~result ~constructor =
    let _ =
      Var_uuid_environment.get_value_outside_scope
        (String_item.create var.name)
        env
    in
    let uuid = Uuid.create () in
    ( add_to_env (String_item.create var.name) uuid env
    , constructor { var with uuid }
    , result )


  let use_existing_uuid ~env ~var ~result ~constructor =
    match Var_uuid_environment.get_value (String_item.create var.name) env with
    | Some uuid -> env, constructor { var with uuid }, result
    | None -> raise (Unbound_var var.name) *)

  let new_var env (var : old_var_annot) =
    let _ =
      Var_uuid_environment.get_value_outside_scope
        (String_item.create var.name)
        env
    in
    let uuid = Uuid.create () in
    ( add_to_env (String_item.create var.name) uuid env
    , { name = var.name; uuid }
    , empty_result () )


  let existing_var env (var : old_var_annot) =
    match Var_uuid_environment.get_value (String_item.create var.name) env with
    | Some uuid -> env, { name = var.name; uuid }, empty_result ()
    | None -> raise (Unbound_var var.name)

  (* let expr env expr result =
    match expr with
    | VarRead var ->
      use_existing_uuid ~env ~var ~result ~constructor:(fun x -> VarRead x)
    | _ -> ignore_branch env expr result


  let var_statement env var_statement result =
    match var_statement with
    | VarNonInit (var, type_id) ->
      create_new_uuid ~env ~var ~result ~constructor:(fun x ->
          VarNonInit (x, type_id))
    | VarInit (var, type_id, expr) ->
      create_new_uuid ~env ~var ~result ~constructor:(fun x ->
          VarInit (x, type_id, expr))
    | VarDecl (var, expr) ->
      create_new_uuid ~env ~var ~result ~constructor:(fun x ->
          VarDecl (x, expr))
    | VarAssign (var, expr) ->
      create_new_uuid ~env ~var ~result ~constructor:(fun x ->
          VarAssign (x, expr))
    | Pre_inc var ->
      use_existing_uuid ~env ~var ~result ~constructor:(fun x -> Pre_inc x)
    | Pre_dec var ->
      use_existing_uuid ~env ~var ~result ~constructor:(fun x -> Pre_dec x)
    | Post_inc var ->
      use_existing_uuid ~env ~var ~result ~constructor:(fun x -> Post_inc x)
    | Post_dec var ->
      use_existing_uuid ~env ~var ~result ~constructor:(fun x -> Post_dec x)


  let user_func env (user_func : new_var_annot user_func) result =
    use_existing_uuid ~env ~var:user_func.name ~result ~constructor:(fun x ->
        { user_func with name = x })


  let write_template env (write_template : new_var_annot write_template) result =
    use_existing_uuid
      ~env
      ~var:write_template.file
      ~result
      ~constructor:(fun x -> { write_template with file = x })


  let for_each env for_each result =
    let _ =
      Var_uuid_environment.get_value_outside_scope
        (String_item.create for_each.item.name)
        env
    in
    let uuid = Uuid.create () in
    let new_env = add_to_env (String_item.create for_each.item.name) uuid env in
    let new_item = { result.item with uuid } in
    match
      Var_uuid_environment.get_value
        (String_item.create for_each.iterator.name)
        env
    with
    | Some uuid ->
      ( new_env
      , { for_each with
          item = new_item
        ; iterator = { result.iterator with uuid }
        }
      , result )
    | None -> raise (Unbound_var for_each.iterator.name)


  let param env (var, type_id) result =
    create_new_uuid ~env ~var ~result ~constructor:(fun x -> x, type_id)


  let func env (func : ('a, new_var_annot) func) result =
    create_new_uuid ~env ~var:func.name ~result ~constructor:(fun x ->
        { func with name = x })


  let program ~env ~new_package ~old_import ~new_global_vars ~new_funcs ~result =
    ( env
    , { package = new_package
      ; imports = old_import
      ; global_vars = new_global_vars
      ; funcs = new_funcs
      }
    , result ) *)
end

module Variable_uuid_ast_pipeline = Ast_pipeline (Variable_uuid_ast_mapping)
