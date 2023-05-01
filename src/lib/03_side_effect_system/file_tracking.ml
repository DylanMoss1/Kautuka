open! Core
open Ast.Ast_types
open Util
open Util.Context
open Ast.Annotated_ast
open Ast.Ast_pipeline
open Preperation.Alpha_conversion
open Preperation.Import

module File_ref_option = struct
  type t = File_ref.t option [@@deriving of_sexp, sexp_of, compare]

  let string_of_t = function
    | None -> "None"
    | Some file_ref -> Fmt.str "Some(%s)" (File_ref.string_of_t file_ref)
end

module Expr_file_ref_annotation = File_ref_option

module File_ref_context =
  Make_context (Alpha) (File_ref_option)
    (struct
      let t = false
    end)

module File_ref_result = struct
  type t = File_ref_option.t

  let create file_ref = Some file_ref
  let empty = None
  let join _ _ = None
  let join_list _ = None
  let union_list _ = None
end

module File_tracking_ast =
  Annotated_ast (Block_scoped_vars_annotation) (Alpha_conversion_annotation)
    (Import_annotation)
    (Expr_file_ref_annotation)

module File_tracking_ast_mapping = struct
  include
    Default_ast_mapping (Alpha_conversion_ast) (File_tracking_ast)
      (File_ref_context)
      (File_ref_result)

  exception File_ref_in_var_not_found

  let generate_ref = File_ref.get_new_generated_ref

  let func_call env (func_call : (old_var_annot, new_expr_annot) func_call) _ =
    match func_call with
    | User_func user_func ->
      ( env
      , User_func user_func
      , (match File_ref_context.get_value user_func.name.alpha env with
        | Some value -> value
        | None -> raise File_ref_in_var_not_found) )
    | Open (filename, file_ref) ->
      env, Open (filename, file_ref), File_ref_result.create file_ref
    | _ -> env, func_call, None


  let expr env expr result =
    match expr with
    | Func_call func_call -> env, Func_call func_call, result
    | Var_read var ->
      ( env
      , expr
      , (match File_ref_context.get_value var.alpha env with
        | Some value -> value
        | None -> raise File_ref_in_var_not_found) )
    | _ -> env, expr, None


  let annotated_expr ~env ~new_expr ~old_annotations:_ ~result =
    env, { expr = new_expr; annotations = result }, None


  let var_statement env var_statement _ =
    match var_statement with
    | Var_init (var, _, annot_expr)
    | Var_decl (var, annot_expr)
    | Var_assign (var, annot_expr) ->
      ( File_ref_context.add_item var.alpha annot_expr.annotations env
      , var_statement
      , None )
    | _ -> env, var_statement, None


  let func env func _ =
    let { name; params = _; body = _; return_type } = func in
    let env =
      File_ref_context.add_item
        name.alpha
        (match return_type with
        | T_File -> File_ref_result.create generate_ref
        | _ -> None)
        env
    in
    env, func, None
end

module File_tracking_ast_pipeline = Ast_pipeline (File_tracking_ast_mapping)
