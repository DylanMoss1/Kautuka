open! Core
open Util.Extended_set
open Util
open Util.Context
open Ast.Ast_types
open Ast.Annotated_ast
open Ast.Ast_pipeline
open Parsing.Parser_types
open Preperation.Import
open Variable_id
open Side_effect

let all_pairs xs ys =
  List.fold_left
    ~init:[]
    ~f:(fun acc_x x ->
      acc_x @ List.fold_left ~init:[] ~f:(fun acc_y y -> (x, y) :: acc_y) ys)
    xs


module Side_effect_set = struct
  include Make_extended_set (Side_effect)

  let disjoint t1 t2 =
    List.fold_left
      ~init:true
      ~f:(fun acc t1_effect ->
        acc
        && List.fold_left
             ~init:true
             ~f:(fun acc t2_effect ->
               acc && Side_effect.is_disjoint t1_effect t2_effect)
             (elements t2))
      (elements t1)
end

type block_side_effect =
  { block_type : block_type
  ; side_effects : Side_effect_set.t
  }

module Block_side_effect_annotation = struct
  type t = block_side_effect

  let string_of_t t =
    Fmt.str
      "[%s, %s]"
      (string_of_block_type t.block_type)
      (Side_effect_set.string_of_t t.side_effects)
end

module Func_side_effect_context = Make_context (Alpha) (Side_effect_set)

module Side_effect_ast =
  Annotated_ast (Block_side_effect_annotation) (Alpha_var_annotation)
    (Import_annotation)
    (Expr_empty_annotation)

module Side_effect_ast_mapping = struct
  include
    Default_ast_mapping (Alpha_var_ast) (Side_effect_ast)
      (Func_side_effect_context)
      (Side_effect_set)

  exception Func_called_before_defined of string

  let add_result = Side_effect_set.add
  let new_effect effect = add_result empty_result effect

  let var env var ~var_effect =
    match var_effect with
    | Init -> ignore_leaf env var
    | Read ->
      env, var, new_effect (Side_effect.create (Read, Var_mut var.alpha))
    | Write ->
      env, var, new_effect (Side_effect.create (Write, Var_mut var.alpha))


  let func_call env func_call result =
    match func_call with
    | User_func user_func ->
      ( env
      , User_func user_func
      , join_results
          result
          (match get_value user_func.name.alpha env with
          | Some effect_set -> effect_set
          | None -> raise (Func_called_before_defined user_func.name.name)) )
    | Print expr ->
      env, Print expr, add_result result (Side_effect.create (Write, Console))
    | Input ->
      env, Input, add_result result (Side_effect.create (Write, Console))
    | Open expr -> env, Open expr, result
    | Read var ->
      env, Read var, add_result result (Side_effect.create (Read, File))
    | Write write_template ->
      ( env
      , Write write_template
      , add_result result (Side_effect.create (Write, File)) )
    | Append write_template ->
      ( env
      , Append write_template
      , add_result result (Side_effect.create (Write, File)) )


  let block ~env ~new_contents ~old_annotations ~result =
    ( env
    , { contents = new_contents
      ; annotations = { block_type = old_annotations; side_effects = result }
      }
    , result )


  let func env (func : ('a, new_var_annot, 'b) func) result =
    add_to_env func.name.alpha result env, func, empty_result
end

module Side_effect_ast_pipeline = Ast_pipeline (Side_effect_ast_mapping)
