open! Core
open Util.Environment
open Ast.Ast_types
open Ast.Annotated_ast
open Ast.Ast_pipeline
open Parsing
open Preperation.Import
open Ast.Ast_to_string
open Side_effect.Variable_id
open Side_effect.Side_effect_tracking
open Util.Item
open Types_cost
open Cost
open Cost_of_instruction
open Util

module Runtime_cost = struct
  include Cost

  let join = sum
  let join_list = List.fold_left ~init:empty ~f:sum
end

(* module Cost_type = struct
  type t = type_id * Cost.t [@@deriving of_sexp, sexp_of, compare]

  let string_of_t (type_id, bound_term) =
    Fmt.str "%s %s" (string_of_type_id type_id) (Cost.string_of_t bound_term)
end *)

type param = Alpha.t * type_id [@@deriving of_sexp, sexp_of, compare]

module Runtime_func_cost = struct 
  type t = { 
    params : param list ;
    runtime_cost : Cost.t 
  } [@@deriving of_sexp, sexp_of, compare]
end 

module Runtime_cost_environment = Environment_ (Alpha) (Runtime_cost)

type block_cost =
  { block_type : Parser_types.block_type
  ; side_effects : Side_effect_set.t
  ; cost_term : Cost.t
  }

module Block_cost_annotation = struct
  type t = block_cost

  let string_of_t t =
    Fmt.str
      "[%s, %s, %s]"
      (Parser_types.string_of_block_type t.block_type)
      (Side_effect_set.string_of_t t.side_effects)
      (Cost.string_of_t t.cost_term)
end

module Multiplicative_context = struct 
  type t = (Cost.t list) ref 
  exception Empty_multiplicative_context 

  let empty = ref [] 
  let push x t = t := x::(!t)
  let pop t = match !t with 
  | [] -> raise Empty_multiplicative_context 
  | _::xs -> t := xs
  let get_total_multiplicatives t = List.fold_left ~init:Cost.one ~f:(fun acc multiplier -> Cost.multiply acc multiplier) !t
end 

module Cost_ast =
  Annotated_ast (Block_cost_annotation) (Alpha_var_annotation)
    (Import_annotation)
    (Expr_type_cost_annotation)

module Cost_ast_mapping = struct
  include
    Default_ast_mapping (Cost_type_ast) (Cost_ast) (Runtime_cost_environment)
      (Runtime_cost)

  exception Type_error

  let multiplicative_context = Multiplicative_context.empty 

  (* let substitute_params_into_cost params args cost =
    let alpha_to_cost_mapping = create_alpha_to_type_cost_mapping params args in
    Cost.substitute alpha_to_cost_mapping cost *)

  let user_func env user_func result = 
    let { name; args } = user_func in
    let { params; runtime_cost } : Runtime_func_cost.t = match Runtime_cost_environment.get_value name.alpha env with | Some runtime_cost -> runtime_cost | None -> raise Type_error in
    Cost.substitute args runtime_cost  


  let expr env expr result = match expr with 
  | Unop (unop, annotated_expr) -> ( 
    match unop with 
    | Not -> cost_of_not
    | U_Minus -> cost_of_u_minus 
  )
  | Binop (annotated_expr1, binop, annotated_expr2) -> ( 
    match binop with 
    | Plus -> (match expr.annotations with 
      | C_Int _ -> cost_of_plus_int
      | C_String _ -> cost_of_plus_str 
      | _ -> raise Type_error) 
    | B_Minus ->  cost_of_b_minus 
    | Mult ->  cost_of_mult 
    | Lt ->  cost_of_lt 
    | Le ->  cost_of_le 
    | Gt ->  cost_of_gt 
    | Ge ->  cost_of_ge 
    | Eq ->  cost_of_eq 
    | Ne ->  cost_of_ne 
    | And ->  cost_of_and 
    | Or -> cost_of_or 
  )
  | VarRead _ -> cost_of_var_read 
  | _ ->

  let var_statement env var_statement result = 
    match var_statement with 
    | VarNonInit(_, _) -> cost_of_var_non_init
    | VarInit (_,_,_) -> cost_of_var_init
    | VarDecl (_,_) -> cost_of_var_decl
    | VarAssign (_,_) -> cost_of_var_assign
    | Pre_inc _ -> cost_of_post_inc
    | Pre_dec _ -> cost_of_post_dec
    | Post_inc _ -> cost_of_post_inc
    | Post_dec _ -> cost_of_post_dec 

  let for_loop ~start_env ~end_env ~for_loop ~result = 

  let for_each ~start_env ~end_env ~for_each ~result = 

  let update_for_loop_env env for_loop result = 
    let { init; cond; iter; contents = _ } = for_loop in
    let init_var, init_type_cost =
      match init with
      | VarInit (var, _, annotated_expr) -> var, annotated_expr.annotations
      | VarDecl (var, annotated_expr) -> var, annotated_expr.annotations
      | _ -> raise Type_error
    in
    let cond_type_cost = cond.annotations in
    let _ =
      match iter with
      | Pre_inc var ->
        if Alpha.compare init_var.alpha var.alpha = 0
        then true
        else raise Type_error
      | Post_inc var ->
        if Alpha.compare init_var.alpha var.alpha = 0
        then false
        else raise Type_error
      | _ -> raise Type_error
    in
    match init_type_cost, cond_type_cost with
    | C_Int init_cost, C_Int cond_cost ->
      Multiplicative_context.push (Cost.subtract (Cost.subtract cond_type_cost Cost.one) init_type_cost); env
    | _ -> raise Type_error


  let update_for_each_env env for_each result = 
    match get_type_cost for_each.iterator env with
    | C_String cost ->
      Multiplicative_context.push cost; env 
    | _ -> raise Type_error

  let if_record env if_record result = 

  let func env func result = 
    let { name; params; body = _; return_type } = func in
    add_to_env
      name.alpha
      { params =
             List.map
               ~f:(fun param ->
                 let var, type_id = param in
                 var.alpha, type_id)
               params
         ; runtime_cost = result 
         }




  let block ~env ~new_contents ~(old_annotations : old_block_annot) ~result =
    ( env
    , { contents = new_contents
      ; annotations =
          { block_type = old_annotations.block_type
          ; side_effects = old_annotations.side_effects
          ; cost_term = result
          }
      }
    , result )
end

module Cost_ast_pipeline = Ast_pipeline (Cost_ast_mapping)
