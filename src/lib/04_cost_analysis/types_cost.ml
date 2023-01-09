open! Core
open Util.Environment
open Ast.Ast_types
open Ast.Annotated_ast
open Ast.Ast_pipeline
open Preperation.Import
open Cost
open Ast.Ast_to_string
open Side_effect.Variable_id
open Side_effect.Side_effect_tracking
open Util

module Types_cost = struct
  type param = Alpha.t * type_id [@@deriving of_sexp, sexp_of, compare]

  type func_type_cost =
    { params : param list
    ; return : t
    }
  [@@deriving of_sexp, sexp_of, compare]

  and t =
    | C_Int of Cost.t
    | C_String of Cost.t
    | C_Bool
    | C_Unit
    | C_File
    | C_Func of func_type_cost
  [@@deriving of_sexp, sexp_of, compare]

  exception Type_error

  let empty = C_Unit

  let string_of_param param =
    let alpha, type_id = param in
    Fmt.str "%s : %s" (Alpha.string_of_t alpha) (string_of_type_id type_id)


  let rec string_of_func_type_cost func_type_cost =
    let { params; return } = func_type_cost in
    String.concat
      ~sep:" -> "
      (List.append (List.map ~f:string_of_param params) [ string_of_t return ])


  and string_of_t = function
    | C_Int cost -> Fmt.str "int%s" (Cost.string_of_t cost)
    | C_String cost -> Fmt.str "string%s" (Cost.string_of_t cost)
    | C_Bool -> "bool"
    | C_Unit -> "unit"
    | C_File -> "file"
    | C_Func func_type_cost ->
      Fmt.str "func%s" (string_of_func_type_cost func_type_cost)


  let verify_type_cost expected_type type_cost =
    match expected_type, type_cost with
    | T_Int, C_Int _ -> true
    | T_String, C_String _ -> true
    | T_Unit, C_Unit -> true
    | T_Bool, C_Bool -> true
    | _ -> false


  let union type_cost1 type_cost2 =
    match type_cost1, type_cost2 with
    | C_Int cost1, C_Int cost2 -> C_Int (Cost.union cost1 cost2)
    | C_String cost1, C_String cost2 -> C_String (Cost.union cost1 cost2)
    | C_Unit, C_Unit -> C_Unit
    | C_Bool, C_Bool -> C_Bool
    | _ -> raise Type_error
end

module Expr_type_cost_annotation = Types_cost
module Cost_type_environment = Environment_ (Alpha) (Types_cost)

module Types_cost_result = struct
  type t = Types_cost.t list

  exception Invalid_arg_number

  let empty = []

  let pop t =
    match List.hd t, List.tl t with
    | Some hd, Some tl -> hd, tl
    | _ -> raise Invalid_arg_number


  let pop_2 t =
    match List.hd t, List.tl t with
    | Some hd, Some tl ->
      let hd2, tl = pop tl in
      hd, hd2, tl
    | _ -> raise Invalid_arg_number


  let pop_n t n =
    let rec pop_n_inner head_list tail n =
      if n = 0
      then head_list, tail
      else (
        let hd, tl = pop t in
        pop_n_inner (List.append head_list [ hd ]) tl (n - 1))
    in
    pop_n_inner [] t n


  let get t = t
  let push x t = List.append t [ x ]
  let join t1 t2 = t1 @ t2
  let join_list = List.fold_left ~init:[] ~f:(fun acc x -> join acc x)
end

module Cost_type_ast =
  Annotated_ast (Block_side_effect_annotation) (Alpha_var_annotation)
    (Import_annotation)
    (Expr_type_cost_annotation)

module Cost_type_ast_mapping = struct
  include
    Default_ast_mapping (Side_effect_ast) (Cost_type_ast)
      (Cost_type_environment)
      (Types_cost_result)

  open Types_cost

  exception Invalid_arg_number
  exception Invalid_arg_type
  exception Unbound_var of string
  exception Invalid_var_type
  exception Invalid_return_type
  exception Type_error
  exception No_return_statement

  let value env = function
    | Int i ->
      env, Int i, [ C_Int (Cost.create_int_cost (Integer_bound.create (i, i))) ]
    | Bool b -> env, Bool b, [ C_Bool ]
    | String s ->
      let len = String.length s in
      ( env
      , String s
      , [ C_String (Cost.create_int_cost (Integer_bound.create (len, len))) ] )


  let rec match_alpha_to_type params args =
    match params, args with
    | (alpha, T_Int) :: params, C_Int cost :: args ->
      (alpha, cost) :: match_alpha_to_type params args
    | (alpha, T_String) :: params, C_String cost :: args ->
      (alpha, cost) :: match_alpha_to_type params args
    | (_, T_Bool) :: params, C_Bool :: args -> match_alpha_to_type params args
    | (_, T_Unit) :: params, C_Unit :: args -> match_alpha_to_type params args
    | [], [] -> []
    | _, [] | [], _ -> raise Invalid_arg_number
    | _ -> raise Invalid_arg_type


  let substitute_params_in_cost params param_types cost =
    let map = match_alpha_to_type params param_types in
    Cost.substitute map cost


  let user_func env (user_func : (old_var_annot, 'a) user_func) result =
    let { name; args = _ } = user_func in
    let func_type_cost = get_value name.alpha env in
    match func_type_cost with
    | Some func_type_cost ->
      (match func_type_cost with
      | C_Func func_type_cost ->
        let { params; return } = func_type_cost in
        (match return with
        | C_Int cost ->
          ( env
          , user_func
          , [ C_Int (substitute_params_in_cost params result cost) ] )
        | C_String cost ->
          ( env
          , user_func
          , [ C_String (substitute_params_in_cost params result cost) ] )
        | C_Unit -> env, user_func, [ C_Unit ]
        | C_Bool -> env, user_func, [ C_Bool ]
        | _ -> raise Invalid_return_type)
      | _ -> raise Invalid_var_type)
    | _ -> raise (Unbound_var name.name)


  let func_call env func_call result =
    match func_call with
    | User_func _ -> env, func_call, result
    | Print _ ->
      let type_cost, result = Types_cost_result.pop result in
      (match type_cost with
      | C_String _ | C_Int _ -> env, func_call, result
      | _ -> raise Type_error)
    | Input ->
      ( env
      , func_call
      , C_String (Cost.create_int_cost (Integer_bound.create (0, 100)))
        :: result )
    | Open _ ->
      let type_cost, result = Types_cost_result.pop result in
      (match type_cost with
      | C_String _ -> env, func_call, C_File :: result
      | _ -> raise Type_error)
    | Read var ->
      (match Cost_type_environment.get_value var.alpha env with
      | Some type_cost ->
        (match type_cost with
        | C_File ->
          ( env
          , func_call
          , C_String (Cost.create_int_cost (Integer_bound.create (0, 100)))
            :: result )
        | _ -> raise Type_error)
      | None -> raise (Unbound_var var.name))
    | Write { file; _ } ->
      (match Cost_type_environment.get_value file.alpha env with
      | Some var_type_cost ->
        let type_cost, result = Types_cost_result.pop result in
        (match var_type_cost, type_cost with
        | C_File, C_String _ -> env, func_call, result
        | _ -> raise Type_error)
      | None -> raise (Unbound_var file.name))
    | Append { file; _ } ->
      (match Cost_type_environment.get_value file.alpha env with
      | Some var_type_cost ->
        let type_cost, result = Types_cost_result.pop result in
        (match var_type_cost, type_cost with
        | C_File, C_String _ -> env, func_call, result
        | _ -> raise Type_error)
      | None -> raise (Unbound_var file.name))


  let bin_op_int_to_bool arg_type1 arg_type2 =
    match arg_type1, arg_type2 with
    | C_Int _, C_Int _ -> C_Bool
    | _ -> raise Invalid_arg_type


  let bin_op_bool_to_bool arg_type1 arg_type2 =
    match arg_type1, arg_type2 with
    | C_Bool, C_Bool -> C_Bool
    | _ -> raise Invalid_arg_type


  let expr env expr result =
    match expr with
    | Unop (unop, _) ->
      let arg_type, result = Types_cost_result.pop result in
      (match unop with
      | Not ->
        (match arg_type with
        | C_Bool -> env, expr, C_Bool :: result
        | _ -> raise Invalid_arg_type)
      | U_Minus ->
        (match arg_type with
        | C_Int type_cost -> env, expr, C_Int (Cost.negate type_cost) :: result
        | _ -> raise Invalid_arg_type))
    | Binop (_, binop, _) ->
      let arg_type1, arg_type2, result = Types_cost_result.pop2 result in
      (match binop with
      | Plus ->
        (match arg_type1, arg_type2 with
        | C_Int type_cost1, C_Int type_cost2 ->
          env, expr, C_Int (Cost.sum type_cost1 type_cost2) :: result
        | C_String type_cost1, C_String type_cost2 ->
          env, expr, C_String (Cost.sum type_cost1 type_cost2) :: result
        | _ -> raise Invalid_arg_type)
      | B_Minus ->
        (match arg_type2, arg_type2 with
        | C_Int type_cost1, C_Int type_cost2 ->
          env, expr, C_Int (Cost.subtract type_cost1 type_cost2) :: result
        | _ -> raise Invalid_arg_type)
      | Mult ->
        (match arg_type2, arg_type2 with
        | C_Int type_cost1, C_Int type_cost2 ->
          env, expr, C_Int (Cost.multiply type_cost1 type_cost2) :: result
        | _ -> raise Invalid_arg_type)
      | Lt | Le | Gt | Ge | Eq | Ne ->
        env, expr, bin_op_int_to_bool arg_type1 arg_type2 :: result
      | And | Or -> env, expr, bin_op_bool_to_bool arg_type1 arg_type2 :: result)
    | Paren _ -> ignore_branch env expr result
    | Value _ -> ignore_branch env expr result
    | VarRead var ->
      let { name = _; alpha } = var in
      (match get_value alpha env with
      | Some key -> env, expr, key :: result
      | None -> raise (Unbound_var var.name))
    | Func_call _ -> ignore_branch env expr result


  let annotated_expr ~env ~new_expr ~old_annotations:_ ~result =
    env, { expr = new_expr; annotations = result }, result


  let var_statement env var_statement result =
    match var_statement with
    | VarNonInit (_, _) -> ignore_branch env var_statement empty_result
    | VarInit (var, _, _) ->
      let type_cost, result = Types_cost_result.pop result in
      add_to_env var.alpha type_cost env, var_statement, result
    | VarDecl (var, _) ->
      let type_cost, result = Types_cost_result.pop result in
      add_to_env var.alpha type_cost env, var_statement, result
    | VarAssign (var, _) ->
      let type_cost, result = Types_cost_result.pop result in
      add_to_env var.alpha type_cost env, var_statement, result
    | Pre_inc var ->
      (match get_value var.alpha env with
      | Some value ->
        (match value with
        | C_Int cost ->
          let new_cost = C_Int (Cost.sum Cost.one cost) in
          add_to_env var.alpha new_cost env, var_statement, result
        | _ -> raise Invalid_var_type)
      | None -> raise (Unbound_var var.name))
    | Post_inc var ->
      (match get_value var.alpha env with
      | Some value ->
        (match value with
        | C_Int cost ->
          ( add_to_env var.alpha (C_Int (Cost.sum Cost.one cost)) env
          , var_statement
          , result )
        | _ -> raise Invalid_var_type)
      | None -> raise (Unbound_var var.name))
    | _ -> raise (Failure "REMOVE POST/PRE DEC")


  let statement env statement result =
    match statement with
    | Expr _ ->
      let _, result = Types_cost_result.pop result in
      env, statement, result
    | _ -> ignore_branch env statement result


  (* let extract_int_cost = function
    | C_Int cost -> cost
    | _ -> raise Type_error *)

  (* let for_loop ~start_env ~end_env ~for_loop ~result =
    let { init; cond; iter; contents = _ } = for_loop in
    let start_value = ( 
    match init with
    | VarInit (_, _, annotated_expr) -> extract_int_cost annotated_expr.annotations
    | VarDecl (_, annotated_expr) -> extract_int_cost annotated_expr.annotations
    | _ -> raise Type_error ) 
  in 
  let end_value = ( 
    let { expr ; annotations=_ } = cond in 
    match expr with 
    | Binop  
  )

  let for_each ~start_env ~end_env ~for_each ~result = ()
  let for_each ~start_env ~end_env ~for_each ~result = end_env, for_each, result
  let for_loop ~start_env ~end_env ~for_loop ~result = end_env, for_loop, result *)

  let rec verify_condition_type_costs = function
    | condition_type_cost :: condition_type_costs ->
      Types_cost.verify_type_cost T_Bool condition_type_cost
      && verify_condition_type_costs condition_type_costs
    | [] -> true


  let if_record env if_record result =
    let number_of_conditions =
      1
      + List.length if_record.else_if
      +
      match if_record.else_contents with
      | Some _ -> 1
      | None -> 0
    in
    let condition_type_costs, result =
      Types_cost_result.pop_n result number_of_conditions
    in
    if verify_condition_type_costs condition_type_costs
    then env if_record result
    else raise Invalid_arg_type


  (* let structure env structure result = 
    match structure with 
    | Block_struct -> ignore_branch env structure result  
    | If _ -> ignore_branch env structure result  
    | For_loop -> 
    | For_each ->  *)

  let func env func result =
    let { name; params; body = _; return_type } = func in
    add_to_env
      name.alpha
      (Types_cost.C_Func
         { params =
             List.map
               ~f:(fun param ->
                 let var, type_id = param in
                 var.alpha, type_id)
               params
         ; return =
             (let rec collect_return_types result =
                match result with
                | [ r ] ->
                  if Types_cost.verify_type_cost return_type r
                  then r
                  else raise Invalid_return_type
                | r :: rs ->
                  if Types_cost.verify_type_cost return_type r
                  then Types_cost.union r (collect_return_types rs)
                  else raise Invalid_return_type
                | [] ->
                  (match return_type with
                  | T_Unit -> C_Unit
                  | _ -> raise No_return_statement)
              in
              collect_return_types result)
         })
      env
end

(* let func env func result =
    let { name; params; body; return_type } = func in
    add_to_env
      name.alpha
      (Types_cost.C_Func
         { args =
             List.map
               ~f:(fun param ->
                 let var, type_id = param in
                 var.alpha, type_id)
               params
         ; return = Types_cost.C_Unit 
         })
      env *)

(* let user_func env user_func result =  *)

(*
module Cost_type_ast =
  Annotated_ast (Block_side_effect_annotation) (Cost_type_var_annotation)
    (Import_annotation)



  let rec type_cost_of_user_func _ _ = C_Unit

  and type_cost_of_func_call env = function
    | User_func user_func -> type_cost_of_user_func env user_func
    | Print expr ->
      (match type_cost_of_expr env expr with
      | C_String _ | C_Int _ -> C_Unit
      | _ -> raise Type_error)
    | Input ->
      C_String (Cost.create_int_cost (Integer_bound.create (0, 100)))
      (* CHANGE INPUT *)
    | Open expr ->
      (match type_cost_of_expr env expr with
      | C_String _ -> C_File
      | _ -> raise Type_error)
    | Read var ->
      (match Cost_type_environment.get_value var env with
      | Some type_cost ->
        (match type_cost with
        | C_File -> C_String
        | _ -> raise Type_error)
      | None -> raise (Unbound_var var.name))
    | Write { file; contents } ->
      (match type_cost_of_expr env file, type_cost_of_expr env contents with
      | C_File, C_String _ -> C_Unit
      | _ -> raise Type_error)
    | Append { file; contents } ->
      (match type_cost_of_expr env file, type_cost_of_expr env contents with
      | C_File, C_String _ -> C_Unit
      | _ -> raise Type_error)


  and type_cost_of_expr env = function
    | Unop (unop, expr) -> type_cost_of_unop (type_cost_of_expr env expr) unop
    | Binop (expr1, binop, expr2) ->
      type_cost_of_binop
        (type_cost_of_expr env expr1)
        (type_cost_of_expr env expr2)
        binop
    | Paren expr -> type_cost_of_expr env expr
    | Value value -> type_cost_of_value value
    | VarRead var ->
      (match Cost_type_environment.get_value var env with
      | Some type_cost -> type_cost
      | None -> raise (Unbound_var var.name))
    | Func_call func_call -> type_cost_of_func_call env func_call
end

(* 


module Cost_type = struct
  type t = type_id * Cost.t [@@deriving of_sexp, sexp_of, compare]

  let string_of_t (type_id, bound_term) =
    Fmt.str "%s%s" (string_of_type_id type_id) (Cost.string_of_t bound_term)


  let create_int_cost type_id int_bound =
    type_id, Cost.create (Cost_term.create_int_t int_bound)
end






module Bounded_type_environment = Environment_ (Alpha) (Cost_type)

type cost_type_var =
  { name : string
  ; alpha : Alpha.t
  ; cost_type : Cost_type.t
  }
[@@deriving compare, sexp_of, of_sexp]

module Cost_type_var_annotation = struct
  type t = cost_type_var [@@deriving compare, sexp_of, of_sexp]

  let string_of_t t = t.name
end

module Cost_type_environment = Environment_ (Alpha) (Cost_type)
module Cost_type_set = Make_extended_set (Cost_type)

module Cost_type_ast =
  Annotated_ast (Block_side_effect_annotation) (Cost_type_var_annotation)
    (Import_annotation)

module Cost_type_ast_mapping = struct
  include
    Default_ast_mapping (Side_effect_ast) (Cost_type_ast)
      (Cost_type_environment)
      (Cost_type_set)

  let add_result = Cost_type_set.add
  let new_cost_type cost_type = add_result empty_result cost_type

  let value env value =
    match value with
    | Int i ->
      env, Int i, new_cost_type (Cost_type.create_int_cost T_Int (i, i))
    | Bool _ -> ignore_leaf env value
    | String s ->
      let len = String.length s in
      ( env
      , String s
      , new_cost_type (Cost_type.create_int_cost T_String (len, len)) )

  
  let expr env expr result = match expr with 
    | Unop (unop, expr) -> match unop with 
    | 




      | Binop(expr, binop, expr) -> 
        | Paren(expr) -> 
          | Value(value) -> 
            | VarRead(var) -> env, VarRead({
              name = var.name ; alpha = var.alpha ; cost_type = get_value var.alpha
            }), empty_result 

            (* 
    let var env var ~var_effect =
      | Init -> 
      | Read -> env, {
        name = var.name ; alpha = var.alpha ; cost_type = get_value var.alpha
      }, empty_result 
      | Write -> 



      match var_effect with
      | Init -> ignore_leaf env var
      | Read -> env, var, new_effect (Read, Var_mut var.alpha)
      | Write -> env, var, new_effect (Write, Var_mut var.alpha)
end

module Cost_type_pipeline = Ast_pipeline (Cost_type_ast_mapping) *) *) *)

module Cost_type_pipeline = Ast_pipeline (Cost_type_ast_mapping)
