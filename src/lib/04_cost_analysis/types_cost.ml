open! Core
open Util.Context
open Ast.Ast_types
open Ast.Annotated_ast
open Ast.Ast_pipeline
open Preperation.Import
open Cost
open Ast.Ast_to_string
open Side_effect.Variable_id
open Side_effect.Side_effect_tracking
open Util

module Type_cost = struct
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


  let verified_type_cost expected_type type_cost =
    match expected_type, type_cost with
    | T_Int, C_Int _ -> type_cost
    | T_String, C_String _ -> type_cost
    | T_Unit, C_Unit -> type_cost
    | T_Bool, C_Bool -> type_cost
    | _ -> raise Type_error


  let union type_cost1 type_cost2 =
    match type_cost1, type_cost2 with
    | C_Int cost1, C_Int cost2 -> C_Int (Cost.union cost1 cost2)
    | C_String cost1, C_String cost2 -> C_String (Cost.union cost1 cost2)
    | C_Unit, C_Unit -> C_Unit
    | C_Bool, C_Bool -> C_Bool
    | _ -> raise Type_error


  let rec create_alpha_to_cost_mapping params args =
    match params, args with
    | (alpha, T_Int) :: params, C_Int cost :: args ->
      (alpha, cost) :: create_alpha_to_cost_mapping params args
    | (alpha, T_String) :: params, C_String cost :: args ->
      (alpha, cost) :: create_alpha_to_cost_mapping params args
    | (_, T_Bool) :: params, C_Bool :: args ->
      create_alpha_to_cost_mapping params args
    | (_, T_Unit) :: params, C_Unit :: args ->
      create_alpha_to_cost_mapping params args
    | [], [] -> []
    | _, [] | [], _ -> raise Type_error
    | _ -> raise Type_error
end

module Type_cost_context = Make_context (Alpha) (Type_cost)

module Type_cost_result = struct
  type t =
    { expr_type_cost : Type_cost.t list
    ; return_type_cost : Type_cost.t list
    }

  exception Invalid_arg_number
  exception Type_error

  let empty = { expr_type_cost = []; return_type_cost = [] }

  let extract xs =
    match xs with
    | [ x ] -> x
    | _ -> raise Invalid_arg_number


  let extract_2 xs =
    match xs with
    | [ x1; x2 ] -> x1, x2
    | _ -> raise Invalid_arg_number


  let rec extract_n xs n =
    match xs, n with
    | [], 0 -> []
    | [], _ | _ :: _, 0 -> raise Invalid_arg_number
    | x :: xs, n -> x :: extract_n xs (n - 1)


  let extract_expr_type_cost t = extract t.expr_type_cost
  let extract_2_expr_type_cost t = extract_2 t.expr_type_cost
  let extract_n_expr_type_cost t = extract_n t.expr_type_cost
  let push_type_cost x t = { t with expr_type_cost = t.expr_type_cost @ [ x ] }

  let push_return_cost x t =
    { t with return_type_cost = t.return_type_cost @ [ x ] }


  let create expr_type_cost =
    { expr_type_cost = [ expr_type_cost ]; return_type_cost = [] }


  let create_with_return expr_type_cost return_type_cost =
    match expr_type_cost with
    | Some expr_type_cost ->
      { expr_type_cost = [ expr_type_cost ]; return_type_cost }
    | None -> { expr_type_cost = []; return_type_cost }


  let add expr_type_cost t =
    { t with expr_type_cost = t.expr_type_cost @ [ expr_type_cost ] }


  let get_return_type_cost return_type_cost expected_type =
    match return_type_cost, expected_type with
    | [], T_Unit -> Type_cost.C_Unit
    | return_type_cost :: return_type_cost_list, _ ->
      List.fold_left
        ~init:(Type_cost.verified_type_cost expected_type return_type_cost)
        ~f:(fun acc type_cost ->
          Type_cost.union
            acc
            (Type_cost.verified_type_cost expected_type type_cost))
        return_type_cost_list
    | [], _ -> raise Type_error


  let get_expr_type_cost t = t.expr_type_cost

  let join t1 t2 =
    { expr_type_cost = t1.expr_type_cost @ t2.expr_type_cost
    ; return_type_cost = t1.return_type_cost @ t2.return_type_cost
    }


  let join_list = List.fold_left ~init:empty ~f:(fun acc x -> join acc x)
  let union_list = join_list
end

module Expr_type_cost_annotation = Type_cost

module Cost_type_ast =
  Make_annotated_ast (Block_side_effect_annotation) (Alpha_var_annotation)
    (Import_annotation)
    (Expr_type_cost_annotation)

module Cost_Ast_mapping = struct
  include
    Default_ast_mapping (Side_effect_ast) (Cost_type_ast) (Type_cost_context)
      (Type_cost_result)

  exception Invalid_arg_number
  exception Invalid_arg_type
  exception Unbound_var of string
  exception Invalid_var_type
  exception Invalid_return_type
  exception Type_error
  exception No_return_statement

  let value env = function
    | Int i ->
      env, Int i, Type_cost_result.create (C_Int (Cost.create_int_cost i))
    | Bool b -> env, Bool b, Type_cost_result.create C_Bool
    | String s ->
      ( env
      , String s
      , Type_cost_result.create
          (C_String (Cost.create_int_cost (String.length s))) )


  let substitute_params_into_type_cost params args cost =
    let alpha_to_cost_mapping =
      Type_cost.create_alpha_to_cost_mapping params args
    in
    Cost.substitute alpha_to_cost_mapping cost


  let get_func_type_cost env func_name : Type_cost.func_type_cost =
    match get_value func_name.alpha env with
    | Some func_type_cost ->
      (match func_type_cost with
      | C_Func func_type_cost -> func_type_cost
      | _ -> raise Invalid_return_type)
    | _ -> raise (Unbound_var func_name.name)


  let user_func env user_func result =
    let { name; args = _ } = user_func in
    let func_type_cost = get_func_type_cost env name in
    match func_type_cost.return with
    | Type_cost.C_Int cost ->
      ( env
      , user_func
      , Type_cost_result.add
          (C_Int
             (substitute_params_into_type_cost
                func_type_cost.params
                (Type_cost_result.get_expr_type_cost result)
                cost))
          result )
    | Type_cost.C_String cost ->
      ( env
      , user_func
      , Type_cost_result.add
          (C_String
             (substitute_params_into_type_cost
                func_type_cost.params
                (Type_cost_result.get_expr_type_cost result)
                cost))
          result )
    | Type_cost.C_Unit -> env, user_func, Type_cost_result.add C_Unit result
    | Type_cost.C_Bool -> env, user_func, Type_cost_result.add C_Bool result
    | _ -> raise Invalid_return_type


  let new_branch env ast (result : Type_cost_result.t) expr_type_cost =
    ( env
    , ast
    , Type_cost_result.create_with_return expr_type_cost result.return_type_cost
    )


  let get_type_cost var env =
    match Type_cost_context.get_value var.alpha env with
    | Some type_cost -> type_cost
    | _ -> raise (Unbound_var var.name)


  let func_call env func_call result =
    match func_call with
    | User_func _ -> env, func_call, result
    | Print _ ->
      (match Type_cost_result.extract_expr_type_cost result with
      | C_String _ | C_Int _ -> new_branch env func_call result None
      | _ -> raise Type_error)
    | Input ->
      new_branch env func_call result (Some (C_String Cost.default_input_bound))
    | Open _ ->
      (match Type_cost_result.extract result.expr_type_cost with
      | C_String _ -> new_branch env func_call result (Some C_File)
      | _ -> raise Type_error)
    | Read var ->
      (match get_type_cost var env with
      | C_File ->
        new_branch
          env
          func_call
          result
          (Some (C_String Cost.default_input_bound))
      | _ -> raise Type_error)
    | Write { file; _ } ->
      (match
         get_type_cost file env, Type_cost_result.extract_expr_type_cost result
       with
      | C_File, C_String _ -> new_branch env func_call result None
      | _ -> raise Type_error)
    | Append { file; _ } ->
      (match
         get_type_cost file env, Type_cost_result.extract_expr_type_cost result
       with
      | C_File, C_String _ -> new_branch env func_call result None
      | _ -> raise Type_error)


  let bin_op_int_to_bool arg_type1 arg_type2 =
    match arg_type1, arg_type2 with
    | Type_cost.C_Int _, Type_cost.C_Int _ -> Type_cost.C_Bool
    | _ -> raise Invalid_arg_type


  let bin_op_bool_to_bool arg_type1 arg_type2 =
    match arg_type1, arg_type2 with
    | Type_cost.C_Bool, Type_cost.C_Bool -> Type_cost.C_Bool
    | _ -> raise Invalid_arg_type


  let expr env expr (result : Type_cost_result.t) =
    match expr with
    | Unop (unop, _) ->
      ( env
      , expr
      , let expr_type_cost = Type_cost_result.extract result.expr_type_cost in
        (match unop with
        | Not ->
          (match expr_type_cost with
          | C_Bool -> Type_cost_result.add C_Bool result
          | _ -> raise Invalid_arg_type)
        | U_Minus ->
          (match expr_type_cost with
          | C_Int cost -> Type_cost_result.add (C_Int (Cost.negate cost)) result
          | _ -> raise Invalid_arg_type)) )
    | Binop (_, binop, _) ->
      ( env
      , expr
      , Type_cost_result.push_type_cost
          (let expr1_type_cost, expr2_type_cost =
             Type_cost_result.extract_2 result.expr_type_cost
           in
           match binop with
           | Plus ->
             (match expr1_type_cost, expr2_type_cost with
             | C_Int cost_1, C_Int cost_2 -> C_Int (Cost.sum cost_1 cost_2)
             | _ -> raise Type_error)
           | B_Minus ->
             (match expr1_type_cost, expr2_type_cost with
             | C_Int cost_1, C_Int cost_2 -> C_Int (Cost.subtract cost_1 cost_2)
             | _ -> raise Type_error)
           | Mult ->
             (match expr1_type_cost, expr2_type_cost with
             | C_Int cost_1, C_Int cost_2 -> C_Int (Cost.multiply cost_1 cost_2)
             | _ -> raise Type_error)
           | Lt | Le | Gt | Ge | Eq | Ne ->
             bin_op_int_to_bool expr1_type_cost expr2_type_cost
           | And | Or -> bin_op_bool_to_bool expr1_type_cost expr2_type_cost)
          result )
    | Var_read var ->
      env, expr, Type_cost_result.push_type_cost (get_type_cost var env) result
    | _ -> ignore_branch env expr result


  let annotated_expr
      ~env
      ~new_expr
      ~old_annotations:_
      ~(result : Type_cost_result.t)
    =
    ( env
    , { expr = new_expr; annotations = result }
    , Type_cost_result.create_with_return None result.return_type_cost )


  let var_statement env var_statement (result : Type_cost_result.t) =
    match var_statement with
    | Var_non_init (_, _) -> ignore_branch env var_statement empty_result
    | Var_init (var, _, _) | Var_decl (var, _) | Var_assign (var, _) ->
      let expr_type_cost = Type_cost_result.extract result.expr_type_cost in
      ( add_to_env var.alpha expr_type_cost env
      , var_statement
      , Type_cost_result.create_with_return None result.return_type_cost )
    | Post_inc var ->
      (match get_type_cost var env with
      | C_Int cost ->
        let new_cost = Type_cost.C_Int (Cost.sum Cost.one cost) in
        ( add_to_env var.alpha new_cost env
        , var_statement
        , Type_cost_result.create_with_return None result.return_type_cost )
      | _ -> raise Invalid_var_type)
    | Post_dec var ->
      (match get_type_cost var env with
      | C_Int cost ->
        let new_cost = Type_cost.C_Int (Cost.subtract cost Cost.one) in
        ( add_to_env var.alpha new_cost env
        , var_statement
        , Type_cost_result.create_with_return None result.return_type_cost )
      | _ -> raise Invalid_var_type)


  let statement env statement (result : Type_cost_result.t) =
    match statement with
    | Expr _ ->
      ( env
      , statement
      , Type_cost_result.create_with_return None result.return_type_cost )
    | Return _ ->
      let type_cost = Type_cost_result.extract result.expr_type_cost in
      ( env
      , statement
      , Type_cost_result.create_with_return
          None
          (type_cost :: result.return_type_cost) )
    | _ -> ignore_branch env statement result


  let update_for_loop_env ~env ~new_init ~new_cond ~new_iter =
    let init_var, init_type_cost =
      match new_init with
      | Var_init (var, _, annotated_expr) -> var, annotated_expr.annotations
      | Var_decl (var, annotated_expr) -> var, annotated_expr.annotations
      | _ -> raise Type_error
    in
    let cond_type_cost = new_cond.annotations in
    let is_inc =
      match new_iter with
      | Post_inc var ->
        if Alpha.compare init_var.alpha var.alpha = 0
        then true
        else raise Type_error
      | Post_dec var ->
        if Alpha.compare init_var.alpha var.alpha = 0
        then false
        else raise Type_error
      | _ -> raise Type_error
    in
    match init_type_cost, cond_type_cost with
    | Type_cost.C_Int init_cost, Type_cost.C_Int cond_cost ->
      Type_cost_context.add_new_item
        init_var.alpha
        (C_Int
           (if is_inc
           then
             Cost.create_lower_upper_bounded_cost
               ~lower:init_cost
               ~upper:(Cost.subtract cond_cost Cost.one)
           else
             Cost.create_lower_upper_bounded_cost
               ~lower:cond_cost
               ~upper:(Cost.sum Cost.one init_cost)))
        env
    | _ -> raise Type_error


  let update_for_each_env ~env ~new_item ~new_iterator =
    match new_iterator with
    | Type_cost.C_String cost ->
      Type_cost_context.add_new_item new_item.alpha (C_String cost) env
    | _ -> raise Type_error


  let if_record env if_record result =
    let number_of_conditions =
      1
      + List.length if_record.else_if
      +
      match if_record.else_contents with
      | Some _ -> 1
      | None -> 0
    in
    let _ = Type_cost_result.extract_n result number_of_conditions in
    env, if_record, Type_cost_result.empty


  let get_new_env start_env end_env iteration_bound =
    Cost.sum
      (Cost.multiply
         (Cost.subtract end_env start_env)
         (Cost.create_int_cost iteration_bound))
      start_env


  let func env func (result : Type_cost_result.t) =
    let { name; params; body = _; return_type } = func in
    add_to_env
      name.alpha
      (Type_cost.C_Func
         { params =
             List.map
               ~f:(fun param ->
                 let var, type_id = param in
                 var.alpha, type_id)
               params
         ; return =
             Type_cost_result.get_return_type_cost
               result.return_type_cost
               return_type
         })
      env
end
