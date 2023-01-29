open! Core
open Util.Context
open Ast.Ast_types
open Ast.Annotated_ast
open Ast.Ast_pipeline
open Parsing
open Preperation.Import
open Side_effect_system.Alpha_conversion
open Side_effect_system.Side_effect_tracking
open Types_cost
open Cost
open Util

module Runtime_cost = struct
  include Cost

  let join = sum
  let join_list = List.fold_left ~init:empty ~f:sum
  let union_list = List.fold_left ~init:empty ~f:union
  let tr = one
  let cost_of_not = tr
  let cost_of_u_minus = tr
  let cost_of_plus_int = tr
  let cost_of_plus_str = tr
  let cost_of_b_minus = tr
  let cost_of_mult = tr
  let cost_of_lt = tr
  let cost_of_le = tr
  let cost_of_gt = tr
  let cost_of_ge = tr
  let cost_of_eq = tr
  let cost_of_ne = tr
  let cost_of_and = tr
  let cost_of_or = tr
  let cost_of_var_read = tr
  let cost_of_var_non_init = tr
  let cost_of_var_init = tr
  let cost_of_var_decl = tr
  let cost_of_var_assign = tr
  let cost_of_post_inc = tr
  let cost_of_post_dec = tr
  let cost_of_user_func_call = create_int_cost 50
  let cost_of_print x = multiply (create_int_cost 20) x
  let cost_of_input = create_int_cost 5000
  let cost_of_open = create_int_cost 300
  let cost_of_read = create_int_cost 300
  let cost_of_write x = sum (create_int_cost 500) x
  let cost_of_append x = sum (create_int_cost 500) x
  let cost_of_for_loop = create_int_cost 20
  let cost_of_for_each = create_int_cost 30
  let cost_of_if_record = create_int_cost 10
end

type param = Alpha.t * type_id [@@deriving of_sexp, sexp_of, compare]

module Runtime_func_cost = struct
  type t =
    { params : param list
    ; runtime_cost : Cost.t
    }
  [@@deriving of_sexp, sexp_of, compare]

  let string_of_param param =
    let alpha, type_id = param in
    Fmt.str "%s : %s" (Alpha.string_of_t alpha) (string_of_type_id type_id)


  let string_of_t t =
    Fmt.str
      "%s : %s"
      (String.concat ~sep:", " (List.map ~f:string_of_param t.params))
      (Cost.string_of_t t.runtime_cost)
end

module Runtime_cost_context = Make_context (Alpha) (Runtime_func_cost)

type block_runtime_cost =
  { block_type : Parser_types.block_type
  ; scoped_vars : Alpha.t list
  ; side_effects : Side_effect_set.t
  ; runtime_cost : Cost.t
  }

module Block_runtime_cost_annotation = struct
  type t = block_runtime_cost

  let string_of_t t =
    Fmt.str
      "{block_type: %s, scoped_vars: %s, side_effects: %s, runtime_cost: %s}"
      (Parser_types.string_of_block_type t.block_type)
      (string_of_scoped_vars t.scoped_vars)
      (Side_effect_set.string_of_t t.side_effects)
      (Cost.string_of_t t.runtime_cost)
end

module Cost_tracking_ast =
  Annotated_ast (Block_runtime_cost_annotation) (Alpha_conversion_annotation)
    (Import_annotation)
    (Expr_type_cost_annotation)

module Cost_tracking_ast_mapping = struct
  include
    Default_ast_mapping (Type_cost_ast) (Cost_tracking_ast) (Runtime_cost_context)
      (Runtime_cost)

  exception Type_error

  let user_func env user_func result =
    let { name; args } = user_func in
    let ({ params; runtime_cost } : Runtime_func_cost.t) =
      match Runtime_cost_context.get_value name.alpha env with
      | Some runtime_cost -> runtime_cost
      | None -> raise Type_error
    in
    ( env
    , user_func
    , Runtime_cost.sum
        result
        (Runtime_cost.substitute
           (Type_cost.create_alpha_to_cost_mapping
              params
              (List.map ~f:(fun arg -> arg.annotations) args))
           runtime_cost) )


  let expr env expr result =
    ( env
    , expr
    , join_results
        result
        (match expr with
        | Unop (unop, _) ->
          (match unop with
          | Not -> Runtime_cost.cost_of_not
          | U_Minus -> Runtime_cost.cost_of_u_minus)
        | Binop (annotated_expr1, binop, annotated_expr2) ->
          (match binop with
          | Plus ->
            (match annotated_expr1.annotations, annotated_expr2.annotations with
            | Type_cost.C_Int _, Type_cost.C_Int _ ->
              Runtime_cost.cost_of_plus_int
            | Type_cost.C_String _, Type_cost.C_String _ ->
              Runtime_cost.cost_of_plus_str
            | _ -> raise Type_error)
          | B_Minus -> Runtime_cost.cost_of_b_minus
          | Mult -> Runtime_cost.cost_of_mult
          | Lt -> Runtime_cost.cost_of_lt
          | Le -> Runtime_cost.cost_of_le
          | Gt -> Runtime_cost.cost_of_gt
          | Ge -> Runtime_cost.cost_of_ge
          | Eq -> Runtime_cost.cost_of_eq
          | Ne -> Runtime_cost.cost_of_ne
          | And -> Runtime_cost.cost_of_and
          | Or -> Runtime_cost.cost_of_or)
        | Var_read _ -> Runtime_cost.cost_of_var_read
        | _ -> empty_result) )


  let var_statement env var_statement result =
    match var_statement with
    | Var_non_init (_, _) ->
      env, var_statement, join_results result Runtime_cost.cost_of_var_non_init
    | Var_init (_, _, _) ->
      env, var_statement, join_results result Runtime_cost.cost_of_var_init
    | Var_decl (_, _) ->
      env, var_statement, join_results result Runtime_cost.cost_of_var_decl
    | Var_assign (_, _) ->
      env, var_statement, join_results result Runtime_cost.cost_of_var_assign
    | Post_inc _ ->
      env, var_statement, join_results result Runtime_cost.cost_of_post_inc
    | Post_dec _ ->
      env, var_statement, join_results result Runtime_cost.cost_of_post_dec


  let for_loop ~start_env:_ ~end_env ~for_loop ~result =
    let { init; cond; iter; contents = _ } = for_loop in
    let init_var, init_type_cost =
      match init with
      | Var_init (var, _, annotated_expr) -> var, annotated_expr.annotations
      | Var_decl (var, annotated_expr) -> var, annotated_expr.annotations
      | _ -> raise Type_error
    in
    let cond_type_cost = cond.annotations in
    let is_inc =
      match iter with
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
      ( end_env
      , for_loop
      , if is_inc
        then
          Runtime_cost.multiply
            (Runtime_cost.subtract
               (Runtime_cost.subtract cond_cost Runtime_cost.one)
               init_cost)
            result
        else
          Runtime_cost.multiply
            (Runtime_cost.subtract (Cost.sum Cost.one init_cost) cond_cost)
            result )
    | _ -> raise Type_error


  let for_each ~start_env:_ ~end_env ~for_each ~result =
    match for_each.iterator.annotations with
    | Type_cost.C_String cost ->
      end_env, for_each, Runtime_cost.multiply cost result
    | _ -> raise Type_error


  let func env func result =
    let { name; params; body = _; return_type = _ } = func in
    ( add_to_env
        name.alpha
        { params =
            List.map
              ~f:(fun param ->
                let var, type_id = param in
                var.alpha, type_id)
              params
        ; runtime_cost = result
        }
        env
    , func
    , empty_result )


  let block ~env ~new_contents ~(old_annotations : old_block_annot) ~result =
    ( env
    , { contents = new_contents
      ; annotations =
          { block_type = old_annotations.block_type
          ; scoped_vars = old_annotations.scoped_vars
          ; side_effects = old_annotations.side_effects
          ; runtime_cost = result
          }
      }
    , result )
end

module Cost_tracking_ast_pipeline = Ast_pipeline (Cost_tracking_ast_mapping)
