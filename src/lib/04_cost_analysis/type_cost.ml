open! Core
open Util.Context
open Ast.Ast_types
open Ast.Annotated_ast
open Ast.Ast_pipeline
open Preperation.Import
open Cost
open Preperation.Alpha_conversion
open Side_effect_system.Side_effect_tracking
open Util
open Parsing.Parser_types

let string_of_type_id = function
  | T_Int -> "int"
  | T_Bool -> "bool"
  | T_String -> "string"
  | T_Unit -> ""
  | T_File -> "file"


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


  let default = function
    | T_Int -> C_Int Cost.zero
    | T_String -> C_String Cost.zero
    | T_Bool -> C_Bool
    | T_Unit -> C_Unit
    | T_File -> C_File


  let size_of_bound = function
    | Upper u -> Cost.create_int_cost u
    | Both (l, u) -> Cost.create_int_bound (l, u)


  let get_size = function
    | C_Int size | C_String size -> size
    | _ -> raise Type_error


  let verified_type_cost expected_type type_cost =
    (* print_endline (string_of_type_id expected_type);
    print_endline (string_of_t type_cost); *)
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


  let apply_cost_unary_fun ~f = function
    | C_Int cost -> C_Int (f cost)
    | C_String cost -> C_String (f cost)
    | type_cost -> type_cost


  let apply_cost_bin_fun ~f t1 t2 =
    match t1, t2 with
    | C_Int cost1, C_Int cost2 -> C_Int (f cost1 cost2)
    | C_String cost1, C_String cost2 -> C_String (f cost1 cost2)
    | C_Bool, C_Bool -> C_Bool
    | C_Unit, C_Unit -> C_Unit
    | C_File, C_File -> C_File
    | C_Func func_type_cost1, C_Func func_type_cost2 ->
      if compare_func_type_cost func_type_cost1 func_type_cost2 = 0
      then C_Func func_type_cost1
      else raise Type_error
    | _ -> raise Type_error


  let sum = apply_cost_bin_fun ~f:Cost.sum
  let subtract = apply_cost_bin_fun ~f:Cost.subtract

  let multiply scalar_cost t =
    apply_cost_unary_fun ~f:(Cost.multiply scalar_cost) t
end

module Type_cost_context =
  Make_context (Alpha) (Type_cost)
    (struct
      let t = false
    end)

module Type_cost_result = struct
  type t =
    { expr_type_cost : Type_cost.t list
    ; return_type_cost : Type_cost.t list
    }

  exception Invalid_arg_number of string
  exception Type_error

  let string_of_type_cost_list type_cost_list =
    Fmt.str
      "[%s]"
      (String.concat
         ~sep:", "
         (List.map ~f:Type_cost.string_of_t type_cost_list))


  let string_of_t t =
    Fmt.str
      "{ expr_type_cost = %s; return_type_cost = %s }"
      (string_of_type_cost_list t.expr_type_cost)
      (string_of_type_cost_list t.return_type_cost)


  let empty = { expr_type_cost = []; return_type_cost = [] }

  let extract xs =
    match xs with
    | [ x ] -> x
    | _ ->
      raise
        (Invalid_arg_number
           (Fmt.str "Expected 1 arg, found %d" (List.length xs)))


  let extract_2 xs =
    match xs with
    | [ x1; x2 ] -> x1, x2
    | _ ->
      raise
        (Invalid_arg_number
           (Fmt.str "Expected 2 args, found %d" (List.length xs)))


  let rec extract_n xs n =
    match xs, n with
    | [], 0 -> []
    | [], _ | _ :: _, 0 ->
      raise
        (Invalid_arg_number
           (Fmt.str "Expected %d arg(s), found %d" n (List.length xs)))
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

type block_type_cost =
  { block_type : block_type
  ; scoped_vars : Alpha.t list
  ; side_effects : Side_effect_set.t
  ; type_cost_context : Type_cost_context.t
  }

module Block_type_cost_annotation = struct
  type t = block_type_cost

  let string_of_t t =
    Fmt.str
      "{block_type: %s, scoped_vars: %s, side_effects: %s, type_cost_context: \
       %s}"
      (string_of_block_type t.block_type)
      (string_of_scoped_vars t.scoped_vars)
      (Side_effect_set.string_of_t t.side_effects)
      (Type_cost_context.string_of_t t.type_cost_context)
end

module Type_cost_ast =
  Annotated_ast (Block_type_cost_annotation) (Alpha_conversion_annotation)
    (Import_annotation)
    (Expr_type_cost_annotation)

module Type_cost_ast_mapping = struct
  include
    Default_ast_mapping (Side_effect_ast) (Type_cost_ast) (Type_cost_context)
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
    (* print_endline (Cost.string_of_t cost); *)
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
          empty_result )
    | Type_cost.C_String cost ->
      ( env
      , user_func
      , Type_cost_result.add
          (C_String
             (substitute_params_into_type_cost
                func_type_cost.params
                (Type_cost_result.get_expr_type_cost result)
                cost))
          empty_result )
    | Type_cost.C_Unit ->
      env, user_func, Type_cost_result.add C_Unit empty_result
    | Type_cost.C_Bool ->
      env, user_func, Type_cost_result.add C_Bool empty_result
    | _ -> raise Invalid_return_type


  let new_branch env ast (result : Type_cost_result.t) expr_type_cost =
    ( env
    , ast
    , Type_cost_result.create_with_return expr_type_cost result.return_type_cost
    )


  let cost_of_bound bound =
    match bound with
    | Upper u -> Cost.create_int_cost u
    | Both (l, u) -> Cost.create_int_bound (l, u)


  let get_type_cost var env =
    match Type_cost_context.get_value var.alpha env with
    | Some type_cost -> type_cost
    | _ ->
      raise
        (Unbound_var (Fmt.str "%s, %s" var.name (Alpha.string_of_t var.alpha)))


  let func_call env func_call result =
    match func_call with
    | User_func _ -> env, func_call, result
    | Print _ ->
      (match Type_cost_result.extract_expr_type_cost result with
      | C_String _ | C_Int _ -> new_branch env func_call result (Some C_Unit)
      | _ -> raise Type_error)
    | Input bound ->
      new_branch env func_call result (Some (C_String (cost_of_bound bound)))
    | Open _ ->
      (match Type_cost_result.extract result.expr_type_cost with
      | C_String _ -> new_branch env func_call result (Some C_File)
      | _ -> raise Type_error)
    | Read (_, bound) ->
      (match Type_cost_result.extract result.expr_type_cost with
      | C_File ->
        new_branch env func_call result (Some (C_String (cost_of_bound bound)))
      | _ -> raise Type_error)
    | Write { file = _; contents = _ } ->
      (match Type_cost_result.extract_2 result.expr_type_cost with
      | C_File, C_String _ -> new_branch env func_call result (Some C_Unit)
      | _ -> raise Type_error)
    | Append { file = _; contents = _ } ->
      (match Type_cost_result.extract_2 result.expr_type_cost with
      | C_File, C_String _ -> new_branch env func_call result (Some C_Unit)
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
      , Type_cost_result.create_with_return
          (Some
             (let expr1_type_cost, expr2_type_cost =
                Type_cost_result.extract_2 result.expr_type_cost
              in
              match binop with
              | Plus ->
                (match expr1_type_cost, expr2_type_cost with
                | C_Int cost_1, C_Int cost_2 -> C_Int (Cost.sum cost_1 cost_2)
                | C_String cost_1, C_String cost_2 ->
                  C_String (Cost.sum cost_1 cost_2)
                | _ -> raise Type_error)
              | B_Minus ->
                (match expr1_type_cost, expr2_type_cost with
                | C_Int cost_1, C_Int cost_2 ->
                  C_Int (Cost.subtract cost_1 cost_2)
                | _ -> raise Type_error)
              | Mult ->
                (match expr1_type_cost, expr2_type_cost with
                | C_Int cost_1, C_Int cost_2 ->
                  C_Int (Cost.multiply cost_1 cost_2)
                | _ -> raise Type_error)
              | Lt | Le | Gt | Ge ->
                bin_op_int_to_bool expr1_type_cost expr2_type_cost
              | Eq | Ne ->
                (match expr1_type_cost, expr2_type_cost with
                | Type_cost.C_Int _, Type_cost.C_Int _ -> Type_cost.C_Bool
                | Type_cost.C_String _, Type_cost.C_String _ -> Type_cost.C_Bool
                | _ -> raise Invalid_arg_type)
              | And | Or -> bin_op_bool_to_bool expr1_type_cost expr2_type_cost))
          result.return_type_cost )
    | Var_read var ->
      env, expr, Type_cost_result.push_type_cost (get_type_cost var env) result
    | _ -> ignore_branch env expr result


  let annotated_expr
      ~env
      ~new_expr
      ~old_annotations:_
      ~(result : Type_cost_result.t)
    =
    (* print_endline
      (String.concat
         ~sep:","
         (List.map ~f:(fun t -> Type_cost.string_of_t t) result.expr_type_cost)); *)
    ( env
    , { expr = new_expr
      ; annotations = Type_cost_result.extract result.expr_type_cost
      }
    , result )


  let var_statement env var_statement (result : Type_cost_result.t) =
    match var_statement with
    | Var_non_init (var, type_id) ->
      ( Type_cost_context.add_item var.alpha (Type_cost.default type_id) env
      , var_statement
      , empty_result )
    | Var_init (var, _, annot_expr)
    | Var_decl (var, annot_expr)
    | Var_assign (var, annot_expr) ->
      let expr_type_cost = annot_expr.annotations in
      ( add_to_env var.alpha expr_type_cost env
      , var_statement
      , Type_cost_result.create_with_return None result.return_type_cost )
      (* if String.compare var.name "_" = 0
      then
        ( env
        , var_statement
        , Type_cost_result.create_with_return None result.return_type_cost )
      else (
        let expr_type_cost = annot_expr.annotations in
        ( add_to_env var.alpha expr_type_cost env
        , var_statement
        , Type_cost_result.create_with_return None result.return_type_cost )) *)
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


  let update_for_loop_env
      ~env
      ~new_init
      ~(new_cond : ('a, 'b) annotated_expr)
      ~new_iter
    =
    let init_var, init_type_cost =
      match new_init with
      | Var_init (var, _, annotated_expr) -> var, annotated_expr.annotations
      | Var_decl (var, annotated_expr) -> var, annotated_expr.annotations
      | _ -> raise Type_error
    in
    let cond_type_cost =
      let new_code_expr = new_cond.expr in
      match new_code_expr with
      | Binop (var, Lt, value) ->
        let var =
          match var.expr with
          | Var_read var -> var
          | _ -> raise Type_error
        in
        if Alpha.compare init_var.alpha var.alpha = 0
        then value.annotations
        else raise Type_error
      | _ -> raise Type_error
    in
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
      Type_cost_context.add_item
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


  let update_for_each_env
      ~env
      ~new_item
      ~(new_iterator : ('a, 'b) annotated_expr)
    =
    match new_iterator.annotations with
    | Type_cost.C_String _ ->
      Type_cost_context.add_item
        new_item.alpha
        (C_String (Cost.create_int_cost 1))
        env
    | _ -> raise Type_error


  let get_new_env ~for_each ~(start_env : env) ~(end_env : env) ~iterations =
    (* print_endline (Type_cost_context.string_of_t start_env);  *)
    (* print_endline (Type_cost_context.string_of_t end_env);  *)
    let start_env = remove_scope ~is_func:false start_env in
    let end_env = remove_scope ~is_func:false end_env in
    let end_env =
      if for_each then end_env else remove_scope ~is_func:false end_env
    in
    (* print_endline (Type_cost_context.string_of_t start_env);  *)
    (* print_endline (Type_cost_context.string_of_t end_env);  *)
    Type_cost_context.add_new_scope
      ~is_func:false
      (Type_cost_context.get_post_loop_context
         ~start_env
         ~end_env
         ~sum:Type_cost.sum
         ~subtract:Type_cost.subtract
         ~multiply:Type_cost.multiply
         ~iterations)


  (* let get_new_env ~(start_env : env) ~(end_env : env) ~iterations =
    print_endline (Type_cost_context.string_of_t start_env);
    print_endline (Type_cost_context.string_of_t end_env);
    let end_env = remove_scope ~is_func:false end_env in
    print_endline (Type_cost_context.string_of_t end_env);
    print_endline "\n\n";
    let delta_env =
      Type_cost_context.apply_bin_fun
        ~f:(Type_cost.apply_cost_bin_fun ~f:Cost.subtract)
        (Type_cost_context.remove_scope ~is_func:true end_env)
        (Type_cost_context.remove_scope ~is_func:true start_env)
    in
    let delta_env =
      Type_cost_context.apply_unary_fun
        ~f:(Type_cost.apply_cost_unary_fun ~f:(Cost.multiply iterations))
        delta_env
    in
    let x =
      Type_cost_context.apply_bin_fun
        ~f:(Type_cost.apply_cost_bin_fun ~f:Cost.sum)
        start_env
        (Type_cost_context.add_new_scope ~is_func:true delta_env)
    in
    (* print_endline (Type_cost_context.string_of_t x); *)
    x *)

  let for_loop ~start_env ~end_env ~for_loop ~result =
    (* print_endline "for_start";
    print_endline (Type_cost_context.string_of_t start_env);
    print_endline (Type_cost_context.string_of_t end_env);
    print_endline "for_end"; *)
    let { init; cond; iter; contents = _ } = for_loop in
    let init_var, init_type_cost =
      match init with
      | Var_init (var, _, annotated_expr) -> var, annotated_expr.annotations
      | Var_decl (var, annotated_expr) -> var, annotated_expr.annotations
      | _ -> raise Type_error
    in
    let cond_type_cost =
      let new_code_expr = cond.expr in
      match new_code_expr with
      | Binop (var, Lt, value) ->
        let var =
          match var.expr with
          | Var_read var -> var
          | _ -> raise Type_error
        in
        if Alpha.compare init_var.alpha var.alpha = 0
        then value.annotations
        else raise Type_error
      | _ -> raise Type_error
    in
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
      let iterations =
        if is_inc
        then Cost.subtract cond_cost Cost.one
        else Cost.subtract (Cost.sum Cost.one init_cost) cond_cost
      in
      ( get_new_env ~for_each:false ~start_env ~end_env ~iterations
      , for_loop
      , result )
    | _ -> raise Type_error


  let for_each ~start_env ~end_env ~for_each ~result =
    match for_each.iterator.annotations with
    | Type_cost.C_String cost ->
      ( get_new_env ~for_each:true ~start_env ~end_env ~iterations:cost
      , for_each
      , result )
    | _ -> raise Type_error


  let if_record env if_record (result : result) =
    let number_of_conditions =
      1
      + List.length if_record.else_if
    in
    let _ =
      Type_cost_result.extract_n result.expr_type_cost number_of_conditions
    in
    ( env
    , if_record
    , Type_cost_result.create_with_return None result.return_type_cost )


  let block
      ~env
      ~old_env
      ~new_contents
      ~(old_annotations : old_block_annot)
      ~result
    =
    (* print_endline "block_start";
    print_endline (Type_cost_context.string_of_t env);
    print_endline (Type_cost_context.string_of_t old_env);
    print_endline "block_end"; *)
    ( env
    , old_env
    , { contents = new_contents
      ; annotations =
          { block_type = old_annotations.block_type
          ; scoped_vars = old_annotations.scoped_vars
          ; side_effects = old_annotations.side_effects
          ; type_cost_context = old_env
          }
      }
    , result )


  (* block_type : block_type
  ; scoped_vars : Alpha.t list
  ; side_effects : Side_effect_set.t
  ; type_cost_contxt : Type_cost_context.t
   *)
  (* let block ~env ~old_env ~new_contents ~old_annotations ~result =
    (* let x = get_new_env ~start_env:old_env ~end_env:env ~iterations:Cost.one in
    print_endline (Type_cost_context.string_of_t old_env);
    print_endline (Type_cost_context.string_of_t env);
    print_endline (Type_cost_context.string_of_t x);
    print_endline "\n\n";
    ( get_new_env ~start_env:old_env ~end_env:env ~iterations:Cost.one
    , old_env
    , { contents = new_contents; annotations = old_annotations }
    , result ) *)
    ( env, ) *)

  let param env param result =
    let var, type_id = param in
    ( add_to_env
        var.alpha
        (match type_id with
        | T_Int -> C_Int (Cost.create_single_var_bound var.alpha)
        | T_String -> C_String (Cost.create_single_var_bound var.alpha)
        | T_Bool -> C_Bool
        | T_Unit -> C_Unit
        | T_File -> C_File)
        env
    , param
    , result )


  let func env func (result : Type_cost_result.t) =
    let { name; params; body = _; return_type } = func in
    ( add_to_env
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
    , func
    , Type_cost_result.empty )
end

module Type_cost_ast_pipeline = Ast_pipeline (Type_cost_ast_mapping)
