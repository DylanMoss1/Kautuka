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
open Item

module Types_cost = struct
  type func_type_cost =
    { args : t list
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

  let rec string_of_func_type_cost func_type_cost =
    let { args; return } = func_type_cost in
    String.concat
      ~sep:" -> "
      (List.map ~f:string_of_t (List.append args [ return ]))


  and string_of_t = function
    | C_Int cost -> Fmt.str "int%s" (Cost.string_of_t cost)
    | C_String cost -> Fmt.str "string%s" (Cost.string_of_t cost)
    | C_Bool -> "bool"
    | C_Unit -> "unit"
    | C_File -> "file"
    | C_Func func_type_cost ->
      Fmt.str "func%s" (string_of_func_type_cost func_type_cost)
end

module Cost_type_environment = Environment_ (Alpha) (Types_cost)

module Runtime_cost = struct
  include Cost

  let join = sum
end

module Cost_type_ast =
  Annotated_ast (Block_side_effect_annotation) (Alpha_var_annotation)
    (Import_annotation)

module Cost_type_ast_mapping = struct
  include
    Default_ast_mapping (Side_effect_ast) (Cost_type_ast)
      (Cost_type_environment)
      (Runtime_cost)
end

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

module Cost_type_ast =
  Annotated_ast (Block_side_effect_annotation) (Cost_type_var_annotation)
    (Import_annotation)

module Cost_type_ast_mapping = struct
  include
    Default_ast_mapping (Side_effect_ast) (Cost_type_ast)
      (Cost_type_environment)
      (Empty_result)

  let type_cost_of_unop expr = function
    | Not ->
      (match expr with
      | C_Bool -> C_Bool
      | _ -> raise Type_error)
    | U_Minus ->
      (match expr with
      | C_Int type_cost -> C_Int (Cost.negate type_cost)
      | _ -> raise Type_error)


  let bin_op_int_to_bool expr1 expr2 =
    match expr1, expr2 with
    | C_Int _, C_Int _ -> C_Bool
    | _ -> raise Type_error


  let bin_op_bool_to_bool expr1 expr2 =
    match expr1, expr2 with
    | C_Bool, C_Bool -> C_Bool
    | _ -> raise Type_error


  let type_cost_of_binop expr1 expr2 = function
    | Plus ->
      (match expr1, expr2 with
      | C_Int type_cost1, C_Int type_cost2 ->
        C_Int (Cost.sum type_cost1 type_cost2)
      | C_String type_cost1, C_String type_cost2 ->
        C_String (Cost.sum type_cost1 type_cost2)
      | _ -> raise Type_error)
    | B_Minus ->
      (match expr1, expr2 with
      | C_Int type_cost1, C_Int type_cost2 ->
        C_Int (Cost.subtract type_cost1 type_cost2)
      | _ -> raise Type_error)
    | Mult ->
      (match expr1, expr2 with
      | C_Int type_cost1, C_Int type_cost2 ->
        C_Int (Cost.multiply type_cost1 type_cost2)
      | _ -> raise Type_error)
    | Lt -> bin_op_int_to_bool expr1 expr2
    | Le -> bin_op_int_to_bool expr1 expr2
    | Gt -> bin_op_int_to_bool expr1 expr2
    | Ge -> bin_op_int_to_bool expr1 expr2
    | Eq -> bin_op_int_to_bool expr1 expr2
    | Ne -> bin_op_int_to_bool expr1 expr2
    | And -> bin_op_bool_to_bool expr1 expr2
    | Or -> bin_op_bool_to_bool expr1 expr2


  let type_cost_of_value = function
    | Int i -> C_Int (Cost.create_int_cost (Integer_bound.create (i, i)))
    | Bool _ -> C_Bool
    | String s ->
      let len = String.length s in
      C_String (Cost.create_int_cost (Integer_bound.create (len, len)))


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

module Cost_type_pipeline = Ast_pipeline (Cost_type_ast_mapping) *) *)
