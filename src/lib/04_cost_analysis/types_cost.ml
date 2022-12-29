(* open! Core
open Cost
open Ast.Ast_types
open Util.Environment
open Side_effect.Variable_id
open Util

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

(* and Cost_type_environment : Type_environment = Environment_ (Alpha) (Type_cost) *)

(* module Cost_type_environment = Environment_ (Alpha) (Cost_type) *)

(* module rec Cost_type : sig
  type t

  exception Cost_type_error

  val string_of_t : t -> string
  val create : type_id -> Cost.t -> t
  val create_zero_cost : type_id -> t
  val t_of_value : value -> Cost.t
  val t_of_unop : Cost.t -> unop -> Cost.t
  val t_of_binop : Cost.t -> Cost.t -> binop -> Cost.t
  val t_of_expr : Cost_type_environment.t -> 'a expr -> Cost.t
end = struct
  type t = type_id * Cost.t [@@deriving of_sexp, sexp_of, compare]

  exception Cost_type_error
  exception Unbound_var of string

  let string_of_t t =
    let type_id, cost = t in
    Fmt.str "%s%s" (string_of_type_id type_id) (Cost.string_of_t cost)


  let create type_id cost = type_id, cost
  let create_zero_cost type_id = type_id, Cost.zero

  let t_of_value = function
    | Int i -> Cost.create_int_cost (Integer_bound.create (i, i))
    | String s ->
      let len = String.length s in
      Cost.create_int_cost (Integer_bound.create (len, len))
    | Bool _ -> raise Cost_type_error


  let t_of_unop expr_cost = function
    | Not -> raise Cost_type_error
    | U_Minus -> Cost.negate expr_cost


  let t_of_binop expr_cost1 expr_cost2 = function
    | Plus -> Cost.sum expr_cost1 expr_cost2
    | B_Minus -> Cost.subtract expr_cost1 expr_cost2
    | Mult -> Cost.multiply expr_cost1 expr_cost2
    | _ -> raise Cost_type_error


  let rec t_of_expr env = function
    | Unop (unop, expr) -> t_of_unop (t_of_expr env expr) unop
    | Binop (expr1, binop, expr2) ->
      t_of_binop (t_of_expr env expr1) (t_of_expr env expr2) binop
    | Paren expr -> t_of_expr env expr
    | Value value -> t_of_value value
    | VarRead var ->
      (match Cost_type_environment.get_value var env with
      | Some cost_type -> cost_type
      | None -> raise (Unbound_var var.name))
end

and Cost_type_environment : Type_environment = Environment_ (Alpha) (Cost_type) *) *)
