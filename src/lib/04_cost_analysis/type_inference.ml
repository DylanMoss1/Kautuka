(* open! Core
open Util.Environment
open Ast.Ast_types
open Ast.Annotated_ast
open Ast.Ast_pipeline
open Preperation.Import
open Cost
open Ast.Ast_to_string
open Side_effect.Side_effect_tracking
open Util

module Cost_type = struct
  type t = type_id * Cost.t [@@deriving of_sexp, sexp_of, compare]

  let string_of_t (t : t) =
    let type_id, cost = t in
    Fmt.str "%s%s" (string_of_type_id type_id) (Cost.string_of_t cost)


  let create type_id cost = type_id, cost
  let create_zero_cost type_id = type_id, Cost.zero
end

module Cost_type_environment = Environment_ (Alpha) (Cost_type)

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
      | Write -> env, var, new_effect (Write, Var_mut var.alpha) *)
end

module Cost_type_pipeline = Ast_pipeline (Cost_type_ast_mapping) *) *)
