(* open! Core
open Util.Environment
open Ast.Ast_types
open Ast.Annotated_ast
open Ast.Ast_pipeline
open Parsing.Parser_types
open Preperation.Import
open Ast.Ast_to_string
open Side_effect.Variable_id
open Side_effect.Side_effect_tracking
open Util.Item
open Types_cost
open Cost

module Runtime_cost = struct
  include Cost

  let join = sum
  let join_list = List.fold_left ~init:empty ~f:sum
end

module Cost_type = struct
  type t = type_id * Cost.t [@@deriving of_sexp, sexp_of, compare]

  let string_of_t (type_id, bound_term) =
    Fmt.str "%s %s" (string_of_type_id type_id) (Cost.string_of_t bound_term)
end

module Cost_multiplier_environment = Environment_ (Unit_item) (Runtime_cost)

type block_cost =
  { block_type : block_type
  ; side_effects : Side_effect_set.t
  ; cost_term : Cost.t
  }

module Block_cost_annotation = struct
  type t = block_cost

  let string_of_block_type = function
    | Default -> "Default"
    | Ignore -> "Ignore"
    | Force_par -> "Force_par"
    | Force_seq -> "Force_seq"


  let string_of_t t =
    Fmt.str
      "[%s, %s, %s]"
      (string_of_block_type t.block_type)
      (Side_effect_set.string_of_t t.side_effects)
      (Cost.string_of_t t.cost_term)
end

module Cost_ast =
  Annotated_ast (Block_cost_annotation) (Alpha_var_annotation)
    (Import_annotation)
    (Expr_type_cost_annotation)

module Cost_ast_mapping = struct
  include
    Default_ast_mapping (Cost_type_ast) (Cost_ast) (Cost_multiplier_environment)
      (Runtime_cost)

  let user_func env user_func result = 
    let { name; args = _ } = user_func in
    let {  }

  let expr env expr result = match expr with 
  | Unop (unop, annotated_expr) -> ( 
    match unop with 
    | Not -> 
    | U_Minus -> 
  )
  | Binop (annotated_expr1, binop, annotated_expr2) -> ( 
    match binop with 
    | Plus ->  
    | B_Minus ->  
    | Mult ->  
    | Lt ->  
    | Le ->  
    | Gt ->  
    | Ge ->  
    | Eq ->  
    | Ne ->  
    | And ->  
    | Or -> 
  )
  | VarRead -> 
  | _ ->

  let var_statement env var_statement result = 
    match var_statement with 
    | VarNonInit(_, _) -> 
    | VarInit (_,_,_) -> 
    | VarDecl (_,_) -> 
    | VarAssign (_,_) -> 
    | Pre_inc _ -> 
    | Post_inc -> 

  let for_loop ~start_env ~end_env ~for_loop ~result = 

  let for_each ~start_env ~end_env ~for_each ~result = 

  let update_for_loop_env env for_loop result = 

  let update_for_each_env env for_each result = 

  let if_record env if_record result = 

  let func env func result = 




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

module Cost_ast_pipeline = Ast_pipeline (Cost_ast_mapping) *)
