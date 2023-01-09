let rec collect_blocks_into_thread block_list collected_blocks remaining_blocks passed_effects = 
  match block_list with 
  | block :: block_list -> (
    let new_passed_effects = Effect.union effect passed_effects in 
    if Effect.disjoint block.effects passed_effects then 
      collect_blocks_into_thread block_list (List.append collected_blocks [block]) remaining_blocks  new_passed_effects
    else 
      collect_blocks_into_thread block_list collected_blocks (List.append remaining_blocks [block])  new_passed_effects
  )
  | [] -> collected_blocks, remaining_blocks 

let collect_blocks_into_thread_list block_list = 
  ( 
    let rec collect_blocks_into_thread_list_inner block_list thread_list = 
      if List.length block_list = 0 then 
        thread_list 
    else
      let collected_blocks, remaining_blocks = collect_blocks_into_thread block_list [] Effect.empty []
      in collect_blocks_into_thread_list_inner remaining_blocks (List.append thread_list [collected_blocks])         
  ) in collect_blocks_into_thread_list_inner block_list [] 



(* open! Core
open Ast.Ast_types

let rec parallelise_expr = function
  | Unop (unop, expr) -> Unop (unop, parallelise_expr expr)
  | Binop (expr1, binop, expr2) ->
    Binop (parallelise_expr expr1, binop, parallelise_expr expr2)
  | Paren expr -> Paren (parallelise_expr expr)
  | Value value -> Value value
  | Var var -> Var var


let parallelise_statement = function
  | Expr expr -> Expr (parallelise_expr expr)
  | Var var -> Var var
  | Func_call func_call -> Func_call func_call


let rec parallelise_for_loop for_loop =
  { init = for_loop.init
  ; cond = for_loop.cond
  ; iter = for_loop.iter
  ; contents = parallelise_block for_loop.contents
  }


and parallelise_for_each for_each =
  { item = for_each.item
  ; iterator = for_each.iterator
  ; contents = parallelise_block for_each.contents
  }


and parallelise_condition_template condition_template =
  { condition = condition_template.condition
  ; contents = parallelise_block condition_template.contents
  }


and parallelise_if_record if_record =
  { _if = parallelise_condition_template if_record._if
  ; else_if = List.map ~f:parallelise_condition_template if_record.else_if
  ; else_contents = Option.map ~f:parallelise_block if_record.else_contents
  }


and parallelise_structure = function
  | Func func -> Func (parallelise_func func)
  | Block block -> Block (parallelise_block block)
  | If if_record -> If (parallelise_if_record if_record)
  | While while_loop -> While (parallelise_condition_template while_loop)
  | For_loop for_loop -> For_loop (parallelise_for_loop for_loop)
  | For_each for_each -> For_each (parallelise_for_each for_each)


and parallelise_command = function
  | Structure structure -> Structure (parallelise_structure structure)
  | Statement statement -> Statement (parallelise_statement statement)


and parallelise_block_record block_record =
  { contents = List.map ~f:parallelise_command block_record.contents
  ; annotations = block_record.annotations
  }


and parallelise_block = function
  | Default_block block_record ->
    (match block_record.annotations.is_parallel with
    | Some true ->
      Go_block (List.map ~f:parallelise_command block_record.contents)
    | Some false -> Default_block (parallelise_block_record block_record)
    | None -> Default_block (parallelise_block_record block_record))
  | For_block block_record -> For_block block_record
  | Ignore contents -> Ignore contents
  | Go_block contents -> Go_block contents


and parallelise_func func =
  { name = func.name
  ; params = func.params
  ; body = parallelise_block func.body
  ; return_type = func.return_type
  }


let parallelise_program program =
  { package = program.package
  ; imports = program.imports
  ; global_vars = program.global_vars
  ; funcs = List.map ~f:parallelise_func program.funcs
  } *)
