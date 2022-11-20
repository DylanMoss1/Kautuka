open! Core
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
  ; effects = block_record.effects
  ; cost = block_record.cost
  ; is_parallel = block_record.is_parallel
  }


and parallelise_block = function
  | Block block_record ->
    (match block_record.is_parallel with
    | Some true ->
      Go_block (List.map ~f:parallelise_command block_record.contents)
    | Some false -> Block (parallelise_block_record block_record)
    | None -> Block (parallelise_block_record block_record))
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
  }
