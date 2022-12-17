open! Core
open Ast.Ast_types

module type Type_ast_mapping = sig
  type result
  type old_block_annot
  type old_import_annot
  type new_block_annot
  type new_import_annot

  val collect_results : result list -> result
  val empty_result : unit -> result
  val id : id -> id * result
  val type_id : type_id -> type_id * result
  val value : value -> value * result
  val unop : unop -> unop * result
  val binop : binop -> binop * result
  val expr : expr -> result -> expr * result
  val var : var -> result -> var * result
  val user_func : user_func -> result -> user_func * result
  val write_template : write_template -> result -> write_template * result
  val func_call : func_call -> result -> func_call * result
  val control : control -> control * result
  val statement : statement -> result -> statement * result

  val for_loop
    :  new_block_annot for_loop
    -> result
    -> new_block_annot for_loop * result

  val for_each
    :  new_block_annot for_each
    -> result
    -> new_block_annot for_each * result

  val condition_template
    :  new_block_annot condition_template
    -> result
    -> new_block_annot condition_template * result

  val if_record
    :  new_block_annot if_record
    -> result
    -> new_block_annot if_record * result

  val structure
    :  new_block_annot structure
    -> result
    -> new_block_annot structure * result

  val command
    :  new_block_annot command
    -> result
    -> new_block_annot command * result

  val block
    :  new_block_annot command list
    -> old_block_annot
    -> result
    -> new_block_annot block * result

  val param : id * type_id -> result -> (id * type_id) * result
  val func : new_block_annot func -> result -> new_block_annot func * result

  val program
    :  id
    -> old_import_annot
    -> var list
    -> new_block_annot func list
    -> result
    -> (new_block_annot, new_import_annot) program * result
end

module type Type_ast_mapping_types = sig
  type result
  type old_block_annot
  type old_import_annot
  type new_block_annot
  type new_import_annot

  val collect_results : result list -> result
  val empty_result : unit -> result
end

module Default_ast_mapping (U : Type_ast_mapping_types) = struct
  include U

  let no_result new_ast = new_ast, empty_result ()
  let relay_result new_ast result = new_ast, result

  let program new_package old_import new_global_vars new_funcs result =
    ( { package = new_package
      ; imports = old_import
      ; global_vars = new_global_vars
      ; funcs = new_funcs
      }
    , result )


  let func = relay_result
  let param = relay_result

  let block new_contents old_annotations result =
    { contents = new_contents; annotations = old_annotations }, result


  let command = relay_result
  let structure = relay_result
  let statement = relay_result
  let for_loop = relay_result
  let for_each = relay_result
  let condition_template = relay_result
  let if_record = relay_result
  let control = no_result
  let func_call = relay_result
  let write_template = relay_result
  let user_func = relay_result
  let var = relay_result
  let expr = relay_result
  let binop = no_result
  let unop = no_result
  let value = no_result
  let type_id = no_result
  let id = no_result
end

module Ast_pipeline (Mapping : Type_ast_mapping) = struct
  let append_tuple (xs, ys) (x, y) = x :: xs, y :: ys
  let unzip l = List.fold_left ~f:append_tuple ~init:([], []) l

  let single_pipeline ~sub_pipeline ~mapping ~new_ast_fun ast =
    let new_ast, ast_result = sub_pipeline ast in
    mapping (new_ast_fun new_ast) ast_result


  let pipeline_map ~f ast_list =
    let new_ast, ast_result_list = unzip (List.map ~f ast_list) in
    let ast_result = Mapping.collect_results ast_result_list in
    new_ast, ast_result


  let pipeline_id = Mapping.id
  let pipeline_type_id = Mapping.type_id
  let pipeline_value = Mapping.value
  let pipeline_unop = Mapping.unop
  let pipeline_binop = Mapping.binop

  let rec pipeline_expr = function
    | Unop (unop, expr) ->
      let new_unop, unop_result = pipeline_unop unop in
      let new_expr, expr_result = pipeline_expr expr in
      let result = Mapping.collect_results [ unop_result; expr_result ] in
      Mapping.expr (Unop (new_unop, new_expr)) result
    | Binop (expr1, binop, expr2) ->
      let new_expr1, expr1_result = pipeline_expr expr1 in
      let new_binop, binop_result = pipeline_binop binop in
      let new_expr2, expr2_result = pipeline_expr expr2 in
      let result =
        Mapping.collect_results [ expr1_result; binop_result; expr2_result ]
      in
      Mapping.expr (Binop (new_expr1, new_binop, new_expr2)) result
    | Paren expr ->
      single_pipeline
        ~sub_pipeline:pipeline_expr
        ~mapping:Mapping.expr
        ~new_ast_fun:(fun x -> Paren x)
        expr
    | Value value ->
      single_pipeline
        ~sub_pipeline:pipeline_value
        ~mapping:Mapping.expr
        ~new_ast_fun:(fun x -> Value x)
        value
    | VarRead id ->
      single_pipeline
        ~sub_pipeline:pipeline_id
        ~mapping:Mapping.expr
        ~new_ast_fun:(fun x -> VarRead x)
        id


  let pipeline_var = function
    | VarNonInit (id, type_id) ->
      let new_id, id_result = pipeline_id id in
      let new_type_id, type_id_result = pipeline_type_id type_id in
      let result = Mapping.collect_results [ id_result; type_id_result ] in
      Mapping.var (VarNonInit (new_id, new_type_id)) result
    | VarInit (id, type_id, expr) ->
      let new_id, id_result = pipeline_id id in
      let new_type_id, type_id_result = pipeline_type_id type_id in
      let new_expr, expr_result = pipeline_expr expr in
      let result =
        Mapping.collect_results [ id_result; type_id_result; expr_result ]
      in
      Mapping.var (VarInit (new_id, new_type_id, new_expr)) result
    | VarDecl (id, expr) ->
      let new_id, id_result = pipeline_id id in
      let new_expr, expr_result = pipeline_expr expr in
      let result = Mapping.collect_results [ id_result; expr_result ] in
      Mapping.var (VarDecl (new_id, new_expr)) result
    | VarAssign (id, expr) ->
      let new_id, id_result = pipeline_id id in
      let new_expr, expr_result = pipeline_expr expr in
      let result = Mapping.collect_results [ id_result; expr_result ] in
      Mapping.var (VarAssign (new_id, new_expr)) result
    | Pre_inc id ->
      single_pipeline
        ~sub_pipeline:pipeline_id
        ~mapping:Mapping.var
        ~new_ast_fun:(fun x -> Pre_inc x)
        id
    | Pre_dec id ->
      single_pipeline
        ~sub_pipeline:pipeline_id
        ~mapping:Mapping.var
        ~new_ast_fun:(fun x -> Pre_dec x)
        id
    | Post_inc id ->
      single_pipeline
        ~sub_pipeline:pipeline_id
        ~mapping:Mapping.var
        ~new_ast_fun:(fun x -> Post_inc x)
        id
    | Post_dec id ->
      single_pipeline
        ~sub_pipeline:pipeline_id
        ~mapping:Mapping.var
        ~new_ast_fun:(fun x -> Post_dec x)
        id


  let pipeline_user_func (user_func : user_func) =
    let new_name, name_result = pipeline_id user_func.name in
    let new_args, args_result = pipeline_map ~f:pipeline_expr user_func.args in
    let result = Mapping.collect_results [ name_result; args_result ] in
    Mapping.user_func { name = new_name; args = new_args } result


  let pipeline_write_template write_template =
    let new_file, file_result = pipeline_id write_template.file in
    let new_contents, contents_result = pipeline_expr write_template.contents in
    let result = Mapping.collect_results [ file_result; contents_result ] in
    Mapping.write_template { file = new_file; contents = new_contents } result


  let pipeline_func_call = function
    | User_func user_func ->
      single_pipeline
        ~sub_pipeline:pipeline_user_func
        ~mapping:Mapping.func_call
        ~new_ast_fun:(fun x -> User_func x)
        user_func
    | Print expr ->
      single_pipeline
        ~sub_pipeline:pipeline_expr
        ~mapping:Mapping.func_call
        ~new_ast_fun:(fun x -> Print x)
        expr
    | Input -> Mapping.func_call Input (Mapping.empty_result ())
    | Open expr ->
      single_pipeline
        ~sub_pipeline:pipeline_expr
        ~mapping:Mapping.func_call
        ~new_ast_fun:(fun x -> Open x)
        expr
    | Read expr ->
      single_pipeline
        ~sub_pipeline:pipeline_expr
        ~mapping:Mapping.func_call
        ~new_ast_fun:(fun x -> Read x)
        expr
    | Write write_template ->
      single_pipeline
        ~sub_pipeline:pipeline_write_template
        ~mapping:Mapping.func_call
        ~new_ast_fun:(fun x -> Write x)
        write_template
    | Append write_template ->
      single_pipeline
        ~sub_pipeline:pipeline_write_template
        ~mapping:Mapping.func_call
        ~new_ast_fun:(fun x -> Append x)
        write_template


  let pipeline_control = Mapping.control

  let pipeline_statement = function
    | Var var ->
      single_pipeline
        ~sub_pipeline:pipeline_var
        ~mapping:Mapping.statement
        ~new_ast_fun:(fun x -> Var x)
        var
    | Func_call func_call ->
      single_pipeline
        ~sub_pipeline:pipeline_func_call
        ~mapping:Mapping.statement
        ~new_ast_fun:(fun x -> Func_call x)
        func_call
    | Control control ->
      single_pipeline
        ~sub_pipeline:pipeline_control
        ~mapping:Mapping.statement
        ~new_ast_fun:(fun x -> Control x)
        control


  let rec pipeline_for_loop for_loop =
    let new_init, init_result = pipeline_var for_loop.init in
    let new_cond, cond_result = pipeline_expr for_loop.cond in
    let new_iter, iter_result = pipeline_var for_loop.iter in
    let new_contents, contents_result = pipeline_block for_loop.contents in
    let result =
      Mapping.collect_results
        [ init_result; cond_result; iter_result; contents_result ]
    in
    Mapping.for_loop
      { init = new_init
      ; cond = new_cond
      ; iter = new_iter
      ; contents = new_contents
      }
      result


  and pipeline_for_each for_each =
    let new_item, item_result = pipeline_id for_each.item in
    let new_iterator, iterator_result = pipeline_id for_each.iterator in
    let new_contents, contents_result = pipeline_block for_each.contents in
    let result =
      Mapping.collect_results [ item_result; iterator_result; contents_result ]
    in
    Mapping.for_each
      { item = new_item; iterator = new_iterator; contents = new_contents }
      result


  and pipeline_condition_template condition_template =
    let new_condition, condition_result =
      pipeline_expr condition_template.condition
    in
    let new_contents, contents_result =
      pipeline_block condition_template.contents
    in
    let result =
      Mapping.collect_results [ condition_result; contents_result ]
    in
    Mapping.condition_template
      { condition = new_condition; contents = new_contents }
      result


  and pipeline_if_record if_record =
    let new_if, if_result = pipeline_condition_template if_record._if in
    let new_else_if, else_if_result =
      pipeline_map ~f:pipeline_condition_template if_record.else_if
    in
    let new_else_contents, else_contents_result =
      match if_record.else_contents with
      | Some else_contents ->
        let else_contents, else_contents_result =
          pipeline_block else_contents
        in
        Some else_contents, else_contents_result
      | None -> None, Mapping.empty_result ()
    in
    let result =
      Mapping.collect_results
        [ if_result; else_if_result; else_contents_result ]
    in
    Mapping.if_record
      { _if = new_if; else_if = new_else_if; else_contents = new_else_contents }
      result


  and pipeline_structure = function
    | Block_struct block ->
      single_pipeline
        ~sub_pipeline:pipeline_block
        ~mapping:Mapping.structure
        ~new_ast_fun:(fun x -> Block_struct x)
        block
    | If if_record ->
      single_pipeline
        ~sub_pipeline:pipeline_if_record
        ~mapping:Mapping.structure
        ~new_ast_fun:(fun x -> If x)
        if_record
    | While condition_template ->
      single_pipeline
        ~sub_pipeline:pipeline_condition_template
        ~mapping:Mapping.structure
        ~new_ast_fun:(fun x -> While x)
        condition_template
    | For_loop for_loop ->
      single_pipeline
        ~sub_pipeline:pipeline_for_loop
        ~mapping:Mapping.structure
        ~new_ast_fun:(fun x -> For_loop x)
        for_loop
    | For_each for_each ->
      single_pipeline
        ~sub_pipeline:pipeline_for_each
        ~mapping:Mapping.structure
        ~new_ast_fun:(fun x -> For_each x)
        for_each


  and pipeline_command = function
    | Structure structure ->
      single_pipeline
        ~sub_pipeline:pipeline_structure
        ~mapping:Mapping.command
        ~new_ast_fun:(fun x -> Structure x)
        structure
    | Statement statement ->
      single_pipeline
        ~sub_pipeline:pipeline_statement
        ~mapping:Mapping.command
        ~new_ast_fun:(fun x -> Statement x)
        statement


  and pipeline_block block =
    let new_contents, contents_result =
      pipeline_map ~f:pipeline_command block.contents
    in
    Mapping.block new_contents block.annotations contents_result


  and pipeline_param (id, type_id) =
    let new_id, id_result = pipeline_id id in
    let new_type_id, type_id_result = pipeline_type_id type_id in
    let result = Mapping.collect_results [ id_result; type_id_result ] in
    Mapping.param (new_id, new_type_id) result


  and pipeline_func func =
    let new_name, name_result = pipeline_id func.name in
    let new_param, param_result = pipeline_map ~f:pipeline_param func.params in
    let new_body, body_result = pipeline_block func.body in
    let new_return_type, return_type_result =
      pipeline_type_id func.return_type
    in
    let result =
      Mapping.collect_results
        [ name_result; param_result; body_result; return_type_result ]
    in
    Mapping.func
      { name = new_name
      ; params = new_param
      ; body = new_body
      ; return_type = new_return_type
      }
      result


  let pipeline_program program =
    let new_package, package_result = pipeline_id program.package in
    let new_global_vars, global_vars_result =
      pipeline_map ~f:pipeline_var program.global_vars
    in
    let new_funcs, funcs_result = pipeline_map ~f:pipeline_func program.funcs in
    let result =
      Mapping.collect_results
        [ package_result; global_vars_result; funcs_result ]
    in
    Mapping.program new_package program.imports new_global_vars new_funcs result


  let pipeline_ast program =
    let new_program, _ = pipeline_program program in
    new_program
end
