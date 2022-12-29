open! Core
open Ast_types
open Annotated_ast
open Util.Environment
open Util.Result

module type Type_ast_mapping = sig
  type env
  type env_key
  type env_value
  type env_scope_tag
  type result
  type old_block_annot
  type old_var_annot
  type old_import_annot
  type new_block_annot
  type new_var_annot
  type new_import_annot

  type var_effect =
    | Init
    | Read
    | Write

  val empty_env : env
  val add_to_env : env_key -> env_value -> env -> env
  val add_new_scope : env -> env_scope_tag -> env
  val remove_scope : env -> env * env_scope_tag 
  val get_value : env_key -> env -> env_value option
  val get_value_outside_scope : env_key -> env -> env_value option
  val empty_scope_tag : env_scope_tag
  val empty_result : result
  val join_results : result -> result -> result
  val join_results_list : result list -> result
  val type_id : env -> type_id -> env * type_id * result
  val value : env -> value -> env * value * result

  val var
    :  env
    -> old_var_annot
    -> var_effect:var_effect
    -> env * new_var_annot var * result

  val unop : env -> unop -> env * unop * result
  val binop : env -> binop -> env * binop * result

  val expr
    :  env
    -> new_var_annot expr
    -> result
    -> env * new_var_annot expr * result

  val var_statement
    :  env
    -> new_var_annot var_statement
    -> result
    -> env * new_var_annot var_statement * result

  val user_func
    :  env
    -> new_var_annot user_func
    -> result
    -> env * new_var_annot user_func * result

  val write_template
    :  env
    -> new_var_annot write_template
    -> result
    -> env * new_var_annot write_template * result

  val func_call
    :  env
    -> new_var_annot func_call
    -> result
    -> env * new_var_annot func_call * result

  val control : env -> control -> env * control * result

  val statement
    :  env
    -> new_var_annot statement
    -> result
    -> env * new_var_annot statement * result

  val for_loop
    :  env
    -> (new_block_annot, new_var_annot) for_loop
    -> result
    -> env * (new_block_annot, new_var_annot) for_loop * result

  val for_each
    :  env
    -> (new_block_annot, new_var_annot) for_each
    -> result
    -> env * (new_block_annot, new_var_annot) for_each * result

  val condition_template
    :  env
    -> (new_block_annot, new_var_annot) condition_template
    -> result
    -> env * (new_block_annot, new_var_annot) condition_template * result

  val if_record
    :  env
    -> (new_block_annot, new_var_annot) if_record
    -> result
    -> env * (new_block_annot, new_var_annot) if_record * result

  val structure
    :  env
    -> (new_block_annot, new_var_annot) structure
    -> result
    -> env * (new_block_annot, new_var_annot) structure * result

  val command
    :  env
    -> (new_block_annot, new_var_annot) command
    -> result
    -> env * (new_block_annot, new_var_annot) command * result

  val block
    :  env:env
    -> new_contents:(new_block_annot, new_var_annot) command list
    -> old_annotations:old_block_annot
    -> result:result
    -> scope_tag:env_scope_tag
    -> env * (new_block_annot, new_var_annot) block * result

  val param
    :  env
    -> new_var_annot var * type_id
    -> result
    -> env * (new_var_annot var * type_id) * result

  val func
    :  env
    -> (new_block_annot, new_var_annot) func
    -> result
    -> env * (new_block_annot, new_var_annot) func * result

  val program
    :  env:env
    -> new_package:string
    -> old_import:old_import_annot
    -> new_global_vars:new_var_annot var_statement list
    -> new_funcs:(new_block_annot, new_var_annot) func list
    -> result:result
    -> env * (new_block_annot, new_var_annot, new_import_annot) program * result
end

module Empty_environment = struct
  type t = unit
  type key = unit
  type value = unit
  type scope_tag = unit 

  let empty = ()
  let add_new_item () () () = ()
  let add_new_scope () () = ()
  let remove_scope () = (), ()
  let get_value () () = None
  let get_value_outside_scope () () = None
  let empty_scope_tag = ()
end

module Empty_result = struct
  type t = unit

  let empty = ()
  let join _ _ = ()
end

module Default_ast_mapping
    (Old_ast : Type_annotated_ast)
    (New_ast : Type_annotated_ast)
    (Env : Type_environment)
    (Result : Type_result) =
struct
  type env = Env.t
  type env_key = Env.key
  type env_value = Env.value
  type env_scope_tag = Env.scope_tag 
  type result = Result.t
  type old_block_annot = Old_ast.block_annot
  type old_var_annot = Old_ast.var_annot
  type old_import_annot = Old_ast.import_annot
  type new_block_annot = New_ast.block_annot
  type new_var_annot = New_ast.var_annot
  type new_import_annot = New_ast.import_annot

  let empty_env = Env.empty
  let add_to_env = Env.add_new_item
  let add_new_scope = Env.add_new_scope
  let remove_scope = Env.remove_scope
  let get_value = Env.get_value
  let get_value_outside_scope = Env.get_value_outside_scope
  let empty_scope_tag = Env.empty_scope_tag 
  let empty_result = Result.empty
  let join_results = Result.join
  let join_results_list = List.fold_left ~init:empty_result ~f:join_results

  type var_effect =
    | Init
    | Read
    | Write

  let ignore_leaf env ast = env, ast, empty_result
  let ignore_branch env ast result = env, ast, result
  let var env old_var_annot ~var_effect:_ = ignore_leaf env old_var_annot

  (* let existing_var = ignore_leaf  *)
  let type_id = ignore_leaf
  let value = ignore_leaf
  let unop = ignore_leaf
  let binop = ignore_leaf
  let expr = ignore_branch
  let var_statement = ignore_branch
  let user_func = ignore_branch
  let write_template = ignore_branch
  let func_call = ignore_branch
  let control = ignore_leaf
  let if_record = ignore_branch
  let condition_template = ignore_branch
  let for_each = ignore_branch
  let for_loop = ignore_branch
  let statement = ignore_branch
  let structure = ignore_branch
  let command = ignore_branch

  let block ~env ~new_contents ~old_annotations ~result =
    env, { contents = new_contents; annotations = old_annotations }, result


  let param = ignore_branch
  let func = ignore_branch

  let program ~env ~new_package ~old_import ~new_global_vars ~new_funcs ~result =
    ( env
    , { package = new_package
      ; imports = old_import
      ; global_vars = new_global_vars
      ; funcs = new_funcs
      }
    , result )
end

module Ast_pipeline (Mapping : Type_ast_mapping) = struct
  let append_tuple (xs, ys) (x, y) = x :: xs, y :: ys
  let unzip l = List.fold_left ~f:append_tuple ~init:([], []) l

  let single_pipeline ~env ~sub_pipeline ~mapping ~new_ast_fun ast =
    let new_env, new_ast, ast_result = sub_pipeline env ast in
    mapping new_env (new_ast_fun new_ast) ast_result


  let rec pipeline_map ~f ~env acc = function
    | ast :: ast_list ->
      let new_env, new_ast, result = f env ast in
      pipeline_map ~f ~env:new_env ((new_ast, result) :: acc) ast_list
    | [] -> env, acc


  let pipeline_map_collect ~f ~env ast =
    let env, pipeline_map_results = pipeline_map ~f ~env [] ast in
    let new_ast, ast_result_list = unzip pipeline_map_results in
    let ast_result = Mapping.join_results_list ast_result_list in
    env, new_ast, ast_result


  let pipeline_type_id = Mapping.type_id
  let pipeline_value = Mapping.value
  let pipeline_var = Mapping.var
  let pipeline_unop = Mapping.unop
  let pipeline_binop = Mapping.binop

  let rec pipeline_user_func env (user_func : 'var user_func) =
    let env, new_name, name_result =
      pipeline_var ~var_effect:Read env user_func.name
    in
    let env, new_args, args_result =
      pipeline_map_collect ~env ~f:pipeline_expr user_func.args
    in
    let result = Mapping.join_results_list [ name_result; args_result ] in
    Mapping.user_func env { name = new_name; args = new_args } result


  and pipeline_write_template env write_template =
    let env, new_file, file_result =
      pipeline_var ~var_effect:Write env write_template.file
    in
    let env, new_contents, contents_result =
      pipeline_expr env write_template.contents
    in
    let result = Mapping.join_results_list [ file_result; contents_result ] in
    Mapping.write_template
      env
      { file = new_file; contents = new_contents }
      result


  and pipeline_func_call env = function
    | User_func user_func ->
      single_pipeline
        ~env
        ~sub_pipeline:pipeline_user_func
        ~mapping:Mapping.func_call
        ~new_ast_fun:(fun x -> User_func x)
        user_func
    | Print expr ->
      single_pipeline
        ~env
        ~sub_pipeline:pipeline_expr
        ~mapping:Mapping.func_call
        ~new_ast_fun:(fun x -> Print x)
        expr
    | Input -> Mapping.func_call env Input Mapping.empty_result
    | Open expr ->
      single_pipeline
        ~env
        ~sub_pipeline:pipeline_expr
        ~mapping:Mapping.func_call
        ~new_ast_fun:(fun x -> Open x)
        expr
    | Read var ->
      single_pipeline
        ~env
        ~sub_pipeline:(pipeline_var ~var_effect:Read)
        ~mapping:Mapping.func_call
        ~new_ast_fun:(fun x -> Read x)
        var
    | Write write_template ->
      single_pipeline
        ~env
        ~sub_pipeline:pipeline_write_template
        ~mapping:Mapping.func_call
        ~new_ast_fun:(fun x -> Write x)
        write_template
    | Append write_template ->
      single_pipeline
        ~env
        ~sub_pipeline:pipeline_write_template
        ~mapping:Mapping.func_call
        ~new_ast_fun:(fun x -> Append x)
        write_template


  and pipeline_expr env = function
    | Unop (unop, expr) ->
      let env, new_unop, unop_result = pipeline_unop env unop in
      let env, new_expr, expr_result = pipeline_expr env expr in
      let result = Mapping.join_results_list [ unop_result; expr_result ] in
      Mapping.expr env (Unop (new_unop, new_expr)) result
    | Binop (expr1, binop, expr2) ->
      let env, new_expr1, expr1_result = pipeline_expr env expr1 in
      let env, new_binop, binop_result = pipeline_binop env binop in
      let env, new_expr2, expr2_result = pipeline_expr env expr2 in
      let result =
        Mapping.join_results_list [ expr1_result; binop_result; expr2_result ]
      in
      Mapping.expr env (Binop (new_expr1, new_binop, new_expr2)) result
    | Paren expr ->
      single_pipeline
        ~env
        ~sub_pipeline:pipeline_expr
        ~mapping:Mapping.expr
        ~new_ast_fun:(fun x -> Paren x)
        expr
    | Value value ->
      single_pipeline
        ~env
        ~sub_pipeline:pipeline_value
        ~mapping:Mapping.expr
        ~new_ast_fun:(fun x -> Value x)
        value
    | VarRead var ->
      single_pipeline
        ~env
        ~sub_pipeline:(pipeline_var ~var_effect:Read)
        ~mapping:Mapping.expr
        ~new_ast_fun:(fun x -> VarRead x)
        var
    | Func_call func_call ->
      single_pipeline
        ~env
        ~sub_pipeline:pipeline_func_call
        ~mapping:Mapping.expr
        ~new_ast_fun:(fun x -> Func_call x)
        func_call


  let pipeline_var_statement env = function
    | VarNonInit (var, type_id) ->
      let env, new_var, var_result = pipeline_var ~var_effect:Init env var in
      let env, new_type_id, type_id_result = pipeline_type_id env type_id in
      let result = Mapping.join_results_list [ var_result; type_id_result ] in
      Mapping.var_statement env (VarNonInit (new_var, new_type_id)) result
    | VarInit (var, type_id, expr) ->
      let env, new_var, var_result = pipeline_var ~var_effect:Init env var in
      let env, new_type_id, type_id_result = pipeline_type_id env type_id in
      let env, new_expr, expr_result = pipeline_expr env expr in
      let result =
        Mapping.join_results_list [ var_result; type_id_result; expr_result ]
      in
      Mapping.var_statement
        env
        (VarInit (new_var, new_type_id, new_expr))
        result
    | VarDecl (var, expr) ->
      let env, new_var, var_result = pipeline_var ~var_effect:Init env var in
      let env, new_expr, expr_result = pipeline_expr env expr in
      let result = Mapping.join_results_list [ var_result; expr_result ] in
      Mapping.var_statement env (VarDecl (new_var, new_expr)) result
    | VarAssign (var, expr) ->
      let env, new_var, var_result = pipeline_var ~var_effect:Write env var in
      let env, new_expr, expr_result = pipeline_expr env expr in
      let result = Mapping.join_results_list [ var_result; expr_result ] in
      Mapping.var_statement env (VarAssign (new_var, new_expr)) result
    | Pre_inc var ->
      single_pipeline
        ~env
        ~sub_pipeline:(pipeline_var ~var_effect:Write)
        ~mapping:Mapping.var_statement
        ~new_ast_fun:(fun x -> Pre_inc x)
        var
    | Pre_dec var ->
      single_pipeline
        ~env
        ~sub_pipeline:(pipeline_var ~var_effect:Write)
        ~mapping:Mapping.var_statement
        ~new_ast_fun:(fun x -> Pre_dec x)
        var
    | Post_inc var ->
      single_pipeline
        ~env
        ~sub_pipeline:(pipeline_var ~var_effect:Write)
        ~mapping:Mapping.var_statement
        ~new_ast_fun:(fun x -> Post_inc x)
        var
    | Post_dec var ->
      single_pipeline
        ~env
        ~sub_pipeline:(pipeline_var ~var_effect:Write)
        ~mapping:Mapping.var_statement
        ~new_ast_fun:(fun x -> Post_dec x)
        var


  let pipeline_control = Mapping.control

  let pipeline_statement env = function
    | Var_statement var_statement ->
      single_pipeline
        ~env
        ~sub_pipeline:pipeline_var_statement
        ~mapping:Mapping.statement
        ~new_ast_fun:(fun x -> Var_statement x)
        var_statement
    | Control control ->
      single_pipeline
        ~env
        ~sub_pipeline:pipeline_control
        ~mapping:Mapping.statement
        ~new_ast_fun:(fun x -> Control x)
        control
    | Expr expr ->
      single_pipeline
        ~env
        ~sub_pipeline:pipeline_expr
        ~mapping:Mapping.statement
        ~new_ast_fun:(fun x -> Expr x)
        expr


  let rec pipeline_for_loop env for_loop =
    let env, new_init, init_result = pipeline_var_statement env for_loop.init in
    let env, new_cond, cond_result = pipeline_expr env for_loop.cond in
    let env, new_iter, iter_result = pipeline_var_statement env for_loop.iter in
    let env, new_contents, contents_result =
      pipeline_block env for_loop.contents Mapping.empty_scope_tag
    in
    let result =
      Mapping.join_results_list
        [ init_result; cond_result; iter_result; contents_result ]
    in
    Mapping.for_loop
      env
      { init = new_init
      ; cond = new_cond
      ; iter = new_iter
      ; contents = new_contents
      }
      result


  and pipeline_for_each env for_each =
    let env, new_item, item_result =
      pipeline_var ~var_effect:Init env for_each.item
    in
    let env, new_iterator, iterator_result =
      pipeline_var ~var_effect:Read env for_each.iterator
    in
    let env, new_contents, contents_result =
      pipeline_block env for_each.contents Mapping.empty_scope_tag
    in
    let result =
      Mapping.join_results_list
        [ item_result; iterator_result; contents_result ]
    in
    Mapping.for_each
      env
      { item = new_item; iterator = new_iterator; contents = new_contents }
      result


  and pipeline_condition_template env condition_template =
    let env, new_condition, condition_result =
      pipeline_expr env condition_template.condition
    in
    let env, new_contents, contents_result =
      pipeline_block env condition_template.contents Mapping.empty_scope_tag
    in
    let result =
      Mapping.join_results_list [ condition_result; contents_result ]
    in
    Mapping.condition_template
      env
      { condition = new_condition; contents = new_contents }
      result


  and pipeline_if_record env if_record =
    let env, new_if, if_result =
      pipeline_condition_template env if_record._if
    in
    let env, new_else_if, else_if_result =
      pipeline_map_collect ~env ~f:pipeline_condition_template if_record.else_if
    in
    let env, new_else_contents, else_contents_result =
      match if_record.else_contents with
      | Some else_contents ->
        let env, else_contents, else_contents_result =
          pipeline_block env else_contents Mapping.empty_scope_tag
        in
        env, Some else_contents, else_contents_result
      | None -> env, None, Mapping.empty_result
    in
    let result =
      Mapping.join_results_list
        [ if_result; else_if_result; else_contents_result ]
    in
    Mapping.if_record
      env
      { _if = new_if; else_if = new_else_if; else_contents = new_else_contents }
      result


  and pipeline_structure env = function
    | Block_struct block ->
      single_pipeline
        ~env
        ~sub_pipeline:pipeline_block
        ~mapping:Mapping.structure
        ~new_ast_fun:(fun x -> Block_struct x)
        block
    | If if_record ->
      single_pipeline
        ~env
        ~sub_pipeline:pipeline_if_record
        ~mapping:Mapping.structure
        ~new_ast_fun:(fun x -> If x)
        if_record
    | While condition_template ->
      single_pipeline
        ~env
        ~sub_pipeline:pipeline_condition_template
        ~mapping:Mapping.structure
        ~new_ast_fun:(fun x -> While x)
        condition_template
    | For_loop for_loop ->
      single_pipeline
        ~env
        ~sub_pipeline:pipeline_for_loop
        ~mapping:Mapping.structure
        ~new_ast_fun:(fun x -> For_loop x)
        for_loop
    | For_each for_each ->
      single_pipeline
        ~env
        ~sub_pipeline:pipeline_for_each
        ~mapping:Mapping.structure
        ~new_ast_fun:(fun x -> For_each x)
        for_each


  and pipeline_command env = function
    | Structure structure ->
      single_pipeline
        ~env
        ~sub_pipeline:pipeline_structure
        ~mapping:Mapping.command
        ~new_ast_fun:(fun x -> Structure x)
        structure
    | Statement statement ->
      single_pipeline
        ~env
        ~sub_pipeline:pipeline_statement
        ~mapping:Mapping.command
        ~new_ast_fun:(fun x -> Statement x)
        statement


  and pipeline_block env block ~scope_tag =
    let env = Mapping.add_new_scope env scope_tag in
    let env, new_contents, contents_result =
      pipeline_map_collect ~env ~f:pipeline_command block.contents
    in
    let env, scope_tag = Mapping.remove_scope env in
    Mapping.block
      ~env
      ~new_contents
      ~old_annotations:block.annotations
      ~result:contents_result
      ~scope_tag 


  and pipeline_param env (var, type_id) =
    let env, new_var, var_result = pipeline_var ~var_effect:Init env var in
    let env, new_type_id, type_id_result = pipeline_type_id env type_id in
    let result = Mapping.join_results_list [ var_result; type_id_result ] in
    Mapping.param env (new_var, new_type_id) result


  and pipeline_func env func =
    let env, new_name, name_result =
      pipeline_var ~var_effect:Init env func.name
    in
    let env, new_param, param_result =
      pipeline_map_collect ~env ~f:pipeline_param func.params
    in
    let env, new_body, body_result = pipeline_block env func.body ~scope_tag:Mapping.empty_scope_tag in
    let env, new_return_type, return_type_result =
      pipeline_type_id env func.return_type
    in
    let result =
      Mapping.join_results_list
        [ name_result; param_result; body_result; return_type_result ]
    in
    Mapping.func
      env
      { name = new_name
      ; params = new_param
      ; body = new_body
      ; return_type = new_return_type
      }
      result


  let pipeline_program env program =
    let env, new_global_vars, global_vars_result =
      pipeline_map_collect ~env ~f:pipeline_var_statement program.global_vars
    in
    let env, new_funcs, funcs_result =
      pipeline_map_collect ~env ~f:pipeline_func program.funcs
    in
    let result =
      Mapping.join_results_list [ global_vars_result; funcs_result ]
    in
    Mapping.program
      ~env
      ~new_package:program.package
      ~old_import:program.imports
      ~new_global_vars
      ~new_funcs
      ~result


  let pipeline_ast program =
    let _, new_program, _ = pipeline_program Mapping.empty_env program in
    new_program
end
