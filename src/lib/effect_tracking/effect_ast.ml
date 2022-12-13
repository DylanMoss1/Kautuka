(* open! Core 
open Ast.Ast_types

let is_ignore = function 
| Default -> false 
| Ignore -> true 
| Force_par -> false 
| Force_seq -> false 

(* let get_block_effects block effect_list = 
  match block with 
  | Block(block_record) -> if is_ignore block_record.annotations.block_type then Effect_set.empty else Effect_set.union_of_list effect_list *)

let append_tuple (xs, ys) (x, y) = (x :: xs, y :: ys)

let unzip l = List.fold_left ~f:append_tuple ~init:([], []) l

let get_uuid_of_id { uuid ; _ } = uuid 


(* let effect_of_id = function
  | ID ({name; _}) -> name


let effect_ast_of_type_id = function
  | T_Int -> "int"
  | T_Bool -> "bool"
  | T_String -> "string"
  | T_Unit -> ""


let effect_ast_of_unop = function
  | Not -> "!"
  | U_Minus -> "-"

let effect_ast_of_binop = function
  | Plus -> "+"
  | B_Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | Eq -> "=="
  | Ne -> "!="
  | And -> "&&"
  | Or -> "||"


let effect_ast_of_value = function
  | Int i -> effect_ast_of_int i
  | Bool b -> effect_ast_of_bool b
  | String s -> Fmt.str "\"%s\"" s


let rec effect_ast_of_expr = function
  | Unop (unop, expr) ->
    Fmt.str "%s %s" (effect_ast_of_unop unop) (effect_ast_of_expr expr)
  | Binop (expr1, binop, expr2) ->
    Fmt.str
      "%s %s %s"
      (effect_ast_of_expr expr1)
      (effect_ast_of_binop binop)
      (effect_ast_of_expr expr2)
  | Paren expr -> Fmt.str "(%s)" (effect_ast_of_expr expr)
  | Value value -> Fmt.str "%s" (effect_ast_of_value value)
  | Var id -> Fmt.str "%s" (effect_ast_of_id id)


let effect_ast_of_var = function
  | VarNonInit (id, type_id) ->
    Fmt.str "var %s %s" (effect_ast_of_id id) (effect_ast_of_type_id type_id)
  | VarInit (id, type_id, expr) ->
    Fmt.str
      "var %s %s = %s"
      (effect_ast_of_id id)
      (effect_ast_of_type_id type_id)
      (effect_ast_of_expr expr)
  | VarDecl (id, expr) ->
    Fmt.str "%s := %s" (effect_ast_of_id id) (effect_ast_of_expr expr)
  | VarAssign (id, expr) ->
    Fmt.str "%s = %s" (effect_ast_of_id id) (effect_ast_of_expr expr)
  | Pre_inc id -> Fmt.str "++%s" (effect_ast_of_id id)
  | Pre_dec id -> Fmt.str "--%s" (effect_ast_of_id id)
  | Post_inc id -> Fmt.str "%s++" (effect_ast_of_id id)
  | Post_dec id -> Fmt.str "%s--" (effect_ast_of_id id)

*)

let effect_of_var = function 
| VarNonInit (id, type_id) -> 
| VarInit(id, type_id, expr) -> 
| VarDecl(id, expr) -> 
| VarAssign(id, expr) -> 
| Pre_inc id -> (Pre_inc id, Effect_set.create (create_effect (Var_mutation (get_uuid_of_id id)) Write)) 
| Pre_dec id -> (Pre_inc id, Effect_set.create (create_effect (Var_mutation (get_uuid_of_id id)) Write)) 
| Post_inc id -> (Pre_inc id, Effect_set.create (create_effect (Var_mutation (get_uuid_of_id id)) Write)) 
| Post_dec id -> (Pre_inc id, Effect_set.create (create_effect (Var_mutation (get_uuid_of_id id)) Write)) 

(* COMPLETE user_func *)
let effect_of_func_call = function 
| User_func(user_func) -> (User_func(user_func), Effect_set.empty)
| Print(expr) -> (Print(expr), Effect_set.create (create_effect Console_IO Write))
| Input -> (Input, Effect_set.create (create_effect Console_IO Write))
| Open(expr) -> (Open(expr), Effect_set.empty)
| Read(expr) -> (Read(expr), Effect_set.create (create_effect File_IO Read) )
| Write(write_template) -> (Write(write_template), Effect_set.create (create_effect File_IO Write))
| Append(write_template) -> (Append(write_template), Effect_set.create (create_effect File_IO Write))

let effect_of_statement = function 
| Var(var) -> effect_of_var var 
| Func_call(func_call) -> effect_of_func_call func_call 
| Control (control) -> (Control(control), Effect_ast.empty)

let rec effect_of_for_loop for_loop =  


and effect_of_for_each for_each = 

and effect_of_condition_template condition_template = 
  let effect_ast_cond, effect_cond = effect_of_expr condition_template.condition in 
  let effect_ast_contents, effect_contents = effect_of_block condition_template.contents in
  let effect_condition_template = Effect_set.union_of_list [effect_cond; effect_contents] in 
  in ( { condition = effect_ast_cond ; contents = effect_ast_contents }, effect_condition_template )

and effect_of_if_record if_record =

  let effect_ast_if, effect_if = effect_of_condition_template if_record._if in 
  let effect_ast_else_if, effect_else_if_list = effect_of_condition_template if_record.else_if in 
  let effect_ast_else, effect_else = ( 
  match if_record.else_contents with 
    | Some(else_contents) -> effect_of_block else_contents
    | None -> (None, Effect_set.empty)  
  ) in 
  
  let effect_else_if = Effect_set.union_of_list effect_else_if_list in 
  let effect_if_record = Effect_set.union_of_list [effect_if; effect_else_if; effect_else] in 

  ({
    _if = effect_ast_if; 
    else_if = effect_ast_else_if; 
    else_contents = effect_ast_else;
  }, effect_if_record)


and effect_of_structure = function
  | Block_struct block -> effect_of_block block
  | If if_record -> effect_of_if_record if_record
  | While while_loop -> effect_of_condition_template while_loop
  | For_loop for_loop -> effect_of_for_loop for_loop
  | For_each for_each -> effect_of_for_each for_each


and effect_of_command = function
  | Structure structure -> effect_of_structure structure
  | Statement statement -> effect_of_statement statement


and effects_of_block_record block_record = (
  let effect_ast_contents, effect_contents_list = unzip (List.map ~f:effects_of_command block_record.contents) in 
  let effect_contents = if is_ignore block_record.annotations.block_type then Effect_set.empty else Effect_set.union_of_list effect_list in 
  let effect_annotations = { block_record.annotations with effect_set = effect_contents }

  ({ contents = effect_ast_contents ; annotations = effect_annotations }, effect_contents)
)

and effect_of_block = function
  | Block block_record -> let effect_block_record = effects_of_block_record block_record in
    (effect_block_record, effect_block_record.annotations.effect_set)

and effect_ast_of_func func =
  {
    func with body = (List.map ~f:effect_ast_of_block func.block)
  }

let effect_ast_of_program program =
  {
    program with funcs = (List.map ~f:effect_ast_of_func program.funcs)
  } *)
