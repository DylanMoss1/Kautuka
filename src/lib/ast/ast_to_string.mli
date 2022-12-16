val map_concat : sep:string -> f:('a -> string) -> 'a list -> string
val string_of_id : Ast_types.id -> string
val string_of_type_id : Ast_types.type_id -> string
val string_of_unop : Ast_types.unop -> string
val string_of_binop : Ast_types.binop -> string
val string_of_value : Ast_types.value -> string
val string_of_expr : Ast_types.expr -> string
val string_of_var : Ast_types.var -> string
val string_of_user_func : Ast_types.user_func -> string
val string_of_write_template : Ast_types.write_template -> string
val string_of_func_call : Ast_types.func_call -> string
val string_of_control : Ast_types.control -> string
val string_of_statement : Ast_types.statement -> string

val string_of_for_loop
  :  string_of_block_annotation:('a -> string)
  -> 'a Ast_types.for_loop
  -> string

val string_of_for_each
  :  string_of_block_annotation:('a -> string)
  -> 'a Ast_types.for_each
  -> string

val string_of_condition_template
  :  string_of_block_annotation:('a -> string)
  -> 'a Ast_types.condition_template
  -> string

val string_of_while
  :  string_of_block_annotation:('a -> string)
  -> 'a Ast_types.condition_template
  -> string

val string_of_if_record
  :  string_of_block_annotation:('a -> string)
  -> 'a Ast_types.if_record
  -> string

val string_of_structure
  :  string_of_block_annotation:('a -> string)
  -> 'a Ast_types.structure
  -> string

val string_of_command
  :  string_of_block_annotation:('a -> string)
  -> 'a Ast_types.command
  -> string

val string_of_block
  :  string_of_block_annotation:('a -> string)
  -> 'a Ast_types.block
  -> string

val string_of_param : Ast_types.param -> string

val string_of_func
  :  string_of_block_annotation:('a -> string)
  -> 'a Ast_types.func
  -> string

val string_of_program
  :  string_of_block_annotation:('a -> string)
  -> string_of_import_annotation:('b -> string)
  -> ('a, 'b) Ast_types.program
  -> string
