module type Annotation = sig
  type t

  val string_of_t : t -> string
end

module type Annotated_ast = sig
  type block_annot
  type var_annot
  type import_annot
  type expr_annot
  type t

  val string_of_t : t -> string

  val create
    :  (block_annot, var_annot, import_annot, expr_annot) Ast_types.program
    -> t
end

module Make_annotated_ast : functor
  (Block_annotation : Annotation)
  (Var_annotation : Annotation)
  (Import_annotation : Annotation)
  (Expr_annotation : Annotation)
  -> sig
  type block_annot = Block_annotation.t
  type var_annot = Var_annotation.t
  type import_annot = Import_annotation.t
  type expr_annot = Expr_annotation.t
  type t = (block_annot, var_annot, import_annot, expr_annot) Ast_types.program

  val string_of_t
    :  ( Block_annotation.t
       , Var_annotation.t
       , Import_annotation.t
       , 'a )
       Ast_types.program
    -> string

  val create
    :  (block_annot, var_annot, import_annot, expr_annot) Ast_types.program
    -> t

  val create_block_annot : Block_annotation.t -> block_annot
  val create_var_annot : Var_annotation.t -> var_annot
  val create_import_annot : Import_annotation.t -> import_annot
  val create_expr_annot : Expr_annotation.t -> expr_annot
  val string_of_block_annot : Block_annotation.t -> string
  val string_of_var_annot : Var_annotation.t -> string
  val string_of_import_annot : Import_annotation.t -> string
  val string_of_expr_annot : Expr_annotation.t -> string
end
