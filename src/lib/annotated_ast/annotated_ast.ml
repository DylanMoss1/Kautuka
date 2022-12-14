open! Core
open Ast.Ast_types
open Ast.Ast_to_string

type effect_set = string
type cost = int
type parallelise = bool

module Func_init_annotations = struct
  type t = { block_type : block_type }

  let string_of_annotations t = string_of_block_type t.block_type
  let create block_type = { block_type }
end

module Func_effect_annotations = struct
  type t =
    { block_type : block_type
    ; effect_set : effect_set
    }

  let string_of_annotations t = string_of_block_type t.block_type ^ t.effect_set

  let create init_annotations effect_set =
    { block_type = init_annotations.block_type; effect_set }
end

module Func_cost_annotations = struct
  type t =
    { block_type : block_type
    ; effect_set : effect_set
    ; cost : cost
    }

  let string_of_annotations t = string_of_block_type t.block_type ^ t.effect_set

  let create effect_annotations cost =
    { block_type = effect_annotations.block_type
    ; effect_set = effect_annotations.effect_set
    ; cost
    }
end

module Import_empty_annotations = struct
  type t = unit

  let string_of_annotations _ = "[No annotations]"
  let create = ()
end

module Import_full_annotations = struct
  type t = string list

  let string_of_annotations t = String.concat ~sep:", " t
  let create x = x
end

module Init_ast = struct
  type t = (Func_init_annotations.t, Import_empty_annotations.t) program

  let string_of_ast = string_of_program
  let annotated_ast_of_ast ast = ast
end

module Import_ast = struct
  type t = (Func_init_annotations.t, Import_full_annotations.t) program

  let string_of_ast = string_of_program
  let annotated_ast_of_ast ast = ast
end

(* module Effect_ast = struct
  type t = Effect_annotations.t program

  let string_of_ast = string_of_program
end

module Cost_ast = struct
  type t = Cost_annotations.t program

  let string_of_ast = string_of_program
end *)
