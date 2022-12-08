open! Core
open Pprint_ast
open Ast

type effect_set = string
type cost = int
type parallelise = bool

module Init_annotations = struct
  type t = { block_type : block_type }

  let string_of_annotations t = string_of_block_type t.block_type
  let create block_type = { block_type }
end

module Effect_annotations = struct
  type t =
    { block_type : block_type
    ; effect_set : effect_set
    }

  let string_of_annotations t = string_of_block_type t.block_type ^ t.effect_set

  let create init_annotations effect_set =
    { block_type = init_annotations.block_type; effect_set }
end

module Cost_annotations = struct
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

module Init_ast = struct
  type t = Init_annotations.t program

  let string_of_ast = string_of_program
  let annotated_ast_of_ast ast = ast
end

module Effect_ast = struct
  type t = Effect_annotations.t program

  let string_of_ast = string_of_program
end

module Cost_ast = struct
  type t = Cost_annotations.t program

  let string_of_ast = string_of_program
end
