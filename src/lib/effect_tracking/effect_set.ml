(* open Core
open Ast
open Ast.Pprint_ast

type effect_value =
  | Console_IO
  | File_IO
  | Var_mutation of var
[@@deriving of_sexp, sexp_of, compare]

let string_of_effect_value = function
  | Console_IO -> "ConsoleIO"
  | File_IO -> "FileIO"
  | Var_mutation var -> Fmt.str "VarMutation(%s)" (string_of_var var)


type effect_type =
  | Read
  | Write
[@@deriving of_sexp, sexp_of, compare]

let string_of_effect_type = function
  | Read -> "READ"
  | Write -> "WRITE"


type effect =
  { effect_value : effect_value
  ; effect_type : effect_type
  }
[@@deriving of_sexp, sexp_of, compare]

let string_of_effect effect =
  Fmt.str
    "%s %s"
    (string_of_effect_value effect.effect_value)
    (string_of_effect_type effect.effect_type)


module Effect = struct
  module T = struct
    type t = effect [@@deriving of_sexp, sexp_of, compare]
  end

  include T
  include Comparable.Make (T)
end

module Effect_set = struct
  include Set.Make (Effect)

  let to_string set =
    Fmt.str
      "{%s}"
      (String.concat ~sep:", " (List.map ~f:string_of_effect (elements set)))
end *)
