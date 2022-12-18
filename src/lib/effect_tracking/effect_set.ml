(* open Core

(* open Ast.Ast_to_string  *)
open Util

type effect_value =
  | Console_IO
  | File_IO
  | Var_mutation of Uuid.t
[@@deriving of_sexp, sexp_of, compare]

let string_of_effect_value = function
  | Console_IO -> "ConsoleIO"
  | File_IO -> "FileIO"
  | Var_mutation uuid -> Fmt.str "VarMutation(%s)" (Uuid.string_of_t uuid)


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

let create_effect effect_value effect_type = { effect_value; effect_type }

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

  let union_of_list l = List.fold_left ~f:union ~init:empty l

  let to_string set =
    Fmt.str
      "{%s}"
      (String.concat
         ~sep:", "
         (List.sort
            ~compare:String.compare
            (List.map ~f:string_of_effect (elements set))))


  let create effect = add empty effect
end *)
