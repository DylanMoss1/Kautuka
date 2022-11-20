open Core

type 'a env = 'a list list

let add_new_var var = function
  | x :: xs -> (var :: x) :: xs
  | [] -> [ [ var ] ]


let add_new_scope env = [] :: env

let remove_scope = function
  | _ :: xs -> xs
  | [] -> []


let is_var_in_scope ~f var = function
  | x :: _ -> List.mem x var ~equal:f
  | [] -> false


let string_of_scope ~f scope =
  Fmt.str "[%s]" (String.concat (List.map ~f scope))


let string_of_env ~f env =
  Fmt.str "[%s]" (String.concat (List.map ~f:(string_of_scope ~f) env))
