open! Core

exception EmptyScope

type ('a, 'b) t = ('a * 'b) list list

let empty = [ [] ]

let add_new_item key value t =
  match t with
  | ts :: tss -> ((key, value) :: ts) :: tss
  | [] -> raise EmptyScope


let add_new_scope t = [] :: t

let remove_scope = function
  | _ :: xs -> xs
  | [] -> raise EmptyScope


let rec assoc ~equal key xs =
  match xs with
  | [] -> None
  | (new_key, new_value) :: xs ->
    if equal key new_key then Some new_value else assoc ~equal key xs


let get_value_in_scope ~equal key = function
  | ts :: _ -> assoc ~equal key ts
  | [] -> raise EmptyScope


let string_of_key_value_pair ~string_of_key ~string_of_val (key, value) =
  Fmt.str "(%s,%s)" (string_of_key key) (string_of_val value)


let string_of_scope ~string_of_key ~string_of_val scope =
  Fmt.str
    "[%s]"
    (String.concat
       (List.map
          ~f:(string_of_key_value_pair ~string_of_key ~string_of_val)
          scope))


let string_of_t ~string_of_key ~string_of_val t =
  Fmt.str
    "[%s]"
    (String.concat
       (List.map ~f:(string_of_scope ~string_of_key ~string_of_val) t))
