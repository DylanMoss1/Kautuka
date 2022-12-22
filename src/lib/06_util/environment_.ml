open! Core
open Item

module Environment_ (Key : Type_item) (Value : Type_item) = struct
  type t = (Key.t * Value.t) list list

  exception Empty_scope
  exception Key_in_inner_scope

  let empty () = [ [] ]

  let add_new_item key value t =
    match t with
    | ts :: tss -> ((key, value) :: ts) :: tss
    | [] -> raise Empty_scope


  let add_new_scope t = [] :: t

  let remove_scope = function
    | _ :: xs -> xs
    | [] -> raise Empty_scope


  let rec get_value_in_scope key = function
    | (k, v) :: ts ->
      if Key.compare k key = 0 then Some v else get_value_in_scope key ts
    | [] -> None


  let rec get_value key = function
    | ts :: tss ->
      (match get_value_in_scope key ts with
      | None -> get_value key tss
      | Some value -> Some value)
    | [] -> None


  let get_value_outside_scope key = function
    | ts :: tss ->
      (match get_value_in_scope key ts with
      | Some _ -> raise Key_in_inner_scope
      | None -> get_value key tss)
    | [] -> raise Empty_scope


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
end
