open! Core
open Item

module type Context = sig
  type key
  type value
  type t

  val empty : t
  val add_new_item : key -> value -> t -> t
  val add_new_scope : t -> t
  val remove_scope : t -> t
  val get_value : key -> t -> value option
  val get_value_outside_scope : key -> t -> value option
  val string_of_t : t -> string
end

module Make_context (Key : Item) (Value : Item) = struct
  type key = Key.t
  type value = Value.t
  type t = (key * value) list list

  exception Empty_scope
  exception Key_in_inner_scope

  let empty = [ [] ]

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


  let string_of_key_value_pair (key, value) =
    Fmt.str "(%s,%s)" (Key.string_of_t key) (Value.string_of_t value)


  let string_of_scope scope =
    Fmt.str "[%s]" (String.concat (List.map ~f:string_of_key_value_pair scope))


  let string_of_t t =
    Fmt.str "[%s]" (String.concat (List.map ~f:string_of_scope t))
end
