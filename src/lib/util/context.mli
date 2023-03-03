open Item

module type Context = sig
  type key
  type value
  type t

  val empty : t
  val add_new_item : key -> value -> t -> t
  val add_new_scope : is_func:bool -> t -> t
  val remove_scope : is_func:bool -> t -> t
  val get_value : key -> t -> value option
  val get_value_outside_scope : key -> t -> value option
  val apply_unary_fun : f:(value -> value) -> t -> t
  val apply_bin_fun : f:(value -> value -> value) -> t -> t -> t
  val get_all_values : t -> value list
  val string_of_t : t -> string

  val get_post_loop_context
    :  start_env:t
    -> end_env:t
    -> sum:(value -> value -> value)
    -> subtract:(value -> value -> value)
    -> multiply:(value -> 'a -> value)
    -> iterations:'a
    -> t
end

module type Bool_value = sig
  val t : bool
end

module Make_context (Key : Item) (Value : Item) (Is_func_context : Bool_value) :
  Context with type key = Key.t and type value = Value.t
