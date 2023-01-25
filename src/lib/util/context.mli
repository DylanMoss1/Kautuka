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

module Make_context (Key : Item) (Value : Item) :
  Context with type key = Key.t and type value = Value.t
