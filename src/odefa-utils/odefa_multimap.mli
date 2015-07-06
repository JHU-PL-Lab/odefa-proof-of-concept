open Batteries;;

module Make(Key_ord : BatInterfaces.OrderedType)
           (Value_ord : BatInterfaces.OrderedType) :
sig
  module M : Map.S with type key = Key_ord.t
  module S : Set.S with type elt = Value_ord.t
  
  type t
  type key = Key_ord.t
  type value = Value_ord.t
  
  val empty : t

  val is_empty : t -> bool
  
  val num_keys : t -> int
  
  val num_values : t -> int
  
  val add : key -> value -> t -> t
  
  val find : key -> t -> value Enum.t
  
  val remove : key -> value -> t -> t
  
  val remove_all : key -> t -> t
  
  val mem : key -> value -> t -> bool

  val mem_any : key -> t -> bool

  val singleton : key -> value -> t

  val keys : t -> key Enum.t

  val enum : t -> (key * value) Enum.t

  val of_enum : (key * value) Enum.t -> t
end;;
