(**
  An interface describing a monoid for a MISA.
*)

open Batteries;;

module type Monoid =
sig
  type t
  
  val zero : t
  val plus : t -> t -> t
end;;
