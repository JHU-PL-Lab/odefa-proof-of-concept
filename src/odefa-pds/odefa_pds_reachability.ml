(**
  Contains the interface for a reachability analysis for a PDS.
*)

open Batteries;;

open Odefa_pds;;

module type Pds_reachability = functor (P : Pds) ->
sig
  type analysis
  
  val analyze_pds : P.pds -> analysis
  val reachable_from : analysis -> P.state -> P.symbol -> P.state Enum.t
  
  val analyze_rpds : P.rpds -> P.state Enum.t 
end;;
