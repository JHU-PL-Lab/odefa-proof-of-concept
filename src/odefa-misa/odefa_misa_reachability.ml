(**
  Contains the interface for a reachability analysis for a PDS.
*)

open Batteries;;

open Odefa_misa;;

module type Misa_reachability = functor (M : Misa) ->
sig
  type umisa_analysis
  
  val analyze_umisa : M.umisa -> umisa_analysis
  val reachable_from : umisa_analysis -> M.state -> M.symbol -> M.state Enum.t
  val valid_paths :
    umisa_analysis -> M.state -> M.symbol -> (M.state * M.recognized) Enum.t
  
  val analyze_rmisa : M.rmisa -> M.state Enum.t 
end;;
