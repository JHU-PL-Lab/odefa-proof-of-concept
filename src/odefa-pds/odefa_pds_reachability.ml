(**
  Contains the interface for a reachability analysis for a PDS.
*)

open Batteries;;

open Odefa_pds;;

module type Pds_reachability_sig =
sig
  module P : Pds
  
  type analysis
  
  type analysis_node
  type analysis_action
  type edge = Edge of analysis_node * analysis_action * analysis_node * bool
  
  val pp_analysis_node : analysis_node -> string
  val pp_analysis_action : analysis_action -> string
  val edges_of_analysis : analysis -> edge Enum.t
    
  val analyze_pds : P.pds -> analysis
  val reachable_from : analysis -> P.state -> P.symbol -> P.state Enum.t
  
  val analyze_rpds : P.rpds -> P.state Enum.t 
end;;

module type Pds_reachability =
  functor (P : Pds) -> Pds_reachability_sig with module P = P
;;