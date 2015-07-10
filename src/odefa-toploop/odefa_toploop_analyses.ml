(**
  A module to perform dynamic dispatch over analysis modules.
*)

open Batteries;;

open Odefa_ast;;
open Odefa_analysis;;
open Odefa_analysis_graph;;

type analysis_functions =
  { analysis_perform_graph_closure : expr -> graph -> graph
  ; analysis_test_graph_inconsistency : expr -> graph -> bool
  ; analysis_lookup : expr -> graph -> var -> annotated_clause -> Value_set.t
  };;

module Analysis_0 =
  Odefa_analysis.Make(Odefa_analysis_unit_stack.Stack);;

let analysis_0 =
  { analysis_perform_graph_closure = Analysis_0.perform_graph_closure
  ; analysis_test_graph_inconsistency = Analysis_0.test_graph_inconsistency
  ; analysis_lookup = Analysis_0.lookup
  };;

module Analysis_1 =
  Odefa_analysis.Make(Odefa_analysis_single_element_stack.Stack);;

let analysis_1 =
  { analysis_perform_graph_closure = Analysis_1.perform_graph_closure
  ; analysis_test_graph_inconsistency = Analysis_1.test_graph_inconsistency
  ; analysis_lookup = Analysis_1.lookup
  };;

module Analysis_2 =
  Odefa_analysis.Make(Odefa_analysis_two_element_stack.Stack);;

let analysis_2 =
  { analysis_perform_graph_closure = Analysis_2.perform_graph_closure
  ; analysis_test_graph_inconsistency = Analysis_2.test_graph_inconsistency
  ; analysis_lookup = Analysis_2.lookup
  };;

module Analysis_NR =
  Odefa_analysis.Make(Odefa_analysis_nonrepeating_stack.Stack);;

let analysis_NR =
  { analysis_perform_graph_closure = Analysis_NR.perform_graph_closure
  ; analysis_test_graph_inconsistency = Analysis_NR.test_graph_inconsistency
  ; analysis_lookup = Analysis_NR.lookup
  };;

