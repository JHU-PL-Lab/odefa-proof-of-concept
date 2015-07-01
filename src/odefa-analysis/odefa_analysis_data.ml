open Batteries;;

open Odefa_ast;;
open Odefa_ast_pretty;;
open Odefa_string_utils;;
open Odefa_utils;;

(* ****************** BASIC DATA STRUCTURES ****************** *)

module Value_ord =
struct
  type t = value
  let compare = compare
end;;

module Value_set = Set.Make(Value_ord);;

type annotated_clause =
  | Annotated_clause of clause
  | Enter_clause of var * var * clause
  | Exit_clause of var * var * clause
  | Start_clause
  | End_clause
;;

module Clause_ord =
struct
  type t = clause
  let compare = compare
end;;

module Clause_set = Set.Make(Clause_ord);;

module Annotated_clause_ord =
struct
  type t = annotated_clause
  let compare = compare
end;;

module Annotated_clause_set = Set.Make(Annotated_clause_ord);;

type edge =
  | Edge of annotated_clause * annotated_clause
;;

module Edge_ord =
struct
  type t = edge
  let compare = compare
end;;

module Edge_set = Set.Make(Edge_ord);;

type graph = Graph of Edge_set.t;;

let graph_equal (Graph es1) (Graph es2) =
  Edge_set.equal es1 es2
;;

(* ****************** LOOKUP STACK STRUCTURES ****************** *)

(** A data type for lookup operations. *)
type lookup_task =
  (* Indicates that we are now looking up a function.  When we are finished, we
     should resume by looking up this variable in the context where the function
     was defined. *)
  | Function_lookup of var
  (* Indicates that we are now looking up a record.  When we find it, we should
     resume by projecting the provided index from that record. *)
  | Record_projection_lookup of ident
;;

module Task_ord =
struct
  type t = lookup_task
  let compare = compare
end;;

module Task_set = Set.Make(Task_ord);;

(** A data type for lookup expressions.  These regular expression forms are
    used in the lookup stack to ensure a finite representation. *)
type lookup_expression =
  | Lookup_atom of lookup_task
  | Lookup_sequence of lookup_expression * lookup_expression
  | Lookup_one_or_more of lookup_expression
;;

(** A data type for lookup stacks.  This is just a distinguished form of option.
    We use this additional data type to ensure that empty strings only appear at
    top level; this limites the equivalences between strings (e.g. {expr} vs.
    {Lookup_sequence(expr,Lookup_empty)} cannot be represented). *)
type lookup_stack =
  | Lookup_stack_empty
  | Lookup_stack_nonempty of lookup_expression
;;

(** A specification for the below NFA module. *)
module Lookup_nfa_spec =
struct
  type state = int;;
  type symbol = lookup_task;;

  module State_order =
  struct
    type t = state;;
    let compare = compare;;
  end;;

  module Symbol_order = Task_ord;;
end;;

(** A module used to represent lookup expressions as NFAs. *)
module Lookup_nfa = Odefa_nfa.Make(Lookup_nfa_spec);;

(** A specification for the below lookup expression canonicalization module. *)
module Lookup_canonicalization_spec =
struct
  type t = lookup_expression;;
  type state = Lookup_nfa.state;;
  type symbol = Lookup_nfa.symbol;;
  
  let generate_nfa expr =
    let state_counter = ref 0 in
    let next_state () = state_counter := !state_counter + 1; !state_counter in
    (* A function to generate NFA fragments.  Given a starting state and an
       expression, this function produces an enumeration of transitions and an
       enumeration of accepting states.  It processes non-trivial expressions
       by recursion. *)
    let rec generate_nfa_part
          (start_states : int Enum.t)
          (expr' : lookup_expression)
          : ((int * lookup_task * int) Enum.t * int Enum.t) =
      match expr' with
        | Lookup_atom(task) ->
          (* This is simple: for some fresh finish state and each start state,
             consume the task and transition onto the finish state. *)
          let finish_state = next_state () in
          let transitions =
            start_states
              |> Enum.map (fun start_state -> (start_state, task, finish_state))
          in
          (transitions, Enum.singleton finish_state)
        | Lookup_sequence(expr1,expr2) ->
          (* Here, we need to execute each expression in sequence.  The finish
             states of each expression become the start states of each
             subsequent expression.  As we proceed, we simply gather up the
             transitions emitted by the handling of each expression. *)
          let (transitions1,intermediate_states) =
            generate_nfa_part start_states expr1
          in
          let (transitions2,finish_states) =
            generate_nfa_part intermediate_states expr2
          in
          (Enum.append transitions1 transitions2, finish_states)
        | Lookup_one_or_more(expr'') ->
          (* Create the NFA given by the inner expression.  We'll need to
             buffer the states, since we'll want to iterate over them more than
             once. *)
          let start_states_list = List.of_enum start_states in
          let (transitions, finish_states) =
            generate_nfa_part (List.enum start_states_list) expr''
          in
          let finish_states_set = Int_set.of_enum finish_states in
          (* For each transition onto one of the finish states, produce
             identical transitions onto each of the start states. *)
          let transitions' =
            transitions
            |> Enum.map
                (fun ((in_state, task, out_state) as transition) ->
                  if not @@ Int_set.mem out_state finish_states_set
                  then Enum.singleton transition
                  else
                    Enum.append (Enum.singleton transition)
                      (List.enum start_states_list
                        |> Enum.map
                            (fun start_state -> (in_state, task, start_state)))
                )
            |> Enum.concat
          in
          (* Now yield the result. *)
          (transitions', Int_set.enum finish_states_set)        
    in
    let initial_state = 0 in
    let (transitions, accepting_states) =
      generate_nfa_part (Enum.singleton initial_state) expr
    in
    Lookup_nfa.create transitions initial_state accepting_states
  ;;
  
  module Gen_hash =
  struct
    type t = lookup_expression;;
    let equal = (=);;
    let hash = Hashtbl.hash;;
  end;;
end;;

(** A module to canonicalize lookup expressions by their NFA representations. *)
module Lookup_canonicalization =
  Odefa_nfa_canonicalization.Make(Lookup_canonicalization_spec)
;;

(** Performs a deterministic "push" operation on a task expression. *)
let lookup_stack_push task stack =
  raise @@ Not_yet_implemented("lookup_stack_push")
;;

(** Performs a non-deterministic "pop" operation on a lookup stack.  The
    resulting enumeration gives pairs of the task which was popped and the
    expression describing the resulting lookup stacks. *)
let lookup_stack_pop stack : ((lookup_task * lookup_stack) option Enum.t) =
  let rec lookup_expr_pop expr
      : ((lookup_task * lookup_expression option) Enum.t) =
    let exprs =
      match expr with
        | Lookup_atom(task) ->
          Enum.singleton @@ (task, None)
        | Lookup_sequence(expr1,expr2) ->
          lookup_expr_pop expr2
          |> Enum.map
              (fun (task,expr2'_option) ->
                match expr2'_option with
                  | Some expr2' -> (task, Some(Lookup_sequence(expr1,expr2')))
                  | None -> (task, Some(expr1))
              )
        | Lookup_one_or_more(expr') ->
          lookup_expr_pop expr'
          |> Enum.map
              (fun ((task, expr''_option) as result) ->
                let result' =
                  match expr''_option with
                    | None -> (task, Some(expr))
                    | Some(expr'') -> (task, Some(Lookup_sequence(expr,expr'')))
                in
                List.enum [result;result']
              )
          |> Enum.concat
    in
    exprs
    |> Enum.map
        (fun (task, expr_option) ->
          let new_expr_option =
            match expr_option with
            | Some(expr') -> Some(Lookup_canonicalization.canonicalize expr')
            | None -> None
          in
          (task, new_expr_option)
        )
  in
  match stack with
    | Lookup_stack_empty -> Enum.singleton None
    | Lookup_stack_nonempty(expr) ->
      lookup_expr_pop expr
      |> Enum.map
            (fun (task, expr_option) ->
              let stack = match expr_option with
                          | Some(expr') -> Lookup_stack_nonempty(expr')
                          | None -> Lookup_stack_empty
              in
              Some (task, stack)
            )
;;

(* ****************** PRETTY PRINTING ****************** *)

let pretty_lookup_task t =
  match t with
  | Function_lookup(x) -> pretty_var x
  | Record_projection_lookup(i) -> "." ^ pretty_ident i
;;

let pretty_acl acl =
  match acl with
  | Annotated_clause(cl) -> pretty_clause cl
  | Enter_clause(x,x',Clause(x'',_)) ->
    pretty_var x ^ " = " ^ pretty_var x' ^ " @ " ^ pretty_var x'' ^ "+"
  | Exit_clause(x,x',Clause(x'',_)) ->
    pretty_var x ^ " = " ^ pretty_var x' ^ " @ " ^ pretty_var x'' ^ "-"
  | Start_clause -> "start"
  | End_clause -> "end"
;;

let pretty_acls acls =
  concat_sep_delim "{" "}" ", " @@ Enum.map pretty_acl @@
  Annotated_clause_set.enum acls
;;

let pretty_edge (Edge(acl1,acl2)) =
  pretty_acl acl1 ^ " --> " ^ pretty_acl acl2
;;

let pretty_edges edges =
  concat_sep_delim "{" "}" ", " @@ Enum.map pretty_edge @@ Edge_set.enum edges
;;

let pretty_graph (Graph edges) = pretty_edges edges;;
