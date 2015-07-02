(**
   A module which defines operations over PDA.
*)

open Batteries;;

open Odefa_utils;;

open Odefa_pda_types;;

(* For convenience. *)
type 'a dq = 'a Deque.dq;;

(**
   A function to determine the reachable goal states of a PDA.
*)
let reachable_goal_states : 'a 'b 'c. ('a,'b,'c) pda -> 'a Enum.t = fun pda ->
  (* Our strategy is simply to close over transitions themselves.  We will
     ignore the input annotations for these purposes, since we aren't trying to
     construct witnesses of reachable states.  After closure is complete, each
     goal state which is generally reachable will be immediately reachable from
     the initial state with a single transition that pops the initial stack
     element.
  *)
  (* Utility functions for convenience. *)
  let states_equal x y = pda.pda_compare_states x y = 0 in
  let stack_symbols_equal x y = pda.pda_compare_stack_symbols x y = 0 in
  (* We'll start by using a simplified form of the transitions which does not
     track alphabet changes. *)
  let compare_transitions (state1,result1,pop_option1,pushes1)
      (state2,result2,pop_option2,pushes2) =
    let c1 = pda.pda_compare_states state1 state2 in
    if c1 <> 0 then c1 else
      let c2 = Option.compare ~cmp:pda.pda_compare_stack_symbols
          pop_option1 pop_option2 in
      if c2 <> 0 then c2 else
        let c3 = pda.pda_compare_states result1 result2 in
        if c3 <> 0 then c3 else
          Enum.compare pda.pda_compare_stack_symbols
            (Deque.enum pushes1) (Deque.enum pushes2)
  in
  (* Simplify the transitions to get them into this form we like. *)
  let initial_transitions =
    pda.pda_enumerate_transitions ()
    |> Enum.map
      (fun (state,_,pop_option,result,pushes) ->
         (state,result,pop_option,Deque.of_enum @@ List.enum pushes))
    |> Set.PSet.of_enum_cmp ~cmp:compare_transitions
  in
  (* We now have a set.  Define a closure function over it.  This closure is
     terminating because it only introduces new edges (and not new states) and
     each edge has push strings no longer than the edges used to create it
     (independently). *)
  let close_transitions_step transitions =
    let unary_closure =
      transitions
      |> Set.PSet.enum
      |> Enum.filter_map
        (fun (state1,state2,pop_option1,pushes1) ->
           let pushes1_front = Deque.front pushes1 in
           match (pop_option1,pushes1_front) with
           | (Some(k1),Some(k2,ks'))
             when stack_symbols_equal k1 k2 ->
             Some(state1,state2,None,ks')
           | _ ->
             None
        )
    in
    let pairwise_closure =
      transitions
      |> Set.PSet.enum
      |> Enum.map
        (fun (state1,state2,pop_option1,pushes1) ->
           transitions
           |> Set.PSet.enum
           |> Enum.filter (fun (s,_,_,_) -> states_equal s state2)
           |> Enum.filter_map
             (fun (_,state3,pop_option2,pushes2) ->
                let pushes1_front = Deque.front pushes1 in
                let pushes2_front = Deque.front pushes2 in
                let pushes1_rear = Deque.rear pushes1 in
                match (pop_option1,pushes1_front,pushes1_rear,
                       pop_option2,pushes2_front) with
                | (None,None,_,None,None) ->
                  (* Stack-nop edges are transitive. *)
                  Some(state1,state3,None,Deque.empty)
                | (None,None,_,_,_) ->
                  (* Composition with stack-nop is identity. *)
                  Some(state1,state3,pop_option2,pushes2)
                | (_,_,_,None,None) ->
                  (* Composition with stack-nop is identity. *)
                  Some(state1,state3,pop_option1,pushes1)
                | (_,_,Some(pushes1',pushes1_last),Some pop2,None)
                  when stack_symbols_equal pushes1_last pop2 ->
                  (* If the second edge pops something that the first edge
                     just pushed, we can simplify... but *only* if the
                     second edge doesn't push anything.  If it does, then we
                     must wait for it to be simplified (or we risk building
                     an arbitrarily long push string). *)
                  Some(state1,state3,pop_option1,pushes1')
                | _ -> None
             )
        )
      |> Enum.concat
    in
    Enum.append unary_closure pairwise_closure
  in
  (* Now compute the full transitive closure of the simplified transition
     set. *)
  let rec full_closure transitions =
    let transitions' =
      close_transitions_step transitions
      |> Enum.fold (fun a e -> Set.PSet.add e a) transitions
    in
    if Set.PSet.cardinal transitions <> Set.PSet.cardinal transitions'
    then full_closure transitions'
    else transitions'
  in
  let closed_transitions = full_closure initial_transitions in
  (* Finally, we can just skim this closed transition set for all transitions
     that immediately pop the initial stack element and push nothing.  The
     states that we reach through these edges are states which accept some
     string in the PDA. *)
  Set.PSet.enum closed_transitions
  |> Enum.filter_map
    (fun (state1,state2,pop_option,pushes) ->
       match pop_option with
       | None -> None
       | Some(pop) ->
         if Deque.is_empty pushes &&
            states_equal state1 pda.pda_initial_state &&
            stack_symbols_equal pop pda.pda_initial_stack_symbol
         then Some(state2)
         else None
    )
;;
