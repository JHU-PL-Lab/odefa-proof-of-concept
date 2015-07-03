(** A module containing a variety of generic caching functions. *)

open Batteries;;

(**
  Creates a single-point caching function.  This function takes a key value and
  a thunk.  If the single-point caching function has not yet been called or if
  the key value differs from the last invocation, the thunk is invoked and the
  result is returned.  Otherwise, the thunk is ignored and the returned value
  is that which was returned during the most recent call to the single-point
  caching function.  Each invocation of this function yields a different
  single-point caching function.
*)
let create_single_point_cache ?cmp:(cmp=compare) =
  let store = ref None in
  fun key thunk ->
    match !store with
      | Some(key',value) when cmp key key' = 0 ->
        value
      | _ ->
        let value = thunk () in
        store := Some(key, value); value
;;

(**
  Creates a cache which stores up to a fixed number of elements.  After reaching
  the specified threshold, the entire cache is discarded.
*)
let create_drop_at_n_cache ?cmp:(cmp=compare) max_size =
  let store = ref @@ Map.PMap.create cmp in
  fun key thunk ->
    let m = !store in
    if Map.PMap.mem key m
    then Map.PMap.find key m
    else
      begin
        let value = thunk () in
        let m' =
          if Map.PMap.cardinal m >= max_size
          then Map.PMap.create cmp
          else Map.PMap.add key value m
        in
        store := m'; value
      end
;;
