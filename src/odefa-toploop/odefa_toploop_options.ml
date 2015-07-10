open Batteries;;
open BatOptParse.Opt;;
open Odefa_toploop_analyses;;

let make_analysis_option () : analysis_functions BatOptParse.Opt.t =
  let choice = ref analysis_1 in
  {
    (* Called whenever e.g. "--analysis 1" appears in the argument list *)
    option_set =
      (fun _ args ->
         (match args with
          | [arg] ->
            let new_choice =
              match arg with
              | "0" -> analysis_0
              | "1" -> analysis_1
              | "2" -> analysis_2
              | "NR" -> analysis_NR
              | _ -> raise @@ Option_error("--analysis", "Invalid argument: " ^ arg)
            in
            choice := new_choice
          | _ -> raise @@ Option_error ("--analysis","Invalid argument")
         )
      )
    ;
    option_set_value = (fun new_choice -> choice := new_choice)
    ;
    option_get = (fun () -> Some(!choice))
    ;
    option_metavars = ["ANALYSIS"]
    ;
    option_defhelp = Some("Chooses an analysis to use based on polyvariance model.  Valid values are 0, 1, 2, and NR (for \"non-repeating\").")
    ;
  };;
