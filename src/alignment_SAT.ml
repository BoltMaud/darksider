open List
open Utils
open Formulas
open Petri_nets
open PN_to_formula
open Logs
open Printf

let sat_solver = "minisat"

(* returns lambda_ia (useful for extraction of results) *)
(* formula is not quantified *)
let pn_and_trace_to_formula_alignment (pn : pn) (trace : alphabet array) (sigma : alphabet list)
    (n : int) max_diffs d (* maximum distance for alignment *) =
  let lengthP = length pn.places
  and lengthSigma = length sigma in
  let variables_per_i = lengthSigma + lengthP + max_diffs in
  let lambda_ia i a = i * variables_per_i + (pos a sigma)
  and m_ip i p = i * variables_per_i + lengthSigma + p
  and diff_ik i k = i * variables_per_i + lengthSigma + lengthP + k - 1
  and nb_vars = (n + 1) * variables_per_i
  in
  lambda_ia,
  nb_vars,
  And([], [],
      [is_run n pn m_ip lambda_ia sigma]
      @ (list_of 1 n
	   (* if d = 0, gives *)
	   (* (not lambda_ia i trace.(i-1)) => ((diff_ik i 1) or ... or (diff_ik i max_diffs)) *)
	   (fun i -> printf "%d/%d\n" i n;
	     Or((list_of (max (i - d) 1) (min (i + d) (Array.length trace))
  		   (fun i' -> lambda_ia i trace.(i'-1))
		 @ (list_of 1 max_diffs
		      (fun k -> (diff_ik i k))),
		 [], []))))
      @ (concat @@ list_of 1 max_diffs
	   (fun k ->
	     (* no more than one i such that diff_ik *)
	     (concat @@ list_of 1 (n - 1)
		(fun i ->
		  printf "%d/%d\n" i (n-1);
		  (list_of (i + 1) n
		     (* not ((diff_ik i k) and (diff_ik i' k)) *)
		     (fun i' -> Or([], [(diff_ik i k); (diff_ik i' k)], []))))))))


(* returns lambda_ia (useful for extraction of results) *)
let pn_and_trace_to_dimacs out_file (pn : pn) (trace : alphabet array) (sigma : alphabet list)
    (n : int) max_diffs d (* maximal distance for alignment *) =
  let start_time = Sys.time () in
  let lambda_ia, num_vars, formula = pn_and_trace_to_formula_alignment pn trace sigma n max_diffs d in
  let ref_num_vars = ref num_vars in
  let cnf_formula = qbf_to_cnf ref_num_vars formula in
  cnf_to_dimacs out_file cnf_formula !ref_num_vars;
  printf "Built formula for %d %d in %f seconds.\n%!" n max_diffs (Sys.time () -. start_time);
  lambda_ia

let extract_solution sol_file =
  let solution_channel = Scanf.Scanning.open_in_bin sol_file
  and list_sol = ref [] in
  Scanf.bscanf solution_channel "SAT\n" (fun i -> i);
  (try
     while true do
       Scanf.bscanf solution_channel "%d "
	 (fun i -> if i > 0 then list_sol := i :: !list_sol)
     done
   with _ -> Scanf.Scanning.close_in solution_channel);
  !list_sol

let extract_run list_sol lambda_ia sigma n =
  let run = Array.make n "" in
  for i = 1 to n do
    iter (fun a -> if mem (lambda_ia i a) list_sol then run.(i-1) <- a) sigma;
  done;
  run

let print_run oc run =
  for i = 0 to (Array.length run) - 1 do
    output_string oc (run.(i) ^ " ");
  done;
  output_newline oc

let solve_pn_and_trace_with_sat dimacs_file (pn : pn) (trace : alphabet array)
    (n : int) max_diffs d (* maximal distance for alignment *) =
  let sigma = sort_uniq compare ((map (fun t -> t.lambda) pn.transitions) @ (Array.to_list trace)) in
  let lambda_ia = pn_and_trace_to_dimacs dimacs_file pn trace sigma n max_diffs d in
  Gc.compact (); (* free memory before calling minisat *)
  ignore (Sys.command (sat_solver ^ " " ^ dimacs_file ^ " sol"));
  print_newline ();
  try print_run stdout (extract_run (extract_solution "sol") lambda_ia sigma n)
  with _ -> ()



(***************** multi-alignments *****************)


(* returns lambda_ia (useful for extraction of results) *)
(* formula is not quantified *)
let pn_and_traces_to_formula_multialignment
    (pn : pn) (traces : alphabet array list) (sigma : alphabet list) (n : int) max_diffs d =
  let lengthP = length pn.places
  and lengthSigma = length sigma
  and nb_traces = length traces in
  let variables_per_i = lengthSigma + lengthP + nb_traces * max_diffs in
  let lambda_ia i a = i * variables_per_i + (pos a sigma)
  and m_ip i p = i * variables_per_i + lengthSigma + p
  and diff_ijk i j k = i * variables_per_i + lengthSigma + lengthP + j * max_diffs + k - 1
  and nb_vars = (n + 1) * variables_per_i
  in
  lambda_ia,
  nb_vars,
  And([], [],
      (is_run n pn m_ip lambda_ia sigma)
      :: (concat @@ list_of 0 (nb_traces - 1)
	    (fun j ->
	      let jth_log = (nth traces j) in
	      (list_of 1 n
		 (* if d = 0, gives *)
		 (* (not lambda_ia i jth_log.(i-1)) => ((diff_ijk i j 1) or ... or (diff_ijk i j max_diffs)) *)
		 (fun i -> printf "%d/%d\n" i n;
		   Or((list_of (max (i - d) 1) (min (i + d) (Array.length jth_log))
  			 (fun i' -> lambda_ia i jth_log.(i'-1))
		       @ (list_of 1 max_diffs
			    (fun k -> (diff_ijk i j k))),
		       [], []))))
	      @ (concat @@ list_of 1 max_diffs
		   (fun k ->
		     (* no more than one i such that diff_ijk *)
		     (concat @@ list_of 1 (n - 1)
			(fun i ->
			  printf "%d/%d\n" i (n-1);
			  (list_of (i + 1) n
			     (* not ((diff_ijk i j k) and (diff_ijk i' j k)) *)
			     (fun i' -> Or([], [(diff_ijk i j k); (diff_ijk i' j k)], []))))))))))


(* returns lambda_ia (useful for extraction of results) *)
let pn_and_traces_multialignment_to_dimacs out_file (pn : pn) (traces : alphabet array list) (sigma : alphabet list)
    (n : int) max_diffs d (* maximal distance for alignment *) =
  let start_time = Sys.time () in
  let lambda_ia, num_vars, formula = pn_and_traces_to_formula_multialignment pn traces sigma n max_diffs d in
  let ref_num_vars = ref num_vars in
  let cnf_formula = qbf_to_cnf ref_num_vars formula in
  cnf_to_dimacs out_file cnf_formula !ref_num_vars;
  printf "Built formula for %d %d in %f seconds.\n%!" n max_diffs (Sys.time () -. start_time);
  lambda_ia


let solve_multialignment_with_sat dimacs_file (pn : pn) (traces : alphabet array list)
    (n : int) max_diffs d (* maximal distance for alignment *) =
  let sigma = sort_uniq compare ((map (fun t -> t.lambda) pn.transitions) @ (concat (map Array.to_list traces))) in
  let lambda_ia = pn_and_traces_multialignment_to_dimacs dimacs_file pn traces sigma n max_diffs d in
  Gc.compact (); (* free memory before calling minisat *)
  ignore (Sys.command (sat_solver ^ " " ^ dimacs_file ^ " sol"));
  print_newline ();
  try print_run stdout (extract_run (extract_solution "sol") lambda_ia sigma n)
  with _ -> ()




let sat_solver_exit_code_satisfiable = 10

(* (\* model is assumed to have infinite runs *\) *)
(* let shortest_run dimacs_file (pn : pn) (logs : alphabet array list) *)
(*     min_diffs d (\* minimal distance for mismatches *\) = *)
(*   let sigma = sort_uniq compare ((map (fun t -> t.lambda) pn.transitions) @ (concat (map Array.to_list logs))) in *)
(*   let inf = ref 0 *)
(*   and sup = ref min_diffs *)
(*   and lambda_ia = ref (fun i a -> 0) *)
(*   and list_sol = ref [] in *)
(*   let try_n n = *)
(*     (print_string "Seeking run of length "; *)
(*      print_int n; *)
(*      print_newline (); *)
(*      let current_lambda_ia = pn_and_logs_to_dimacs dimacs_file pn logs sigma n min_diffs d in *)
(*      if Sys.command (sat_solver ^ " " ^ dimacs_file ^ " sol") = sat_solver_exit_code_satisfiable *)
(*      then (lambda_ia := current_lambda_ia; *)
(* 	   list_sol := extract_solution "sol"; *)
(* 	   true) *)
(*      else false) *)
(*   in *)
(*   (\* find first solution *\) *)
(*   while (not (try_n !sup)) do *)
(*     inf := !sup; *)
(*     sup := 2 * !sup *)
(*   done; *)
(*   (\* then decrease by dichotomy *\) *)
(*   while !inf + 1 < !sup do *)
(*     let mid = (!inf + !sup) / 2 in *)
(*     if try_n mid then sup := mid else inf := mid *)
(*   done; *)
(*   print_string ("A shortest run with " ^ (string_of_int min_diffs) ^ " misalignments is:\n"); *)
(*   print_run stdout (extract_run !list_sol !lambda_ia sigma !sup) *)



(* model is assumed to have infinite runs *)
(* scale_factor would allow to scale more or less aggressively during the search
   of the upper bound, in order to avoid trying too many diffs *)
let optimal_multialignment_sat dimacs_file (pn : pn) (traces : alphabet array list)
    n d (* minimal distance for mismatches *) (* (scale_factor: float) *) =
  let sigma = sort_uniq compare ((map (fun t -> t.lambda) pn.transitions) @ (concat (map Array.to_list traces))) in
  let inf = ref (-1) (* there is no solution for !inf *)
  and sup = ref 1 (* after the first while loop, there is a solution for !sup *)
  and lambda_ia = ref (fun i a -> 0)
  and list_sol = ref [] in
  let try_m max_diffs =
    (print_string ("Seeking run with at most " ^ (string_of_int max_diffs) ^ " misalignments");
     print_newline ();
     let current_lambda_ia = pn_and_traces_multialignment_to_dimacs dimacs_file pn traces sigma n max_diffs d in
     if Sys.command (sat_solver ^ " " ^ dimacs_file ^ " sol") = sat_solver_exit_code_satisfiable
     then (lambda_ia := current_lambda_ia;
	   list_sol := extract_solution "sol";
	   true)
     else false)
  in
  (* find upper bound (better than n) *)
  while !sup < n && not (try_m !sup) do
    inf := !sup;
    sup := 2 * !sup
  (* sup := int_of_float (ceil (scale_factor *. (float_of_int !sup))) *)
  done;
  sup := min !sup n;
  (* dichotomy *)
  while !inf + 1 < !sup do
    let mid = (!inf + !sup) / 2 in
    if try_m mid then sup := mid else inf := mid
  done;
  print_string ("Optimal: " ^ (string_of_int !sup) ^ " misalignments for:\n");
  let run = extract_run !list_sol !lambda_ia sigma n in
  print_run stdout run;
  !sup, run


(* (\* We assume that (Array.length run1) = (Array.length run2) *\) *)
(* let dist run1 run2 d (\* minimal distance for mismatches *\) = *)
(*   let n = Array.length run1 *)
(*   and numdiffs = ref 0 in *)
(*   for i = 1 to n do *)
(*     let diff = ref true in *)
(*     for i' = (max (i - d) 1) to (min (i + d) n) do *)
(*       if run1.(i-1) = run2.(i'-1) then diff := false *)
(*     done; *)
(*     if !diff then (incr numdiffs); *)
(*   done; *)
(*   !numdiffs *)
