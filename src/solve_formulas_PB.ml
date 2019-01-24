open List
open Utils
open Petri_nets
open Formulas
open Printf
open Logs
open FullrunClustering_PB
open SubnetClustering_PB

(*----------------------------------------------------------------------------------*)
(* system command for PB solver *)
let pb_solver = ref "minisat+"

(*----------------------------------------------------------------------------------*)
(* exception for Unsatisfiable formulas *)
exception Unsatisfiable

(*----------------------------------------------------------------------------------*)
(* from a sol file, extracts the variables which are true *)
let extract_solution 
	sol_file = (* name of the file containing the SAT_PB solution *)
  let solution_channel = open_in_bin sol_file
  and list_sol = ref [] in
  while ( mem (input_line solution_channel).[0]  ['c';'o'] ) do () done;
  (* the line starting by 's' has just been read *)
  (* now, for minisat+ => solution with 'v' and sat4j => 1 line 'c' and then 'v'  *)
  if !pb_solver<>"minisat+" then ignore (input_line solution_channel);
  let solution_scanning_channel = Scanf.Scanning.from_channel solution_channel in
  (try ignore (Scanf.bscanf solution_scanning_channel "v" (fun () -> ( )))
   with
   | _ -> raise Unsatisfiable);
  (try
    while true do
      (* now comes either "x%d" or "-x%d" *)
      if (Scanf.bscanf solution_scanning_channel " %c" (fun c -> c)) = 'x'
      (* here minisat+ can use x0 but not other PB_solver but we created varibles x0 @see formulas.ml *)
      then list_sol := (Scanf.bscanf solution_scanning_channel "%d" (fun i -> if !pb_solver<>"minisat+" then i-1 else i)) :: !list_sol
      else ignore (Scanf.bscanf solution_scanning_channel "x%d" (fun i -> if !pb_solver<>"minisat+" then i-1 else i))
    done
   with _ -> (Scanf.Scanning.close_in solution_scanning_channel; close_in solution_channel));
  !list_sol

(*----------------------------------------------------------------------------------*)
let solve_enriched_formula_with_option opb_file option formula_builder =
 (* (formula_builder ()) returns (num_vars, formula, pb_min_objective, extractor_function) *)
  let start_time =  Unix.gettimeofday() in
  let num_vars, formula, pb_constraints, pb_min_objective, extractor_function = formula_builder () in
  let ref_num_vars = ref num_vars in
  let cnf_formula = qbf_to_cnf ref_num_vars formula in
  pb_formula_to_opb opb_file ((cnf_to_pb cnf_formula) @ pb_constraints) pb_min_objective !ref_num_vars !pb_solver;
  let time_after_formula = Unix.gettimeofday() in
  printf "\n\nBuilt formula in %f seconds.\n%!" (time_after_formula -. start_time);
  Gc.compact (); (* free memory before calling minisat+ *)
  let time_after_free = Unix.gettimeofday() in
  printf "Free memory in %f seconds.\n" (time_after_free -. time_after_formula);
  ignore (Sys.command (!pb_solver ^ " " ^ option ^ " " ^ opb_file ^ " > sol"));
  let end_time = Unix.gettimeofday() in
  printf "Optimal solution found in %f seconds.\n%!" (end_time -. time_after_free);
  printf "Total time: %f seconds.\n%!" (end_time -. start_time);
  (* return sol, nb variables, nb constraints, total time*)	
  (extractor_function (extract_solution "sol")), (length ((cnf_to_pb cnf_formula) @ pb_constraints)), !ref_num_vars, (end_time -. start_time)

let solve_enriched_formula opb_file formula_builder  =
  solve_enriched_formula_with_option opb_file "" formula_builder 

(*----------------------------------------------------------------------------------*)
(* launch pn_and_traces_to_formula_fullrunClustering_pb *)
(* save result in rfile *)
let solve_clustering_and_save type_of_clustering (pn : pn) (traces : alphabet array list) (d : int) (nb_clusters : int) (nb_transitions_per_cluster : int) rfile =
	let clustering_function = if type_of_clustering = "full run" 
				  then pn_and_traces_to_formula_fullRunClustering_pb 
				  else pn_and_traces_to_formula_subnetClustering_pb in 
	let n = (fold_left max 0 (map Array.length traces))
	and sigma = sort_uniq compare ((map (fun t -> t.lambda) pn.transitions) @ (concat (map Array.to_list traces))) in
	let (delta,cluster,(nc,ratio)),nb_constraints, nb_vars, total_time = solve_enriched_formula "alignment.pb" (fun () ->
							clustering_function
							pn (List.map log_to_pn_with_w_and_trailing_ww traces)
							sigma nb_clusters n d nb_transitions_per_cluster) in
	(* start to write the result in file named rfile *)
	if rfile <> "" then begin
		let oc = open_out rfile in 
		fprintf oc "|T|: %d\n" (length pn.transitions);
		fprintf oc "|P|: %d\n" (length pn.places);
		fprintf oc "|L|: %d\n" (length traces);
		fprintf oc "clusters : full runs\n";
		fprintf oc "nb variables : %d\n" nb_vars;
		fprintf oc "nb constrains : %d\n" nb_constraints ;
		fprintf oc "total time :%f\n" total_time;
		fprintf oc "d(c) : %d\n" d;
		fprintf oc "delta(c) : %d\n" delta;
		fprintf oc "n(c) : %d\n" nb_clusters;
		fprintf oc "ĉ(c) : %f\n\n" (1. -. ratio);
		List.iter (fun (centroid,traces,ratio)-> 
				fprintf oc "(";
				List.iter (fun a -> ( if a<> "ww" then fprintf oc "%s" a) ) centroid; fprintf oc ", [";
				List.iter (fun trace -> (List.iter (fun a -> ( if (a<> "ww" && a<> "w") then fprintf oc "%s" a) ) trace; 
				fprintf oc "  ") ) traces; fprintf oc "],";
				fprintf oc "%f" ratio;
				fprintf oc ")\n";
			   ) cluster ;
		fprintf oc "(nc, [ ";
		List.iter (fun trace -> (List.iter (fun a -> ( if (a<> "ww" && a<> "w") then fprintf oc "%s" a) ) trace; fprintf oc "  ") ) nc; 
		fprintf oc "],%f)\n" ratio;
		close_out oc;
	end;

