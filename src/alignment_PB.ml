open List
open Utils
open Formulas
open Petri_nets
open PN_to_formula
open Logs
open Printf
open Solve_formulas_PB


let extract_run list_sol lambda_ia sigma n =
  let run = Array.make n "" in
  for i = 1 to n do
    iter (fun a -> if mem (lambda_ia i a) list_sol then run.(i-1) <- a) sigma;
  done;
  run

let print_run oc run =
  for i = 0 to (Array.length run) - 1 do
    if run.(i) <> "w" then output_string oc (run.(i) ^ " ");
  done;
  output_newline oc



(* formula is not quantified *)
let pn_and_trace_to_formula_alignment_pb (pn : pn) (trace : alphabet array) (sigma : alphabet list)
    (n : int) d (* maximum distance for alignment *) =
  let lengthP = length pn.places
  and lengthSigma = length sigma in
  let variables_per_i = lengthSigma + lengthP + 1 in
  let lambda_ia i a = i * variables_per_i + (pos a sigma)
  and m_ip i p = i * variables_per_i + lengthSigma + p
  and diff_i i = i * variables_per_i + lengthSigma + lengthP
  and nb_vars = (n + 1) * variables_per_i
  in
  nb_vars,
  And([], [],
      [is_run n pn m_ip lambda_ia sigma]
      @ (list_of 1 n
	   (* if d = 0, gives *)
	   (* (not lambda_ia i trace.(i-1)) => (diff_i i) *)
	   (fun i -> printf "*%!"; (* "%d/%d\n%!" i n; *)
	     Or((diff_i i)
		:: (list_of (max (i - d) 1) (min (i + d) (Array.length trace))
  		      (fun i' -> lambda_ia i trace.(i'-1))),
		[], [])))),
  [], (* no additional pseudo boolean constraints *)
  (* pseudo-boolean objective to be minimized *)
  (list_of 1 n (fun i -> (1, (diff_i i)))),
  (* solution extractor function *)
  fun list_sol -> print_run stdout (extract_run list_sol lambda_ia sigma n)



let optimal_alignment_pb (pn : pn) (trace : alphabet array)
    (n : int) d (* maximal distance for alignment *) =
  let sigma = sort_uniq compare ((map (fun t -> t.lambda) pn.transitions) @ (Array.to_list trace)) in
  solve_enriched_formula "alignment.pb" (fun () -> pn_and_trace_to_formula_alignment_pb pn trace sigma n d)

(* deprecated *)
let solve_pn_and_trace_with_pb = optimal_alignment_pb



(***************** multi-alignments *****************)


(* formula is not quantified *)
let pn_and_traces_to_formula_multialignment_pb (pn : pn) (traces : alphabet array list) (sigma : alphabet list)
    (n : int) d (* maximum distance for alignment *) =
  let lengthP = length pn.places
  and lengthSigma = length sigma
  and nb_traces = length traces in
  let variables_per_i = lengthSigma + lengthP + nb_traces in
  let lambda_ia i a = i * variables_per_i + (pos a sigma)
  and m_ip i p = i * variables_per_i + lengthSigma + p
  and diff_ij i j = i * variables_per_i + lengthSigma + lengthP + j
  and nb_vars = (n + 1) * variables_per_i
  in
  nb_vars,
  And([], [],
      (is_run n pn m_ip lambda_ia sigma)
      :: (concat @@ list_of 0 (nb_traces - 1)
	    (fun j ->
	      let jth_log = (nth traces j) in
	      (list_of 1 n
		 (* if d = 0, gives *)
		 (* (not lambda_ia i jth_log.(i-1)) => (diff_ij i j) *)
		 (fun i -> printf "%d/%d\n%!" i n;
		   Or((diff_ij i j)
		      :: (list_of (max (i - d) 1) (min (i + d) (Array.length jth_log))
  			    (fun i' -> lambda_ia i jth_log.(i'-1))),
		      [], [])))))),
  [], (* no additional pseudo boolean constraints *)
  (* pseudo-boolean objective to be minimized (here sum of diffs) *)
  (concat @@ list_of 0 (nb_traces - 1)
     (fun j -> (list_of 1 n (fun i -> (1, (diff_ij i j)))))),
  fun list_sol -> print_run stdout (extract_run list_sol lambda_ia sigma n)



let optimal_multialignment_pb (pn : pn) (traces : alphabet array list)
    (n : int) d (* maximal distance for alignment *) =
  let sigma = sort_uniq compare ((map (fun t -> t.lambda) pn.transitions) @ (concat (map Array.to_list traces))) in
  solve_enriched_formula "alignment.pb" (fun () -> pn_and_traces_to_formula_multialignment_pb pn traces sigma n d)



(* formula is not quantified *)
let pn_and_pns_to_formula_multialignment_pb (pn : pn) (pns : pn list) (sigma : alphabet list) (n : int) =
  let maxlengthP = fold_left max 0 (map (fun pn -> length pn.places) (pn :: pns))
  and lengthSigma = length sigma
  and nb_pns = length pns in
  let variables_per_i = (1 + nb_pns) * (lengthSigma + maxlengthP) + nb_pns in
  (* pn is considered as the nb_pns'th *)
  let lambda_jia j i a = i * variables_per_i + j * lengthSigma + (pos a sigma)
  and m_jip j i p = i * variables_per_i + (1 + nb_pns) * lengthSigma + j * maxlengthP + p
  and diff_ji j i = i * variables_per_i + (1 + nb_pns) * (lengthSigma + maxlengthP) + j
  and nb_vars = (n + 1) * variables_per_i
  in
  nb_vars,
  And([], [],
      (* could even be treated with the others *)
      (is_run n pn (m_jip nb_pns) (lambda_jia nb_pns) sigma)
      ::
	(list_of 0 (nb_pns - 1)
	   (fun j -> (is_run n (nth pns j) (m_jip j) (lambda_jia j) sigma)))
      @ (concat @@ list_of 0 (nb_pns - 1)
	   (fun j ->
	     (list_of 1 n
		(fun i ->
		  Or([diff_ji j i], [],
		     (map (fun a -> And([(lambda_jia nb_pns i a); (lambda_jia j i a)], [], []))
			sigma))))))),
  [], (* no additional pseudo boolean constraints *)
  (* pseudo-boolean objective to be minimized (here sum of diffs) *)
  (concat @@ list_of 0 (nb_pns - 1)
     (fun j -> (list_of 1 n (fun i -> (1, (diff_ji j i)))))),
  fun list_sol -> print_run stdout (extract_run list_sol (lambda_jia nb_pns) sigma n)



let optimal_multialignment_pns (pn : pn) (pns : pn list) (n : int) =
  let sigma = sort_uniq compare (concat (map
					   (fun pn' -> (map (fun t -> t.lambda) pn'.transitions))
					   (pn :: pns))) in
  solve_enriched_formula "alignment.pb" (fun () -> pn_and_pns_to_formula_multialignment_pb pn pns sigma n)



let optimal_multialignment_edit_distance (pn : pn) (traces : alphabet array list) (n : int) =
  optimal_multialignment_pns (add_ww pn) (List.map log_to_pn_with_w traces) n;;



(***************** subnet-alignments *****************)

(* - strict means that transitions not in the subnet are not used in the alignments *)
(* - non-strict means that they can be used at cost 1 *)
let strict = ref true

(* formula is not quantified *)
let pn_and_pns_to_formula_subnetalignment (pn : pn) (pns : pn list) (sigma : alphabet list)
    (n : int) (min_nb_used_pns : int) (max_delta : int) =
  let maxlengthP = fold_left max 0 (map (fun pn -> length pn.places) (pn :: pns))
  and lengthSigma = length sigma
  and nb_pns = length pns in
  let variables_per_i = 2 * nb_pns * (lengthSigma + maxlengthP) + nb_pns in
  (* pn is considered as the nb_pns'th *)
  let lambda_jia j i a = i * variables_per_i + j * lengthSigma + (pos a sigma)
  and lambda'_jia j i a = i * variables_per_i + (nb_pns + j) * lengthSigma + (pos a sigma)
  and m_jip j i p = i * variables_per_i + 2 * nb_pns * lengthSigma + j * maxlengthP + p
  and m'_jip j i p = i * variables_per_i + 2 * nb_pns * lengthSigma + (nb_pns + j) * maxlengthP + p
  and diff_ji j i = i * variables_per_i + 2 * nb_pns * (lengthSigma + maxlengthP) + j
  and used_a a = (n + 1) * variables_per_i + (pos a sigma)
  and used_j j = (n + 1) * variables_per_i + lengthSigma + j
  and nb_vars = (n + 1) * variables_per_i + lengthSigma + nb_pns
  in
  nb_vars,
  And([], [],
      (concat @@ list_of 0 (nb_pns - 1)
	 (fun j ->
	   (is_run n (nth pns j) (m'_jip j) (lambda'_jia j) sigma)
	   :: (is_run n pn (m_jip j) (lambda_jia j) sigma)
	   :: (list_of 1 n
		 (fun i ->
		   And ([], [],
			Or([diff_ji j i], [used_j j],
			   (map (fun a -> And([(used_a a); (lambda'_jia j i a); (lambda_jia j i a)], [], []))
			      sigma))
			:: (if !strict
			  then (map (fun a -> Or([used_a a], [lambda_jia j i a], []))
				  sigma)
			  else []))))))),
  PB_geq ((list_of 0 (nb_pns - 1) (fun j -> (1, (used_j j)))), min_nb_used_pns)
  :: (list_of 0 (nb_pns - 1)
	(fun j -> PB_leq ((list_of 1 n (fun i -> (1, (diff_ji j i)))), max_delta))),
  (* pseudo-boolean objective to be minimized (here nb of used actions) *)
  (map (fun a -> ((if a = "ww" then 0 else 1), (used_a a))) sigma),
  fun list_sol ->
    let list_actions = ref [] in
    iter (fun a ->
      if a <> "ww" && (mem (used_a a) list_sol)
      then list_actions := a :: !list_actions) sigma;
    !list_actions



let optimal_subnetalignment_pns (pn : pn) (pns : pn list) (n : int) (min_nb_used_pns : int) (max_delta : int) =
  let sigma = sort_uniq compare (concat (map
					   (fun pn' -> (map (fun t -> t.lambda) pn'.transitions))
					   (pn :: pns))) in
  solve_enriched_formula "alignment.pb" (fun () ->
    pn_and_pns_to_formula_subnetalignment pn pns sigma n min_nb_used_pns max_delta)


let optimal_subnetalignment_traces (pn : pn) (traces : alphabet array list) (min_nb_used_pns : int) (max_delta : int) =
  let n = fold_left max 0 (map Array.length traces)
  and sigma = sort_uniq compare ((map (fun t -> t.lambda) pn.transitions) @ (concat (map Array.to_list traces))) in
  solve_enriched_formula "alignment.pb" (fun () ->
    pn_and_pns_to_formula_subnetalignment
      pn (List.map log_to_pn_with_w_and_trailing_ww traces)
      sigma n min_nb_used_pns max_delta) 


let optimal_subnetalignment_all_traces (pn : pn) (traces : alphabet array list) (max_delta : int) =
  optimal_subnetalignment_traces pn traces (List.length traces) max_delta



(***************** multi-alignments for clustering *****************)


(* formula is not quantified *)
let pn_and_traces_to_formula_multialignment_clustering (pn : pn) (traces : alphabet array list) (sigma : alphabet list)
    (n : int) d (* maximum distance for alignment *)
    (max_delta : int)
    min_cluster_size =
  let lengthP = length pn.places
  and lengthSigma = length sigma
  and nb_traces = length traces in
  let variables_per_i = lengthSigma + lengthP + nb_traces in
  let lambda_ia i a = i * variables_per_i + (pos a sigma)
  and m_ip i p = i * variables_per_i + lengthSigma + p
  and diff_ij i j = i * variables_per_i + lengthSigma + lengthP + j
  and used_j j = (n + 1) * variables_per_i + j
  and nb_vars = (n + 1) * variables_per_i + nb_traces
  in
  nb_vars,
  And([], [],
      (is_full_run n pn m_ip lambda_ia sigma)
      :: (concat @@ list_of 0 (nb_traces - 1)
	    (fun j ->
	      let jth_log = (nth traces j) in
	      (list_of 1 n
		 (* if d = 0, gives *)
		 (* (not lambda_ia i jth_log.(i-1)) => (diff_ij i j) *)
		 (fun i -> printf "%d/%d\n%!" i n;
		   Or((diff_ij i j)
		      :: (list_of (max (i - d) 1) (min (i + d) (Array.length jth_log))
  			    (fun i' -> lambda_ia i jth_log.(i'-1))),
		      [used_j j], [])))))),
  PB_geq ((list_of 0 (nb_traces - 1) (fun j -> (1, (used_j j)))), min_cluster_size)
  :: (list_of 0 (nb_traces - 1)
	(fun j -> PB_leq ((list_of 1 n (fun i -> (1, (diff_ij i j)))), max_delta))),
  (* pseudo-boolean objective to be minimized (here nb of traces at dist <= max_delta to the run) *)
  (list_of 0 (nb_traces - 1) (fun j -> (-1, (used_j j)))),
  fun list_sol ->
    let centroid = extract_run list_sol lambda_ia sigma n
    and cluster = ref []
    and remaining_traces = ref [] in
    print_run stdout centroid;
    iteri
      (fun j jth_trace ->
	push jth_trace (if mem (used_j j) list_sol then cluster else remaining_traces))
        (* if (mem (used_j j) list_sol) *)
        (* then cluster := jth_trace :: !cluster *)
        (* else remaining_traces := jth_trace :: !remaining_traces) *)
      traces;
    centroid, !cluster, !remaining_traces


let optimal_cluster (pn : pn) (traces : alphabet array list)
    (n : int) d (* maximal distance for alignment *) max_delta =
  let sigma = sort_uniq compare ((map (fun t -> t.lambda) pn.transitions) @ (concat (map Array.to_list traces))) in
  solve_enriched_formula "alignment.pb"
    (fun () -> pn_and_traces_to_formula_multialignment_clustering pn traces sigma n d max_delta 0) 

let good_cluster min_cluster_size (pn : pn) (traces : alphabet array list)
    (n : int) d (* maximal distance for alignment *) max_delta =
  let sigma = sort_uniq compare ((map (fun t -> t.lambda) pn.transitions) @ (concat (map Array.to_list traces))) in
  solve_enriched_formula_with_option "alignment.pb" "-1"
    (fun () -> pn_and_traces_to_formula_multialignment_clustering pn traces sigma n d max_delta min_cluster_size)


exception Empty_cluster

let full_clustering clustering_function (pn : pn) (traces : alphabet array list) clusters_file
    (n : int) d (* maximal distance for alignment *) max_delta =
  let clusters = ref []
  and remaining_traces = ref traces
  and oc = open_out_bin clusters_file in
  (try
     while !remaining_traces <> [] do
       let (centroid, new_cluster, new_remaining_traces), _,_,_ = clustering_function pn !remaining_traces n d max_delta in
       if new_cluster = [] then raise Empty_cluster;
       remaining_traces := new_remaining_traces;
       clusters := new_cluster :: !clusters;
       output_string oc "Cluster with centroid:\n";
       print_run oc centroid;
       output_newline oc;
       iter (print_run oc) new_cluster;
       output_newline oc;
       flush oc;
     done;
   with _ ->
     output_string oc "\nRemaining traces, not clustered because too far form all the runs of the model:\n";
     iter (print_run oc) !remaining_traces);
  close_out oc

let full_optimal_clustering (pn : pn) (traces : alphabet array list) clusters_file
    (n : int) d (* maximal distance for alignment *) max_delta =
  full_clustering optimal_cluster pn traces clusters_file n d max_delta

let full_good_clustering min_cluster_size (pn : pn) (traces : alphabet array list) clusters_file
    (n : int) d (* maximal distance for alignment *) max_delta =
  full_clustering (good_cluster min_cluster_size) pn traces clusters_file n d max_delta


(***************** multi-alignments for clustering, minimize sum of distances to centroids *****************)



(* formula is not quantified *)
let pn_and_traces_to_formula_sum_clustering (pn : pn) (traces : alphabet array list) (sigma : alphabet list)
    (n : int) d (* maximum distance for alignment *)
    nb_clusters max_nb_unclustered_traces =
  let lengthP = length pn.places
  and lengthSigma = length sigma
  and nb_traces = length traces in
  let variables_per_i = nb_clusters * (lengthSigma + lengthP + nb_traces) in
  let lambda_kia k i a = i * variables_per_i + k * lengthSigma + (pos a sigma)
  and m_kip k i p = i * variables_per_i + nb_clusters * lengthSigma + k * lengthP + p
  and diff_kij k i j = i * variables_per_i + nb_clusters * (lengthSigma + lengthP) + k * nb_traces + j
  and used_kj k j = (n + 1) * variables_per_i + k * nb_traces + j
  and nb_vars = (n + 1) * variables_per_i + nb_clusters * nb_traces
  in
  nb_vars,
  And([], [],
      (concat @@ list_of 0 (nb_clusters - 1)
	 (fun k ->
	   (is_full_run n pn (m_kip k) (lambda_kia k) sigma)
	   :: (concat @@ list_of 0 (nb_traces - 1)
		 (fun j ->
		   let jth_log = (nth traces j) in
		   (list_of 1 n
   		      (* if d = 0, gives *)
		      (* (not lambda_ia i jth_log.(i-1)) => (diff_ij i j) *)
		      (fun i -> printf "%d/%d\n%!" i n;
			Or((diff_kij k i j)
			   :: (list_of (max (i - d) 1) (min (i + d) (Array.length jth_log))
  				 (fun i' -> lambda_kia k i jth_log.(i'-1))),
			   [used_kj k j], [])))))))),
  PB_geq ((concat @@ list_of 0 (nb_traces - 1)
  	     (fun j ->
  	       (list_of 0 (nb_clusters - 1)
  		  (fun k -> (1, (used_kj k j)))))),
  	  nb_traces - max_nb_unclustered_traces)
  :: (list_of 0 (nb_traces - 1)
  	(fun j -> PB_leq ((list_of 0 (nb_clusters - 1) (fun k -> (1, (used_kj k j)))), 1))),
  (* pseudo-boolean objective to be minimized (here sum of diffs) *)
  (concat @@ list_of 0 (nb_traces - 1)
     (fun j ->
       (concat @@ list_of 0 (nb_clusters - 1)
	  (fun k ->
	    (list_of 1 n (fun i -> (1, (diff_kij k i j)))))))),
  fun list_sol ->
    ((list_of 0 (nb_clusters - 1)
	(fun k ->
	  let centroid = extract_run list_sol (lambda_kia k) sigma n
	  and cluster = ref [] in
	  print_string "\nCluster with centroid:\n";
	  print_run stdout centroid;
	  print_newline;
	  iteri
	    (fun j jth_trace ->
	      (if mem (used_kj k j) list_sol
	       then (push jth_trace cluster; print_run stdout jth_trace)))
	    traces;
	  centroid, !cluster)),
     let remaining_traces = ref [] in
     iteri
       (fun j jth_trace ->
	 (if (exists (fun used -> mem used list_sol)
		(list_of 0 (nb_clusters - 1) (fun k -> used_kj k j)))
	  then push jth_trace remaining_traces))
       traces;
     !remaining_traces)


let clusters (pn : pn) (traces : alphabet array list)
    (n : int) d (* maximal distance for alignment *) nb_clusters max_nb_unclustered_traces =
  let sigma = sort_uniq compare ((map (fun t -> t.lambda) pn.transitions) @ (concat (map Array.to_list traces))) in
  solve_enriched_formula "alignment.pb"
    (fun () -> pn_and_traces_to_formula_sum_clustering pn traces sigma n d nb_clusters max_nb_unclustered_traces)
