open List
open Utils
open Formulas
open Petri_nets
open PN_to_formula
open Printf

let read_logs log_file =
  let ic = open_in_bin log_file
  and logs = ref [] in
  (try
     while true do
       logs := Array.of_list (Str.split (Str.regexp "[ \t\r]+") (input_line ic)) :: !logs
     done
   with _ -> close_in ic);
  rev !logs


(* the logs are no longer supposed to have the same length *)
let logs_to_pn (logs : alphabet array list) =
  {places = list_of 0 (fold_left (+) 0 (map Array.length logs)) (fun i -> i);
   transitions =
      concat
	(let next_place = ref 0 in
	 map (fun log ->
	   list_of 0 (Array.length log - 1)
	     (fun j ->
	       let input_place = (if j = 0 then 0 else !next_place)
	       and output_place = !next_place + 1 in
	       incr next_place; (* unfortunately Ocaml does not have a convenient prog1 *)
	       {pre = [input_place];
		post = [output_place];
		lambda = log.(j)}))
	   logs);
   m0 = [0]; mf=[]}

let log_to_pn_with_w_at_the_end  (log : alphabet array) =
	let {places = p; transitions = t; m0 = m; mf=mf} = logs_to_pn [log] in
	{places = p;
	 transitions =  {pre = [((length p)-1)]; post = [((length p)-1)] ; lambda = "w"}:: t;
	 m0 = m; mf=[]}

let logs_to_pn_with_w (logs : alphabet array list) =
  let {places = p; transitions = t; m0 = m; mf=mf} = logs_to_pn logs
  in {places = p;
      transitions = {pre = []; post = []; lambda = "w"} :: t;
      m0 = m; mf=[]}

let log_to_pn_with_w (log : alphabet array) = logs_to_pn_with_w [log]

let log_to_pn_with_w_and_trailing_ww (log : alphabet array) =
  let length = Array.length log
  and {places = p; transitions = t; m0 = m} = log_to_pn_with_w log
  in {places = p;
      transitions = {pre = [length]; post = [length]; lambda = "ww"} :: t;
      m0 = m; mf=[]}


(* let valid_diff i j k diff_ijk lambda_ia jth_log d (\* minimum distance for misalignment *\) = *)
(*   (if Array.length jth_log >= i *)
(*    then (\* diff_ijk i j k => not lambda_ia i (nth logs j).(i-1) *\) *)
(*       Or([], [diff_ijk i j k; lambda_ia i jth_log.(i-1)], []) *)
(*    else And([], [], []) (\* true *\)) *)

let valid_diff i j k diff_ijk lambda_ia jth_log d (* minimum distance for misalignment *) =
  (* diff_ijk i j k =>
      ((not lambda_ia i jth_log.(i-1)) and
       ... and
       (not lambda_ia i jth_log.(i-1))) *)
  Or([], [diff_ijk i j k],
     [And([],
  	  list_of (max (i - d) 1) (min (i + d) (Array.length jth_log))
  	    (fun i' -> lambda_ia i jth_log.(i'-1)),
  	  [])])
  (* And([], [], *)
  (*     list_of (max (i - d) 1) (min (i + d) (Array.length jth_log)) *)
  (* 	(fun i' -> Or([], [diff_ijk i j k; lambda_ia i jth_log.(i'-1)], []))) *)

(* returns lambda_ia (useful for extraction of results) *)
(* formula is not quantified *)
let pn_and_logs_to_formula (pn : pn) (logs : alphabet array list) (sigma : alphabet list)
    (n : int) min_diffs d (* minimum distance for misalignment *) =
  let lengthP = length pn.places
  and lengthSigma = length sigma
  and nb_logs = length logs in
  let variables_per_i = lengthSigma + lengthP + nb_logs * min_diffs in
  let lambda_ia i a = i * variables_per_i + (pos a sigma)
  and m_ip i p = i * variables_per_i + lengthSigma + p
  and diff_ijk i j k = i * variables_per_i + lengthSigma + lengthP + j * min_diffs + k - 1
  and nb_vars = (n + 1) * variables_per_i
  in
  lambda_ia,
  nb_vars,
  And([], [],
      (is_run n pn m_ip lambda_ia sigma)
      :: (concat @@ list_of 0 (nb_logs - 1)
	    (fun j ->
	      let jth_log = (nth logs j) in
	      (concat @@ list_of 1 min_diffs
	      	 (fun k ->
		   (* at least one i such that diff_ijk *)
		   Or((list_of 1 n (fun i -> diff_ijk i j k)), [], [])
		   :: (concat @@ list_of 1 n
			 (fun i ->
			   (valid_diff i j k diff_ijk lambda_ia jth_log d)
			   :: (list_of (k + 1) min_diffs
				 (fun k' ->
				   (* no more than one k such that diff_ijk *)
				   Or([], [(diff_ijk i j k); (diff_ijk i j k')], []))))))))))


(* returns lambda_ia (useful for extraction of results) *)
let pn_and_logs_to_qpro out_file (pn : pn) (logs : alphabet array list) (sigma : alphabet list) (n : int) min_diffs =
  let lambda_ia, num_vars, formula = pn_and_logs_to_formula pn logs sigma n min_diffs 0
  and out_channel = open_out_bin out_file in
  qbf_to_qpro out_channel (Exists((list_of 1 num_vars (fun n -> n)), formula)) num_vars;
  close_out out_channel;
  lambda_ia
