open List
open Utils

(* pseudo-boolean inequalities are treated separately *)
type pb_constraint =
  | PB_leq of (int * int) list * int
  | PB_geq of (int * int) list * int
  | PB_eq of (int * int) list * int

type pb_objective = (int * int) list


type qbf_formula = And of int list * int list * qbf_formula list
		 | Or of int list * int list * qbf_formula list
		 (* (\* pseudo-boolean inequality *)
		 (*    PB_leq([(2, 1); (-1, 2)], 1) means 2 x1 -1 x2 <= 1 (see the OPB format for pseudo-boolean constraints) *\) *)
		 (* | PB_leq of (int * int) list * int *)
		 | Exists of int list * qbf_formula
		 | Forall of int list * qbf_formula

(* essentially for debugging *)
let rec formula_size = function
  | And(l_pos, l_neg, l_formulas) ->
     1 + (length l_neg) + (length l_pos)
     + (fold_left (+) 0 (map formula_size l_formulas))
  | Or(l_pos, l_neg, l_formulas) ->
     1 + (length l_neg) + (length l_pos)
     + (fold_left (+) 0 (map formula_size l_formulas))
  | Exists(vars, formula) -> (length vars) + (formula_size formula)
  | Forall(vars, formula) -> (length vars) + (formula_size formula)

let rec simplify = function
  | And([], [], [f]) -> simplify f
  | Or([], [], [f]) -> simplify f
  | And(l_pos, l_neg, [And(l_pos2, l_neg2, l_formulas2)]) ->
     simplify (And((l_pos @ l_pos2), (l_neg @ l_neg2), (map simplify l_formulas2)))
  | Or(l_pos, l_neg, [Or(l_pos2, l_neg2, l_formulas2)]) ->
     simplify (Or((l_pos @ l_pos2), (l_neg @ l_neg2), (map simplify l_formulas2)))
  | And(l_pos, l_neg, l_formulas) -> And(l_neg, l_pos, (map simplify l_formulas))
  | Or(l_pos, l_neg, l_formulas) -> Or(l_neg, l_pos, (map simplify l_formulas))
  | Exists(vars, formula) -> Exists (vars, (simplify formula))
  | Forall(vars, formula) -> Forall (vars, (simplify formula))


let rec negation = function
  | And(l_pos, l_neg, l_formulas) -> Or(l_neg, l_pos, (map negation l_formulas))
  | Or(l_pos, l_neg, l_formulas) -> And(l_neg, l_pos, (map negation l_formulas))
  (* | PB_leq(l, c) -> PB_leq((map (fun (n, num_var) -> (-n, num_var)) l), -c-1) *)
  | Exists(vars, formula) -> Forall (vars, (negation formula))
  | Forall(vars, formula) -> Exists (vars, (negation formula))


let rec output_Or_or_And (oc : out_channel) l_pos l_neg l_formulas (mark : string) =
  output_string oc mark; output_newline oc;
  output_int_list oc l_pos; output_newline oc;
  output_int_list oc l_neg; output_newline oc;
  iter (qbf_to_qpro_body oc) l_formulas;
  output_string oc "/"; output_string oc mark; output_newline oc

and output_Exists_or_Forall (oc : out_channel) vars formula (mark : string) =
  output_string oc "q\n"; output_string oc mark; output_string oc " ";
  output_int_list oc vars; output_newline oc;
  qbf_to_qpro_body oc formula;
  output_string oc "/q\n"

and qbf_to_qpro_body (oc : out_channel) = function
  | And(l_pos, l_neg, l_formulas) -> output_Or_or_And oc l_pos l_neg l_formulas "c"
  | Or(l_pos, l_neg, l_formulas) -> output_Or_or_And oc l_pos l_neg l_formulas "d"
  (* | PB_leq(l, c) -> failwith "attempt to output pseudo-boolean constraint in qpro" *)
  | Exists(vars, formula) -> output_Exists_or_Forall oc vars formula "e"
  | Forall(vars, formula) -> output_Exists_or_Forall oc vars formula "a"

and qbf_to_qpro (oc : out_channel) formula num_vars =
  output_string oc "QBF\n";
  output_int oc num_vars; output_newline oc;
  qbf_to_qpro_body oc formula



(* formula has no quantification *)
type cnf_formula = (int list * int list) list

(* rewrites the implication var => formula *)
(* formula has no quantification *)
let distribute_implication var formula =
  match formula with
  | Or(l_pos, l_neg, l_formulas) -> Or(l_pos, var::l_neg, l_formulas)
  | And(l_pos, l_neg, l_formulas) ->
     And([], [],
	 (map (fun v -> Or([v], [var], [])) l_pos)
	 @ (map (fun v -> Or([], [var; v], [])) l_neg)
	 @ (map (fun f -> Or([], [var], [f])) l_formulas))
  (* | PB_leq(l, c) -> failwith "pseudo-boolean constraint in call to distribute_implication" *)
  | _ -> failwith "quantifier in call to distribute_implication"


(* formula has no quantification *)
(* qbf_to_cnf is called with num_vars initialized to the number of variables in the formula,
   and increments it when it adds intermediate variables *)
let rec qbf_to_cnf (num_vars : int ref) formula =
  let result =
    match formula with
    | And(l_pos, l_neg, l_formulas) ->
       (map (fun v -> ([v], [])) l_pos)
       @ (map (fun v -> ([], [v])) l_neg)
       @ (concat @@ map (qbf_to_cnf num_vars) l_formulas)
    | Or(l_pos, l_neg, l_formulas) ->
     (match l_formulas with
     | [Or([], [], l)] ->
	(* this case could be treated like the general case,
	   matching it is just an optimization *)
	qbf_to_cnf num_vars (Or(l_pos, l_neg, l))
     | _ ->
     (* v1 or ... or -v'1 or ... or f1 or ... or f[length l_formulas] *)
     (* becomes *)
     (* (v1 or ... or -v'1 or ... or v[num_vars+1] or ... or v[num_vars + 1 + length l_formulas]) *)
     (* and (v[num_vars+1] => f1) and ... and (v[num_vars+1+length l_formulas] => f[length l_formulas]) *)
	let old_num_vars = !num_vars
	and length_l_formulas = length l_formulas in
	num_vars := old_num_vars + length_l_formulas;
	(l_pos @ (list_of 1 length_l_formulas (fun i -> old_num_vars + i)),
	 l_neg)
	:: (qbf_to_cnf num_vars
	      (And([], [],
		   (mapi (fun i f -> distribute_implication (old_num_vars + 1 + i) f)
		      l_formulas)))))
    | _ -> failwith "quantifier in call to qbf_to_cnf" in
  (* let result_size = fold_left (+) 0 (map (function (l1, l2) -> (length l1) + (length l2)) result) *)
  (* and formula_size_f = formula_size formula in *)
  (* print_int formula_size_f; print_newline(); *)
  (* print_int result_size; print_newline(); *)
  (* print_string "ratio: "; *)
  (* print_int (10 * result_size / formula_size_f); *)
  (* print_newline(); *)
  result
  (* | PB_leq(l, c) -> failwith "pseudo-boolean constraint in call to qbf_to_cnf" *)


(* qbf_to_cnf (ref 3) (Or([1], [2], [And([3; 1], [2], [])])) *)

(* slightly optimized version *)
(* let rec qbf_to_cnf (num_vars : int ref) formula = *)
(*   match formula with *)
(*   | And(l_pos, l_neg, l_formulas) -> *)
(*      (map (fun v -> ([v], [])) l_pos) *)
(*      @ (map (fun v -> ([], [v])) l_neg) *)
(*      @ (concat @@ map (qbf_to_cnf num_vars) l_formulas) *)
(*   | Or(l_pos, l_neg, l_formulas) -> *)
(*      (match l_formulas with *)
(*      | [Or([], [], l)] -> qbf_to_cnf num_vars (Or(l_pos, l_neg, l)) *)
(*      | _ -> *)
(*      (\* v1 or ... or -v'1 or ... or f1 or ... or f[length l_formulas] *\) *)
(*      (\* becomes *\) *)
(*      (\* (v1 or ... or -v'1 or ... or v[num_vars+1] or ... or v[num_vars + 1 + length l_formulas]) *\) *)
(*      (\* and (v[num_vars+1] => f1) and ... and (v[num_vars+1+length l_formulas] => f[length l_formulas]) *\) *)
(*      let old_num_vars = !num_vars *)
(*      and length_l_formulas = length l_formulas in *)
(*      print_int (length l_pos); print_string " "; *)
(*      print_int (length l_neg); print_string " "; *)
(*      print_int length_l_formulas; print_newline(); *)
(*      num_vars := old_num_vars + length_l_formulas; *)
(*      (l_pos @ (list_of 1 length_l_formulas (fun i -> old_num_vars + i)), *)
(*       l_neg) *)
(*      :: (qbf_to_cnf num_vars *)
(* 	   (And([], [], *)
(* 		(mapi (fun i f -> distribute_implication (old_num_vars + 1 + i) f) *)
(* 		   l_formulas)))) *)
(*   (\* | PB_leq(l, c) -> failwith "pseudo-boolean constraint in call to qbf_to_cnf" *\) *)
(*      ) *)
(*   | _ -> failwith "quantifier in call to qbf_to_cnf" *)



let cnf_to_dimacs dimacs_file (f : cnf_formula) num_vars =
  let oc = open_out_bin dimacs_file in
  Printf.fprintf oc "p cnf %d %d\n" num_vars (length f);
  iter
    (function (l_pos, l_neg) ->
      output_int_list oc l_pos;
      output_int_list oc (map (( * ) (-1)) l_neg);
      output_int oc 0;
      output_newline oc)
    f;
  close_out oc


let cnf_to_pb (f : cnf_formula) =
  map
    (function (l_pos, l_neg) ->
      PB_geq((map (fun num_var -> (1, num_var)) l_pos)
	     @ (map (fun num_var -> (-1, num_var)) l_neg),
	     1 - (length l_neg)))
    f


let pb_formula_to_opb opb_file (f : pb_constraint list) (min_objective : pb_objective) num_vars pb_solver=
  let oc = open_out_bin opb_file in
  let output_left_part l =
    iter (function (n, num_var) -> Printf.fprintf oc "%d x%d " n (if pb_solver <> "minisat+" then (num_var+1) else num_var))
      l in
  Printf.fprintf oc "* #variable= %d #constraint= %d\n" (if pb_solver <> "minisat+" then (num_vars+1) else num_vars) (length f);
  output_string oc "min: ";
  output_left_part min_objective;
  output_string oc ";\n";
  iter
    (function
    | PB_leq(l, c) ->
       (output_left_part l;
	Printf.fprintf oc "<= %d;\n" c)
    | PB_eq(l, c) ->
       (output_left_part l;
	Printf.fprintf oc "= %d;\n" c)
    | PB_geq(l, c) ->
       (output_left_part l;
	Printf.fprintf oc ">= %d;\n" c))
    f;
  close_out oc
