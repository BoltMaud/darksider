open List
open Utils
open Petri_nets
open Formulas
open Printf


let is_transition t pn
		  (m1_p : place -> int) (* number of variable associated to place p in marking m1 *)
		  (m2_p : place -> int) (* number of variable associated to place p in marking m2 *) =
  And([], [],
      map (fun p ->
	   match (mem p t.pre, mem p t.post) with
	   | true, false -> And([m1_p p], [m2_p p], [])
	   | false, true -> And([m2_p p], [m1_p p], [])
	   | true, true -> And([m2_p p; m1_p p], [], [])
	   | false, false -> Or([], [], [And([m1_p p; m2_p p], [], []);
					 And([], [m1_p p; m2_p p], [])]))
	  pn.places)

let is_action pn
	      (m1_p : place -> int) (* number of variable associated to place p in marking m1 *)
	      (m2_p : place -> int) (* number of variable associated to place p in marking m2 *)
	      (lambda_a : alphabet -> int) (* number of variable associated to action a *)
	      (sigma : alphabet list) =
  And ([], [],
       (Or(map lambda_a sigma, [], []))
       ::
       	 (* (\* mutual exclusion, to help the solver -> does not seem to help *\) *)
	 (* 	 (concat (map (fun a -> map (fun b -> Or([], [lambda_a a; lambda_a b], [])) *)
	 (* 	 			    (set_minus sigma [a])) *)
	 (* 	 	      sigma)) *)
	 (* @ *)
	 (map (fun a ->
	      Or([], [lambda_a a],
		 map (fun t -> is_transition t pn m1_p m2_p)
		   (filter (fun t -> t.lambda = a)
		      pn.transitions)))
	    sigma))

let is_run n pn
	   (m_ip : int -> place -> int)
	   (lambda_ia : int -> alphabet -> int)
	   (sigma : alphabet list) =
  
  And((map (m_ip 0) pn.m0) @ (map (m_ip n) pn.mf),
      (map (m_ip 0) (set_minus pn.places pn.m0)),
      list_of 1 n
	(fun i -> 
	  is_action pn
	    (m_ip (i - 1))
	    (m_ip i)
	    (lambda_ia i) sigma))

(* here full run is understood as reaching empty marking *)
let is_full_run n pn
	   (m_ip : int -> place -> int)
	   (lambda_ia : int -> alphabet -> int)
	   (sigma : alphabet list) =
  And([],
      map (m_ip n) pn.places,
      [is_run n pn m_ip lambda_ia sigma])
