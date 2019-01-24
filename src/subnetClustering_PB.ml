open List
open Utils
open Formulas
open Petri_nets
open PN_to_formula
open Logs
open Printf


(* for each instant i, for each action a : *)
(* ( lamdba_ChiJia and not lamdba_jia) => diff_ji *)
(* not (lamdba_ChiJia and not lamdba_jia) v diff_ji *)
(* not lamdba_ChiJia v lamdba_jia v diff_ji *)
let get_diff_ia diff_i lamdba_jia lamdba_ChiJia size_of_run sigma =
  And([], [],
      (concat @@
         list_of 1 (size_of_run)
           (fun i -> (
              map (fun a -> (
                     Or([lamdba_jia i a ; diff_i i],[(lamdba_ChiJia i a)],[])))
                sigma))))


(* the transition t is added in one cluster,  *)
(* for each cluster, find the one of k and add t *)
let in_cluster_of_j t c_kt chi_k nb_clusters=
      And([],[],
          list_of 0 (nb_clusters-1)
            (fun k -> Or([c_kt k t],[chi_k k],[]))
        );;


(* if an action has a transition in pn then the transition must be added in centroid of j *)
(* lamdba_ia => is_transition(t) and  is_transition(t) => in_cluster_of_j(t,j,k) *)
let is_action_subnet pn
          (m1_p : place -> int)
          (m2_p : place -> int)
          (lambda_a : alphabet -> int)
          c_kt
          chi_k
          nb_clusters
          (sigma : alphabet list) =
  And ([], [],
       (Or(map lambda_a sigma, [], []))
       ::
      (map (fun a ->
           And([],[],
               (* if the action is a, then a transition must be true *)
                Or([], [lambda_a a],
                  map (fun t -> And([], [],
                                    [is_transition t pn m1_p m2_p;
                                     in_cluster_of_j t c_kt chi_k nb_clusters]))
                    (filter (fun t -> t.lambda = a)
                       pn.transitions))::[]))
            sigma));;


(* the difference between the other is_run function is in the action *)
let is_run_subnet size_of_run pn m_ip lambda_ia c_kt chi_jk nb_clusters sigma =
  And(map (m_ip 0) pn.m0,
      map (m_ip 0) (set_minus pn.places pn.m0),
      list_of 1 (size_of_run)
	(fun i ->
	  is_action_subnet pn
	    (m_ip (i - 1))
	    (m_ip i)
	    (lambda_ia i) c_kt chi_jk nb_clusters sigma)
    );;


let rec listinit last l=
	if last==0 then l else (listinit (last-1) ((last-1)::l));;


(* from a model and trances returns the SAT formula for subnet clustering, the constraints and a result format *)
(* j represents the jth trace @see traces*)
(* k represents the kth cluster @see nb_clusters  *)
(* i represents the ith instant @see the size_of_run *)
(* a represents the ath action @see sigma *)
(* chij represents the subnet s of the jth trace *)
let pn_and_traces_to_formula_subnetClustering_pb
            ( pn : pn ) (* model, Petri Net *)
            ( traces : pn list ) (* list of traces previously transformed in full runs with w *)
            ( sigma : alphabet list ) (* list of all the possible actions *)
            ( nb_clusters : int ) (* number of clusters *)
            ( size_of_run : int ) (* n in others algorithm, limit the size of research *)
            ( d : int ) (* minimal distance of the traces to the logs *)
            ( nb_transitions_per_cluster : int ) (* threshold max for number of transitions *) =

    let lenP = fold_left max 0 (map (fun pn -> length pn.places) (pn :: traces)) in
    let lenT = length pn.transitions in
    let lenTraces = length traces in
    let numberOfVariablesPerJ = nb_clusters + 2*((size_of_run+1)*(lenP+(length sigma))) + (size_of_run+1) +1(*inC*) in
    let numberOfVariablesPerI = lenP+(length sigma) in

    (* for each trace j and each cluster k : is k the cluster of j ? *)
    let chi_jk j k = j*numberOfVariablesPerJ + k in

    (* for each place p, for each instant i of each trace j, is the place p marked at the instant i for j ? *)
    let m_jip j i p = j*numberOfVariablesPerJ + nb_clusters + i*numberOfVariablesPerI + p in

    (* for each trace j, is the action a enable at the instant i ? *)
    let lambda_jia j i a = j*numberOfVariablesPerJ + nb_clusters + i*numberOfVariablesPerI + lenP + (pos a sigma) in

    (* for each cluster ChiJ of each trace j, is the place p marked at the instant i ? *)
    let m_ChiJip chiJ i p = chiJ *numberOfVariablesPerJ + nb_clusters + (size_of_run+1)*numberOfVariablesPerI +
                            i*numberOfVariablesPerI + p in

    (* for each cluster ChiJ of J, is the action a enable at instant i ? *)
    let lambda_ChiJia chiJ i a = chiJ *numberOfVariablesPerJ + nb_clusters + (size_of_run+1)*numberOfVariablesPerI +
                            i*numberOfVariablesPerI + lenP + (pos a sigma) in

    (* for each instant and each tracej, add true if it is distant to its centroid *)
    let diff_ji j i= j*numberOfVariablesPerJ + nb_clusters + 2*(size_of_run+1)*numberOfVariablesPerI +
                     + i in

    (* for each transition t, true if exist in cluster k *)
    let c_kt k t= lenTraces*numberOfVariablesPerJ + k*lenT + (pos t pn.transitions) in

    (*for each j add the variable that says if it's in a cluster or not*)
    let inC_j j = j*numberOfVariablesPerJ + nb_clusters + 2*(size_of_run+1)*numberOfVariablesPerI +
                     + (size_of_run+1)  in
	
    (*k1 and k2 are clusters and share the transition t if common_kkt is true*)
    let common_kkt k1 k2 t = nb_clusters*lenTraces
				  + 2*lenP*(size_of_run+1)*lenTraces
				  + 2*(length sigma)*lenTraces*(size_of_run+1)
				  + nb_clusters*lenT
				  + lenTraces*(size_of_run+1) + lenTraces
			          + k1*(lenT)*nb_clusters + k2*lenT + (pos t pn.transitions) in 
    (*need the nb of variables for SAT solver*)
    let nb_vars = nb_clusters*lenTraces
                  + 2*lenP*(size_of_run+1)*lenTraces
                  + 2*(length sigma)*lenTraces*(size_of_run+1)
                  + nb_clusters*lenT
                  + lenTraces*(size_of_run+1) + lenTraces 
		  + nb_clusters*nb_clusters*lenT in

    (*nb_vars, formulas, constraints, minimize, return*)
    nb_vars,
    And([],[],
        (*  for every trace *)
          (concat @@
          list_of 0 (lenTraces - 1)
            (fun j ->
	      (* create the pn of the traces *)
              [is_run size_of_run (nth traces j) (m_jip j) (lambda_jia j) sigma ;

               (* create its centroid formula if trace is in a cluster (not or) *)
               Or([], [inC_j j], [is_run_subnet size_of_run pn (m_ChiJip j) (lambda_ChiJia j) c_kt (chi_jk j) nb_clusters sigma]) ;

               (* get the diff between them at each i *)
               get_diff_ia  (diff_ji j) (lambda_jia j) (lambda_ChiJia j) size_of_run sigma ]))
    @
    (* transitions in common of centroids *)
    (concat @@
       list_of 0 (nb_clusters - 1)
	 (fun k1 ->
	  (concat @@ 
	        list_of k1 (nb_clusters - 1)
		(fun k2 -> 
			(*if t in k1 and t in k2 then common_k1k2t*)
			map (fun t -> Or([common_kkt k1 k2 t],[],[Or([],[c_kt k1 t;c_kt k2 t],[])])) pn.transitions
		)
 	  )))
	),
    (* PB constraints *)
    (concat @@
        list_of 0 (lenTraces - 1)
         ( fun j ->
            (* each transition is in a cluster and only one or not *)
            [PB_eq ((-1, (inC_j j)) :: (list_of 0 (nb_clusters-1) (fun k -> (1, (chi_jk j k)))), 0);

            (* sum of diff_ij *)
            PB_leq ((list_of 0 (size_of_run) (fun i -> (1, (diff_ji j i)))), d)]))
	@
 	(* max number of t per c_kt*)
        (list_of 0 (nb_clusters-1)
           (fun k -> PB_leq ((map (fun t -> (1, (c_kt k t))) pn.transitions), nb_transitions_per_cluster))), 

    (* PB objective to be minimized *)
    (concat @@
       (* minimize the number of common transitions *)
       list_of 0 (nb_clusters - 1)
         (fun k1 ->
	  (concat @@ 
                list_of k1 (nb_clusters - 1)
		(fun k2 -> 
			map (fun t -> (1, common_kkt k1 k2 t)) pn.transitions
		)
 	  )
	))
	@
	(* minimize the number of traces that are not classified *)
	(list_of 0 (lenTraces -1)
		(fun j-> (-10, (inC_j j) ) ))
	@
	(*minimize the number of diff*)
	(concat @@ 
		(list_of 0 (lenTraces -1)
		(fun j-> 
			(list_of 1 size_of_run (fun i-> (1,diff_ji j i)))
		))
	),
    fun list_sol -> 
    (( 	(*Delta*)
	(List.fold_left (+) 0 
		(concat @@ (list_of 1 (size_of_run) 
			(fun i->
			     (list_of 0 (lenTraces-1) 
		                (fun j ->
				    if (mem (diff_ji j i) list_sol) then 1 else 0
				 )
	)))))
    ),
    (list_of 0 (nb_clusters - 1)
      (fun k -> let traces = (map ( fun j -> (map (fun t-> t.lambda) (nth traces j).transitions)) 
				(filter ( fun j -> (mem (chi_jk j k) list_sol))
		  		(listinit lenTraces []) )) in
		(* centroids *)
                ((map (fun t -> t.lambda) 
                   (filter (fun t -> t.lambda <> "ww" && (mem (c_kt k t) list_sol))
                      pn.transitions)), 
		 (* [trace1 ...tracen] *)
	        traces,
		 (* percentage *)
		 (float (length traces) /. float lenTraces)
		 )
	)),
        (
	(* Deviated traces *)
	((map ( fun j -> 
		(map (fun t-> t.lambda)
		     (nth traces j).transitions))
        (filter ( fun j -> not ((mem (inC_j j) list_sol)))
	(listinit lenTraces []) )),
	(* percentage *)
        (float (length (map ( fun j -> 
		(map (fun t-> t.lambda)
		     (nth traces j).transitions))
        (filter ( fun j -> not ((mem (inC_j j) list_sol)))
	(listinit lenTraces []) ))) /.  float lenTraces)
	)
        )	
	);;

