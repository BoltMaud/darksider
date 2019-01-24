open List
open Utils
open Formulas
open Petri_nets
open PN_to_formula

(*----------------------------------------------------------------------------------*)
(* for each instant i, for each action a : *)
(* ( lamdba_ChiJia and not lamdba_jia) => diff_ji *)
(* not (lamdba_ChiJia and not lamdba_jia) v diff_ji *)
(* not lamdba_ChiJia v lamdba_jia v diff_ji *)
let get_diff_ia diff_i lamdbaj_ia lamdbak_ia size_of_run sigma =
And([], [],
(concat @@
 list_of 1 (size_of_run)
   (fun i -> (
      map (fun a -> (
             Or([lamdbaj_ia i a ; diff_i i],[(lamdbak_ia i a)],[])))
        sigma))))

(*----------------------------------------------------------------------------------*)
(* from a model and trances returns the SAT formula for subnet clustering, the constraints and a result format *)
(* j represents the jth trace @see traces*)
(* k represents the kth cluster @see nb_clusters  *)
(* i represents the ith instant @see the size_of_run *)
(* a represents the ath action @see sigma *)
(* chij represents the subnet s of the jth trace *)
let pn_and_traces_to_formula_fullRunClustering_pb
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
	let numberOfVariablesPerJ = nb_clusters + ((size_of_run+1)*(lenP+(length sigma))) + (size_of_run+1)*nb_clusters +1(*inC*) in
	let numberOfVariablesPerI = lenP+(length sigma) in

	(* for each trace j and each cluster k : is k the cluster of j ? *)
	let chi_jk j k = j*numberOfVariablesPerJ + k in

	(* for each place p, for each instant i of each trace j, is the place p marked at the instant i for j ? *)
	let m_jip j i p = j*numberOfVariablesPerJ + nb_clusters + i*numberOfVariablesPerI + p in

	(* for each trace j, is the action a enable at the instant i ? *)
	let lambda_jia j i a = j*numberOfVariablesPerJ + nb_clusters + i*numberOfVariablesPerI + lenP + (pos a sigma) in

	(* for each instant and each tracej, add true if it is distant to its centroid *)
	let diff_jki j k i = j*numberOfVariablesPerJ + nb_clusters + (size_of_run+1)*numberOfVariablesPerI + k*(size_of_run+1) + 
		     + i in 

	(*for each j add the variable that says if it's in a cluster or not*)
	let inC_j j = j*numberOfVariablesPerJ + nb_clusters + (size_of_run+1)*numberOfVariablesPerI +
		     + (size_of_run+1)*nb_clusters  in

	(* for each marking, true if in k *)
	let m_kip k i p = lenTraces*numberOfVariablesPerJ + i*(nb_clusters*(lenT+lenP)) + k*(lenT+lenP) + p in 

	(* for each lambda, true if in k *)
	let lambda_kia k i a = lenTraces*numberOfVariablesPerJ + i*(nb_clusters*(lenT+lenP)) + k*(lenT+lenP) + lenP +(pos a sigma) in  

	(* nb of vars *)
	let nb_vars = nb_clusters*lenTraces
		  + lenP*(size_of_run+1)*lenTraces
		  + (length sigma)*lenTraces*(size_of_run+1)
		  + nb_clusters*(size_of_run+1)*(lenT+lenP)
		  + lenTraces*(size_of_run+1)*nb_clusters + lenTraces  in

	nb_vars,
	And([],[],
	(*for every cluster*)
	  (concat @@ 
	    list_of 0 (nb_clusters-1)
	      (fun k -> 
		(*create a formula of the model *)
		[is_run size_of_run pn (m_kip k) (lambda_kia k) sigma ]
		 @
		(*create diff_jki if j is not in k*)
		list_of 0 (lenTraces -1)
		  (fun j->
		    Or([],[chi_jk j k], [get_diff_ia (diff_jki j k) (lambda_jia j) (lambda_kia k) size_of_run sigma])
		  )
	      )
	   )
	   @
	   (   (* for each trace, create the formula *)
	       list_of 0 (lenTraces - 1) 
	       	 (fun j ->
		   is_run size_of_run (nth traces j) (m_jip j) (lambda_jia j) sigma 
		 )
	   )),
	(* PB constraints *)
	(concat @@
	   list_of 0 (lenTraces - 1)
	     (fun j ->
	       (* each trace is in a cluster and only one or not *)
	       [PB_eq ((-1, (inC_j j)) :: (list_of 0 (nb_clusters-1) (fun k -> (1, (chi_jk j k)))), 0)]
	      @
	      (* sum of diff_ij *)
		 list_of 0 (nb_clusters-1)
	       	   (fun k -> PB_leq ((list_of 1 (size_of_run) (fun i -> (1, (diff_jki j k i)))), d)))),
	(* PB objective to be minimized *)
	(* minimize the number of traces that are not classified *)
	(list_of 0 (lenTraces -1)
		(fun j-> (-10, (inC_j j) ) )
	@
	(* minimize the number of diff*)
	(concat @@ (list_of 0 (nb_clusters-1)
		(fun k -> 
		(concat @@ (list_of 0 (lenTraces-1)
			     (fun j -> (list_of 1 (size_of_run) (fun i-> (1,diff_jki j k i) ))
			     ) 
			   )
		)))
	)),
	fun list_sol ->
	( (*Delta*)
		(List.fold_left (+) 0 
		(concat @@ (list_of 0 (nb_clusters - 1) 
			     (fun k->
				concat @@ (list_of 1 (size_of_run) 
                                  (fun i->
				     (list_of 0 (lenTraces-1) 
                                        (fun j ->
					    if (mem (diff_jki j k i) list_sol) then 1 else 0
					 )
				      )			
		))
		)))
		)		
	),
        list_of 0 (nb_clusters - 1)
		(fun k -> let traces = (map ( fun j -> 
				(map (fun t-> t.lambda) (nth traces j).transitions))
		          	(filter ( fun j -> (mem (chi_jk j k) list_sol))
			  	(listinit lenTraces []) )) in
			(*centroids*)
			(concat @@
				(list_of 1 (size_of_run) 
					(fun i -> 
					 (map (fun t-> t.lambda)
		                	 (filter (fun t->  t.lambda <> "ww" && mem (lambda_kia k i t.lambda ) list_sol)
		               		 pn.transitions))
		                )			
			), 
			(*traces*)
		 	traces,
			(*percentage*)
			float (length traces) /. float lenTraces    
		)
	),
	(
		(* Deviated traces *)
		let traces_nc = (map ( fun j -> (map (fun t-> t.lambda) (nth traces j).transitions))
						 (filter ( fun j -> not ((mem (inC_j j) list_sol)))
						 (listinit lenTraces []) )) in  
			traces_nc,
			(* percentage *)
			float (length traces_nc) /. float lenTraces
	)	
	;;
	 

