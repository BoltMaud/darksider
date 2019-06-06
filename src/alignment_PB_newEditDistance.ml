open List
open Utils
open Formulas
open Petri_nets
open PN_to_formula
open Logs
open Printf


let initialisation lambdaM_ia lambdaT_ia diid sigma size_of_run size_of_d=
	And(concat @@
	        list_of 0 size_of_run
	          (fun i1 ->
	            list_of 0 size_of_run
	              (fun i2->
	        	(diid i1 i2 0);
	              )
	          ),
	      list_of 1 (size_of_d)
	        (fun d-> diid 0 0 d ),
	  concat @@ 
	  list_of 0 (size_of_d-1)
	        (fun d1-> 
			(concat @@
			(list_of 0 (size_of_run-1)
				(fun i1 -> 
				(* ui1 <> w => di1+1,O,d+1 <=> di1,O,d *)
				 [Or([(lambdaM_ia (i1+1) "w")],[],[
					Or([],[],[
						And([(diid i1 0 (d1));(diid (i1+1) 0 (d1+1))],[],[]);
						And([],[(diid i1 0 (d1) );(diid (i1+1) 0 (d1+1))],[]);
				   	]);
				 ]);
				(* ui1 == w => di1+1,O,d <=> di1,O,d *)
 				Or([],[(lambdaM_ia (i1+1) "w")],[
					Or([],[],[
						And([(diid i1 0 (d1));(diid (i1+1) 0 (d1))],[],[]);
						And([],[(diid i1 0 (d1) );(diid (i1+1) 0 (d1))],[]);
				   	]);
				 ]);]				 
			  ) ))
			  @
			  (concat @@
			  (list_of 0 (size_of_run-1)
				(fun i2 -> 
					[Or([(lambdaT_ia (i2+1) "w")],[],[
						 Or([],[],[
							And([(diid  0 i2 (d1));(diid 0 (i2+1) (d1+1))],[],[]);
							And([],[(diid 0 i2 (d1));(diid 0 (i2+1) (d1+1))],[]);
						   ]);
					]);
					(* ui2 == w => d0,i2+1,d <=> d0,i2+1,d *)
					Or([],[(lambdaT_ia (i2+1) "w")],[
						 Or([],[],[
							And([(diid  0 i2 (d1));(diid 0 (i2+1) (d1))],[],[]);
							And([],[(diid 0 i2 (d1));(diid 0 (i2+1) (d1))],[]);
						   ]);
					]);]
				)))
			
		)
	  )



let bodyOfSatEncodingEditDistance size_of_run lambdaT_ia lambdaM_ia diid sigma (size_of_d)=
 (concat @@
	     list_of 0 (size_of_run-1)
	       (fun i1 ->
	         (concat @@
		    list_of 0 (size_of_run-1)
		      (fun i2 ->
		        (concat @@
			   list_of 0 (size_of_d-1)
			     (fun d1 ->	       
				[ 
                                 (* ui+1=uj+1 => (di+1i+1d <=> di+1i+1d *)
			         Or([],[],[
				        And([],[],map (fun a-> Or([],[(lambdaT_ia (i2+1) a); (lambdaM_ia (i1+1) a)  ],[])) sigma);
				        Or([],[],[
				               And([(diid i1 i2 (d1));(diid (i1+1) (i2+1) (d1))],[],[]);
				               And([],[(diid i1 i2 (d1));(diid (i1+1) (i2+1) (d1))],[])
				          ])
			           ]);
			         (* ui+1<>uj+1 => (di+1j+1d+1 <=> (di+1jd et dij+1d)) *)
			         Or([],[],[
				        Or([(lambdaM_ia (i1+1) "w");(lambdaT_ia (i2+1) "w")],[],map (fun a-> And([(lambdaT_ia (i2+1) a); (lambdaM_ia (i1+1) a)],[],[]))  sigma );
				        Or([],[],[
					       And([],[(diid (i1+1) (i2+1) (d1+1))], [Or([],[(diid (i1+1) i2 ((d1))) ;(diid i1 (i2+1) ((d1)))],[]) ]);
					       And([(diid (i1+1) (i2+1) (d1+1));(diid (i1+1) i2 ((d1)));(diid i1 (i2+1) ((d1)))],[],[])
				          ])
			           ]);

				(* (u2 == w & u1 <> w) => (di1+1,i2+1,d <=> di1+1,i2,d)*)				
				Or([(lambdaM_ia (i1+1) "w")],[(lambdaT_ia (i2+1) "w")],[
					Or([],[],[
					       And([(diid (i1+1) (i2+1) (d1));(diid (i1+1) (i2) (d1))],[],[]);
					       And([],[(diid (i1+1) (i2+1) (d1));(diid (i1+1) (i2) (d1))],[])
					  ])
				   ]);
				(* (u2 == w & u1 <> w) => (di1+1,i2+1,d <=> di1,i2+1,d)*)				
				Or([(lambdaT_ia (i2+1) "w")],[(lambdaM_ia (i1+1) "w")],[
					Or([],[],[
					       And([(diid (i1+1) (i2+1) (d1));(diid (i1) (i2+1) (d1))],[],[]);
					       And([],[(diid (i1+1) (i2+1) (d1));(diid (i1) (i2+1) (d1))],[])
					  ])
				   ])
				]  
			     )
		        )
		      )
	       ))
      )

let newEditDistanceForAlignment
            ( pn : pn ) (* model, Petri Net *)
            ( traces : pn list ) (* one trace Pn structure *)
            ( sigma : alphabet list ) (* list of all the possible actions *)
            ( size_of_run : int ) (* n in others algorithm, limit the size of research *) 
	    ( size_of_d_min_1 : int )=
    let size_of_d = size_of_d_min_1 + 1 in 
    let trace = (nth traces 0) in
    printf "Trace : ";
    iter (fun p -> printf "%s " p.lambda ; ) trace.transitions ;
    printf "\nPN : ";
    iter (fun p -> printf "%s " p.lambda ; ) pn.transitions ;
    printf "\nSize of Run :%d" size_of_run; 
    printf "\nNumber of editions : %d" size_of_d;
    let lenP = fold_left max 0 (map (fun pn -> length pn.places) (pn :: traces)) in

    let lenT = length sigma in
    let lambdaM_ia i a = (i-1)*(lenT) + (pos a sigma)  in
    let mM_ip i p = ((size_of_run)*lenT) + i*(lenP) + p in

    let lambdaT_ia i a = (size_of_run+1)*(lenP) + (size_of_run*lenT) + (i-1)*(lenT) + (pos a sigma) in
    let mT_ip i p = (size_of_run+1)*(lenP) +(size_of_run*lenT)*2 + (i)*(lenP) + p in

    let diid i i2 d=
     assert(0 <= i && 0 <= i2 && 0 <= d && i <= size_of_run && i2 <= size_of_run && d <= 2*size_of_run);
      2*(size_of_run+1)*(lenP) + 2*size_of_run*(lenT) + (i)*((size_of_run+1)*(size_of_d+1)) + (i2)*(size_of_d+1) + d in

    let num_vars = (size_of_run+1)*(size_of_run+1)*(size_of_d+1) + 2*(size_of_run+1)*(lenP) + 2*size_of_run*(lenT)-1 in

   (*nb_vars, formulas, constraints, minimize, return*)
    (num_vars),
    And([],
        [],
	[is_run size_of_run pn mM_ip lambdaM_ia sigma ;
	 is_run size_of_run trace mT_ip lambdaT_ia sigma ;
	 initialisation lambdaM_ia lambdaT_ia diid sigma size_of_run size_of_d]
	@
	 bodyOfSatEncodingEditDistance size_of_run lambdaT_ia lambdaM_ia diid sigma size_of_d
	),
    [],
    (* we want to minimize the number of diid*)  
     list_of 0 (size_of_d)
     (fun d1 ->	 
	(1, diid size_of_run size_of_run d1)),

    fun list_sol -> let max_dist = ref 0 in 
	 (* what is the minimal d of diid *)
	 for d1=0 to (size_of_d) do
		if mem (diid size_of_run size_of_run d1) list_sol then max_dist:=d1;
	 done;
	printf "\nOptimal alignment is d=%d : " !max_dist ;print_run stdout (extract_run list_sol lambdaM_ia sigma size_of_run);
printf "Trace : ";print_run stdout (extract_run list_sol lambdaT_ia sigma size_of_run);

