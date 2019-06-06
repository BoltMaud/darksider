open List
open Utils
open Formulas
open Petri_nets
open PN_to_formula
open Logs
open Printf
open Alignment_PB_newEditDistance


let antiAlignment
            ( pn : pn ) (* model, Petri Net *)
            ( traces : pn list ) (* one trace Pn structure *)
            ( sigma : alphabet list ) (* list of all the possible actions *)
            ( size_of_run : int ) (* n in others algorithm, limit the size of research *)
	    ( size_of_d_min_1 : int )=

    let size_of_d = size_of_d_min_1 +1 in 
    let lenP = fold_left max 0 (map (fun pn -> length pn.places) (pn :: traces)) in
    let lenT = length sigma in
    let lenTraces = length traces in 
    printf "\nTraces : ";
    iter (fun trace -> iter (fun p -> printf "%s " p.lambda ) trace.transitions; printf " - ";) traces ;
    printf "\nPN : ";
    iter (fun p -> printf "%s " p.lambda  ) pn.transitions ;
    printf "\nSize of Run :%d" size_of_run; 

    let lambdaM_ia i a = (i-1)*(lenT) + (pos a sigma)  in
    let mM_ip i p = ((size_of_run)*lenT) + i*(lenP) + p in

    let lambdaT_jia j i a = (size_of_run+1)*(lenP) + (size_of_run*lenT)+ j*((size_of_run)*lenT) + (i-1)*(lenT) + (pos a sigma) in
    let mT_jip j i p = (size_of_run+1)*(lenP) +(size_of_run*lenT)*(lenTraces+1) + j*((size_of_run+1)*lenP) + (i)*(lenP) + p in

    let diid j i i2 d=
     (* assert(0 <= i && 0 <= i2 && 0 <= d && i <= size_of_run && i2 <= size_of_run && d <= 2*size_of_run);*)
      (lenTraces+1)*(size_of_run+1)*(lenP) + (lenTraces+1)*size_of_run*(lenT) + 
	j * (size_of_run+1) * (size_of_run+1) * (size_of_d+1) +
	(i)*((size_of_run+1)*(size_of_d+1)) + (i2)*(size_of_d+1) + d in

    let num_vars = (lenTraces+1)*(size_of_run+1)*(lenP) + (lenTraces+1)*size_of_run*(lenT) +
		   lenTraces*(size_of_run+1) * (size_of_run+1) *(size_of_d+1)-1 in
    printf "\nnum_vars : %d \n" num_vars;

    (num_vars), 
    And([],
        [],
	[is_run size_of_run pn mM_ip lambdaM_ia sigma]
	@
	concat @@ (
		list_of 0 (lenTraces-1)
			(fun j ->
				[initialisation lambdaM_ia (lambdaT_jia j)  (diid j) sigma size_of_run size_of_d;
				is_run size_of_run (nth traces j) (mT_jip j) (lambdaT_jia j) sigma]
				@
				bodyOfSatEncodingEditDistance size_of_run (lambdaT_jia j) lambdaM_ia (diid j) sigma size_of_d	
			)
		)
	),
    [],
     (* we want to maximize the number of diid*) 
	concat @@      
	list_of 0 (lenTraces-1)
	  (fun j -> 
	     list_of 0 size_of_d
	     (fun d1 ->	 
		let size_of_trace =  float_of_int (length (nth traces j).transitions) in 
		((*(int_of_float (size_of_trace/.(1.+.0.5)**size_of_trace))*)(-1), diid j size_of_run size_of_run d1))
	 ),

    fun list_sol -> let max_dist = ref 0 in 
	 (* what is the minimal d of diid *)(*
	 for d1=0 to size_of_d do
		let everyJtoD = ref 0 in 
		for j=0 to (lenTraces-1) do 
			if mem (diid j size_of_run size_of_run d1) list_sol then everyJtoD:=!everyJtoD+1;
		if !everyJtoD == lenTraces then max_dist:=d1;
	 done;
	done;
*)
	for d1=0 to size_of_d do
	 for j=0 to (lenTraces-1) do 
	 	 if mem (diid j size_of_run size_of_run d1) list_sol then  max_dist:=d1;
	 done;
	done;
	printf "\nAnti-Alignment for d=%d : " !max_dist ;print_run stdout (extract_run list_sol lambdaM_ia sigma size_of_run);
