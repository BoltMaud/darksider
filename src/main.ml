open List
open Array
open String
open Utils
open Formulas
open Petri_nets
open PN_to_formula
open Logs
open Printf
open Solve_formulas_PB
open Alignment_PB
open FullrunClustering_PB
open SubnetClustering_PB
open Alignment_PB_newEditDistance
open Anti_alignment_PB_newEditDistance;;

(*------------------------------------------------------------*)
(* index to get argvs *)
let i = ref 1 ;;

(*-------------------------------------------------------------*)
(* if -p in argvs then move index i*)
(*if (Array.mem "-p" Sys.argv) then 
				begin pb_solver:=Sys.argv.(2) ; 
				      i:=3 ; 
				end
			     else () ;;*)

(*-------------------------------------------------------------*)
(*
	-FC : full run clustering
	-SC : subnet clustering
	-MA : multi-alignments
	-SA : subnet-alignments
	-help : afficher les commandes
*)

match Sys.argv.(!i) with 
|"-FC" -> 
	let tpn_file = Sys.argv.(!i+1)
	and tr_file = Sys.argv.(!i+2)
	and d = int_of_string Sys.argv.(!i+3)
	and nb_clusters = int_of_string Sys.argv.(!i+4)
	and out_file = Sys.argv.(!i+5) in 
	solve_clustering_and_save "full run" (add_ww (read_pn tpn_file)) (read_logs tr_file) d nb_clusters 0 out_file

|"-SC" -> 
	let tpn_file = Sys.argv.(!i+1)
	and tr_file = Sys.argv.(!i+2)
	and d = int_of_string Sys.argv.(!i+3)
	and nb_clusters = int_of_string Sys.argv.(!i+4)
	and nb_transitions = int_of_string Sys.argv.(!i+5)
	and out_file = Sys.argv.(!i+6) in 
	solve_clustering_and_save "subnet" (add_ww (read_pn tpn_file)) (read_logs tr_file) d nb_clusters nb_transitions out_file

|"-MA" -> 
	let tpn_file = Sys.argv.(!i+1) 
	and tr_file = Sys.argv.(!i+2) 
	and d = int_of_string Sys.argv.(!i+3) in 
	let _, _, _, _ = optimal_multialignment_pns (add_ww (read_pn tpn_file)) (List.map log_to_pn_with_w (read_logs tr_file)) d in
	() (* TODO write in file *)

|"-eD" -> 
	let tpn_file = Sys.argv.(!i+1) 
	and tr_file = Sys.argv.(!i+2)
	and divide_n_by =  int_of_string Sys.argv.(!i+3)  
	and size_of_d =  int_of_string Sys.argv.(!i+4) in 
	solve_alignment_edit_distance (read_pn tpn_file) (read_logs tr_file) newEditDistanceForAlignment divide_n_by size_of_d

|"-AA" -> 
	let tpn_file = Sys.argv.(!i+1) 
	and tr_file = Sys.argv.(!i+2)
	and divide_n_by =  int_of_string Sys.argv.(!i+3)  
	and size_of_d =  int_of_string Sys.argv.(!i+4) in
	solve_alignment_edit_distance (read_pn tpn_file) (read_logs tr_file) antiAlignment divide_n_by size_of_d

|"-SA" -> printf "TODO"

|("-help" | _ )  ->
	printf "-FC [options] [parameters] : Full Run Clustering  
	<file.tpn> : petri net 
	<file.tr> : traces 
	<d> : distance max between traces and centroids
	<nb_clusters> : number maximal of clusters 
	<out_file> : file to store the result \n";
	printf "-SC [options] [parameters] : Subnet Clustering  
	<file.tpn> : petri net 
	<file.tr> : traces 
	<d> : distance max between traces and centroids
	<nb_clusters> : number maximal of clusters 
	<max_nb_transitions> : maximum number of transitions per centroid
	<out_file> : file to store the result \n";
	(*printf "-MA [options] [parameters] : Multialigner 
	<file.tpn> : petri net 
	<file.tr> : traces 
	<d> : distance max between traces and full run\n";*)
	printf "-eD [options] [parameters] : Alignment with SAT edit distance 
	<file.tpn> : petri net 
	<file.tr> : traces";
	printf "\n-AA [options] [parameters] : Anti-alignment with SAT edit distance 
	<file.tpn> : petri net 
	<file.tr> : traces";
	printf "\n-p <solver cmd>: option to change PB solver (tested for sat4j and minisat+) \n";
	printf "-help : display this awesome list\n";
	








 

