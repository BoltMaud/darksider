>Author : Boltenhagen Mathilde, Chatain Thomas, Josep Carmona <br>
>Date : 07.2019

## INTRODUCTION 

This project implements _Process Mining_ algorithms with a _SAT encoding_ for optimal results and _verification_. SAT formulas of algorithms are created with ocaml programs and solved by a SAT solver. 

## ENVIRONNEMENT 

- minisat+ or any other PB solver. Darksider was also tried with sat4j
- ocaml

## INSTALLATION

To compile the files, just launch the next command : 

 `make `

## AVAILABLES COMMANDS  

### Full run clustering  

Presented in [TODO], full run clustering returns clusters with full runs centroids. 

`./darksider -FC <petri_net_fil.tpn> <traces.tr> <distance_centroids-traces> <number_of_clusters> <output_file>`

### Subnet clustering 

`./darksider -SC <petri_net_fil.tpn> <traces.tr> <distance_centroids-traces> <number_of_clusters> <number_max_transitions_centroid> <output_file>`

## Alignment with Edit distance

`./darksider -eD <petri_net_fil.tpn> <traces.tr> <quotient*> <maximal_number_of_diff>`

where *quotient* divise the maximal size of run, determined by the number of transition in the net.

## Anti-alignment with Edit distance

`./darksider -AA <petri_net_fil.tpn> <traces.tr> <quotient*>  <maximal_number_of_diff>`

where *quotient* divise the maximal size of run, determined by the number of transition in the net.


### Help 
	
`./darksider -help `




# FOLDERS 
<pre>
┬  
├ src
     ┬  
     ├ main.ml
     ├ alignment_PB.ml
     └ ...
 ├ data_test
     ┬  
     ├ example_M1
        ┬
        ├ M1.tpn
        └ ...
     ├ example_Clustering
     └ ...
 ├ test -> to remove when software is completed 
     ┬  
     ├ Aligner.ml
     ├ Multialignments.ml
    └ ...
</pre>


