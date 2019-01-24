>Author : Boltenhagen Mathilde, Chatain Thomas <br>
>Date : 01.2019

## ENVIRONNEMENT 

- need minisat+ or any other PB solver. Darksider was also tried with sat4j

## INSTALLATION

 `make 

## EXAMPLE OF UTILISATION 

### For a full run clustering  

`./darksider -FC <petri_net_fil.tpn> <traces.tr> <distance_centroids-traces> <number_of_clusters> <output_file>`

### For a subnet clustering 

`./darksider -SC <petri_net_fil.tpn> <traces.tr> <distance_centroids-traces> <number_of_clusters> <number_max_transitions_centroid> <output_file>`

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


