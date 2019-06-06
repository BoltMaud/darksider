#!/bin/bash 

for tpn in 1 2 3 4 5 6 7 8 9 10
do
	echo "Dataset n°$tpn"
	for size in 1 5 10 15 20 30 40 50 100 
	do 
		head -n $size ./data_test/example_anti/Josep_benchmark/M$tpn.tr >> temp.tr
		./darksider -AA ./data_test/example_anti/Josep_benchmark/M$tpn.tpn  ./temp.tr >> result/Anti-Alignment_Thomas_$tpn-$size.txt
		rm temp.tr
	done
done
	
	

