#!/bin/bash


./darksider -FC ./data_test/example_Clustering/model1.tpn ./data_test/example_Clustering/exampleOfModel1.tr 2 3 result/model1_FC_d-2_n-3.txt
./darksider -SC ./data_test/example_Clustering/model1.tpn ./data_test/example_Clustering/exampleOfModel1.tr 2 3 6 result/model1_SC_d-2_n-3_t-6.txt
./darksider -FC ./data_test/example_Clustering/model1.tpn ./data_test/example_Clustering/exampleOfModel1.tr 0 3 result/model1_FC_d-0_n-3.txt
./darksider -SC ./data_test/example_Clustering/model1.tpn ./data_test/example_Clustering/exampleOfModel1.tr 0 3 6 result/model1_SC_d-0_n-3_t-6.txt
echo "finish 1"
./darksider -FC ./data_test/example_Clustering/model1.tpn ./data_test/example_Clustering/mediumOfModel1.tr 2 3 result/model1m_FC_d-2_n-3.txt
./darksider -SC ./data_test/example_Clustering/model1.tpn ./data_test/example_Clustering/mediumOfModel1.tr 2 3 6 result/model1m_SC_d-2_n-3_t-6.txt
./darksider -FC ./data_test/example_Clustering/model1.tpn ./data_test/example_Clustering/largeOfModel1.tr 2 3 result/model1l_FC_d-0_n-3.txt
./darksider -SC ./data_test/example_Clustering/model1.tpn ./data_test/example_Clustering/largeOfModel1.tr 2 3 6 result/model1l_SC_d-0_n-3_t-6.txt
echo "finish 2"
./darksider -FC ./data_test/example_Clustering/M1_small.tpn ./data_test/example_Clustering/M1_small_traces.tr 2 3 result/M1_small_FC_d-2_n-3.txt
./darksider -SC ./data_test/example_Clustering/M1_small.tpn ./data_test/example_Clustering/M1_small_traces.tr 2 3 9 result/M1_small_SC_d-2_n-3_t-9.txt
./darksider -FC ./data_test/example_Clustering/M1_small.tpn ./data_test/example_Clustering/M1_small_m_traces.tr 2 3 result/M1_small_m_FC_d-2_n-3.txt
./darksider -SC ./data_test/example_Clustering/M1_small.tpn ./data_test/example_Clustering/M1_small_m_traces.tr 2 3 9 result/M1_small_m_SC_d-2_n-3_t-9.txt
echo "finish 3"
./darksider -FC ./data_test/example_Boudewijn/M1.tpn ./data_test/example_Clustering/M1_small.tr 3 4 result/M1_FC_d-3_n-4.txt
./darksider -SC ./data_test/example_Boudewijn/M1.tpn ./data_test/example_Clustering/M1_small.tr 3 4 13 result/M1_SC_d-3_n-4_t-13.txt



