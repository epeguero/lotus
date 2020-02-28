#!/bin/bash

# copy kernel to gorgonzola
scp -r vvadd_target.c emp238@gorgonzola.cs.cornell.edu:~/gem5-mesh/lotus/vvadd/vvadd.c

ssh emp238@gorgonzola.cs.cornell.edu \
  'cd ~/gem5-mesh &&\
   make clean -C lotus/vvadd && 
   make -C lotus/vvadd && 
   timeout 20 ./build/RVSP/gem5.opt -d lotus/vvadd/result \
   \configs/phil/brg_hammerblade.py \
   --cmd=lotus/vvadd/vvadd\
   --num-cpu=4 \ &&
   cat lotus/vvadd/results/stats.txt | grep -E "(numCycles|CPI)"'
