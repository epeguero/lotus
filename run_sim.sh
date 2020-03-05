#!/bin/bash

# copy kernel to gorgonzola
ssh emp238@gorgonzola.cs.cornell.edu "rm -rf ~/gem5-mesh/lotus/*"

scp -r example/* emp238@gorgonzola.cs.cornell.edu:~/gem5-mesh/lotus/

ssh emp238@gorgonzola.cs.cornell.edu \
  'cd ~/gem5-mesh &&\
   make -C lotus/vvadd && 
   timeout 20 ./build/RVSP/gem5.opt -d lotus/vvadd/result \
   \configs/phil/brg_hammerblade.py \
   --cmd=lotus/vvadd/vvadd\
   --num-cpu=4 \'

# append something like this to also output stats results
# cat lotus/vvadd/results/stats.txt | grep -E "(numCycles|CPI)"'
