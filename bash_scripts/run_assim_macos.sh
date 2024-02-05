#!/bin/bash

export PDAF_ARCH=macos_gfortran_openmpi
export makefilepath="/Users/shoale/PDAF-heart-ms_v3/PDAF_V2.0/make.arch/macos_gfortran_openmpi.h"

# ######## RUN ASSIMILATION ########
if grep -q "#-DUSE_PDAF" $makefilepath; then
	sed -i '' 's/^CPP_DEFS = #-DUSE_PDAF/CPP_DEFS = -DUSE_PDAF/' $makefilepath
fi
cd ..
make clean
make model_pdaf
mpirun -np 4 ./model_pdaf -dim_ens 4 -exp_type obs_spacing_exp -filt_type 6 -filter_type estkf -obs_type uniform6 -fout_choice 1