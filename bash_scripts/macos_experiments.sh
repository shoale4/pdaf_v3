#!/bin/bash

export PDAF_ARCH=macos_gfortran_openmpi
export makefilepath="/Users/shoale/PDAF-heart-ms_v3/PDAF_V2.0/make.arch/macos_gfortran_openmpi.h"
# export fout_choice=1

######## GENERATE INIT FILES + NETCDF STATE FILE ########
cd ..
if grep -q "\-DUSE_PDAF" $makefilepath; then
	sed -i '' 's/^CPP_DEFS = -DUSE_PDAF/CPP_DEFS = #-DUSE_PDAF/' $makefilepath
fi
make clean
make model
./model -spinup 0 -fout_choice 1
./model -spinup 1 -fout_choice 1


######## GENERATE ENSEMBLE AND OBSERVATIONS ########
cd tools
make clean
gfortran -c -o parser_no_mpi.o parser_no_mpi.F90
gfortran -c -o output_txt.o output_txt.F90
make all
./generate_ens -ens_size 4
./generate_obs -obs_choice 1 -obs_spacing 2


######### SET UP ASSIMILATION ########
if grep -q "#-DUSE_PDAF" $makefilepath; then
	sed -i '' 's/^CPP_DEFS = #-DUSE_PDAF/CPP_DEFS = -DUSE_PDAF/' $makefilepath
fi
cd ..
make clean
make model_pdaf


############# EXPERIMENTS #############

# estkf, uniform2
mpirun -np 4 ./model_pdaf -dim_ens 4 -exp_type obs_spacing_exp -filt_type 4 -filter_type etkf -obs_type uniform2 -fout_choice 1

# lestkf, uniform2
mpirun -np 4 ./model_pdaf -dim_ens 4 -exp_type obs_spacing_exp -filt_type 6 -filter_type estkf -obs_type uniform2 -fout_choice 1

# pf, uniform2
mpirun -np 4 ./model_pdaf -dim_ens 4 -exp_type obs_spacing_exp -filt_type 12 -filter_type pf -obs_type uniform2 -fout_choice 1