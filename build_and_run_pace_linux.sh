#!/bin/bash

module load mvapich2 netlib-scalapack
module load netcdf-fortran
module load openblas
export PDAF_ARCH=linux_gfortran_openmpi
export makefilepath="/storage/home/hcoda1/8/sbadr3/shoale/PDAF_new/make.arch/linux_gfortran_openmpi.h"
# echo $makefilepath
export fout_choice=1

######## GENERATE INIT FILES + NETCDF STATE FILE ########
if grep -q "\-DUSE_PDAF" $makefilepath; then
	sed -i 's/^CPP_DEFS = -DUSE_PDAF/CPP_DEFS = #-DUSE_PDAF/' $makefilepath
fi
make clean
make model
./model -spinup 0 -fout_choice $(fout_choice)
make clean
make model
./model -spinup 1 -fout_choice $(fout_choice)


######## GENERATE ENSEMBLE AND OBSERVATIONS ########
cd tools
make clean
gfortran -c -o parser_no_mpi.o parser_no_mpi.F90
make all
./generate_ens -ens_size 4
./generate_obs -obs_choice 1 -obs_spacing 6


# ######## RUN ASSIMILATION ########
if grep -q "#-DUSE_PDAF" $makefilepath; then
	sed -i 's/^CPP_DEFS = #-DUSE_PDAF/CPP_DEFS = -DUSE_PDAF/' $makefilepath
fi
cd ..
make clean
make model_pdaf
# mpirun -np 4 ./model_pdaf -dim_ens 4 -exp_type obs_spacing_exp -filt_type 6 -filter_type estkf -obs_type uniform6
# srun -n 10 ./model_pdaf -dim_ens 10 -exp_type obs_spacing_exp -filt_type 6 -filter_type estkf -obs_type uniform6 -fout_choice $(fout_choice)