#!/bin/bash

module load mvapich2 netlib-scalapack
module load netcdf-fortran
module load openblas
export PDAF_ARCH=linux_gfortran_openmpi
export makefilepath="/storage/home/hcoda1/8/sbadr3/shoale/PDAF_new/make.arch/linux_gfortran_openmpi.h"
export fout_choice=1

######## GENERATE INIT FILES + NETCDF STATE FILE ########
if grep -q "\-DUSE_PDAF" $makefilepath; then
	sed -i 's/^CPP_DEFS = -DUSE_PDAF/CPP_DEFS = #-DUSE_PDAF/' $makefilepath
fi
cd ..
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
./generate_ens -ens_size 10
./generate_obs -obs_choice 1 -obs_spacing 4


# ######## RUN ASSIMILATIONS ########
if grep -q "#-DUSE_PDAF" $makefilepath; then
	sed -i 's/^CPP_DEFS = #-DUSE_PDAF/CPP_DEFS = -DUSE_PDAF/' $makefilepath
fi
cd ..
make clean
make model_pdaf

# LESTKF, UNIFORM2, CRADIUS2
srun -n 10 ./model_pdaf -dim_ens 10 -exp_type obs_spacing_exp -filt_type 7 -filter_type lestkf -obs_type uniform2 -fout_choice 1 -cradius 4 -locweight 5