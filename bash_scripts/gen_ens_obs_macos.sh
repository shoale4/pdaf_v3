#!/bin/bash

export PDAF_ARCH=macos_gfortran_openmpi

######## GENERATE ENSEMBLE AND OBSERVATIONS ########
cd ..
cd tools
make clean
gfortran -c -o parser_no_mpi.o parser_no_mpi.F90
gfortran -c -o output_txt.o output_txt.F90
make all
./generate_ens -ens_size 4
./generate_obs -obs_choice 1 -obs_spacing 6

