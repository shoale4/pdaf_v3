#!/bin/bash

export PDAF_ARCH=macos_gfortran_openmpi
export makefilepath="/Users/shoale/PDAF-heart-ms_v3/PDAF_V2.0/make.arch/macos_gfortran_openmpi.h"

######## GENERATE INIT FILES + NETCDF STATE FILE ########
if grep -q "\-DUSE_PDAF" $makefilepath; then
	sed -i '' 's/^CPP_DEFS = -DUSE_PDAF/CPP_DEFS = #-DUSE_PDAF/' $makefilepath
fi
cd ..
make clean
make model
./model -spinup 0 -fout_choice 1
./model -spinup 1 -fout_choice 1

