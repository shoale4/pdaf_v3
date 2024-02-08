Welcome to Shoale Badr's modified version of PDAF (used for synthetic cardiac data assimilation).

Now, PDAF can be run with just a few tweaks to this code here. 

To download and run:
1. Make sure your machine has the dependencies installed listed on the PDAF website (`cmake`, `gfortran`, etc).
2. Clone the PDAF 2.0 repo: https://github.com/PDAF/PDAF/tree/PDAF_V2.0 (this is a slightly older version of PDAF now- I think they updated it within the last few months).
3. Navigate to the `/tutorial` folder.
4. Clone this repo into that folder and rename it whatever you'd like (mMS for example, for Modified Mitchell-Schaeffer).
5. Create an `/outputs` folder, a `/results` folder, and a `/txt_file_data` folder within it.
6. In the `/txt_file_data` folder, add a `/ens_obs` folder.
7. Open `prepoststep_ens_pdaf.F90` and find lines 335 and 354 (variable name `filename`). Change `'results/for_elizabeth/jan_2024/rmserror_'` to just `'results/rmserror_'`  and `'results/for_elizabeth/jan_2024/spread_'` to just `'results/spread_'`.
8. Open the `bash_scripts/build_and_run_macos.sh` file.
9. In line 4, change `makefilepath` to the path that your PDAF header file is located (should be something similar to what I have there).
10. Now, the code is ready to run. To modify values for experimentation (more options will be added soon, such as changing parameter sets for the model),
   you can edit:
   - line 22: change the ensemble size (`-ens_size`) 
   - line 23: change the observation type (`-obs_type`, see `tools/generate_ens.F90` for more info)
              change the observation spacing (`-obs_spacing`, if observation type is uniform)
   - line 33: set the number of processors (`-np`, should be equal to ensemble size)  
              set the ensemble size (`-dim_ens`),
              set strings for results files: 
                - `-exp_type`: experiment type, should be something like "obs_spacing"
                - `-filter_type`: filter type (e.g. "estkf", "etkf", etc.)
                - `-obs_type`: observation type (e.g. uniformly distributed observations every 8th grid point - e.g. "uniform8")
              set the filter (`-filt_type`, see `init_pdaf.F90` for more info).
12. In your terminal, run `chmod +x build_and_run_macos.sh` and then `./build_and_run_macos.sh`.
13. Congrats, you've run PDAF!



