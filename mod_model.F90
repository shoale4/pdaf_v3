!$Id: mod_model.F90 332 2019-12-30 09:37:03Z lnerger $
!> Module for model-specific variables
!!
!! This module provides variables needed for the 
!! 2-dimensional tutorial model without parallelization.
!!
!! __Revision history:__
!! * 2013-09 - Lars Nerger - Initial code
!! * Later revisions - see repository log
!!
MODULE mod_model

  IMPLICIT NONE
  SAVE

! *** Variables specific for 2D tutorial model ***

  INTEGER :: nx                   !< Size of 2D grid in x-direction
  INTEGER :: ny                   !< Size of 2D grid in y-direction
  REAL :: dt
  INTEGER :: total_steps          !< Total number of time steps
  REAL(8), ALLOCATABLE ::  v(:,:)!! ADD MODEL FIELD 1 HERE
  REAL(8), ALLOCATABLE ::  h(:,:)!! ADD MODEL FIELD 2 HERE
  ! integer :: i, j   ! counters
  character(len=32) :: prepoststate, prepostens, init1, init2, intpdaf1, intpdaf2, initens, obs
  real(8) :: tau_in, tau_out, tau_open, tau_close, v_gate, v_stim  
  integer :: endtime, stim_dur, spiraltime
  real(8) :: dx, diff
  real(8) :: jstim, xlap1, xlap2, d_to_dx2
  real(8), allocatable :: jin(:,:), jout(:,:), dh(:,:), dv(:,:), xlap(:,:)
  logical(4), allocatable :: log1(:,:)
  integer, allocatable :: logint1(:,:)
  integer :: nsteps, nperiod, nstimdur, nspiraltime 
  real, allocatable :: rmse_arr(:)
  integer :: ass_step, rmse_arr_dim
  integer :: step_null
  real(8), allocatable :: rmse_true(:), rmse_spread(:)
  integer :: spinup_time      ! new sb 9/6/23 -- time (ms) to wait for model to spin-up before writing states to file
  integer :: model_start   ! new sb for spinup phase
  integer :: spinup_phase  ! new sb 1/9/24 for spinup phase (0 or 1 to produce init files vs. netcdf file)
  integer :: file_output_choice ! new sb 1/23/24 update so that pdaf can run with txt files again rather than netcdf files
  
END MODULE mod_model
