!$Id: initialize.F90 870 2021-11-22 14:02:55Z lnerger $
!>  Initialize model
!!
!! Initialization routine for the simple 2D model without
!! parallelization of the model.
!!
!! The routine defines the size of the model grid and
!! reads the initial state from a file. 
!!
!! __Revision history:__
!! * 2013-09 - Lars Nerger - Initial code
!! * Later revisions - see repository log
!!
SUBROUTINE initialize()

  USE mod_model, &          ! Model variables
      ONLY: nx, ny, v, h, total_steps, dt, init1, init2, initens, prepostens, prepoststate, init1, init2, &
      obs, tau_in, tau_out, tau_open, tau_close, v_gate, v_stim, endtime, stim_dur, spiraltime, dx, diff, &
      nsteps, nstimdur, d_to_dx2, nspiraltime, log1, logint1, jin, jout, dv, dh, xlap, jstim, &
      xlap1, xlap2, intpdaf1, intpdaf2, init1, init2, obs, intpdaf1, intpdaf2, step_null, & 
      tau_in, tau_out, tau_open, tau_close, v_gate, v_stim, dx, diff, rmse_arr, rmse_spread, ass_step, &
      spinup_time, model_start, spinup_phase, file_output_choice

  ! USE mod_assimilation, &
      ! ONLY: delt_obs

  ! new sb 1/9/24
  USE parser_no_mpi, & 
      ONLY: parse

  IMPLICIT NONE

! *** local variables ***
  INTEGER :: i, k              ! Counters
  CHARACTER(len=32) :: handle  ! new sb 1/9/24 -- for command line parsing, finally adding it!
  real(8), allocatable :: v_bin(:)
  real(8), allocatable :: h_bin(:)

! **********************
! *** INITIALIZATION ***
! **********************

! *** Model specifications ***
  nx = 200          ! Extent of grid in x-direction
  ny = 200         ! Extent of grid in y-direction
  dt = 0.25
  spinup_time = 500/dt ! number of time steps to let model spin-up (new sb 3/6/23)
  total_steps = 4000/dt + spinup_time ! Total number of time steps to perform

  !!! -- new sb 2/14 !!!
  ! rmse_arr_dim = total_steps / ass_step
  ! print *, floor(total_steps / delt_obs)
  ! print *, rmse_arr_dim

  !! ADD FILE OUTPUT STRINGS BELOW -- this is so that you have the location of all file outputs in one file) !!

  ! parameter values
	tau_in=0.35
	tau_out=6
	tau_open=120
	tau_close=150
	v_gate=0.13
	! v_stim=0.056

	! numerical and stimulation parameters
	endtime=4000
	stim_dur=2
	spiraltime=250
  dx=0.05
	diff=0.001

  ! nsteps = endtime/dt
	nstimdur = stim_dur/dt
	d_to_dx2 = dt/(dx*dx)
	nspiraltime = spiraltime/dt
	allocate(log1(nx,ny))
	allocate(logint1(nx,ny))
	allocate(jin(nx,ny))
	allocate(jout(nx,ny))
	allocate(dv(nx,ny))
	allocate(dh(nx,ny))
	allocate(xlap(nx,ny))
  ! allocate(rmse_arr(rmse_arr_dim))
  step_null = 0


! *** Screen output ***
  WRITE (*, '(1x, a)') 'INITIALIZE 2D TUTORIAL MODEL'
  WRITE (*, '(10x,a,i4,1x,a1,1x,i4)') 'Grid size:', nx, 'x', ny
  WRITE (*, '(10x,a,i4)') 'Time steps', total_steps

  ! allocate array for model field
  ALLOCATE(v(ny, nx))
  ALLOCATE(h(ny, nx))
    !! -- new sb 2/14 !!!
  ! allocate(rmse_arr(rmse_arr_dim))

  ! initialize rmse output files
  allocate(rmse_arr(total_steps))
  rmse_arr(:) = 0.0
  allocate(rmse_spread(total_steps))
  rmse_arr(:) = 0.0

  ! new sb for spinup phase
  ! model_start = 0
  spinup_phase = 1    ! new sb 1/9/24 (0 to produce init files, 1 to produce netcdf state file)

  ! new sb 1/9/24 -- calling parse function given by pdaf
  handle = 'spinup'
  CALL parse(handle, spinup_phase)
  ! READ(TRIM(spinup_option), *) spinup_phase
  file_output_choice = 1
  handle = 'fout_choice'
  call parse(handle, file_output_choice)

  allocate(v_bin(nx*nx))
  allocate(h_bin(nx*nx))

#ifndef USE_PDAF
  if (spinup_phase .eq. 0) then
    v(:,:) = 0
    h(:,:) = 1 * 0.5
  elseif (spinup_phase .eq. 1) then
    OPEN(11, file='outputs/for_elizabeth/v_init', status='old', form='unformatted', access='stream')
    ! DO i = 1, ny
    ! READ (11) v(:,i)
    READ (11) v_bin
    ! END DO

    OPEN(11, file='outputs/for_elizabeth/h_init', status='old', form='unformatted', access='stream')
    ! DO i = 1, ny
    ! READ (11) h(:,i)
    READ (11) h_bin
    ! END DO
    v = reshape(v_bin, (/nx,nx/))
    h = reshape(h_bin, (/nx,nx/))
  endif
#endif 

#ifdef USE_PDAF
  OPEN(11, file='outputs/for_elizabeth/v_init', status='old', form='unformatted', access='stream')
  ! DO i = 1, ny
  ! READ (11) v(:,i)
  READ (11) v_bin
  ! END DO

  OPEN(11, file='outputs/for_elizabeth/h_init', status='old', form='unformatted', access='stream')
  ! DO i = 1, ny
  ! READ (11) h(:,i)
  READ (11) h_bin
  ! END DO

  v = reshape(v_bin, (/nx,nx/))
  h = reshape(h_bin, (/nx,nx/))
#endif 



  
END SUBROUTINE initialize
