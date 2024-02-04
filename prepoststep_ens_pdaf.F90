!$Id: prepoststep_ens_pdaf.F90 870 2021-11-22 14:02:55Z lnerger $
!>  Used-defined Pre/Poststep routine for PDAF
!!
!! User-supplied call-back routine for PDAF.
!!
!! Used in all ensemble filters.
!! 
!! The routine is called for global filters (e.g. ESTKF)
!! before the analysis and after the ensemble transformation.
!! For local filters (e.g. LESTKF) the routine is called
!! before and after the loop over all local analysis
!! domains.
!!
!! The routine provides full access to the state 
!! estimate and the state ensemble to the user.
!! Thus, user-controlled pre- and poststep 
!! operations can be performed here. For example 
!! the forecast and the analysis states and ensemble
!! covariance matrix can be analyzed, e.g. by 
!! computing the estimated variances. 
!! For the offline mode, this routine is the place
!! in which the writing of the analysis ensemble
!! can be performed.
!!
!! If a user considers to perform adjustments to the 
!! estimates (e.g. for balances), this routine is 
!! the right place for it.
!!
!! Implementation for the 2D example
!! without model parallelization.
!!
!! __Revision history:__
!! * 2013-02 - Lars Nerger - Initial code based on offline_1D
!! * Later revisions - see repository log
!!
SUBROUTINE prepoststep_ens_pdaf(step, dim_p, dim_ens, dim_ens_p, dim_obs_p, &
     state_p, Uinv, ens_p, flag)

  USE mod_model, &           ! Model variables
       ONLY: nx, ny, rmse_arr, total_steps, rmse_spread

  use mod_assimilation, &
       only: delt_obs
   
   ! use mod_parallel_pdaf, &
   !       only: n_modeltasks

   ! new sb 1/9/24
   USE parser_no_mpi, & 
       ONLY: parse


  IMPLICIT NONE

! *** Arguments ***
  INTEGER, INTENT(in) :: step        !< Current time step (negative for call after forecast)
  INTEGER, INTENT(in) :: dim_p       !< PE-local state dimension
  INTEGER, INTENT(in) :: dim_ens     !< Size of state ensemble
  INTEGER, INTENT(in) :: dim_ens_p   !< PE-local size of ensemble
  INTEGER, INTENT(in) :: dim_obs_p   !< PE-local dimension of observation vector
  REAL, INTENT(inout) :: state_p(dim_p) !< PE-local forecast/analysis state
  !< (The array 'state_p' is not generally not initialized in the case of SEIK.
  !< It can be used freely here.)
  REAL, INTENT(inout) :: Uinv(dim_ens-1, dim_ens-1) !< Inverse of matrix U
  REAL, INTENT(inout) :: ens_p(dim_p, dim_ens)      !< PE-local state ensemble
  INTEGER, INTENT(in) :: flag        !< PDAF status flag


! *** local variables ***
  INTEGER :: i, j, member             ! Counters
  LOGICAL, SAVE :: firsttime = .TRUE. ! Routine is called for first time?
  REAL :: invdim_ens                  ! Inverse ensemble size
  REAL :: invdim_ensm1                ! Inverse of ensemble size minus 1
  REAL :: rmserror_est                ! estimated RMS error
  REAL, ALLOCATABLE :: variance(:)    ! model state variances
  REAL, ALLOCATABLE :: field(:,:)     ! global model field
  CHARACTER(len=2) :: ensstr          ! String for ensemble member
  CHARACTER(len=4) :: stepstr         ! String for time step
  CHARACTER(len=3) :: anastr          ! String for call type (initial, forecast, analysis)
  character(len=4) :: results_str     ! string for results output file (sb 2/14 new)
  character(len=2) :: ens_size_str
  REAL(8) :: rmse_true
  character(len=5) :: ens_out_str
  character(len=32) :: obs_string     ! string for observation type results output
  integer :: ens_mean_output_times(4)
  character(len=3) :: grid_size

  ! new sb 1/9/24
  CHARACTER(len=32) :: handle  ! new sb 1/9/24 -- for command line parsing, finally adding it!
  character(len=32) :: exp_var_spec
  character(len=32) :: exp_type
  integer :: mod_error_option
  character(len=32) :: mod_error_str
  character(len=32) :: filter_type
  character(len=128) :: filename


! **********************
! *** INITIALIZATION ***
! **********************

  IF (firsttime) THEN
     WRITE (*, '(8x, a)') 'Analyze initial state ensemble'
     anastr = 'ini'
  ELSE
     IF (step<0) THEN
        WRITE (*, '(8x, a)') 'Analyze and write forecasted state ensemble'
        anastr = 'for'
     ELSE
        WRITE (*, '(8x, a)') 'Analyze and write assimilated state ensemble'
        anastr = 'ana'
     END IF
  END IF

  ! Allocate fields
  ALLOCATE(variance(dim_p))


  ! Initialize numbers
  rmserror_est  = 0.0
  invdim_ens    = 1.0 / REAL(dim_ens)  
  invdim_ensm1  = 1.0 / REAL(dim_ens - 1)


! **************************************************************
! *** Perform prepoststep for SEIK with re-inititialization. ***
! *** The state and error information is completely in the   ***
! *** ensemble.                                              ***
! *** Also performed for SEIK without re-init at the initial ***
! *** time.                                                  ***
! **************************************************************

  ! *** Compute mean state
  WRITE (*, '(8x, a)') '--- compute ensemble mean'

  grid_size = '100'  ! grid size for file output strings (new sb 9/17/23)

  state_p = 0.0
  DO member = 1, dim_ens
     DO i = 1, dim_p
        state_p(i) = state_p(i) + ens_p(i, member)
     END DO
  END DO
  state_p(:) = invdim_ens * state_p(:)

  write (results_str, '(i4.4)') delt_obs
  write (ens_size_str, '(i2.2)') dim_ens

  ! handle = 'obs_type'
  ! call parse(handle, obs_int)

  ! if (obs_int .eq. 1) then
  !   obs_string = 'uniform_'
  ! elseif (obs_int .eq. 2) then
  !   obs_string = 'only_border_'
  ! elseif (obs_int .eq. 3) then
  !   obs_string = 'border_and_sparse_inside_' 
  ! elseif (obs_int .eq. 4) then
  !   obs_string = 'only_left_half_'
  ! elseif (obs_int .eq. 5) then
  !   obs_string = 'only_bottom_half_'
  ! endif 
  ! obs_string = 'lc'   ! using this variable for breakup/spiral type for the model bc it's already been created (new sb 9/22/23)

  ! ! write out ensemble mean to file
  ! ens_mean_output_times = (/300, 750, 3000, 4000/) * 4
  ! allocate(field(nx,nx))
  ! field = reshape(state_p, (/nx, nx/))
  ! if ((step .eq. 0) .or. mod(step, 800) == 0) then
  ! ! if (any(ens_mean_output_times == step)) then
  !     write(ens_out_str, '(i5.5)') step
  !     ! OPEN(11, file = 'outputs/ens_size_10_model_error_ens_mean_step_'//TRIM(ens_out_str)//'.txt')
  !     ! OPEN(11, file = 'outputs/'//TRIM(obs_string)//'_step_'//TRIM(ens_out_str)//'.txt')
  !     OPEN(11, file='outputs/for_elizabeth/ens_mean_size_'//TRIM(ens_size_str)//'_Obs_' &
  !       //TRIM(grid_size)//'_step_'//TRIM(ens_out_str)//'_'//TRIM(obs_string)//'.txt')
  !     DO i = 1, nx
  !        WRITE (11, *) field(i, :)
  !     END DO
  !  end if


  ! *** Compute sampled variances ***
  variance(:) = 0.0
  DO member = 1, dim_ens
     DO j = 1, dim_p
        variance(j) = variance(j) + (ens_p(j, member) - state_p(j)) * (ens_p(j, member) - state_p(j))
     END DO
  END DO
  variance(:) = invdim_ensm1 * variance(:)
!   print *, variance

! ************************************************************
! *** Compute RMS errors according to sampled covar matrix ***
! ************************************************************

  ! total estimated RMS error
  DO i = 1, dim_p
     rmserror_est = rmserror_est + variance(i)
  ENDDO
  rmserror_est = SQRT(rmserror_est / dim_p)
!   print *, rmserror_est

  ! true rmse error (sb 3/11/22)
  ! call get_true_rmse(anastr, step, nx*nx, state_p, rmse_true, dim_ens)
  if (anastr .eq. 'ana') then
    call get_true_rmse_bin(step, state_p, rmse_true) ! new sb 2/4/24
  endif 

  ! spread rmse
!   rmse_spread = 0.0
!   do i = 1, dim_p
!       rmse_spread = rmse_spread + variance(i)**2
!   end do
!   rmse_spread = sqrt(rmse_spread / dim_p)



  !!! new sb 2/14 !!! writes out true rmse and ensemble spread rmse
  if (mod(step, delt_obs) == 0) then
   rmse_arr(step/delt_obs) = rmse_true
   rmse_spread(step/delt_obs) = rmserror_est
end if




! *****************
! *** Screen IO ***
! *****************

  ! Output RMS errors given by sampled covar matrix
  WRITE (*, '(12x, a, es12.4)') &
       'RMS error according to sampled variance: ', rmserror_est

  write (*,*) "True RMSE: ", rmse_true 
  write (*,*) "Spread: ", rmserror_est
! *******************
! *** File output ***
! *******************

!   IF (.not. firsttime) THEN

!      WRITE (*, '(8x, a)') '--- write ensemble and state estimate'

!      ALLOCATE(field(ny, nx))

!      ! Set string for time step
!      IF (step < 0) then
!       write (stepstr, '(i4.4)') -step
!      ELSE
!          IF (step < 100) THEN
!             WRITE (stepstr, '(i2.2)') step
!          ELSE IF (step >= 100 .and. step < 1000) then
!             write (stepstr, '(i3.3)') step
!          ELSE
!             write (stepstr, '(i4.4)') step
!          end if
!    !   ELSE
!       !   WRITE (stepstr, '(i2)') -step
!       !  WRITE (stepstr, *) -step
!      END IF

!      if (mod(step, 100) == 0) then

!       ! Write analysis ensemble
!       DO member = 1, dim_ens
!          DO j = 1, nx
!             field(1:ny, j) = ens_p(1 + (j-1)*ny : j*ny, member)
!          END DO

!          WRITE (ensstr, '(i2.2)') member

!          OPEN(11, file = TRIM(ensstr)//'_step'//TRIM(stepstr)//'_'//TRIM(anastr)//'.txt')
!          !   OPEN(11, file = 'data/outputs/spiral_wave/ens_'//TRIM(ensstr)//'_step'//TRIM(stepstr)//'_'//TRIM(anastr)//'.txt', status = 'replace')

!          DO i = 1, ny
!             WRITE (11, *) field(i, :)
!          END DO

!          CLOSE(11)
!       END DO

!       ! Write analysis state
!       !   print *, "WRITING FORECASTED STATE"
!       !   print *, step
!       DO j = 1, nx
!          field(1:ny, j) = state_p(1 + (j-1)*ny : j*ny)
!       END DO

!       OPEN(11, file = 'state_'//TRIM(stepstr)//'_'//TRIM(anastr)//'.txt')
!       !   OPEN(11, file = 'state_step'//TRIM(stepstr)//'_'//TRIM(anastr)//'.txt', status = 'replace')
!       !   OPEN(11, file = 'data/outputs/spiral_wave/state_step'//TRIM(stepstr)//'_'//TRIM(anastr)//'.txt', status = 'replace')


!       DO i = 1, ny
!          WRITE (11, *) field(i, :)
!       END DO

!       CLOSE(11)


!      DEALLOCATE(field)
!    end if
!   END IF


! ********************
! *** finishing up ***
! ********************

  DEALLOCATE(variance)

  firsttime = .FALSE.

  !! new sb 2/14 -- rmse results output file !! 
  ! write (results_str, '(i4.4)') delt_obs
  ! write (ens_size_str, '(i2.2)') dim_ens

  ! new sb 1/9/24
  handle = 'exp_type'
  call parse(handle, exp_type)
  ! handle = 'mod_error'
  ! call parse(handle, mod_error_str)
  handle = 'filter_type'
  call parse(handle, filter_type)
  handle = 'obs_type'
  call parse(handle, exp_var_spec)

  ! sb 1/11/24
  ! filter type is estkf, lestkf, etc
  ! exp_type is obs_spacing, diff_ass_interval, diff_filter, etc 
  ! exp_var_type - experimental variable specification
  ! exp_var_type is uniform2 (uniform, obs every 2 GPs), uniform4, left_half, bottom_half, etc for diff_obs
  ! exp_var_type is 5, 10, 15, etc for diff_ass_interval, etc, etc, so on, so forth 
  filename = 'results/for_elizabeth/jan_2024/rmserror_'//TRIM(filter_type)//'_'// &
             TRIM(exp_type)//'_'//TRIM(exp_var_spec)//'.txt'

  !!!!!!!! rmse  !!!!!!!!!!!
  ! new sb 1/9/24
  OPEN(11, file = TRIM(filename))  

  ! OPEN(11, file = 'results/for_elizabeth/rmse_ens_size_'//TRIM(ens_size_str)//'_obs_'// &
  !   TRIM(grid_size)//'_'//TRIM(obs_string)//'.txt')  
  ! OPEN(11, file = 'results/true_rmse_ens_size_'//TRIM(ens_size_str)//'no_model_error.txt')
  ! OPEN(11, file = 'results/true_rmse_ens_size_'//TRIM(ens_size_str)//'with_error.txt') ! with model error
  ! OPEN(11, file = 'results/true_rmse_ens_size_'//TRIM(ens_size_str)//'delt_obs_'//TRIM(results_str)//'.txt') ! different assimilation intervals
  ! OPEN(11, file = 'results/true_rmse_ens_size_'//TRIM(ens_size_str)//'obs_type_'//TRIM(obs_string)//'.txt') ! different obs types
  DO i = 1, total_steps/delt_obs
     WRITE (11, *) rmse_arr(i)
  END DO
  CLOSE(11)

  ! new sb 1/9/24
  filename = 'results/for_elizabeth/jan_2024/spread_'//TRIM(filter_type)//'_'// &
             TRIM(exp_type)//'_'//TRIM(exp_var_spec)//'.txt'

  ! spread 
  OPEN(11, file = TRIM(filename)) ! new sb 1/9/24
  ! OPEN(11, file = 'results/for_elizabeth/spread_ens_size_'//TRIM(ens_size_str)// &
  !   '_obs_'//TRIM(grid_size)//'_'//TRIM(obs_string)//'.txt')
  ! OPEN(11, file = 'results/spread_rmse_ens_size_'//TRIM(ens_size_str)//'_no_model_error.txt')
  ! OPEN(11, file = 'results/spread_rmse_ens_size_'//TRIM(ens_size_str)//'_with_error.txt')  ! with model error
  ! OPEN(11, file = 'results/spread_rmse_ens_size_'//TRIM(ens_size_str)//'delt_obs_'//TRIM(results_str)//'.txt') ! different assimilation intervals
  ! OPEN(11, file = 'results/spread_rmse_ens_size_'//TRIM(ens_size_str)//'obs_type_'//TRIM(obs_string)//'.txt') ! different obs types
  DO i = 1, total_steps/delt_obs
     WRITE (11, *) rmse_spread(i)
  END DO
  CLOSE(11)


END SUBROUTINE prepoststep_ens_pdaf






