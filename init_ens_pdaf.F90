!$Id: init_ens_pdaf.F90 870 2021-11-22 14:02:55Z lnerger $
!>  Initialize ensemble
!!
!! User-supplied call-back routine for PDAF.
!!
!! Used in all ensemble filters.
!!
!! The routine is called when the filter is
!! initialized in PDAF_filter_init.  It has
!! to initialize an ensemble of dim_ens states.
!!
!! The routine is called by all filter processes and 
!! initializes the ensemble for the PE-local domain.
!!
!! Implementation for the 2D online example
!! without parallelization. Here, the ensmeble is
!! directly read from files.
!!
!! __Revision history:__
!! * 2013-02 - Lars Nerger - Initial code
!! * Later revisions - see repository log
!!
SUBROUTINE init_ens_pdaf(filtertype, dim_p, dim_ens, state_p, Uinv, &
     ens_p, flag)

  ! use netcdf
  USE mod_model, &         ! Model variables
       ONLY: nx, ny, initens, file_output_choice
  USE mod_assimilation, &  ! Assimilation variables
       ONLY: ensgroup
  use ouput_txt, &
       only: read_txt

  IMPLICIT NONE

! *** Arguments ***
  INTEGER, INTENT(in) :: filtertype                !< Type of filter to initialize
  INTEGER, INTENT(in) :: dim_p                     !< PE-local state dimension
  INTEGER, INTENT(in) :: dim_ens                   !< Size of ensemble
  REAL, INTENT(inout) :: state_p(dim_p)            !< PE-local model state
  !< (It is not necessary to initialize the array 'state_p' for ensemble filters.
  !< It is available here only for convenience and can be used freely.)
  REAL, INTENT(inout) :: Uinv(dim_ens-1,dim_ens-1) !< Array not referenced for ensemble filters
  REAL, INTENT(out)   :: ens_p(dim_p, dim_ens)     !< PE-local state ensemble
  INTEGER, INTENT(inout) :: flag                   !< PDAF status flag

! *** local variables ***
  REAL, ALLOCATABLE :: ens(:,:)       ! global ensemble
  REAL, ALLOCATABLE :: state(:)       ! global state vector
  integer :: i, k, j
  character(len=100) :: ens_file
  integer :: stat(50)
  integer :: file_id, step_id, ens_id
  real(8), allocatable :: ens_arr(:,:)
  integer :: pos(2), cnt(2)
  integer :: nsteps_file
  character(len=2) :: i_str
  character(len=10) :: fname


! **********************
! *** INITIALIZATION ***
! **********************

  ! *** Generate full ensemble on filter-PE 0 ***
  WRITE (*, '(/9x, a)') 'Initialize state ensemble'
  WRITE (*, '(9x, a)') '--- read ensemble from files'
  WRITE (*, '(9x, a, i5)') '--- Ensemble size:  ', dim_ens
  
  if (file_output_choice .eq. 0) then

    ! ! open ens file
    ! ens_file = 'ens.nc'
    ! j = 1
    ! stat(j) = NF90_OPEN(TRIM(ens_file), NF90_NOWRITE, file_id) 

    ! ! read number of time steps
    ! j = j + 1
    ! stat(j) = NF90_INQ_DIMID(file_id, 'time_steps', step_id)
    ! j = j + 1
    ! stat(j) = NF90_Inquire_dimension(file_id, step_id, len=nsteps_file)

    ! ! read time step info
    ! ! j = j + 1
    ! ! stat(j) = NF90_INQ_VARID(file_id, 'step', step_id)


    ! ! initialize obs interval
    ! ! delt_obs = 5    ! fine hardcoded for now but needs to be made dynamic (shoale 2/27)

    ! ! read observations
    ! ! dim_ens = nx*nx
    ! allocate(ens_arr(nx*nx, dim_ens))

    ! j = 1
    ! stat(j) = NF90_INQ_VARID(file_id, 'ens', ens_id)

    ! do i = 1, dim_ens
    !   pos(2) = i
    !   cnt(2) = 1
    !   pos(1) = 1
    !   cnt(1) = nx*nx
    !   j = j + 1
    !   stat(j) = NF90_GET_VAR(file_id, ens_id, ens_arr(:,i), start=pos, count=cnt)
    ! end do

    ! ens_p = ens_arr

    ! j = j + 1
    ! stat(j) = NF90_CLOSE(file_id)


  else if (file_output_choice .eq. 1) then

    allocate(ens_arr(nx*nx, dim_ens))
    do i = 1, dim_ens
      if (i .lt. 10) then
        write(i_str, "(I2.1)") i
      else
        write(i_str, "(I2.2)") i
      endif 
      fname = trim('ens_'//adjustl(i_str))
      call read_txt(fname, ens_arr(:,i))
    end do

    ens_p = ens_arr

  end if

    deallocate(ens_arr)

END SUBROUTINE init_ens_pdaf
