

subroutine get_true_rmse(calltype, step, dim, state_est, rmse_true, dim_ens)

    use netcdf
    use mod_assimilation, &
            only: delt_obs

    implicit none

    ! args
    character(len=3), intent(in) :: calltype
    integer, intent(in) :: step
    integer, intent(in) :: dim
    real(8), intent(in) :: state_est(dim)
    real(8), intent(out) :: rmse_true
    integer, intent(in) :: dim_ens

    ! local vars
    character(len=20) :: in_file
    integer :: i, j, k
    integer :: ncid_in, dim_id, nsteps, state_id, dim_state, step_id
    real(8), allocatable :: state(:)
    integer :: stat(50)
    integer :: start_v(2), count_v(2)
    integer, allocatable :: steps(:)
    ! integer :: delt_obs = 10


    ! path to file with true model trajectory
  	in_file = 'state.nc'
  	in_file = TRIM(in_file)


  	! open trajectory file and read information
	j = 1
  	stat(j) = NF90_OPEN(in_file, NF90_NOWRITE, ncid_in)
  	j = j + 1

  	! get dimensions
  	stat(j) = NF90_INQ_DIMID(ncid_in, 'dim_state', dim_id)
  	j = j + 1
  	stat(j) = NF90_Inquire_dimension(ncid_in, dim_id, len=dim_state)
  	j = j + 1
  	stat(j) = NF90_INQ_DIMID(ncid_in, 'time_steps', dim_id)
  	j = j + 1
  	stat(j) = NF90_Inquire_dimension(ncid_in, dim_id, len=nsteps)
  	j = j + 1

  	! get var id
  	stat(j) = NF90_INQ_VARID(ncid_in, 'state', state_id)

  	! initialize step array
  	allocate(steps(nsteps))

  	j = j + 1
  	stat(j) = NF90_INQ_VARID(ncid_in, 'step', step_id)
  	j = j + 1
  	stat(j) = NF90_GET_VAR(ncid_in, step_id, steps)

	do i = 1, j-1
		if (stat(i) /= NF90_noerr) then
			write (*,*) "NETCDF ERROR, STAT LINE ", i 
		end if
	end do 

	!!! READ TRAJECTORY!!!
	allocate(state(dim))


    ! read state vector
    start_v(2) = abs(step)/delt_obs
    count_v(2) = 1
    start_v(1) = 1
    count_v(1) = dim
    stat(1) = NF90_GET_VAR(ncid_in, state_id, state, start=start_v, count=count_v)


    ! compute true rms
    rmse_true = 0.0
    do k = 1, dim
        rmse_true = rmse_true + (state(k) - state_est(k))**2
    end do
    rmse_true = sqrt(rmse_true / dim)

    stat(1) = NF90_CLOSE(ncid_in)
    IF (stat(1) /= NF90_NOERR) then
         WRITE(*, *) 'NetCDF error in closing file with true states'
    end if


end subroutine get_true_rmse