module read_obs

	use netcdf

	implicit none
	save 
	public

	contains

		subroutine read_observations(step)

				! new sb 1/9/24
			implicit none

			! args
			integer, intent(in) :: step
			! integer, intent(inout) :: dim_obs


			! local vars
			integer :: i, k, j
			character(len=100) :: obs_file
			integer :: stat(50)
			integer :: file_id, step_id, obs_id
			integer :: delt_obs
			real(8), allocatable :: obs_arr(:)
			integer :: pos(2), cnt(2)
			integer :: nx=200
			integer :: dim_obs
			integer :: nsteps_file

			! open obs file
			obs_file = 'obs.nc'
			 j = 1
			 stat(j) = NF90_OPEN(TRIM(obs_file), NF90_NOWRITE, file_id) 

			 ! read number of time steps
			 j = j + 1
			 stat(j) = NF90_INQ_DIMID(file_id, 'time_steps', step_id)
			 j = j + 1
			 stat(j) = NF90_Inquire_dimension(file_id, step_id, len=nsteps_file)

			 ! read time step info
			 j = j + 1
			 stat(j) = NF90_INQ_VARID(file_id, 'step', step_id)


			 ! initialize obs interval
			 delt_obs = 5    ! fine hardcoded for now but needs to be made dynamic (shoale 2/27)

			 ! read observations
			 dim_obs = nx*nx
			 allocate(obs_arr(dim_obs))

			 j = 1
			 stat(j) = NF90_INQ_VARID(file_id, 'obs', obs_id)

			 pos(2) = step
			 cnt(2) = 1
			 pos(1) = 1
			 cnt(1) = delt_obs
			 j = j + 1
			 stat(j) = NF90_GET_VAR(file_id, obs_id, obs_arr(:), start=pos, count=cnt)

		 end subroutine

end module

program obs_prog

	use read_obs
	use netcdf

	! new sb 1/9/24
	USE parser_no_mpi, & 
      ONLY: parse

	implicit none

	! local vars
	integer :: i, j, iter, x, y
	character(len=120) :: in_path, out_path
	character(len=120) :: in_file, out_file
  	character(len=150) :: ncfile_in, ncfile_out
  	character(len=150) :: attribute_str
  	real(8) :: stderr
  	integer :: ncid_in, ncid_out
  	integer :: dim_id, step_id, state_id, dim_id_iter, dim_id_state, obs_id
  	integer, allocatable :: steps(:)
  	integer :: nsteps
  	integer :: stat(100)
  	integer :: start_v(2), count_v(2)
  	integer :: dim_ids(2)
  	integer :: dim
  	real(8), allocatable :: state(:)
  	integer :: nx = 200
  	real(8), allocatable :: state_reshaped(:,:), observations(:,:)
  	integer :: obs_spacing
	integer :: delt_obs = 5
	integer :: obs_choice
	integer :: add_noise
	real(8), allocatable :: noise(:)   ! noise array
	integer :: iseed(4)   ! for random number generation
	real(8), allocatable :: obs_copy(:,:)
	character(len=32) :: handle  ! new sb 1/9/24 -- for command line parsing, finally adding it!



  	stderr = 1.0 

  	! path to file with model trajectory
  	in_file = '../state.nc'
  	in_file = TRIM(in_file)

  	! path to file (output) with observations
  	out_file = '../obs.nc'
  	out_file = TRIM(out_file)


  	! open trajectory file and read information
	j = 1
  	stat(j) = NF90_OPEN(in_file, NF90_NOWRITE, ncid_in)
  	j = j + 1

  	! get dimensions
  	stat(j) = NF90_INQ_DIMID(ncid_in, 'dim_state', dim_id)
  	j = j + 1
  	stat(j) = NF90_Inquire_dimension(ncid_in, dim_id, len=dim)
  	j = j + 1
  	stat(j) = NF90_INQ_DIMID(ncid_in, 'time_steps', dim_id)
  	j = j + 1
  	stat(j) = NF90_Inquire_dimension(ncid_in, dim_id, len=nsteps)
  	j = j + 1

    print *, "dim state:"
    print *, dim
    print *, "time steps:"
    print *, nsteps

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



	!!! INITIALIZE OUTPUT FILE FOR OBSERVATIONS !!!
	j = 1
	stat(j) = NF90_CREATE(out_file, 0, ncid_out)

	attribute_str = "Observations for MS model"
	j = j + 1
	stat(j) = NF90_PUT_ATT(ncid_out, NF90_GLOBAL, 'title', TRIM(attribute_str))
	
	! define dimensions
	j = j + 1
	stat(j) = NF90_DEF_DIM(ncid_out, 'dim_state', dim, dim_id_state)
	j = j + 1
	stat(j) = NF90_DEF_DIM(ncid_out, 'time_steps', nsteps-1, dim_id_iter)


	dim_ids(1) = dim_id_state
	dim_ids(2) = dim_id_iter

	j = j + 1
	stat(j) = NF90_DEF_VAR(ncid_out, 'obs', NF90_DOUBLE, dim_ids, obs_id)
	j = j + 1
	stat(j) = NF90_ENDDEF(ncid_out)

	! write time steps
	j = j + 1
	stat(j) = NF90_PUT_VAR(ncid_out, step_id, steps(2:nsteps))

	do i = 1, j-1
		if (stat(i) /= NF90_noerr) then
			write (*,*) "NETCDF ERROR, STAT LINE ", i 
		end if
	end do 	


	!!! READ TRAJECTORY AND GENERATE OBSERVATIONS !!!
	allocate(state(dim))
	allocate(noise(dim))
	allocate(observations(nx, nx))
	allocate(obs_copy(nx,nx))

	write (*,*) "Start writing observations"

	loop: do iter = 2, nsteps

		write (*,*) "Writing observations @ time step: ", ((iter-2) * delt_obs)

		! read state vector
		start_v(2) = iter
		count_v(2) = 1
		start_v(1) = 1
		count_v(1) = dim
		stat(1) = NF90_GET_VAR(ncid_in, state_id, state, start=start_v, count=count_v)

		! new observation sparcity -- sb 3/29 !!!
		! obs_choice is 1 for default obs (uniform at every xth grid point)
		! 2 for just borders
		! 3 for borders and uniform insid
		! 4 for left/right
		! 5 for top/bottom
		! set add_noise to 0 to run with no noise and 1 to add noise to observations
		add_noise = 1
		! new sb 1/9/24
		! obs_choice = 1
		handle = 'obs_choice'
		call parse(handle, obs_choice)
		handle = 'obs_spacing'
		call parse(handle, obs_spacing)

		if (add_noise == 0) then
			noise(:) = 0
		else
			iseed(1) = 456
			iseed(2) = 75
			iseed(3) = 992
			iseed(4) = 55
			call dlarnv(3, iseed, nx*nx, noise)
			noise = noise / maxval(noise)
		end if

		if (obs_choice == 1) then
			! loop through state and choose obs at intervals ! 
			observations(:,:) = 0.0 - 999.0
			state_reshaped = reshape(state, (/nx, nx/)) + reshape(noise, (/nx,nx/))
			do y = 1, nx, obs_spacing
				do x = 1, nx, obs_spacing
					observations(y, x) = state_reshaped(y, x) 
				end do 
			end do
			! state = reshape(observations, (/nx*nx/))
			! print *, reshape(observations, (/nx*nx/))
		elseif (obs_choice == 2) then
			observations(:,:) = reshape(state, (/nx, nx/)) + reshape(noise, (/nx,nx/))
			observations(10:nx-10, 10:nx-10) = -999.0
		elseif (obs_choice == 3) then
			observations(:,:) = reshape(state, (/nx, nx/)) + reshape(noise, (/nx,nx/))
			obs_copy = observations
			obs_copy(10:nx-10, 10:nx-10) = -999.0
			do y = 10,nx-10,10
				do x = 10,nx-10,10
					obs_copy(y,x) = observations(y,x)
				end do
			end do
			observations = obs_copy
		elseif (obs_choice == 4) then 
			observations(:,:) = reshape(state, (/nx, nx/)) + reshape(noise, (/nx,nx/))
			observations(:, ceiling(real(nx)/2):nx) = -999.0 ! left
			! observations(:, 1:ceiling(real(nx)/2)) = -999 ! right
		elseif (obs_choice == 5) then
			observations(:,:) = reshape(state, (/nx, nx/)) + reshape(noise, (/nx,nx/))
			observations(ceiling(real(nx)/2):nx, :) = -999.0  ! bottom
			! observations(1:ceiling(real(nx)/2), :) = -999 ! top
		end if



		
		! write observations
		start_v(2) = iter-1
		count_v(2) = 1
		start_v(1) = 1
		count_v(1) = dim
		stat(1) = NF90_PUT_VAR(ncid_out, obs_id, reshape(observations, (/nx*nx/)), start=start_v, count=count_v)
		if (stat(1) /= NF90_noerr) then
			write (*,*) "ERROR WRITING OBSERVATION FILE" 
		end if

        ! print *, observations

	end do loop


	! close files, deallocate arrays
	j = j + 1
	stat(j) = NF90_CLOSE(ncid_in)
	j = j + 1
	stat(j) = NF90_CLOSE(ncid_out)

	deallocate(state)


	write (*,*) "Done writing observations"

	! call read_observations(1600)



end program