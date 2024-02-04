! module read_ens

! 	use netcdf

! 	implicit none
! 	save 
! 	public

! 	contains

! 		subroutine read_ens()

! 			implicit none

! 			! args
! 			! integer, intent(in) :: step
! 			! integer, intent(inout) :: dim_obs


! 			! local vars
! 			integer :: i, k, j
! 			character(len=100) :: ens_file
! 			integer :: stat(50)
! 			integer :: file_id, step_id, ens_id
! 			! integer :: delt_obs
! 			real(8), allocatable :: ens_arr(:)
! 			integer :: pos(2), cnt(2)
! 			integer :: nx=200
! 			integer :: dim_ens
! 			integer :: nsteps_file

! 			! open obs file
! 			ens_file = '../ens.nc'
! 			 j = 1
! 			 stat(j) = NF90_OPEN(TRIM(ens_file), NF90_NOWRITE, file_id) 

! 			 ! read number of time steps
! 			 j = j + 1
! 			 stat(j) = NF90_INQ_DIMID(file_id, 'time_steps', step_id)
! 			 j = j + 1
! 			 stat(j) = NF90_Inquire_dimension(file_id, step_id, len=nsteps_file)

! 			 ! read time step info
! 			 ! j = j + 1
! 			 ! stat(j) = NF90_INQ_VARID(file_id, 'step', step_id)


! 			 ! initialize obs interval
! 			 ! delt_obs = 100    ! fine hardcoded for now but needs to be made dynamic (shoale 2/27)

! 			 ! read observations
! 			 dim_ens = nx*nx
! 			 allocate(ens_arr(dim_obs))

! 			 j = 1
! 			 stat(j) = NF90_INQ_VARID(file_id, 'ens', ens_id)

! 			 pos(2) = step
! 			 cnt(2) = 1
! 			 pos(1) = 1
! 			 cnt(1) = dim_ens
! 			 j = j + 1
! 			 stat(j) = NF90_GET_VAR(file_id, ens_id, ens_arr(:), start=pos, count=cnt)

! 			!  print *, ens_arr

! 		 end subroutine

! end module

program ens_prog

	! use read_ens
	use netcdf

	! new sb 1/9/24
	USE parser_no_mpi, & 
      ONLY: parse

	implicit none

	! define variables here !
	character(len=100) :: in_file, out_file, attribute_str
	integer :: j, iter
	integer :: file_id_in, file_id_out, dim_id
	integer :: dim, nsteps
	integer :: stat(50)
	integer :: ens_size
	integer :: dim_id_state, dim_id_ens, ens_id, state_id
	integer :: dim_ids(2)
	integer :: nx=200
	real(8), allocatable :: state(:)
	integer :: start_v(2), count_v(2)
	integer :: cnt
	integer :: add_noise
	real(8), allocatable :: noise(:)   ! noise array
	integer :: iseed(4)   ! for random number generation
	character(len=32) :: handle  ! new sb 1/9/24 -- for command line parsing, finally adding it!


	! input file
	in_file = "../state.nc"
	in_file = TRIM(in_file)

	! open trajectory file
	j = 1
	stat(j) = NF90_OPEN(in_file, NF90_NOWRITE, file_id_in)
	j = j + 1

	! get dimensions and time steps
	stat(j) = NF90_INQ_DIMID(file_id_in, 'dim_state', dim_id)
  	j = j + 1
  	stat(j) = NF90_Inquire_dimension(file_id_in, dim_id, len=dim)
  	j = j + 1
  	stat(j) = NF90_INQ_DIMID(file_id_in, 'time_steps', dim_id)
  	j = j + 1
  	stat(j) = NF90_Inquire_dimension(file_id_in, dim_id, len=nsteps)
  	j = j + 1

  	! get var id
  	stat(j) = NF90_INQ_VARID(file_id_in, 'state', state_id)

  	! compute steps to get ensemble from

  	! print *, dim
  	! print *, nsteps

  	!!! initialize output file for ensemble members !!!
  	
  	! define out file
  	out_file = "../ens.nc"
  	out_file = TRIM(out_file)
  	j = 1
  	stat(j) = NF90_CREATE(out_file, 0, file_id_out)

  	! title
  	attribute_str = "Ensemble members"
  	j = j + 1
  	stat(j) = NF90_PUT_ATT(file_id_out, NF90_GLOBAL, 'title', TRIM(attribute_str))

  	! define dimensions (1/9/24 - can now set ens_size from command line)
  	handle = 'ens_size'
  	call parse(handle, ens_size) ! hardcoded for now but needs to be made dynamic (sb 3/7) -- never made it dynamic lol (sb 9/7/23) -- 1/9/24 finally dynamic
  	j = j + 1
  	stat(j) = NF90_DEF_DIM(file_id_out, 'dim_state', dim, dim_id_state)
  	j = j + 1
  	stat(j) = NF90_DEF_DIM(file_id_out, 'ens_size', ens_size, dim_id_ens) 

	dim_ids(1) = dim_id_state
	dim_ids(2) = dim_id_ens

	j = j + 1
	stat(j) = NF90_DEF_VAR(file_id_out, 'ens', NF90_DOUBLE, dim_ids, ens_id)
	j = j + 1
	stat(j) = NF90_ENDDEF(file_id_out)
  	!!!!!											!!!


  	!!! READ TRAJECTORY AND GENERATE ENSEMBLE !!!
  	allocate(state(dim))
  	allocate(noise(dim))

  	! write (*,*) "Variable ID for 'state' in output file:", state_id
	! write (*,*) "Variable ID for 'ens' in output file:", ens_id


  	write (*,*) "Start ensemble generation"
  	write (*,*) nsteps

  	cnt = 1
  	loop: do iter = 2, nsteps-1, (nsteps-1)/(ens_size)
 		! read state vector
		start_v(2) = iter
		count_v(2) = 1
		start_v(1) = 1
		count_v(1) = dim
		stat(1) = NF90_GET_VAR(file_id_in, state_id, state, start=start_v, count=count_v)
		if (stat(1) /= NF90_noerr) then
			write (*,*) "ERROR READING STATE FILE" 
			write (*,*) stat(1)
		end if		


		! create noise (new sb 3/30/23)
		add_noise = 1

		if (add_noise == 0) then
			noise(:) = 0
		else
			iseed(1) = 543
			iseed(2) = 98
			iseed(3) = 1003
			iseed(4) = 31
			call dlarnv(3, iseed, nx*nx, noise)
			noise = noise / maxval(noise)
			! print *, noise
		end if

		! write ensemble
		! start_v(2) = iter-1
		write (*,*) "writing ensemble member: ", cnt
		start_v(2) = cnt
		count_v(2) = 1
		start_v(1) = 1
		count_v(1) = dim
		stat(1) = NF90_PUT_VAR(file_id_out, ens_id, state+noise, start=start_v, count=count_v)
		! stat(1) = NF90_PUT_VAR(file_id_out, ens_id, state)
		if (stat(1) /= NF90_noerr) then
			write (*,*) "ERROR WRITING ENSEMBLE FILE" 
			write (*,*) stat(1)
		end if
		! print *, count_v
		cnt = cnt + 1


	end do loop

	! close files, deallocate arrays
	j = j + 1
	stat(j) = NF90_CLOSE(file_id_in)
	j = j + 1
	stat(j) = NF90_CLOSE(file_id_out)


	deallocate(state)


	! call read_ens()


end program 