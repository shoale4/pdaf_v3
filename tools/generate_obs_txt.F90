program generate_obs_txt


	use output_txt, &
		only: write_txt, read_txt

	use parser_no_mpi, &
		only: parse

	implicit none
	
	! local vars
	integer, allocatable :: steps(:)
  	integer :: nsteps
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
	character(len=16) :: filename
	integer :: x, y, iter


	dim = nx*nx
	allocate(state(dim))
	allocate(noise(dim))
	allocate(observations(nx, nx))
	allocate(obs_copy(nx,nx))
	nsteps = 18000

	write (*,*) "Start writing observations"

	loop: do iter = 5, nsteps, delt_obs

		write (*,*) "Writing observations @ time step: ", iter

		! read state vector
		write(filename, "(I0)") iter
		call read_txt(filename, state)


		! new observation sparcity -- sb 3/29 !!!
		! obs_choice is 1 for default obs (uniform at every xth grid point)
		! 2 for just borders
		! 3 for borders and uniform insid
		! 4 for left/right
		! 5 for top/bottom
		! set add_noise to 0 to run with no noise and 1 to add noise to observations
		add_noise = 1
		! new sb 1/9/24
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
		call write_txt(nx*nx, reshape(observations, (/nx*nx/)), 'obs_'//TRIM(filename))

        ! print *, observations

	end do loop



end program