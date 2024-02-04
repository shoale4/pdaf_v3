module ouput_netcdf


	use netcdf

	implicit none
	save
	public

	character(len=100) :: filename = 'state.nc'
	integer :: delt_write = 1    ! output interval

	integer, private :: file_position, count_step, file_id

	contains

		subroutine init_netcdf(step, dt, dim, state)

			implicit none

			! args
			integer, intent(in) :: step
			! real, intent(in) :: time
			real(8), intent(in) :: dt
			integer, intent(in) :: dim
			real(8), intent(in) :: state(dim)

			! local vars
			integer :: i, j
			integer :: dim_id_state, dim_id_1  ! dimension ids
			integer :: dim_id_step  ! dimension id
			integer :: dt_id, step_id, state_id  ! variable ids
			integer :: stat(50)   ! for status flag
			integer :: dim_arr(2) ! array for dimensions
			integer :: pos(2)     ! position index for writing
			integer :: cnt(2)   ! count index for writing
			character(len=100) :: attribute_str

			! init file !
			print *, "Init netcdf file output"

			! init file position and counter
			file_position = 1
			count_step = 1

			! initialize file and write global attributes
			doout: if (delt_write>0) then
				j = 1

				stat(j) = NF90_CREATE(TRIM(filename), 0, file_id)   ! 0 means to overwrite file with same name
				j = j + 1

				attribute_str = "ms_v4"
				stat(j) = NF90_PUT_ATT(file_id, NF90_GLOBAL, 'title', TRIM(attribute_str))
				j = j + 1

				! define dimensions
				stat(j) = NF90_DEF_DIM(file_id, 'dim_state', dim, dim_id_state)
				j = j + 1
				stat(j) = NF90_DEF_DIM(file_id, 'time_steps', NF90_UNLIMITED, dim_id_step)
				j = j + 1

				! define variables
				stat(j) = NF90_DEF_VAR(file_id, 'dt', NF90_DOUBLE, dt_id)
				j = j + 1
				stat(j) = NF90_DEF_VAR(file_id, 'step', NF90_DOUBLE, step_id)
				j = j + 1

				dim_arr(1) = dim_id_state
				dim_arr(2) = dim_id_step
				stat(j) = NF90_DEF_VAR(file_id, 'state', NF90_DOUBLE, dim_arr, state_id)
				j = j + 1

				stat(j) = NF90_ENDDEF(file_id)
				j = j + 1

				! write initial and constant variables
				stat(j) = NF90_PUT_VAR(file_id, dt_id, dt)  ! dt
				j = j + 1

				pos(1) = 1
				cnt(1) = 1
				stat(j) = NF90_PUT_VAR(file_id, step_id, step, start=pos(1:1))  ! step
				j = j + 1

				pos(1) = 1
				pos(2)= file_position
				cnt(1) = dim
				cnt(2) = 1
				stat(j) = NF90_PUT_VAR(file_id, state_id, state, start=pos(1:1))  ! model field
				j = j + 1

				do i = 1, j-1
					if (stat(i) /= NF90_noerr) then
						write (*,*) "NETCDF INITIALIZATION ERROR, STAT LINE ", i 
					end if
				end do 					
				
			end if doout


		end subroutine


		subroutine write_netcdf(step, dim, state)

			implicit none

			! args
			integer, intent(in) :: step
			integer, intent(in) :: dim
			real(8), intent(in) :: state(dim)

			! local vars
			integer :: i, j
			integer :: step_id
			integer :: state_id
			integer :: stat(50)
			integer :: pos(2)
			integer :: cnt(2)
			logical :: dowrite


			! check if we write at this time step
			if (count_step == delt_write) then 
				dowrite = .true.
				count_step = 1
				file_position = file_position + 1
			else
				dowrite = .false.
				count_step = count_step + 1
			end if 

			dooutput: if (dowrite) then

				! get var ids
				j = 1
				stat(j) = NF90_INQ_VARID(file_id, 'step', step_ID)
				j = j + 1
				stat(j) = NF90_INQ_VARID(file_id, 'state', state_id)  
				j = j + 1

				pos(1) = file_position
				cnt(1) = 1
				stat(j) = NF90_PUT_VAR(file_id, step_id, step, start=pos(1:1))
				j = j + 1

				pos(1) = 1
				pos(2) = file_position
				cnt(1) = dim
				cnt(2) = 1
				stat(j) = NF90_PUT_VAR(file_id, state_id, state, start=pos(1:2), count=cnt)


			end if dooutput

		end subroutine 

		subroutine close_netcdf()

			implicit none

			! local vars
			integer :: stat(50)

			! close netcdf file
			stat(1) = NF90_CLOSE(file_id)
			if (stat(1) /= NF90_NOERR) then
				write (*,*) "ERROR CLOSING NETCDF FILE"
			end if

		end subroutine


end module 