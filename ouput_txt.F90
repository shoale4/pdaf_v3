!!!! NEW SB 1/23/24 !!!!
!! same idea as ouput_netcdf !!
!! but uses txt file output !!
!! rather than netcdf !!

module ouput_txt

	implicit none
	save
	public

	contains

		subroutine write_txt(dim, state, filename)

			implicit none

			! args
			integer, intent(in) :: dim
			real(8), intent(in) :: state(dim)
			character(len=5), intent(in) :: filename

			! local vars
			integer :: i, j
			integer :: nx

			print *, 'I/O File Option selected: TXT'


			OPEN (11, file='txt_file_data/state_step_'//TRIM(filename), form='unformatted', access='stream', &
				action='write', status='replace')
			! state_p = reshape(state, (/nx, nx/))
			! do i = 1, nx
			write (11) state
			! end do
			CLOSE(11)

		end subroutine

		subroutine read_txt(filename, state_p)

			implicit none

			! args
			character(len=16), intent(in) :: filename

			! local vars
			integer :: i, j
			real(8), intent(out) :: state_p(200*200)

			print *, 'I/O File Option selected: TXT'

			OPEN (11, file='txt_file_data/ens_obs/'//TRIM(filename), form='unformatted', &
					access='stream', action='read')
			! state_p = reshape(state, (/nx, nx/))
			! do i = 1, nx
			read (11) state_p
			! end do
			CLOSE(11)

		end subroutine



end module ouput_txt