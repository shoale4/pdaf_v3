

subroutine get_true_rmse_bin(step, state_est, rmse_true)


    implicit none

    ! args
    integer, intent(in) :: step
    real(8), intent(in) :: state_est(200*200)
    real(8), intent(out) :: rmse_true

    ! local vars
    character(len=20) :: in_file
    integer :: k
    real(8), allocatable :: state(:)
    character(len=16) :: filename
    integer :: dim = 200*200
    integer :: dummy_step
    

    print *, 'I/O File Option selected: TXT'
    allocate(state(dim))
    dummy_step = step
    write(*,*) dummy_step
    write (filename, '(I5)') dummy_step
    filename = adjustl(trim(filename))

    OPEN (11, file='txt_file_data/state_step_'//trim(filename), form='unformatted', &
            access='stream', action='read')
    ! state_p = reshape(state, (/nx, nx/))
    ! do i = 1, nx
    read (11) state
    ! end do
    CLOSE(11)


    ! compute true rms
    rmse_true = 0.0
    do k = 1, dim
        rmse_true = rmse_true + (state(k) - state_est(k))**2
    end do
    rmse_true = sqrt(rmse_true / dim)



end subroutine get_true_rmse_bin