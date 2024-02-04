module output_netcdf_ass

     use netcdf

     implicit none
     save
     public

     ! public data
     character(len=20) :: ass_file = 'ass.nc'
     integer :: delt_write_ass = 1
     logical :: write_state = .true.
     logical :: write_stats = .false.
     logical :: write_ens = .false.

     integer, private :: file_position
     integer, private :: file_id
     integer, private :: count_steps

     contains


     subroutine init_netcdf_ass(step, dt, dim, dim_ens, rms_obs, delt_obs, &
                                total_steps, stepnull_means)
        
        implicit none

        ! args
        integer, intent(in) :: step
        real, intent(in) :: dt
        integer, intent(in) :: dim
        integer, intent(in) :: dim_ens
        real, intent(in) :: rms_obs
        integer, intent(in) :: delt_obs
        integer, intent(in) :: total_steps
        integer, intent(in) :: stepnull_means
        

        ! local vars
        integer :: i, j
        character(len=20) :: attstr
        integer :: dim_id_state, dim_id_step, dim_id_ens
        integer :: temp_id, dim_id_1
        integer :: dim_arr(2)
        integer :: dim_arr_3(3)
        integer :: state_id
        integer :: stat(100)


        write (*,*) 'Initialize assimilation output file'


        ! init file pos and count steps
        file_position = 1
        count_steps = 1

        ! init file 
        j = 1
        stat(j) = nf90_create(trim(ass_file), 0, file_id)
        j = j + 1

        attstr = "Assimilation output for mMS model"
        stat(j) = nf90_put_att(file_id, nf90_global, 'title', trim(attstr))
        j = j + 1

        ! define dims
        stat(j) = nf90_def_dim(file_id, 'dim_state', dim, dim_id_state)
        j = j + 1
        stat(j) = nf90_def_dim(file_id, 'iteration', nf90_unlimited, dim_id_step)
        j = j + 1
        stat(j) = nf90_def_dim(file_id, 'dim_ens', dim_ens, dim_id_ens)
        j = j + 1

        ! define vars that characterize experiment
        stat(j) = nf90_def_var(file_id, 'dim_ens', nf90_int, dim_id_1, temp_id)
        j = j + 1
        stat(j) = nf90_def_var(file_id, 'step_null', nf90_int, dim_id_1, temp_id)
        j = j + 1
        stat(j) = nf90_def_var(file_id, 'total_steps', nf90_int, dim_id_1, temp_id)
        j = j + 1
        stat(j) = nf90_def_var(file_id, 'rms_obs', nf90_int, dim_id_1, temp_id)
        j = j + 1
        stat(j) = nf90_def_var(file_id, 'delt_obs', nf90_int, dim_id_1, temp_id)
        j = j + 1
        stat(j) = nf90_def_var(file_id, 'dim_ens', nf90_int, dim_id_1, temp_id)
        j = j + 1
        stat(j) = nf90_def_var(file_id, 'step_null', nf90_int, dim_id_1, temp_id)
        j = j + 1

        
        ! define vars to be outputted
        j = 1
        stat(j) = nf90_def_var(file_id, 'step', nf90_int, dim_id_step, temp_id)
        j = j + 1
        stat(j) = nf90_def_var(file_id, 'step_ini', nf90_int, dim_id_1, temp_id)
        j = j + 1
        stat(j) = nf90_def_var(file_id, 'rmse_init', nf90_double, dim_id_1, temp_id)
        j = j + 1
        stat(j) = nf90_def_var(file_id, 'rmse_for', nf90_double, dim_id_1, temp_id)
        j = j + 1
        stat(j) = nf90_def_var(file_id, 'rmse_ana', nf90_double, dim_id_1, temp_id)
        j = j + 1
        stat(j) = nf90_def_var(file_id, 'true_rmse_init', nf90_double, dim_id_1, temp_id)
        j = j + 1
        stat(j) = nf90_def_var(file_id, 'true_rmse_for', nf90_double, dim_id_1, temp_id)
        j = j + 1
        stat(j) = nf90_def_var(file_id, 'true_rmse_ana', nf90_double, dim_id_1, temp_id)
        j = j + 1

        ! stat(j) = nf90_def_var(file_id, 'hist_true_null', nf90_int, dim_id_ensp1, temp_id)
        ! j = j + 1
        ! stat(j) = nf90_def_var(file_id, 'hist_mean_null', nf90_int, dim_id_ensp1, temp_id)
        ! j = j + 1
        ! stat(j) = nf90_def_var(file_id, 'hist_true_step', nf90_int, dim_id_ensp1, temp_id)
        ! j = j + 1
        ! stat(j) = nf90_def_var(file_id, 'hist_mean_step', nf90_int, dim_id_ensp1, temp_id)
        ! j = j + 1

        ! write states
        writestates: if (write_state) then
            dim_arr(1) = dim_id_state
            dim_arr(2) = dim_id_1
            stat(j) = nf90_def_var(file_id, 'state_ini', nf90_double, dim_arr, state_id)
            j = j + 1
            dim_arr(1) = dim_id_state
            dim_arr(2) = dim_id_step
            stat(j) = nf90_def_var(file_id, 'state_for', nf90_double, dim_arr, state_id)
            j = j + 1
            dim_arr(1) = dim_id_state
            dim_arr(2) = dim_id_step
            stat(j) = nf90_def_var(file_id, 'state_ana', nf90_double, dim_arr, state_id)
            j = j + 1
        end if writestates

        ! write ens
        writeens: if (write_ens) then
            dim_arr_3(1) = dim_id_state
            dim_arr_3(2) = dim_id_ens
            dim_arr_3(3) = dim_id_1
            stat(j) = nf90_def_var(file_id, 'ens_ini', nf90_double, dim_arr_3, state_id)
            j = j + 1
            dim_arr_3(1) = dim_id_state
            dim_arr_3(2) = dim_id_ens
            dim_arr_3(3) = dim_id_1
            stat(j) =  nf90_def_var(file_id, 'ens_for', nf90_double, dim_arr_3, state_id)          
            j = j + 1
            dim_arr_3(1) = dim_id_state
            dim_arr_3(2) = dim_id_ens
            dim_arr_3(3) = dim_id_step
            stat(j) = nf90_def_var(file_id, 'ens_ana', nf90_double, dim_arr_3, state_id)
            j = j + 1
        end if writeens

        ! end definitions
        stat(j) = nf90_enddef(file_id)
        j = j + 1

        ! write vars characterizing experiment
        stat(j) = nf90_inq_varid(file_id, 'dim_ens', temp_id)
        j = j + 1
        stat(j) = nf90_put_var(file_id, temp_id, dim_ens)
        j = j + 1
        stat(j) = nf90_inq_varid(file_id, 'step_null', temp_id)
        j = j + 1
        stat(j) = nf90_put_var(file_id, temp_id, step)
        j = j + 1
        stat(j) = nf90_inq_varid(file_id, 'total_steps', temp_id)
        j = j + 1
        stat(j) = nf90_put_var(file_id, temp_id, total_steps)
        j = j + 1
        stat(j) = nf90_inq_varid(file_id, 'rms_obs', temp_id)
        j = j + 1
        stat(j) = nf90_put_var(file_id, temp_id, rms_obs)
        j = j + 1
        stat(j) = nf90_inq_varid(file_id, 'delt_obs', temp_id)
        j = j + 1
        stat(j) = nf90_put_var(file_id, temp_id, delt_obs)
        j = j + 1
        ! stat(j) = nf90_put_var(file_id, temp_id, seedset)
        ! j = j + 1
        stat(j) = nf90_inq_varid(file_id, 'stepnull_means', temp_id)
        j = j + 1
        stat(j) = nf90_put_var(file_id, temp_id, stepnull_means)
        j = j + 1

        do i = 1, j-1
            if (stat(i) /= nf90_noerr) then
                write (*,*) 'NETCDF ASS OUTPUT FILE ERROR INITIALIZING, LINE', i
            end if
        end do

        write (*,*) 'Initialize assimilation output file'

     end subroutine init_netcdf_ass


     subroutine write_netcdf_ass(calltype, step, dim, state, rmse, true_rmse, &
                                dim_ens, ens)

     
        implicit none

        ! args
        character(len=3) :: calltype
        integer, intent(in) :: step
        integer, intent(in) :: dim
        real, intent(in) :: state(dim)
        real, intent(in) :: rmse
        real, intent(in) :: true_rmse
        integer, intent(in) :: dim_ens
        real, intent(in) :: ens(dim, dim_ens)
        ! integer, intent(in) :: hist_true(dim_ens+1, 2)
        ! integer, intent(in) :: hist_mean(dim_ens+1, 2)

        ! local vars
        integer :: i, j
        integer :: stat(100)
        integer :: state_id, ens_id, step_id
        logical :: dowrite
        



        if (count_steps == delt_write_ass) then
            dowrite = .true.
            if (calltype == 'ana') then
                count_steps = 1
            end if
        else
            dowrite = .false.
            if (calltype == 'ana') then
                count_steps = count_steps + 1
            end if
        end if


        ! inquire var ids
        j = 1
        if (calltype == 'ini') then
            if (write_state) then
                stat(j) = nf90_inq_varid(file_id, 'state_ini', state_id)
                j = j + 1
            end if
            if (write_ens) then
                stat(j) = nf90_inq_varid(file_id, 'ens_ini', ens_id)
                j = j + 1
            end if
            stat(j) = nf90_inq_varid(file_id, 'step_ini', step_id)
            j = j + 1
            stat(j) = nf90_inq_varid(file_id, 'rmse_ini', ens_id)
            j = j + 1
            stat(j) = nf90_inq_varid(file_id, 'true_rmse_ini', ens_id)
            j = j + 1
        else if (calltype == 'for') then
            if (write_state) then
                stat(j) = nf90_inq_varid(file_id, 'state_for', state_id)
                j = j + 1
            end if
            if (write_ens) then
                stat(j) = nf90_inq_varid(file_id, 'ens_for', ens_id)
                j = j + 1
            end if
            stat(j) = nf90_inq_varid(file_id, 'step_for', step_id)
            j = j + 1
            stat(j) = nf90_inq_varid(file_id, 'rmse_for', ens_id)
            j = j + 1
            stat(j) = nf90_inq_varid(file_id, 'true_rmse_for', ens_id)
            j = j + 1
        else
            if (write_state) then
                stat(j) = nf90_inq_varid(file_id, 'state_ana', state_id)
                j = j + 1
            end if
            if (write_ens) then
                stat(j) = nf90_inq_varid(file_id, 'ens_ana', ens_id)
                j = j + 1
            end if
            stat(j) = nf90_inq_varid(file_id, 'step_ana', step_id)
            j = j + 1
            stat(j) = nf90_inq_varid(file_id, 'rmse_ana', ens_id)
            j = j + 1
            stat(j) = nf90_inq_varid(file_id, 'true_rmse_ana', ens_id)
            j = j + 1
        end if

        do i = 1, j-1
            if (stat(i) /= nf90_noerr) then
                write (*,*) 'NETCDF ASS OUTPUT FILE ERROR PREPPING OUTPUT, LINE', i
            end if
        end do     


     end subroutine write_netcdf_ass

end module