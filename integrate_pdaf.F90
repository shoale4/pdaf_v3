!$Id: integrate_pdaf.F90 870 2021-11-22 14:02:55Z lnerger $
!>  Time stepping loop with adaption for assimilation
!!
!! Time integration for simple 2D tutorial model
!! without parallelization of the model. In this
!! code variant the coupling to PDAF for ensemble
!! assimilation is completed.
!!
!! Each time step the field is shifted by one grid 
!! point in the vertical direction (first array index).
!!
!! __Revision history:__
!! * 2013-09 - Lars Nerger - Initial code
!! * Later revisions - see repository log
!!

SUBROUTINE integrate_pdaf()

  USE mod_model, &          ! Model variables
      ONLY: nx, ny, v, h, total_steps, dt, init1, init2, initens, prepostens, prepoststate, init1, init2, &
      obs, tau_in, tau_out, tau_open, tau_close, v_gate, v_stim, endtime, stim_dur, spiraltime, dx, diff, &
      total_steps, nsteps, nstimdur, d_to_dx2, nspiraltime, log1, logint1, jin, jout, dv, dh, xlap, jstim, &
      xlap1, xlap2, intpdaf1, intpdaf2, init1, init2, obs, intpdaf1, intpdaf2, & 
      tau_in, tau_out, tau_open, tau_close, v_gate, v_stim, dx, diff, spinup_time, model_start, spinup_phase, &
      file_output_choice
  ! USE mod_model

!   USE mod_parallel_pdaf, &  ! Parallelization variables
!        ONLY: mype_world
	use ouput_txt, &
		 only: write_txt

  IMPLICIT NONE

! *** local variables ***
  INTEGER :: step, k, count, i, j      ! Counters
  character(len=5) :: filename
!   CHARACTER(len=5) :: stepstr  ! String for time step

! #ifdef USE_PDAF
! 	! tau_close = 150
! 	 total_steps = total_steps - spinup_time
! #endif USE_PDAF

! #ifndef USE_PDAF
! 	 if (model_start .eq. 1) then
! 	 	total_steps = total_steps - spinup_time
! 	 endif 
! #endif USE_PDAF

! time loop (ifdef stuff new sb 1/9/24)
#ifndef USE_PDAF 
	if (spinup_phase .eq. 0) then
		total_steps = spinup_time
	end if
	do step = 1, total_steps
#endif 

#ifdef USE_PDAF
	do step = 1, total_steps
#endif 


#ifndef USE_PDAF
	if (spinup_phase .eq. 0) then
		if (step .eq. 1) then
			v(:, 1:10) = 0.5
		end if 
		if (step .eq. nspiraltime) then
			v(1:(nx/2), :) = 0
		end if 
	endif
#endif

		jstim = 0

		! calculate currents
		jin = h*v*(v-v_gate)*(1-v)/tau_in
		jout = -(1-h)*v/tau_out


		! update derivitaves for state variables
		dv = jin + jout + jstim

		! doing the (v<vgate) and (v>=vgate)
		log1 = (v<v_gate)
		where(log1)
			logint1 = 1
		else where
			logint1 = 0
		end where


		dh = (logint1)*((1-h)/tau_open)+(1-logint1)*(-h/tau_close)

		! calculate diffusive coupling
		xlap(:,:) = 0
		do j = 1, nx
			do i = 1, nx
				if (i .eq. 1) then
					xlap1 = 2*(v(2,j) - v(1,j))
				else if (i .eq. nx) then
					xlap1 = 2*(v(nx-1,j) - v(nx,j)) 
				else
					xlap1 = v(i-1,j) - 2*v(i,j) + v(i+1,j)
				end if 

				if (j .eq. 1) then 
					xlap2 = 2*(v(i,2) - v(i,1))
				else if (j .eq. nx) then
					xlap2 = 2*(v(i, nx-1) - v(i, nx))
				else
					xlap2 = v(i,j-1) - 2*v(i,j) + v(i,j+1)
				end if 

				xlap(i,j) = xlap1 + xlap2
			end do 			
		end do
		xlap = xlap * diff * d_to_dx2


		! integrate using forward euler method
		v = v + dt*dv + xlap
		h = h + dt*dh

#ifndef USE_PDAF
		if (spinup_phase .eq. 0) then
			if (step == spinup_time) then
				! initial file output for spin-up cases (new sb 9/18/23)
			  OPEN(11, file = 'outputs/for_elizabeth/v_init', form='unformatted', &
			  	access='stream', action='write', status='replace')
			  ! DO i = 1, nx
			  ! WRITE (11) v(i, :)
			  WRITE (11) reshape(v, (/nx*nx/))
			  ! END DO
			  close(11)

			  OPEN(12, file = 'outputs/for_elizabeth/h_init', form='unformatted', &
			  	access='stream', action='write', status='replace')
			  ! DO i = 1, nx
			  ! WRITE (12) h(i, :)
			  write(12) reshape(h, (/nx*nx/))
			  ! END DO
			  close(12)
			end if
		elseif (spinup_phase .eq. 1) then
			! netcdf file output
			if (mod(step, 5) == 0) then
				if (file_output_choice .eq. 1) then
					write (*,*) "writing state @ time step: ", step 
					write(filename, '(I0)') step
					! write (*,*) filename
					call write_txt(nx*nx, reshape(v, (/nx*nx/)), trim(adjustl(filename)))
				end if
			end if
		end if 
#endif

#ifdef USE_PDAF
    call assimilate_pdaf()
#endif 

  end do 

END SUBROUTINE integrate_pdaf
