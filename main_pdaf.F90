!$Id: main_pdaf.F90 870 2021-11-22 14:02:55Z lnerger $
!>  Main driver for PDAF tutorial
!!
!! This is a simple model program to demonstrate the
!! fully-parallel implementation of the online mode of PDAF. 
!!
!! The simple model has a 2-dimensional mesh. The initial state
!! is read from a file. The time stepping consists in shifting
!! the field vertically (in the direction of the first array index)
!! by one grid point per time step. A period boundary condition is
!! applied by inserting the field from the upper boundary into the
!! lower one. 
!!
!! In this code variant the coupling to PDAF is completed.
!!
!! __Revision history:__
!! * 2013-09 - Lars Nerger - Initial code
!! * Later revisions - see repository log
!!
PROGRAM MAIN

#ifdef USE_PDAF
  USE mod_parallel_pdaf, &    ! Parallelization
       ONLY: mype_world
#endif 

  IMPLICIT NONE


! ********************************
! ***      INITIALIZATION      ***
! ********************************

#ifdef USE_PDAF
  ! Add parallelization for ensemble integration
  CALL init_parallel_pdaf(0, 1)
#endif

#ifdef USE_PDAF
  ! *** Initial Screen output ***
  IF (mype_world==0) THEN
     WRITE (*, '(/17x, a/)') '+++++ PDAF: Modified by Shoale Badr for Cardiac Data Assimilation +++++'
     WRITE (*, '(16x, a)') '2-D Modified Mitchell-Schaefer Model: Assimilation Phase'
     WRITE (*, '(/)')
  END IF
#endif 

  ! *** Initialize model ***
  CALL initialize()  

#ifdef USE_PDAF
  ! Initialize PDAF
  CALL init_pdaf()
#endif 


! *****************************
! ***      Integration      ***
! *****************************

  ! *** Perform ensmeble assimilation ***
  CALL integrate_pdaf()
  ! CALL assimilate_pdaf()

#ifdef USE_PDAF
  ! End parallelization
  CALL finalize_pdaf()
#endif 

END PROGRAM MAIN
