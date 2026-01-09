!!! based on enkf_chgres_recenter_nc
!!! cory.r.martin@noaa.gov 2025-12-04
 program tref_calc

 use setup, only       : program_setup
 use interp, only      : gaus_to_gaus
 use input_data, only  : read_input_data
 use output_data, only : set_output_grid, write_output_data

 implicit none

 call w3tagb('TREF_CALC',2025,0330,0085,'NP20')

 print*,"STARTING PROGRAM"

!--------------------------------------------------------
! Read configuration namelist.
!--------------------------------------------------------

 call program_setup

!--------------------------------------------------------
! Read input grid data
!--------------------------------------------------------

 call read_input_data

!--------------------------------------------------------
! Get output grid specs
!--------------------------------------------------------

 call set_output_grid

!--------------------------------------------------------
! Interpolate data to output grid
!--------------------------------------------------------

 call gaus_to_gaus

!--------------------------------------------------------
! Write output data to file.
!--------------------------------------------------------

 call write_output_data

 print*
 print*,"PROGRAM FINISHED NORMALLY!"

 call w3tage('TREF_CALC')

 stop

 end program tref_calc
