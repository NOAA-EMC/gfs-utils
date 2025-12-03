!!! Compute tref difference between sfcanl.nc and sfcf006.nc
!!! Based on enkf_chgres_recenter_nc
!!! cory.r.martin@noaa.gov 2025-12-03
 program tref_diff

 use setup, only       : program_setup
 use interp, only      : interpolate_to_target
 use input_data, only  : read_input_data
 use output_data, only : write_output_data

 implicit none

 call w3tagb('TREF_CALC',2025,0337,0085,'NP20')

 print*,"STARTING TREF_CALC PROGRAM"

!--------------------------------------------------------
! Read configuration namelist.
!--------------------------------------------------------

 call program_setup

!--------------------------------------------------------
! Read input grid data from both files
!--------------------------------------------------------

 call read_input_data

!--------------------------------------------------------
! Interpolate sfcanl.nc to resolution of sfcf006.nc
!--------------------------------------------------------

 call interpolate_to_target

!--------------------------------------------------------
! Write output data to file (difference already computed).
!--------------------------------------------------------

 call write_output_data

 print*
 print*,"TREF_CALC PROGRAM FINISHED NORMALLY!"

 call w3tage('TREF_CALC')

 stop

 end program tref_diff
