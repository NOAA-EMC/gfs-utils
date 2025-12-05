 module input_data

 use utils
 use setup
 use module_ncio

 implicit none

 private

 integer, public                              :: ij_input, kgds_input(200)
 integer, public                              :: i_input, j_input


 real, allocatable, public                    :: tref_highres(:)
 real, allocatable, public                    :: tref_lowres(:)
 integer, allocatable, public                 :: slmsk_lowres(:)

 real  :: missing_value=1.e30

 public                                       :: read_input_data

 contains

 subroutine read_input_data

!-------------------------------------------------------------------------------------
! Read input grid data from a netcdf file.
!-------------------------------------------------------------------------------------

 implicit none

 type(Dataset) :: indset
 type(Dimension) :: ncdim
 real, allocatable                            :: work2d(:,:)

 print*
 print*,"OPEN INPUT FILE: ",trim(sfcanl_file)
 indset = open_dataset(sfcanl_file)

 print*,"GET INPUT FILE HEADER"
 ncdim = get_dim(indset, 'grid_xt'); i_input = ncdim%len
 ncdim = get_dim(indset, 'grid_yt'); j_input = ncdim%len

 print*,'DIMENSIONS OF DATA ARE: ', i_input, j_input

 ij_input = i_input * j_input

  print*
 print*,"READ TREF FROM input FILE"
 call read_vardata(indset, 'tref', work2d)

 allocate(tref_highres(ij_input))
 tref_highres = reshape(work2d,(/ij_input/))
 print*,'MAX/MIN HIGH RES TREF: ',maxval(tref_highres), minval(tref_highres)

 print*,"CLOSE FILE"
 call close_dataset(indset)
 deallocate(work2d)

!---------------------------------------------------------------------------------------
! Set the grib 1 grid description array need by the NCEP IPOLATES library.
!---------------------------------------------------------------------------------------

 call calc_kgds(i_input, j_input, kgds_input)

!-------------------------------------------------------------------------------------
! Read target file (sfcf006.nc)
!-------------------------------------------------------------------------------------

 print*
 print*,"OPEN TARGET FILE: ",trim(sfcf006_file)
 indset = open_dataset(sfcf006_file)

 print*,"GET TARGET FILE HEADER"
 ncdim = get_dim(indset, 'grid_xt'); i_output = ncdim%len
 ncdim = get_dim(indset, 'grid_yt'); j_output = ncdim%len

 print*,'DIMENSIONS OF TARGET DATA ARE: ', i_output, j_output

 ij_output = i_output * j_output

 print*
 print*,"READ TREF FROM TARGET FILE"
 call read_vardata(indset, 'tref', work2d)

 allocate(tref_lowres(ij_output))
 tref_lowres = reshape(work2d,(/ij_output/))
 print*,'MAX/MIN TARGET TREF: ',maxval(tref_lowres), minval(tref_lowres)

 deallocate(work2d)

 print*
 print*,"READ LAND MASK FROM TARGET FILE"
 call read_vardata(indset, 'land', work2d)

 allocate(slmsk_lowres(ij_output))
 slmsk_lowres = nint(reshape(work2d,(/ij_output/)))
 print*,'MAX/MIN TARGET LAND MASK: ',maxval(slmsk_lowres), minval(slmsk_lowres)

 deallocate(work2d)

 return

 end subroutine read_input_data

 end module input_data
