 module output_data

 implicit none

 private

 public                            :: write_output_data

 contains

 subroutine write_output_data

!-------------------------------------------------------------------
! Write dtf to a netcdf file matching write_tf_inc_nc format.
!-------------------------------------------------------------------

 use netcdf
 use input_data
 use setup
 use interp

 implicit none

 integer                           :: ncid
 integer                           :: lat_dimid, lon_dimid
 integer                           :: lat_varid, lon_varid
 integer                           :: dtf_varid, msk_varid
 integer, dimension(2)             :: start, count, dimids
 real, allocatable                 :: out2d(:,:)
 integer, allocatable              :: msk2d(:,:)
 real, allocatable                 :: lat_extended(:)
 integer                           :: j_extended
 real, parameter                   :: rad2deg = 57.2957795130823
 integer                           :: iret

 character (len = *), parameter :: lat_name = "latitude"
 character (len = *), parameter :: lon_name = "longitude"
 character (len = *), parameter :: dtf_name = "dtf"
 character (len = *), parameter :: msk_name = "msk"
 character (len = *), parameter :: units = "units"
 character (len = *), parameter :: dtf_units = "kelvin"
 character (len = *), parameter :: msk_units = "none"
 character (len = *), parameter :: lat_units = "degrees_north"
 character (len = *), parameter :: lon_units = "degrees_east"

!-------------------------------------------------------------------
! Create output file
!-------------------------------------------------------------------

 print*
 print*,'CREATE OUTPUT FILE: ',trim(output_file)
 
 iret = nf90_create(trim(output_file), cmode=ior(nf90_clobber,nf90_64bit_offset), ncid=ncid)
 if (iret /= nf90_noerr) then
   print*,'ERROR creating file: ',trim(nf90_strerror(iret))
   stop
 endif

!-------------------------------------------------------------------
! Define dimensions (add 2 to j_target for poles at -90 and 90)
!-------------------------------------------------------------------

 j_extended = j_target + 2

 iret = nf90_def_dim(ncid, lat_name, j_extended, lat_dimid)
 if (iret /= nf90_noerr) stop 'ERROR defining lat dimension'

 iret = nf90_def_dim(ncid, lon_name, i_target, lon_dimid)
 if (iret /= nf90_noerr) stop 'ERROR defining lon dimension'

!-------------------------------------------------------------------
! Define coordinate variables
!-------------------------------------------------------------------

 iret = nf90_def_var(ncid, lat_name, nf90_real, lat_dimid, lat_varid)
 if (iret /= nf90_noerr) stop 'ERROR defining lat variable'

 iret = nf90_def_var(ncid, lon_name, nf90_real, lon_dimid, lon_varid)
 if (iret /= nf90_noerr) stop 'ERROR defining lon variable'

!-------------------------------------------------------------------
! Assign units to coordinate variables
!-------------------------------------------------------------------

 iret = nf90_put_att(ncid, lat_varid, units, lat_units)
 if (iret /= nf90_noerr) stop 'ERROR defining lat units'

 iret = nf90_put_att(ncid, lon_varid, units, lon_units)
 if (iret /= nf90_noerr) stop 'ERROR defining lon units'

!-------------------------------------------------------------------
! Define dtf variable
!-------------------------------------------------------------------

 dimids = (/ lon_dimid, lat_dimid /)

 iret = nf90_def_var(ncid, dtf_name, nf90_double, dimids, dtf_varid)
 if (iret /= nf90_noerr) stop 'ERROR defining dtf variable'

 iret = nf90_def_var(ncid, msk_name, nf90_byte, dimids, msk_varid)
 if (iret /= nf90_noerr) stop 'ERROR defining msk variable'

 iret = nf90_put_att(ncid, dtf_varid, units, dtf_units)
 if (iret /= nf90_noerr) stop 'ERROR defining dtf units'

 iret = nf90_put_att(ncid, msk_varid, units, msk_units)
 if (iret /= nf90_noerr) stop 'ERROR defining msk units'

!-------------------------------------------------------------------
! End define mode
!-------------------------------------------------------------------

 iret = nf90_enddef(ncid)
 if (iret /= nf90_noerr) stop 'ERROR ending define mode'

!-------------------------------------------------------------------
! Write coordinate variables
!-------------------------------------------------------------------

 allocate(out2d(i_target,j_target))

 print*,"WRITE LAT"
 out2d = reshape(rlat_target, (/i_target,j_target/))
 
! Create extended latitude array with -90 at beginning and 90 at end
 allocate(lat_extended(j_extended))
 lat_extended(1) = -90.0
 lat_extended(2:j_target+1) = out2d(1,:)*rad2deg
 lat_extended(j_extended) = 90.0
 
 iret = nf90_put_var(ncid, lat_varid, lat_extended)
 if (iret /= nf90_noerr) stop 'ERROR writing lat'
 
 deallocate(lat_extended)

 print*,"WRITE LON"
 out2d = reshape(rlon_target, (/i_target,j_target/))
 iret = nf90_put_var(ncid, lon_varid, out2d(1,:)*rad2deg)
 if (iret /= nf90_noerr) stop 'ERROR writing lon'

!-------------------------------------------------------------------
! Write dtf variable (with extended dimension for poles)
!-------------------------------------------------------------------

 print*,"WRITE DTF"
 deallocate(out2d)
 allocate(out2d(i_target,j_extended))
 
! Fill with dummy value (0.0) at poles
 out2d(:,1) = 0.0
 out2d(:,2:j_target+1) = reshape(dtf, (/i_target,j_target/))
 out2d(:,j_extended) = 0.0
 
 count = (/ i_target, j_extended /)
 start = (/ 1, 1 /)
 
 iret = nf90_put_var(ncid, dtf_varid, out2d, start, count)
 if (iret /= nf90_noerr) stop 'ERROR writing dtf'

 print*,"WRITE MSK"
 allocate(msk2d(i_target,j_extended))
 
! Fill with dummy value (0) at poles
 msk2d(:,1) = 0
 msk2d(:,2:j_target+1) = reshape(msk_target, (/i_target,j_target/))
 msk2d(:,j_extended) = 0
 
 iret = nf90_put_var(ncid, msk_varid, msk2d, start, count)
 if (iret /= nf90_noerr) stop 'ERROR writing msk'

!-------------------------------------------------------------------
! Close file
!-------------------------------------------------------------------

 iret = nf90_close(ncid)
 if (iret /= nf90_noerr) stop 'ERROR closing file'

 deallocate(out2d, msk2d)
 deallocate(dtf)
 deallocate(msk_target)

 print*,"*** SUCCESS writing dtf file ", trim(output_file), "!"

 end subroutine write_output_data

 end module output_data
