 module interp

 implicit none

 private

 real, allocatable, public      :: dtf(:)

 public                         :: interpolate_to_target

 contains

 subroutine interpolate_to_target

!---------------------------------------------------------------------------------
! Interpolate tref from source grid to target grid and compute difference
!---------------------------------------------------------------------------------

 use input_data
 use setup
 use ip_mod, only: ipolates

 implicit none

 integer                       :: iret, numpts
 integer                       :: ip, ipopt(20)
 integer                       :: num_fields
 integer, allocatable          :: ibi(:), ibo(:)
 logical*1, allocatable        :: bitmap_input(:,:), bitmap_output(:,:)
 real, allocatable             :: tref_source_interp(:)
 real, allocatable             :: data_input(:,:), data_output(:,:)
 real, allocatable             :: rlat_output(:), rlon_output(:)
 real, allocatable             :: rlat_target_deg(:), rlon_target_deg(:)

!---------------------------------------------------------------------------------
! Set up interpolation
!---------------------------------------------------------------------------------

 ip = 0        ! bilinear interpolation
 ipopt = 0

 num_fields = 1        ! number of fields to interpolate (just tref)

!---------------------------------------------------------------------------------
! Set up bitmaps and input/output arrays (no missing values)
!---------------------------------------------------------------------------------

 allocate(ibi(num_fields))
 ibi = 0 ! no bitmap
 allocate(ibo(num_fields))
 ibo = 0 ! no bitmap
 allocate(bitmap_input(ij_source,num_fields))
 bitmap_input = .true.
 allocate(bitmap_output(ij_target,num_fields))
 bitmap_output = .true.

 allocate(data_input(ij_source,num_fields))
 data_input(:,1) = tref_source(:)
 deallocate(tref_source)

 allocate(data_output(ij_target,num_fields))
 data_output = 0.0

!---------------------------------------------------------------------------------
! Allocate array for interpolated source tref
!---------------------------------------------------------------------------------

 allocate(tref_source_interp(ij_target))
 tref_source_interp = 0.0

!---------------------------------------------------------------------------------
! Allocate lat/lon arrays
!---------------------------------------------------------------------------------

 allocate(rlat_output(ij_target))
 rlat_output = 0.0
 allocate(rlon_output(ij_target))
 rlon_output = 0.0

!---------------------------------------------------------------------------------
! Perform interpolation using IPOLATES
!---------------------------------------------------------------------------------
 call ipolates(ip, ipopt, kgds_source, kgds_target, ij_source, ij_target, &
               num_fields, ibi, bitmap_input, data_input, &
               numpts, rlat_output, rlon_output, ibo, bitmap_output, &
               data_output, iret)

 if (iret /= 0) then
   print*,"FATAL ERROR IN IPOLATES. IRET IS: ", iret
   stop
 endif


 if (iret == 0) then
   tref_source_interp = data_output(:,num_fields)
 endif
 print*,'MAX/MIN INTERPOLATED SOURCE TREF: ',maxval(tref_source_interp), minval(tref_source_interp)

!---------------------------------------------------------------------------------
! Compute difference: dtf = tref_source_interp - tref_target
!---------------------------------------------------------------------------------

 allocate(dtf(ij_target))
 dtf = tref_source_interp - tref_target

 print*,'MAX/MIN DTF: ',maxval(dtf), minval(dtf)

!---------------------------------------------------------------------------------
! Clean up
!---------------------------------------------------------------------------------

 deallocate(ibi, ibo)
 deallocate(bitmap_input, bitmap_output)
 deallocate(data_input, data_output)
 deallocate(tref_source_interp)
 deallocate(rlat_output, rlon_output)

 end subroutine interpolate_to_target

 end module interp
