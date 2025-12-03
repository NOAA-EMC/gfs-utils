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

 implicit none

 integer                       :: iret
 integer                       :: ip, ipopt(20)
 integer                       :: km, count_target
 integer                       :: no, ij_target
 logical*1, allocatable        :: bitmap_source(:), bitmap_target(:)
 real, allocatable             :: tref_source_interp(:)
 real, allocatable             :: rlat_target_deg(:), rlon_target_deg(:)
 real, allocatable             :: rlat_source_deg(:), rlon_source_deg(:)

!---------------------------------------------------------------------------------
! Set up interpolation
!---------------------------------------------------------------------------------

 ip = 0        ! bilinear interpolation
 ipopt = 0

 km = 1        ! number of fields to interpolate (just tref)
 no = ij_target

!---------------------------------------------------------------------------------
! Set up bitmaps (no missing values)
!---------------------------------------------------------------------------------

 allocate(bitmap_source(ij_source))
 bitmap_source = .true.

 allocate(bitmap_target(ij_target))
 bitmap_target = .false.

!---------------------------------------------------------------------------------
! Allocate array for interpolated source tref
!---------------------------------------------------------------------------------

 allocate(tref_source_interp(ij_target))
 tref_source_interp = 0.0

!---------------------------------------------------------------------------------
! Convert lat/lon from radians to degrees if necessary
! (module_ncio typically returns degrees, but check)
!---------------------------------------------------------------------------------

 allocate(rlat_source_deg(ij_source))
 allocate(rlon_source_deg(ij_source))
 allocate(rlat_target_deg(ij_target))
 allocate(rlon_target_deg(ij_target))

 rlat_source_deg = rlat_source
 rlon_source_deg = rlon_source
 rlat_target_deg = rlat_target
 rlon_target_deg = rlon_target

!---------------------------------------------------------------------------------
! Perform interpolation using IPOLATES
!---------------------------------------------------------------------------------

#ifdef IP_V4
 print*,"INTERPOLATE TREF FROM SOURCE TO TARGET GRID USING IPOLATES (V4)"
 call ipolates(ip, ipopt, kgds_source, kgds_target, &
               ij_source, ij_target, km, &
               bitmap_source, tref_source, &
               no, rlat_target_deg, rlon_target_deg, &
               tref_source_interp, iret)
#else
 print*,"INTERPOLATE TREF FROM SOURCE TO TARGET GRID USING IPOLATES"
 call ipolates(ip, ipopt, kgds_source, kgds_target, &
               ij_source, ij_target, km, &
               bitmap_source, tref_source, &
               count_target, &
               rlat_target_deg, rlon_target_deg, &
               bitmap_target, tref_source_interp, iret)
#endif

 if (iret /= 0) then
   print*,"FATAL ERROR IN IPOLATES. IRET IS: ", iret
   stop
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

 deallocate(bitmap_source, bitmap_target)
 deallocate(tref_source_interp)
 deallocate(rlat_source_deg, rlon_source_deg)
 deallocate(rlat_target_deg, rlon_target_deg)

 end subroutine interpolate_to_target

 end module interp
