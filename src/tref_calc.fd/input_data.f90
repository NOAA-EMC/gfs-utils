 module input_data

 use setup
 use module_ncio

 implicit none

 private

 real, allocatable, public                    :: tref_source(:)
 real, allocatable, public                    :: tref_target(:)
 real, allocatable, public                    :: rlat_source(:)
 real, allocatable, public                    :: rlon_source(:)
 real, allocatable, public                    :: rlat_target(:)
 real, allocatable, public                    :: rlon_target(:)
 integer, allocatable, public                 :: msk_target(:)

 integer, public                              :: kgds_source(200)
 integer, public                              :: kgds_target(200)

 public                                       :: read_input_data

 contains

 subroutine read_input_data

!-------------------------------------------------------------------------------------
! Read tref from sfcanl.nc and sfcf006.nc
!-------------------------------------------------------------------------------------

 implicit none

 type(Dataset) :: indset
 type(Dimension) :: ncdim
 real, allocatable                            :: work2d(:,:)
 real, allocatable                            :: lat2d(:,:), lon2d(:,:)

!-------------------------------------------------------------------------------------
! Read source file (sfcanl.nc)
!-------------------------------------------------------------------------------------

 print*
 print*,"OPEN SOURCE FILE: ",trim(sfcanl_file)
 indset = open_dataset(sfcanl_file)

 print*,"GET SOURCE FILE HEADER"
 ncdim = get_dim(indset, 'grid_xt'); i_source = ncdim%len
 ncdim = get_dim(indset, 'grid_yt'); j_source = ncdim%len

 print*,'DIMENSIONS OF SOURCE DATA ARE: ', i_source, j_source

 ij_source = i_source * j_source

 print*
 print*,"READ TREF FROM SOURCE FILE"
 call read_vardata(indset, 'tref', work2d)

 allocate(tref_source(ij_source))
 tref_source = reshape(work2d,(/ij_source/))
 print*,'MAX/MIN SOURCE TREF: ',maxval(tref_source), minval(tref_source)

 deallocate(work2d)

!-------------------------------------------------------------------------------------
! Read lat/lon for source grid
!-------------------------------------------------------------------------------------

 print*,"READ SOURCE GRID LAT"
 call read_vardata(indset, 'lat', lat2d)
 allocate(rlat_source(ij_source))
 rlat_source = reshape(lat2d,(/ij_source/))

 print*,"READ SOURCE GRID LON"
 call read_vardata(indset, 'lon', lon2d)
 allocate(rlon_source(ij_source))
 rlon_source = reshape(lon2d,(/ij_source/))

 deallocate(lat2d, lon2d)

 call close_dataset(indset)

!-------------------------------------------------------------------------------------
! Calculate kgds for source grid
!-------------------------------------------------------------------------------------

 kgds_source = 0
 call calc_kgds(i_source, j_source, kgds_source)

!-------------------------------------------------------------------------------------
! Read target file (sfcf006.nc)
!-------------------------------------------------------------------------------------

 print*
 print*,"OPEN TARGET FILE: ",trim(sfcf006_file)
 indset = open_dataset(sfcf006_file)

 print*,"GET TARGET FILE HEADER"
 ncdim = get_dim(indset, 'grid_xt'); i_target = ncdim%len
 ncdim = get_dim(indset, 'grid_yt'); j_target = ncdim%len

 print*,'DIMENSIONS OF TARGET DATA ARE: ', i_target, j_target

 ij_target = i_target * j_target

 print*
 print*,"READ TREF FROM TARGET FILE"
 call read_vardata(indset, 'tref', work2d)

 allocate(tref_target(ij_target))
 tref_target = reshape(work2d,(/ij_target/))
 print*,'MAX/MIN TARGET TREF: ',maxval(tref_target), minval(tref_target)

 deallocate(work2d)

 print*
 print*,"READ LAND MASK FROM TARGET FILE"
 call read_vardata(indset, 'land', work2d)

 allocate(msk_target(ij_target))
 msk_target = nint(reshape(work2d,(/ij_target/)))
 print*,'MAX/MIN TARGET LAND MASK: ',maxval(msk_target), minval(msk_target)

 deallocate(work2d)

!-------------------------------------------------------------------------------------
! Read lat/lon for target grid
!-------------------------------------------------------------------------------------

 print*,"READ TARGET GRID LAT"
 call read_vardata(indset, 'lat', lat2d)
 allocate(rlat_target(ij_target))
 rlat_target = reshape(lat2d,(/ij_target/))

 print*,"READ TARGET GRID LON"
 call read_vardata(indset, 'lon', lon2d)
 allocate(rlon_target(ij_target))
 rlon_target = reshape(lon2d,(/ij_target/))

 deallocate(lat2d, lon2d)

 call close_dataset(indset)

!-------------------------------------------------------------------------------------
! Calculate kgds for target grid
!-------------------------------------------------------------------------------------

 kgds_target = 0
 call calc_kgds(i_target, j_target, kgds_target)

 end subroutine read_input_data

 subroutine calc_kgds(im, jm, kgds)
!------------------------------------------------------------------
! Create the grib 1 grid description section for a global
! gaussian grid.
!------------------------------------------------------------------

 implicit none

 integer, intent(in)               :: im, jm
 integer, intent(inout)            :: kgds(200)

 integer                           :: kscan=0, nscan=0, jm2
 real                              :: dx, dy
 real, parameter                   :: PI=3.14159265358979323

 kgds=0

 kgds(1)=4          ! oct 6, type of grid (gaussian)
 kgds(2)=im         ! oct 7-8, # pts on latitude circle
 jm2 = jm/2
 kgds(3)=jm2        ! oct 9-10, # pts on longitude meridian
 kgds(4)=90000      ! oct 11-13, lat of origin
 kgds(5)=0          ! oct 14-16, lon of origin
 kgds(6)=128        ! oct 17, resolution flag
 kgds(7)=-90000     ! oct 18-20, lat of extreme point
 dx = float(360*1000)/float(im)
 kgds(8)=nint(dx)   ! oct 21-23, lon of extreme point
 dy = float(jm2)
 kgds(9)=nint(dy)   ! oct 24-25, number of circles pole to equator
 kgds(10)=0         ! oct 26-27, not used
 kgds(11)=kscan     ! oct 28, scanning mode flag
 kgds(12) = nscan   ! oct 29-32, not used
 kgds(20)=255       ! oct 5, not used
 kgds(21) = jm      ! full number of latitude points

 end subroutine calc_kgds

 end module input_data
