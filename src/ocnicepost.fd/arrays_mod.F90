module arrays_mod

  use init_mod , only : nxt, nyt, nlevs, nxr, nyr
  use init_mod , only : debug, logunit
  use init_mod , only : vardefs

  implicit none

  integer :: nbilin2d     !< the number of 2D fields mapped bilinearly
  integer :: nbilin3d     !< the number of 3D fields mapped bilinearly
  integer :: nconsd2d     !< the number of 2D fields mapped conservatively

  ! source arrays
  real, allocatable, dimension(:,:)   :: bilin2d  !< packed 2D source fields for bilinear remap
  real, allocatable, dimension(:,:)   :: consd2d  !< packed 2D source fields for conservative remap
  real, allocatable, dimension(:,:,:) :: bilin3d  !< packed 3D source fields for bilinear remap

  type(vardefs), allocatable, dimension(:) :: b2d !< variable metadata for 2D source fields bilinear remap
  type(vardefs), allocatable, dimension(:) :: c2d !< variable metadata for 2D source fields conservative remap
  type(vardefs), allocatable, dimension(:) :: b3d !< variable metadata for 3D source fields bilinear remap

  ! destination arrays
  real, allocatable, dimension(:,:)   :: rgb2d !< packed 2D fields with bilinear remap
  real, allocatable, dimension(:,:)   :: rgc2d !< packed 2D fields with conservative remap
  real, allocatable, dimension(:,:,:) :: rgb3d !< packed 3D fields with bilinear remap

  real, allocatable, dimension(:,:)  :: dstlon !< the destination grid longitudes
  real, allocatable, dimension(:,:)  :: dstlat !< the destination grid latitudes

  public setup_packing

contains

  subroutine setup_packing(nvalid, vars)

    type(vardefs), intent(inout) :: vars(:)
    integer      , intent(in)    :: nvalid

    ! local variables
    integer :: n,i,j,k

    ! --------------------------------------------------------
    ! count numbers of fields to remapped for each
    ! mapping type; these can be remapped as packed arrays
    ! --------------------------------------------------------

    nbilin2d = 0; nbilin3d = 0; nconsd2d = 0
    do n = 1,nvalid
       if (trim(vars(n)%var_remapmethod)  == 'bilinear') then
          if (vars(n)%var_dimen == 2) nbilin2d = nbilin2d + 1
          if (vars(n)%var_dimen == 3) nbilin3d = nbilin3d + 1
       end if
       if (trim(vars(n)%var_remapmethod)  == 'conserve')nconsd2d = nconsd2d + 1  !no 3d variables w/ conservative mapping
    end do
    if (debug) write(logunit,'(3(a,i4))')'bilin 2d ',nbilin2d,' bilin 3d ',nbilin3d,' conserv 2d ',nconsd2d

    ! initialization required when compiled with sinit_arrays=nan
    if (nbilin2d > 0) then
       allocate(bilin2d(nxt*nyt,nbilin2d)); bilin2d = 0.0
       allocate(b2d(1:nbilin2d))
       if (debug) write(logunit,'(a)')'allocate bilin2d fields and types '
    end if
    if (nconsd2d > 0) then
       allocate(consd2d(nxt*nyt,nconsd2d)); consd2d = 0.0
       allocate(c2d(1:nconsd2d))
       if (debug) write(logunit,'(a)')'allocate consd2d fields and types '
    end if
    if (nbilin3d > 0) then
       allocate(bilin3d(nxt*nyt,nlevs,nbilin3d)); bilin3d = 0.0
       allocate(b3d(1:nbilin3d))
       if (debug) write(logunit,'(a)')'allocate bilin3d fields and types '
    end if

    ! --------------------------------------------------------
    ! create types for each packed array and fill values
    ! --------------------------------------------------------

    i = 0; j = 0; k = 0
    do n = 1,nvalid
       if (trim(vars(n)%var_remapmethod) == 'bilinear') then
          if (vars(n)%var_dimen == 2 .and. allocated(b2d)) then
             i = i+1; b2d(i) = vars(n)
          end if
          if (vars(n)%var_dimen == 3 .and. allocated(b3d)) then
             j = j+1; b3d(j) = vars(n)
          end if
       end if
       if (trim(vars(n)%var_remapmethod) == 'conserve' .and. allocated(c2d)) then
          k = k+1; c2d(k) = vars(n)
       end if
    end do

    ! --------------------------------------------------------
    ! create arrays for remapped packed fields
    ! --------------------------------------------------------

     if (nbilin2d > 0) then
        allocate(rgb2d(nxr*nyr,nbilin2d)); rgb2d = 0.0
     end if
     if (nconsd2d > 0) then
        allocate(rgc2d(nxr*nyr,nconsd2d)); rgc2d = 0.0
     end if
     if (nbilin3d > 0) then
        allocate(rgb3d(nxr*nyr,nlevs,nbilin3d)); rgb3d = 0.0
     end if

  end subroutine setup_packing
end module arrays_mod
