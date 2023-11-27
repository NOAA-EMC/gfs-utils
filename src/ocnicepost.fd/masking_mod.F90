module masking_mod

  use netcdf

  use init_mod   , only : nxt, nyt, nlevs, nxr, nyr, vardefs
  use init_mod   , only : wgtsdir, ftype, fsrc, fdst, input_file
  use init_mod   , only : do_ocnpost, debug, logunit, maskvar
  use arrays_mod , only : dstlat, dstlon
  use utils_mod  , only : getfield, dumpnc, remap, nf90_err

  implicit none

  real, allocatable, dimension(:,:) :: mask3d     !< the 3D mask of the source fields on Ct grid points
  real, allocatable, dimension(:)   :: mask2d     !< the 2D mask of the source fields on Ct grid points

  real, allocatable, dimension(:,:) :: rgmask3d  !< the 3D mask of the destination fields on Ct grid points
  real, allocatable, dimension(:)   :: rgmask2d  !< the 2D mask of the destination fields on Ct grid points

  public remap_masks

contains

  subroutine remap_masks(vfill)

    real, intent(out) :: vfill

    ! local variables
    integer            :: rc, ncid, varid, n
    character(len=240) :: wgtsfile
    real               :: minlat = -79.75

    real, allocatable, dimension(:) :: out1d

    ! --------------------------------------------------------
    ! obtain the destination lat and lon directly from the weights file
    ! --------------------------------------------------------

    wgtsfile = trim(wgtsdir)//'tripole.'//trim(fsrc)//'.Ct.to.rect.'//trim(fdst)//'.bilinear.nc'

    allocate(dstlon(nxr,nyr)); dstlon = 0.0
    allocate(dstlat(nxr,nyr)); dstlat = 0.0
    allocate(out1d(nxr*nyr)); out1d = 0.0

    call nf90_err(nf90_open(trim(wgtsfile), nf90_nowrite, ncid), 'open: '//wgtsfile)
    call nf90_err(nf90_inq_varid(ncid, 'xc_b', varid), 'get variable Id: xc_b')
    call nf90_err(nf90_get_var(ncid,    varid, out1d), 'get variable: xc_b')
    dstlon = reshape(out1d,(/nxr,nyr/))
    call nf90_err(nf90_inq_varid(ncid, 'yc_b', varid), 'get variable Id: yc_b')
    call nf90_err(nf90_get_var(ncid,    varid, out1d), 'get variable: yc_b')
    dstlat = reshape(out1d,(/nxr,nyr/))
    call nf90_err(nf90_close(ncid), 'close: '//wgtsfile)

    ! --------------------------------------------------------
    ! mask is a 2d (ice) or 3d (ocn) array which contains 1's
    ! on land and 0's at valid points.
    ! when remapped, any mask value > 0 identifies land values that
    ! have crept into the field. remapped model fields are then
    ! masked with this interpolation mask
    ! --------------------------------------------------------

    if (do_ocnpost) then
       call makemask3d(vfill)
    else
       call makemask2d(vfill)
    end if

    ! --------------------------------------------------------
    ! remap the source grid 2/3D mask to obtain the interpolation mask.
    ! --------------------------------------------------------

    if (do_ocnpost) then
       call remap(trim(wgtsfile), dim2=nlevs, src_field=mask3d, dst_field=rgmask3d)
       ! set interpolation mask missing on land, 1.0 on ocean on destination grids
       where(rgmask3d > 0.0)rgmask3d = vfill
       where(rgmask3d /= vfill)rgmask3d = 1.0
       ! out1d contains dstlat
       do n = 1,nlevs
          where(out1d(:) <= minlat)rgmask3d(:,n) = vfill
       end do

       if (debug) then
          write(logunit,'(a,2g14.4)')'mask min/max on destination grid ',minval(rgmask3d),maxval(rgmask3d)
          call dumpnc(trim(ftype)//'.'//trim(fdst)//'.rgmask3d.nc', 'rgmask3d', dims=(/nxr,nyr,nlevs/), &
               field=rgmask3d)
       end if
    else
       call remap(trim(wgtsfile), src_field=mask2d, dst_field=rgmask2d)
       ! set interpolation mask missing on land, 1.0 on ocean on destination grids
       where(rgmask2d > 0.0)rgmask2d = vfill
       where(rgmask2d /= vfill)rgmask2d = 1.0
       ! out1d contains dstlat
       where(out1d(:) <= minlat)rgmask2d(:) = vfill

       if (debug) then
          write(logunit,'(a,2g14.4)')'mask min/max on destination grid ',minval(rgmask2d),maxval(rgmask2d)
          call dumpnc(trim(ftype)//'.'//trim(fdst)//'.rgmask2d.nc', 'rgmask2d', dims=(/nxr,nyr/),       &
               field=rgmask2d)
       end if
    end if

  end subroutine remap_masks

  subroutine makemask3d(vfill)

    real, intent(out) :: vfill

    ! local variables
    integer :: rc, ncid, varid
    real, allocatable, dimension(:,:,:) :: tmp3d

    allocate(tmp3d(nxt,nyt,nlevs)); tmp3d = 0.0

    call nf90_err(nf90_open(trim(input_file), nf90_nowrite, ncid), 'open: '//trim(input_file))
    ! Obtain maskvar directly from file to set fill value
    call nf90_err(nf90_inq_varid(ncid, trim(maskvar), varid)    , 'get variable Id: '// trim(maskvar))
    call nf90_err(nf90_get_att(ncid, varid, '_FillValue', vfill), 'get variable attribute: FillValue '// trim(maskvar))
    call nf90_err(nf90_get_var(ncid, varid, tmp3d)              , 'get variable: '//trim(maskvar))
    call nf90_err(nf90_close(ncid), 'close: '//trim(input_file))

    mask3d = reshape(tmp3d, (/nxt*nyt,nlevs/))
    ! set mask3d to 0 on ocean, 1 on land on source grid
    where(mask3d .eq. vfill)mask3d = 1.0
    where(mask3d .ne.   1.0)mask3d = 0.0

    if (debug) then
       write(logunit,'(a,2g14.4)')'mask3d min/max on source grid ',minval(mask3d),maxval(mask3d)
       call dumpnc(trim(ftype)//'.mask3d.nc', 'mask3d', dims=(/nxt,nyt,nlevs/), field=mask3d)
    end if

  end subroutine makemask3d

  subroutine makemask2d(vfill)

    real, intent(out) :: vfill

    ! local variables
    integer :: rc, ncid, varid
    real, allocatable, dimension(:,:) :: tmp2d

    allocate(tmp2d(nxt,nyt)); tmp2d = 0.0

    call nf90_err(nf90_open(trim(input_file), nf90_nowrite, ncid), 'open: '//trim(input_file))
    ! Obtain maskvar directly from file to set fill value
    call nf90_err(nf90_inq_varid(ncid, trim(maskvar), varid), 'get variable Id: '// trim(maskvar))
    call nf90_err(nf90_get_att(ncid, varid, '_FillValue', vfill), 'get variable attribute: FillValue '// trim(maskvar))
    call nf90_err(nf90_get_var(ncid, varid, tmp2d), 'get variable: '//trim(maskvar))
    call nf90_err(nf90_close(ncid), 'close: '//trim(input_file))

    mask2d = reshape(tmp2d, (/nxt*nyt/))
    ! set mask2d to 0 on ocean, 1 on land on source grid
    mask2d = mask2d - 1.0
    where(mask2d .eq. -1.0)mask2d = 1.0

    if (debug) then
       write(logunit,'(a,2g14.4)')'mask2d min/max on source grid ',minval(mask2d),maxval(mask2d)
       call dumpnc(trim(ftype)//'.mask2d.nc', 'mask2d', dims=(/nxt,nyt/), field=mask2d)
    end if

  end subroutine makemask2d

end module masking_mod
