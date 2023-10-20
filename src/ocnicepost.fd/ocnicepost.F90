program ocnicepost

  use netcdf
  use init_mod   , only : nxt, nyt, nlevs, nxr, nyr, outvars, readnml, readcsv
  use init_mod   , only : wgtsdir, ftype, fsrc, fdst, input_file, cosvar, sinvar, angvar
  use init_mod   , only : do_ocnpost, debug, logunit
  use arrays_mod , only : b2d, c2d, b3d, rgb2d, rgc2d, rgb3d, dstlon, dstlat, setup_packing
  use arrays_mod , only : nbilin2d, nbilin3d, nconsd2d, bilin2d, bilin3d, consd2d
  use masking_mod, only : mask2d, mask3d, rgmask2d, rgmask3d, remap_masks
  use utils_mod  , only : getfield, packarrays, remap, dumpnc

  implicit none

  character(len=120) :: wgtsfile
  character(len=120) :: fout

  ! dimensions, units and variables from source file used in creation of
  ! output netcdf
  real, allocatable, dimension(:) :: z_l            !< the vertical grid center
  real, allocatable, dimension(:) :: z_i            !< the vertical grid interfaces
  real, allocatable, dimension(:) :: cosrot, sinrot !< the cos and sin of the rotation angle at Ct points (ocean)
  real, allocatable, dimension(:) :: anglet         !< the rotation angle at the Ct points (ice)

  ! work arrays for output netcdf
  real, allocatable, dimension(:,:)   :: out2d !< 2D destination grid output array
  real, allocatable, dimension(:,:,:) :: out3d !< 3D destination grid output array

  real(kind=8)       :: timestamp
  character(len= 40) :: timeunit, timecal
  character(len= 20) :: vname, vunit
  character(len=120) :: vlong

    real    :: vfill
  integer :: nvalid
  integer :: n,rc,ncid,varid
  integer :: idimid,jdimid,kdimid,edimid,timid
  integer :: idx1,idx2,idx3

  ! --------------------------------------------------------
  ! read the nml file and a file containing the list of
  ! variables to be remapped
  ! --------------------------------------------------------

  call readnml
  call readcsv(nvalid)

  ! --------------------------------------------------------
  ! read the source file and obtain the units and long name,
  ! rotation angles, vertical grid and time axis
  ! --------------------------------------------------------

  rc = nf90_open(trim(input_file), nf90_nowrite, ncid)
  do n = 1,nvalid
     rc = nf90_inq_varid(ncid, trim(outvars(n)%var_name), varid)
     rc = nf90_get_att(ncid, varid,  'long_name', outvars(n)%long_name)
     rc = nf90_get_att(ncid, varid,      'units', outvars(n)%units)
     rc = nf90_get_att(ncid, varid, '_FillValue', outvars(n)%var_fillvalue)
  end do

  ! timestamp
  rc = nf90_inq_varid(ncid, 'time', varid)
  rc = nf90_get_var(ncid, varid, timestamp)
  rc = nf90_get_att(ncid, varid,    'units', timeunit)
  rc = nf90_get_att(ncid, varid, 'calendar', timecal)
  if (do_ocnpost) then
     allocate(z_l(nlevs))  ; z_l = 0.0
     allocate(z_i(0:nlevs)); z_i = 0.0
     allocate(cosrot(nxt*nyt)); cosrot = 0.0
     allocate(sinrot(nxt*nyt)); sinrot = 0.0

     ! cell centers
     rc = nf90_inq_varid(ncid, 'z_l', varid)
     rc = nf90_get_var(ncid, varid, z_l)
     ! cell edges
     rc = nf90_inq_varid(ncid, 'z_i', varid)
     rc = nf90_get_var(ncid, varid, z_i)
     rc = nf90_close(ncid)
     ! rotation angles
     call getfield(trim(input_file), trim(cosvar), dims=(/nxt,nyt/), field=cosrot)
     call getfield(trim(input_file), trim(sinvar), dims=(/nxt,nyt/), field=sinrot)
  else
     allocate(anglet(nxt*nyt)); anglet = 0.0
     call getfield(trim(input_file), trim(angvar), dims=(/nxt,nyt/), field=anglet)
     cosrot =  cos(anglet)
     sinrot = -sin(anglet)
  end if

  if (debug) then
     do n = 1,nvalid
        write(logunit,'(a12,i4,a10,3(a6))')trim(outvars(n)%var_name)//', ',outvars(n)%var_dimen, &
             ', '//trim(outvars(n)%var_remapmethod),', '//trim(outvars(n)%var_grid),             &
             ', '//trim(outvars(n)%var_pair),', '//trim(outvars(n)%var_pair_grid)
     end do
  end if

  ! --------------------------------------------------------
  ! create interpolation masks
  ! --------------------------------------------------------

  if (do_ocnpost) then
     allocate(mask3d(nxt*nyt,nlevs)); mask3d = 0.0
     allocate(rgmask3d(nxr*nyr,nlevs)); rgmask3d = 0.0
  else
     allocate(mask2d(nxt*nyt)); mask2d = 0.0
     allocate(rgmask2d(nxr*nyr)); rgmask2d = 0.0
  end if

  call remap_masks(vfill)

  ! --------------------------------------------------------
  ! create packed arrays for mapping and remap packed arrays
  ! to the destination grid
  ! --------------------------------------------------------

  call setup_packing(nvalid,outvars)

  ! 2D bilin
  if (allocated(bilin2d)) then

     wgtsfile = trim(wgtsdir)//'tripole.'//trim(fsrc)//'.Ct.to.rect.'//trim(fdst)//'.bilinear.nc'
     call packarrays(trim(input_file), trim(wgtsdir), cosrot, sinrot, b2d, dims=(/nxt,nyt/),            &
          nflds=nbilin2d, fields=bilin2d)
     call remap(trim(wgtsfile), dim2=nbilin2d, src_field=bilin2d, dst_field=rgb2d)

     if (debug) then
        write(logunit,'(a)')'remap 2D fields bilinear with '//trim(wgtsfile)
        write(logunit,'(a)')'packed min/max values, mapped min/max values'
        do n = 1,nbilin2d
           write(logunit,'(i4,a10,3(a2,a6),4g14.4)')n,trim(b2d(n)%var_name),'  ',                  &
                trim(b2d(n)%var_grid),'  ',trim(b2d(n)%var_pair),'  ', trim(b2d(n)%var_pair_grid),      &
                minval(bilin2d(:,n)), maxval(bilin2d(:,n)),minval(rgb2d(:,n)), maxval(rgb2d(:,n))
        end do
        call dumpnc(trim(ftype)//'.'//trim(fsrc)//'.bilin2d.nc', 'bilin2d', dims=(/nxt,nyt/),           &
             nflds=nbilin2d, field=bilin2d)
        call dumpnc(trim(ftype)//'.'//trim(fdst)//'.rgbilin2d.nc', 'rgbilin2d', dims=(/nxr,nyr/),       &
             nflds=nbilin2d, field=rgb2d)

     end if
  end if

  ! 2D conserv
  if (allocated(consd2d)) then

     wgtsfile = trim(wgtsdir)//'tripole.'//trim(fsrc)//'.Ct.to.rect.'//trim(fdst)//'.conserve.nc'
     call packarrays(trim(input_file), trim(wgtsdir), cosrot, sinrot, c2d, dims=(/nxt,nyt/),            &
          nflds=nconsd2d, fields=consd2d)
     call remap(trim(wgtsfile), dim2=nconsd2d, src_field=consd2d, dst_field=rgc2d)

     if (debug) then
        write(logunit,'(a)')'remap 2D fields conserv with '//trim(wgtsfile)
        write(logunit,'(a)')'packed min/max values, mapped min/max values'
        do n = 1,nconsd2d
           write(logunit,'(i4,a10,3(a2,a6),4g14.4)')n,trim(c2d(n)%var_name),'  ',                  &
                trim(c2d(n)%var_grid),'  ',trim(c2d(n)%var_pair),'  ', trim(c2d(n)%var_pair_grid),      &
                minval(consd2d(:,n)), maxval(consd2d(:,n)), minval(rgc2d(:,n)), maxval(rgc2d(:,n))
        end do
        call dumpnc(trim(ftype)//'.'//trim(fsrc)//'.consd2d.nc', 'consd2d', dims=(/nxt,nyt/),           &
             nflds=nconsd2d, field=consd2d)
        call dumpnc(trim(ftype)//'.'//trim(fdst)//'.rgconsd2d.nc', 'rgconsd2d', dims=(/nxr,nyr/),       &
             nflds=nconsd2d, field=rgc2d)
     end if
  end if

  ! 3D bilin
  if (allocated(bilin3d))then

     wgtsfile = trim(wgtsdir)//'tripole.'//trim(fsrc)//'.Ct.to.rect.'//trim(fdst)//'.bilinear.nc'
     call packarrays(trim(input_file), trim(wgtsdir), cosrot, sinrot, b3d, dims=(/nxt,nyt,nlevs/),      &
          nflds=nbilin3d, fields=bilin3d)
     call remap(trim(wgtsfile), nk=nlevs, nflds=nbilin3d, src_field=bilin3d, dst_field=rgb3d)

     if (debug) then
        write(logunit,'(a)')'remap 3D fields bilinear with '//trim(wgtsfile)
        write(logunit,'(a)')'packed min/max values,mapped min/max values'
        do n = 1,nbilin3d
           write(logunit,'(i4,a10,3(a2,a6),4g14.4)')n,trim(b3d(n)%var_name),'  ',                  &
                trim(b3d(n)%var_grid),'  ',trim(b3d(n)%var_pair),'  ', trim(b3d(n)%var_pair_grid),      &
                minval(bilin3d(:,:,n)), maxval(bilin3d(:,:,n)),minval(rgb3d(:,:,n)), maxval(rgb3d(:,:,n))
        end do
        call dumpnc(trim(ftype)//'.'//trim(fsrc)//'.bilin3d.nc', 'bilin3d', dims=(/nxt,nyt,nlevs/),     &
             nk=nlevs, nflds=nbilin3d, field=bilin3d)
        call dumpnc(trim(ftype)//'.'//trim(fdst)//'.rgbilin3d.nc', 'rgbilin3d', dims=(/nxr,nyr,nlevs/), &
             nk=nlevs, nflds=nbilin3d, field=rgb3d)
     end if
  end if

     ! --------------------------------------------------------
     ! mask the mapped fields
     ! --------------------------------------------------------

     do n = 1,nbilin2d
        if (allocated(rgmask3d)) then
           where(rgmask3d(:,1) .eq. vfill)rgb2d(:,n) = vfill
        end if
        if (allocated(rgmask2d))then
           where(rgmask2d(:) .eq. vfill)rgb2d(:,n) = vfill
        end if
     end do
     do n = 1,nconsd2d
        if (allocated(rgmask3d)) then
           where(rgmask3d(:,1) .eq. vfill)rgc2d(:,n) = vfill
        end if
        if (allocated(rgmask2d))then
           where(rgmask2d(:) .eq. vfill)rgc2d(:,n) = vfill
        end if
     end do
     do n = 1,nbilin3d
        if (allocated(rgmask3d)) then
           where(rgmask3d(:,:) .eq. vfill)rgb3d(:,:,n) = vfill
        end if
     end do

     ! --------------------------------------------------------
     ! replace model native speed field with a value calculated
     ! from remapped ssu,ssv
     ! --------------------------------------------------------

     if (do_ocnpost) then
        do n = 1,nbilin2d
           if (trim(b2d(n)%var_name) == 'speed')idx1 = n
           if (trim(b2d(n)%var_name) ==   'SSU')idx2 = n
           if (trim(b2d(n)%var_name) ==   'SSV')idx3 = n
        enddo
        where(rgb2d(:,idx1) .ne. vfill)rgb2d(:,idx1) = &
             sqrt(rgb2d(:,idx2)**2 + rgb2d(:,idx3)**2)
     end if

     ! --------------------------------------------------------
     ! write the mapped fields
     ! --------------------------------------------------------

     allocate(out2d(nxr,nyr)); out2d = 0.0
     allocate(out3d(nxr,nyr,nlevs)); out3d = 0.0

     fout = trim(ftype)//'.'//trim(fdst)//'.nc'
     if (debug) write(logunit, '(a)')'output file: '//trim(fout)

     rc = nf90_create(trim(fout), nf90_clobber, ncid)
     rc = nf90_def_dim(ncid, 'longitude', nxr, idimid)
     rc = nf90_def_dim(ncid,  'latitude', nyr, jdimid)
     rc = nf90_def_dim(ncid, 'time', nf90_unlimited, timid)

     ! define the time variable
     rc = nf90_def_var(ncid, 'time', nf90_double, (/timid/), varid)
     rc = nf90_put_att(ncid, varid,    'units', trim(timeunit))
     rc= nf90_put_att(ncid,  varid, 'calendar', trim(timecal))
     ! spatial grid
     rc = nf90_def_var(ncid, 'longitude', nf90_float,  (/idimid/), varid)
     rc = nf90_put_att(ncid, varid, 'units', 'degrees_east')
     rc = nf90_def_var(ncid, 'latitude', nf90_float,  (/jdimid/), varid)
     rc = nf90_put_att(ncid, varid, 'units', 'degrees_north')
     ! vertical grid
     if (do_ocnpost) then
        rc = nf90_def_dim(ncid,  'z_l',  nlevs  , kdimid)
        rc = nf90_def_dim(ncid,  'z_i',  nlevs+1, edimid)
        rc = nf90_def_var(ncid, 'z_l', nf90_float,  (/kdimid/), varid)
        rc = nf90_put_att(ncid, varid,    'units', 'm')
        rc = nf90_put_att(ncid, varid, 'positive', 'down')
        rc = nf90_def_var(ncid, 'z_i', nf90_float,  (/edimid/), varid)
        rc = nf90_put_att(ncid, varid,    'units', 'm')
        rc = nf90_put_att(ncid, varid, 'positive', 'down')
     end if

     if (allocated(b2d)) then
        do n = 1,nbilin2d
           vname = trim(b2d(n)%var_name)
           vunit = trim(b2d(n)%units)
           vlong = trim(b2d(n)%long_name)
           vfill = b2d(n)%var_fillvalue
           rc = nf90_def_var(ncid, vname, nf90_float, (/idimid,jdimid,timid/), varid)
           rc = nf90_put_att(ncid, varid,      'units', vunit)
           rc = nf90_put_att(ncid, varid,  'long_name', vlong)
           rc = nf90_put_att(ncid, varid, '_FillValue', vfill)
        enddo
     end if
     if (allocated(c2d)) then
        do n = 1,nconsd2d
           vname = trim(c2d(n)%var_name)
           vunit = trim(c2d(n)%units)
           vlong = trim(c2d(n)%long_name)
           vfill = c2d(n)%var_fillvalue
           rc = nf90_def_var(ncid, vname, nf90_float, (/idimid,jdimid,timid/), varid)
           rc = nf90_put_att(ncid, varid,      'units', vunit)
           rc = nf90_put_att(ncid, varid,  'long_name', vlong)
           rc = nf90_put_att(ncid, varid, '_FillValue', vfill)
        enddo
     end if
     if (allocated(b3d)) then
        do n = 1,nbilin3d
           vname = trim(b3d(n)%var_name)
           vunit = trim(b3d(n)%units)
           vlong = trim(b3d(n)%long_name)
           vfill = b3d(n)%var_fillvalue
           rc = nf90_def_var(ncid, vname, nf90_float, (/idimid,jdimid,kdimid,timid/), varid)
           rc = nf90_put_att(ncid, varid,      'units', vunit)
           rc = nf90_put_att(ncid, varid,  'long_name', vlong)
           rc = nf90_put_att(ncid, varid, '_FillValue', vfill)
        enddo
     end if
     rc = nf90_enddef(ncid)

     ! dimensions
     rc = nf90_inq_varid(ncid, 'longitude', varid)
     rc = nf90_put_var(ncid,   varid, dstlon(:,1))
     rc = nf90_inq_varid(ncid,  'latitude', varid)
     rc = nf90_put_var(ncid,   varid, dstlat(1,:))
     ! time
     rc = nf90_inq_varid(ncid, 'time', varid)
     rc = nf90_put_var(ncid, varid, timestamp)
     ! vertical
     if (do_ocnpost) then
        rc = nf90_inq_varid(ncid, 'z_l', varid)
        rc = nf90_put_var(ncid, varid, z_l)
        rc = nf90_inq_varid(ncid, 'z_i', varid)
        rc = nf90_put_var(ncid, varid, z_i)
     end if
     if (allocated(rgb2d)) then
        do n = 1,nbilin2d
           out2d(:,:) = reshape(rgb2d(:,n), (/nxr,nyr/))
           out2d(:,nyr) = vfill
           vname = trim(b2d(n)%var_name)
           rc = nf90_inq_varid(ncid, vname, varid)
           rc = nf90_put_var(ncid,   varid, out2d)
        end do
     end if
     if (allocated(rgc2d)) then
        do n = 1,nconsd2d
           out2d(:,:) = reshape(rgc2d(:,n), (/nxr,nyr/))
           out2d(:,nyr) = vfill
           vname = trim(c2d(n)%var_name)
           rc = nf90_inq_varid(ncid, vname, varid)
           rc = nf90_put_var(ncid,   varid, out2d)
        end do
     end if
     if (allocated(rgb3d)) then
        do n = 1,nbilin3d
           out3d(:,:,:) = reshape(rgb3d(:,:,n), (/nxr,nyr,nlevs/))
           out3d(:,nyr,:) = vfill
           vname = trim(b3d(n)%var_name)
           rc = nf90_inq_varid(ncid, vname, varid)
           rc = nf90_put_var(ncid,   varid, out3d)
        end do
     end if
     rc = nf90_close(ncid)
     write(logunit,'(a)')trim(fout)//' done'

end program ocnicepost
