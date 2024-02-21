program ocnicepost

  ! This program will remap MOM6 ocean or CICE6 ice output on the tripole grid to a
  ! rectilinear grid using pre-computed ESMF weights to remap the chosen fields to the
  ! destination grid and write the results to a new netCDF file. The ESMF weights needed
  ! are pre-generated using the UFS-UTILS cpld_gridgen utility described at
  ! https://ufs-community.github.io/UFS_UTILS/cpld_gridgen/index.html. Weights are currently
  ! generated only for mapping a source grid to a similar or lower resolution rectilinear grid.
  !
  ! Two control files determine the code behaviour. A name list file ocnicepost.nml is used
  ! to set the input file type (ice or ocean), the location of the ESMF weights, the dimensions
  ! of the source and destination grids, the source variable to used to create an interpolation
  ! mask, the required rotation variables and a flag to produce debugging output.
  !
  ! The list of variables to be remapped is expected in either ocean.csv or ice.csv. Each
  ! variable is specified by name, dimensionality, and the required mapping method. For vectors,
  ! the paired vector and it's grid is also listed.
  !
  ! Either a 2D (ice) or 3D (ocean) interpolation mask is used to mask remapped fields so that
  ! only valid ocean grid points on the source grid appear in the remapped fields.
  !
  ! Source fields are packed by mapping method and the remapped. Vector fields are first remapped
  ! to the center (Ct) grid points and rotated from the I-J orientation to E-W before remapping.
  ! The interpolation mask is used to masks out land contaminated points prior to writing the
  ! output netCDF file.

  use netcdf
  use init_mod   , only : nxt, nyt, nlevs, nxr, nyr, outvars, readnml, readcsv
  use init_mod   , only : wgtsdir, ftype, fsrc, fdst, input_file, cosvar, sinvar, angvar
  use init_mod   , only : do_ocnpost, debug, logunit
  use arrays_mod , only : b2d, c2d, b3d, rgb2d, rgc2d, rgb3d, dstlon, dstlat, setup_packing
  use arrays_mod , only : nbilin2d, nbilin3d, nconsd2d, bilin2d, bilin3d, consd2d
  use masking_mod, only : mask2d, mask3d, rgmask2d, rgmask3d, remap_masks
  use utils_mod  , only : getfield, packarrays, remap, dumpnc, nf90_err

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

  call nf90_err(nf90_open(trim(input_file), nf90_nowrite, ncid), 'open: '//trim(input_file))
  if (do_ocnpost) then
    call nf90_err(nf90_inq_dimid(ncid, 'z_l', varid), 'get dimension Id: z_l'//trim(input_file))
    call nf90_err(nf90_inquire_dimension(ncid, varid, len=nlevs), 'get dimension Id: z_l'//trim(input_file))
  endif
  do n = 1,nvalid
     if (debug) then
        write(logunit,'(a12,i4,a10,3(a6))')trim(outvars(n)%var_name)//', ',outvars(n)%var_dimen, &
           ', '//trim(outvars(n)%var_remapmethod),', '//trim(outvars(n)%var_grid),             &
           ', '//trim(outvars(n)%var_pair),', '//trim(outvars(n)%var_pair_grid)
     end if
     call nf90_err(nf90_inq_varid(ncid, trim(outvars(n)%var_name), varid), 'get variable Id: '//trim(outvars(n)%var_name))
     call nf90_err(nf90_get_att(ncid, varid,  'long_name', outvars(n)%long_name), 'get variable attribute: long_name '//trim(outvars(n)%var_name))
     call nf90_err(nf90_get_att(ncid, varid,      'units', outvars(n)%units), 'get variable attribute: units '//trim(outvars(n)%var_name)        )
     call nf90_err(nf90_get_att(ncid, varid, '_FillValue', outvars(n)%var_fillvalue), 'get variable attribute: FillValue'//trim(outvars(n)%var_name))
  end do

  ! timestamp
  call nf90_err(nf90_inq_varid(ncid, 'time', varid), 'get variable Id: time '//trim(input_file))
  call nf90_err(nf90_get_var(ncid, varid, timestamp), 'get variable: time '//trim(input_file))
  call nf90_err(nf90_get_att(ncid, varid,    'units', timeunit), 'get variable attribute : units '//trim(input_file))
  call nf90_err(nf90_get_att(ncid, varid, 'calendar', timecal), 'get variable attribute : calendar '//trim(input_file))
  if (do_ocnpost) then
     allocate(z_l(nlevs))  ; z_l = 0.0
     allocate(z_i(0:nlevs)); z_i = 0.0
     allocate(cosrot(nxt*nyt)); cosrot = 0.0
     allocate(sinrot(nxt*nyt)); sinrot = 0.0

     ! cell centers
     call nf90_err(nf90_inq_varid(ncid, 'z_l', varid), 'get variable Id: z_l '//trim(input_file))
     call nf90_err(nf90_get_var(ncid, varid, z_l), 'get variable: z_l '//trim(input_file))
     ! cell edges
     call nf90_err(nf90_inq_varid(ncid, 'z_i', varid), 'get variable Id: z_i '//trim(input_file))
     call nf90_err(nf90_get_var(ncid, varid, z_i), 'get variable: z_i '//trim(input_file))
     call nf90_err(nf90_close(ncid), 'close: '//trim(input_file))
     ! rotation angles
     call getfield(trim(input_file), trim(cosvar), dims=(/nxt,nyt/), field=cosrot)
     call getfield(trim(input_file), trim(sinvar), dims=(/nxt,nyt/), field=sinrot)
  else
     allocate(anglet(nxt*nyt)); anglet = 0.0
     call getfield(trim(input_file), trim(angvar), dims=(/nxt,nyt/), field=anglet)
     cosrot =  cos(anglet)
     sinrot = -sin(anglet)
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
           write(logunit,'(i4,a10,3(a2,a6),4g14.4)')n,trim(b2d(n)%var_name),'  ',                       &
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
           write(logunit,'(i4,a10,3(a2,a6),4g14.4)')n,trim(c2d(n)%var_name),'  ',                       &
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
           write(logunit,'(i4,a10,3(a2,a6),4g14.4)')n,trim(b3d(n)%var_name),'  ',                       &
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

  call nf90_err(nf90_create(trim(fout), nf90_clobber, ncid), 'create: '//trim(fout))
  call nf90_err(nf90_def_dim(ncid, 'longitude', nxr, idimid), 'define dimension: longitude')
  call nf90_err(nf90_def_dim(ncid,  'latitude', nyr, jdimid), 'define dimension: latitude')
  call nf90_err(nf90_def_dim(ncid, 'time', nf90_unlimited, timid), 'define dimension: time')

  ! define the time variable
  call nf90_err(nf90_def_var(ncid, 'time', nf90_double, (/timid/), varid), 'define variable: time')
  call nf90_err(nf90_put_att(ncid, varid,    'units', trim(timeunit)), 'put variable attribute: units')
  call nf90_err(nf90_put_att(ncid,  varid, 'calendar', trim(timecal)), 'put variable attribute: calendar')
  ! spatial grid
  call nf90_err(nf90_def_var(ncid, 'longitude', nf90_float,  (/idimid/), varid), 'define variable: longitude')
  call nf90_err(nf90_put_att(ncid, varid, 'units', 'degrees_east'), 'put variable attribute: units')
  call nf90_err(nf90_def_var(ncid, 'latitude', nf90_float,  (/jdimid/), varid), 'define variable: latitude' )
  call nf90_err(nf90_put_att(ncid, varid, 'units', 'degrees_north'), 'put variable attribute: units')
  ! vertical grid
  if (do_ocnpost) then
     call nf90_err(nf90_def_dim(ncid,  'z_l',  nlevs  , kdimid), 'define dimension: z_l')
     call nf90_err(nf90_def_dim(ncid,  'z_i',  nlevs+1, edimid), 'define dimension: z_i')
     call nf90_err(nf90_def_var(ncid, 'z_l', nf90_float,  (/kdimid/), varid), 'define variable: z_l')
     call nf90_err(nf90_put_att(ncid, varid,    'units', 'm'), 'put variable attribute: units')
     call nf90_err(nf90_put_att(ncid, varid, 'positive', 'down'), 'put variable attribute: positive')
     call nf90_err(nf90_def_var(ncid, 'z_i', nf90_float,  (/edimid/), varid), 'define variable: z_i')
     call nf90_err(nf90_put_att(ncid, varid,    'units', 'm'), 'put variable attribute: units')
     call nf90_err(nf90_put_att(ncid, varid, 'positive', 'down'), 'put variable attribute: positive')
  end if

  if (allocated(b2d)) then
     do n = 1,nbilin2d
        vname = trim(b2d(n)%var_name)
        vunit = trim(b2d(n)%units)
        vlong = trim(b2d(n)%long_name)
        vfill = b2d(n)%var_fillvalue
        call nf90_err(nf90_def_var(ncid, vname, nf90_float, (/idimid,jdimid,timid/), varid), 'define variable: '// vname)
        call nf90_err(nf90_put_att(ncid, varid,      'units', vunit), 'put variable attribute: units')
        call nf90_err(nf90_put_att(ncid, varid,  'long_name', vlong), 'put variable attribute: long_name')
        call nf90_err(nf90_put_att(ncid, varid, '_FillValue', vfill), 'put variable attribute: FillValue')
     enddo
  end if
  if (allocated(c2d)) then
     do n = 1,nconsd2d
        vname = trim(c2d(n)%var_name)
        vunit = trim(c2d(n)%units)
        vlong = trim(c2d(n)%long_name)
        vfill = c2d(n)%var_fillvalue
        call nf90_err(nf90_def_var(ncid, vname, nf90_float, (/idimid,jdimid,timid/), varid), 'define variable: '// vname)
        call nf90_err(nf90_put_att(ncid, varid,      'units', vunit), 'put variable attribute: units' )
        call nf90_err(nf90_put_att(ncid, varid,  'long_name', vlong), 'put variable attribute: long_name' )
        call nf90_err(nf90_put_att(ncid, varid, '_FillValue', vfill), 'put variable attribute: FillValue' )
     enddo
  end if
  if (allocated(b3d)) then
     do n = 1,nbilin3d
        vname = trim(b3d(n)%var_name)
        vunit = trim(b3d(n)%units)
        vlong = trim(b3d(n)%long_name)
        vfill = b3d(n)%var_fillvalue
        call nf90_err(nf90_def_var(ncid, vname, nf90_float, (/idimid,jdimid,kdimid,timid/), varid), 'define variable: '// vname)
        call nf90_err(nf90_put_att(ncid, varid,      'units', vunit), 'put variable attribute: units' )
        call nf90_err(nf90_put_att(ncid, varid,  'long_name', vlong), 'put variable attribute: long_name' )
        call nf90_err(nf90_put_att(ncid, varid, '_FillValue', vfill), 'put variable attribute: FillValue' )
     enddo
  end if
  call nf90_err(nf90_enddef(ncid), 'enddef: '// trim(fout))

  ! dimensions
  call nf90_err(nf90_inq_varid(ncid, 'longitude', varid), 'get variable Id: longitude')
  call nf90_err(nf90_put_var(ncid,   varid, dstlon(:,1)), 'put variable: longitude')
  call nf90_err(nf90_inq_varid(ncid,  'latitude', varid), 'get variable Id: latitude')
  call nf90_err(nf90_put_var(ncid,   varid, dstlat(1,:)), 'put variable: latitude')
  ! time
  call nf90_err(nf90_inq_varid(ncid, 'time', varid), 'get variable Id: time')
  call nf90_err(nf90_put_var(ncid, varid, timestamp), 'put variable: time')
  ! vertical
  if (do_ocnpost) then
     call nf90_err(nf90_inq_varid(ncid, 'z_l', varid), 'get variable Id: z_l')
     call nf90_err(nf90_put_var(ncid, varid, z_l)    , 'put variable: z_l')
     call nf90_err(nf90_inq_varid(ncid, 'z_i', varid), 'get variable Id: z_i')
     call nf90_err(nf90_put_var(ncid, varid, z_i)    , 'put variable: z_i')
  end if
  if (allocated(rgb2d)) then
     do n = 1,nbilin2d
        out2d(:,:) = reshape(rgb2d(:,n), (/nxr,nyr/))
        out2d(:,nyr) = vfill
        vname = trim(b2d(n)%var_name)
        call nf90_err(nf90_inq_varid(ncid, vname, varid), 'get variable Id: '//vname)
        call nf90_err(nf90_put_var(ncid,   varid, out2d), 'put variable: '//vname)
     end do
  end if
  if (allocated(rgc2d)) then
     do n = 1,nconsd2d
        out2d(:,:) = reshape(rgc2d(:,n), (/nxr,nyr/))
        out2d(:,nyr) = vfill
        vname = trim(c2d(n)%var_name)
        call nf90_err(nf90_inq_varid(ncid, vname, varid), 'get variable Id: '//vname)
        call nf90_err(nf90_put_var(ncid,   varid, out2d), 'put variable: '//vname)
     end do
  end if
  if (allocated(rgb3d)) then
     do n = 1,nbilin3d
        out3d(:,:,:) = reshape(rgb3d(:,:,n), (/nxr,nyr,nlevs/))
        out3d(:,nyr,:) = vfill
        vname = trim(b3d(n)%var_name)
        call nf90_err(nf90_inq_varid(ncid, vname, varid), 'get variable Id: '//vname)
        call nf90_err(nf90_put_var(ncid,   varid, out3d), 'put variable: '//vname)
     end do
  end if
  call nf90_err(nf90_close(ncid), 'close: '// trim(fout))
  write(logunit,'(a)')trim(fout)//' done'

  stop

end program ocnicepost
