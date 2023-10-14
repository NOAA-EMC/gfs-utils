program ocnicepost

  use utils_mod, only : debug, logunit, getfield, packarrays, remap, dumpnc
  use outputvars
  use netcdf

  implicit none

  character(len= 20) :: fnml, ftype
  character(len=120) :: wgtsdir
  character(len=120) :: input_file, wgtsfile, output_file

  ! source grid, tripole 1/4 deg, 40 vertical levels
  integer, parameter :: nxt = 1440, nyt = 1080, nlevs = 40

  ! destination grids
  integer, parameter :: ndest = 3
  integer, parameter, dimension(ndest) :: nxrs = (/1440, 720, 360/)
  integer, parameter, dimension(ndest) :: nyrs = (/ 721, 361, 181/)
  character(len=4), dimension(ndest)   :: dstgrds = (/'0p25', '0p5 ', '1p0 '/)

  ! packed arrays on source grid
  real, allocatable, dimension(:,:)   :: bilin2d, consd2d
  real, allocatable, dimension(:,:,:) :: bilin3d
  ! variable types
  type(vardefs), allocatable :: b2d(:)
  type(vardefs), allocatable :: c2d(:)
  type(vardefs), allocatable :: b3d(:)

  ! source grid fields
  real, dimension(nxt,nyt,nlevs) :: tmp3d
  real, dimension(nxt*nyt,nlevs) :: mask3d
  real, dimension(nxt*nyt)       :: mask2d
  real, dimension(nxt*nyt)       :: cosrot, sinrot, anglet
  real, dimension(nlevs)         :: z_l
  real, dimension(0:nlevs)       :: z_i

  ! destination grid fields
  real, allocatable, dimension(:,:)  :: dstlon, dstlat
  real, allocatable, dimension(:,:)  :: rgmask3d
  real, allocatable, dimension(:)    :: rgmask2d
  ! output fields (x,y,z)
  real, allocatable, dimension(:)     :: out1d
  real, allocatable, dimension(:,:)   :: out2d
  real, allocatable, dimension(:,:,:) :: out3d
  ! packed remapped fields
  real, allocatable, dimension(:,:)   :: rgb2d, rgc2d
  real, allocatable, dimension(:,:,:) :: rgb3d

  real(kind=8)       :: timestamp
  character(len= 40) :: timeunit, timecal
  character(len= 20) :: vname, vunit
  character(len=120) :: vlong
  character(len=4)   :: dstgrid

  real    :: vfill
  integer :: nd, nxr, nyr
  integer :: i,j,k,n,nn,nvalid,iounit
  integer :: rc,ncid,varid,dimid
  integer :: nbilin2d,nbilin3d,nconsd2d
  integer :: idimid,jdimid,kdimid,edimid,timid
  integer :: idx1,idx2,idx3
  logical :: do_ocnpost

  namelist /ocnicepost_nml/ ftype, wgtsdir, debug

  ! --------------------------------------------------------
  ! read the name list
  ! --------------------------------------------------------

  fnml = 'ocnicepost.nml'
  inquire (file=trim(fnml), iostat=rc)
  if (rc /= 0) then
     write (6, '(3a)') 'Error: input file "', trim(fnml), '" does not exist.'
     stop
  end if

  ! Open and read Namelist file.
  open (action='read', file=trim(fnml), iostat=rc, newunit=iounit)
  read (nml=ocnicepost_nml, iostat=rc, unit=iounit)
  if (rc /= 0) then
     write (6, '(a)') 'Error: invalid Namelist format.'
  end if
  close (iounit)

  ! initialize the source file type and variables
  if (trim(ftype) == 'ocean') then
     do_ocnpost = .true.
     call ocnvars_typedefine
  else
     do_ocnpost = .false.
     call icevars_typedefine
  end if
  input_file = trim(ftype)//'.nc'

  open(newunit=logunit, file=trim(ftype)//'.post.log',form='formatted')
  if (debug) write(logunit, '(a)')'input file: '//trim(input_file)

  ! --------------------------------------------------------
  ! read the source file and obtain the units and long name,
  ! rotation angles, vertical grid and time axis
  ! --------------------------------------------------------

  nvalid = 0
  rc = nf90_open(trim(input_file), nf90_nowrite, ncid)
  do i = 1,maxvars
     if (len_trim(outvars(i)%input_var_name) > 0 ) then
        rc = nf90_inq_varid(ncid, trim(outvars(i)%input_var_name), varid)
        rc = nf90_get_att(ncid, varid,  'long_name', outvars(i)%long_name)
        rc = nf90_get_att(ncid, varid,      'units', outvars(i)%units)
        rc = nf90_get_att(ncid, varid, '_FillValue', outvars(i)%var_fillvalue)
        nvalid = nvalid+1
        if (trim(outvars(i)%input_var_name) ==   'temp')vfill = outvars(i)%var_fillvalue
        if (trim(outvars(i)%input_var_name) == 'aice_h')vfill = outvars(i)%var_fillvalue
     end if
  end do

  ! timestamp
  rc = nf90_inq_varid(ncid, 'time', varid)
  rc = nf90_get_var(ncid, varid, timestamp)
  rc = nf90_get_att(ncid, varid,    'units', timeunit)
  rc = nf90_get_att(ncid, varid, 'calendar', timecal)
  if (do_ocnpost) then
     ! cell centers
     rc = nf90_inq_varid(ncid, 'z_l', varid)
     rc = nf90_get_var(ncid, varid, z_l)
     ! cell edges
     rc = nf90_inq_varid(ncid, 'z_i', varid)
     rc = nf90_get_var(ncid, varid, z_i)
     rc = nf90_close(ncid)
     ! rotation angles
     call getfield(trim(input_file), 'cos_rot', dims=(/nxt,nyt/), field=cosrot)
     call getfield(trim(input_file), 'sin_rot', dims=(/nxt,nyt/), field=sinrot)
  else
     call getfield(trim(input_file),  'ANGLET', dims=(/nxt,nyt/), field=anglet)
     cosrot =  cos(anglet)
     sinrot = -sin(anglet)
  end if

  ! --------------------------------------------------------
  ! mask is a 2d (ice) or 3d (ocn) array which contains 1's
  ! on land and 0's at valid points.
  ! when remapped, any mask value > 0 identifies land values that
  ! have crept into the field. remapped model fields are then
  ! masked with this interpolation mask
  ! --------------------------------------------------------

  if (do_ocnpost) then
     rc = nf90_open(trim(input_file), nf90_nowrite, ncid)
     ! 3D temp to use as mask, obtain directly from file to preserve vfill
     rc = nf90_inq_varid(ncid, 'temp', varid)
     rc = nf90_get_var(ncid, varid, tmp3d)
     rc = nf90_close(ncid)

     mask3d = reshape(tmp3d, (/nxt*nyt,nlevs/))
     ! set mask3d to 0 on ocean, 1 on land on source grid
     where(mask3d .eq. vfill)mask3d = 1.0
     where(mask3d .ne.   1.0)mask3d = 0.0

     if (debug) then
        write(logunit,'(a,2g14.4)')'mask3d min/max on source grid ',minval(mask3d),maxval(mask3d)
        call dumpnc(trim(ftype)//'.mask3d.nc', 'mask3d', dims=(/nxt,nyt,nlevs/), field=mask3d)
     end if
  else
     call getfield(trim(input_file),  'tmask', dims=(/nxt,nyt/), field=mask2d)
     ! set mask2d to 0 on ocean, 1 on land on source grid
     mask2d = mask2d - 1.0
     where(mask2d .eq. -1.0)mask2d = 1.0

     if (debug) then
        write(logunit,'(a,2g14.4)')'mask2d min/max on source grid ',minval(mask2d),maxval(mask2d)
        call dumpnc(trim(ftype)//'.mask2d.nc', 'mask2d', dims=(/nxt,nyt/), field=mask2d)
     end if
  end if

  ! --------------------------------------------------------
  ! count numbers of fields to remapped for each
  ! mapping type; these can be remapped as packed arrays
  ! --------------------------------------------------------

  nbilin2d = 0; nbilin3d = 0; nconsd2d = 0
  do n = 1,nvalid
     if (trim(outvars(n)%var_remapmethod)  == 'bilinear') then
        if (outvars(n)%var_dimen == 2) nbilin2d = nbilin2d + 1
        if (outvars(n)%var_dimen == 3) nbilin3d = nbilin3d + 1
     end if
     if (trim(outvars(n)%var_remapmethod)  == 'conserve')nconsd2d = nconsd2d + 1  !no 3d variables w/ conservative mapping
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
  ! create types for each packed array
  ! --------------------------------------------------------

  i = 0; j = 0; k = 0
  do n = 1,nvalid
     if (trim(outvars(n)%var_remapmethod) == 'bilinear') then
        if (outvars(n)%var_dimen == 2 .and. allocated(b2d)) then
           i = i+1; b2d(i) = outvars(n)
        end if
        if (outvars(n)%var_dimen == 3 .and. allocated(b3d)) then
           j = j+1; b3d(j) = outvars(n)
        end if
     end if
     if (trim(outvars(n)%var_remapmethod) == 'conserve' .and. allocated(c2d)) then
        k = k+1; c2d(k) = outvars(n)
     end if
  end do

  ! --------------------------------------------------------
  ! create packed arrays for mapping
  ! --------------------------------------------------------

  ! 2D bilin
  if (allocated(bilin2d)) then
     call packarrays(trim(input_file), trim(wgtsdir), cosrot, sinrot, b2d, dims=(/nxt,nyt/),       &
          nflds=nbilin2d, fields=bilin2d)

     if (debug) then
        write(logunit,'(/,a)')'2D fields mapped bilin, packed field min/max values'
        do n = 1,nbilin2d
           write(logunit,'(i6,4(a,a),2g14.4)')n,'  ',trim(b2d(n)%input_var_name),'  ',             &
                trim(b2d(n)%var_grid),'  ',trim(b2d(n)%var_pair),'  ', trim(b2d(n)%var_pair_grid), &
                minval(bilin2d(:,n)), maxval(bilin2d(:,n))
        end do
        call dumpnc(trim(ftype)//'.bilin2d.nc', 'bilin2d', dims=(/nxt,nyt/), nflds=nbilin2d, field=bilin2d)
     end if
  end if

  ! 2D conserv
  if (allocated(consd2d)) then
     call packarrays(trim(input_file), trim(wgtsdir), cosrot, sinrot, c2d, dims=(/nxt,nyt/),       &
          nflds=nconsd2d, fields=consd2d)

     if (debug) then
        write(logunit,'(a)')'2D fields mapped conserv, packed field min/max values'
        do n = 1,nconsd2d
           write(logunit,'(i6,4(a,a),2g14.4)')n,'  ',trim(c2d(n)%input_var_name),'  ',             &
                trim(c2d(n)%var_grid),'  ',trim(c2d(n)%var_pair),'  ', trim(c2d(n)%var_pair_grid), &
                minval(consd2d(:,n)), maxval(consd2d(:,n))
        end do
        call dumpnc(trim(ftype)//'.consd2d.nc', 'consd2d', dims=(/nxt,nyt/), nflds=nconsd2d, field=consd2d)
     end if
  end if

  ! 3D bilin
  if (allocated(bilin3d))then
     call packarrays(trim(input_file), trim(wgtsdir), cosrot, sinrot, b3d, dims=(/nxt,nyt,nlevs/), &
          nflds=nbilin3d, fields=bilin3d)

     if (debug) then
        write(logunit,'(a)')'3D fields mapped bilin, packed field min/max values'
        do n = 1,nbilin3d
           write(logunit,'(i6,4(a,a),2g14.4)')n,'  ',trim(b3d(n)%input_var_name),'  ',             &
                trim(b3d(n)%var_grid),'  ',trim(b3d(n)%var_pair),'  ', trim(b3d(n)%var_pair_grid), &
                minval(bilin3d(:,:,n)), maxval(bilin3d(:,:,n))
        end do
        call dumpnc(trim(ftype)//'.bilin3d.nc', 'bilin3d', dims=(/nxt,nyt,nlevs/), nk=nlevs,       &
             nflds=nbilin3d, field=bilin3d)
     end if
  end if

  ! --------------------------------------------------------
  ! remap packed arrays to each destination grid
  ! --------------------------------------------------------

  do nd = 1,ndest
     dstgrid =  trim(dstgrds(nd))
     nxr = nxrs(nd); nyr = nyrs(nd)

     if (nbilin2d > 0) then
        allocate(rgb2d(nxr*nyr,nbilin2d)); rgb2d = 0.0
     end if
     if (nconsd2d > 0) then
        allocate(rgc2d(nxr*nyr,nconsd2d)); rgc2d = 0.0
     end if
     if (nbilin3d > 0) then
        allocate(rgb3d(nxr*nyr,nlevs,nbilin3d)); rgb3d = 0.0
        allocate(out3d(nxr,nyr,nlevs)); out3d = 0.0
     end if
     allocate(dstlon(nxr,nyr)); dstlon = 0.0
     allocate(dstlat(nxr,nyr)); dstlat = 0.0
     allocate(out1d(nxr*nyr)); out1d = 0.0
     allocate(out2d(nxr,nyr)); out2d = 0.0

     if (do_ocnpost) then
        allocate(rgmask3d(nxr*nyr,nlevs)); rgmask3d = 0.0
     else
        allocate(rgmask2d(nxr*nyr)); rgmask2d = 0.0
     end if
     ! lat,lon of destination grid can be obtained from xc_b,yc_b in wgtsfile
     wgtsfile = trim(wgtsdir)//'tripole.mx025.Ct.to.rect.'//trim(dstgrid)//'.bilinear.nc'
     rc = nf90_open(trim(wgtsfile), nf90_nowrite, ncid)
     rc = nf90_inq_varid(ncid, 'xc_b', varid)
     rc = nf90_get_var(ncid,    varid, out1d)
     dstlon = reshape(out1d,(/nxr,nyr/))
     rc = nf90_inq_varid(ncid, 'yc_b', varid)
     rc = nf90_get_var(ncid,    varid, out1d)
     dstlat = reshape(out1d,(/nxr,nyr/))
     rc = nf90_close(ncid)

     if (allocated(bilin2d)) then
        wgtsfile = trim(wgtsdir)//'tripole.mx025.Ct.to.rect.'//trim(dstgrid)//'.bilinear.nc'
        if (debug) write(logunit,'(/,a)')'remapping 2D fields bilinear with '//trim(wgtsfile)
        call remap(trim(wgtsfile), dim2=nbilin2d, src_field=bilin2d, dst_field=rgb2d)

        if (debug) then
           write(logunit,'(a)')'2D fields mapped bilin, mapped field min/max values'
           do n = 1,nbilin2d
              write(logunit,'(i6,4(a,a),2g14.4)')n,'  ',trim(b2d(n)%input_var_name),'  ',                     &
                   trim(b2d(n)%var_grid),'  ',trim(b2d(n)%var_pair),'  ', trim(b2d(n)%var_pair_grid),         &
                   minval(rgb2d(:,n)), maxval(rgb2d(:,n))
           end do
           call dumpnc(trim(ftype)//'.rgbilin2d.'//trim(dstgrid)//'.nc', 'rgbilin2d', dims=(/nxr,nyr/),       &
                nflds=nbilin2d, field=rgb2d)
        end if
     end if

     if (allocated(consd2d)) then
        wgtsfile = trim(wgtsdir)//'tripole.mx025.Ct.to.rect.'//trim(dstgrid)//'.conserve.nc'
        if (debug) write(logunit,'(a)')'remapping 2D fields conserv with '//trim(wgtsfile)
        call remap(trim(wgtsfile), dim2=nconsd2d, src_field=consd2d, dst_field=rgc2d)

        if (debug) then
           write(logunit,'(a)')'2D fields mapped conserv, mapped field min/max values'
           do n = 1,nconsd2d
              write(logunit,'(i6,4(a,a),2g14.4)')n,'  ',trim(c2d(n)%input_var_name),'  ',                     &
                   trim(c2d(n)%var_grid),'  ',trim(c2d(n)%var_pair),'  ', trim(c2d(n)%var_pair_grid),         &
                   minval(rgc2d(:,n)), maxval(rgc2d(:,n))
           end do
           call dumpnc(trim(ftype)//'.rgconsd2d.'//trim(dstgrid)//'.nc', 'rgconsd2d', dims=(/nxr,nyr/),       &
                nflds=nconsd2d, field=rgc2d)
        end if
     end if

     if (allocated(bilin3d)) then
        wgtsfile = trim(wgtsdir)//'tripole.mx025.Ct.to.rect.'//trim(dstgrid)//'.bilinear.nc'
        if (debug) write(logunit,'(a)')'remapping 3D fields bilinear with '//trim(wgtsfile)
        call remap(trim(wgtsfile), nk=nlevs, nflds=nbilin3d, src_field=bilin3d, dst_field=rgb3d)

        if (debug) then
           write(logunit,'(a)')'3D fields mapped bilin, mapped field  min/max values'
           do n = 1,nbilin3d
              write(logunit,'(i6,4(a,a),2g14.4)')n,'  ',trim(b3d(n)%input_var_name),'  ',                     &
                   trim(b3d(n)%var_grid),'  ',trim(b3d(n)%var_pair),'  ', trim(b3d(n)%var_pair_grid),         &
                   minval(rgb3d(:,:,n)), maxval(rgb3d(:,:,n))
           end do
           call dumpnc(trim(ftype)//'.rgbilin3d.'//trim(dstgrid)//'.nc', 'rgbilin3d', dims=(/nxr,nyr,nlevs/), &
                nk=nlevs, nflds=nbilin3d, field=rgb3d)
        end if
     end if

     ! --------------------------------------------------------
     ! remap the source grid 2/3D mask to obtain the interpolation mask.
     ! --------------------------------------------------------

     wgtsfile = trim(wgtsdir)//'tripole.mx025.Ct.to.rect.'//trim(dstgrid)//'.bilinear.nc'
     if (do_ocnpost) then
        call remap(trim(wgtsfile), dim2=nlevs, src_field=mask3d, dst_field=rgmask3d)
        ! set interpolation mask missing on land, 1.0 on ocean on destination grids
        where(rgmask3d > 0.0)rgmask3d = vfill
        where(rgmask3d /= vfill)rgmask3d = 1.0
        ! out1d contains dstlat
        do n = 1,nlevs
           where(out1d(:) <= -79.75)rgmask3d(:,n) = vfill
        end do

        if (debug) then
           write(logunit,'(a,2g14.4)')'mask min/max on destination grid ',minval(rgmask3d),maxval(rgmask3d)
           call dumpnc(trim(ftype)//'.rgmask3d.'//trim(dstgrid)//'.nc', 'rgmask3d', dims=(/nxr,nyr,nlevs/), &
                field=rgmask3d)
        end if
     else
        call remap(trim(wgtsfile), src_field=mask2d, dst_field=rgmask2d)
        ! set interpolation mask missing on land, 1.0 on ocean on destination grids
        where(rgmask2d > 0.0)rgmask2d = vfill
        where(rgmask2d /= vfill)rgmask2d = 1.0
        ! out1d contains dstlat
        where(out1d(:) <= -79.75)rgmask2d(:) = vfill

        if (debug) then
           write(logunit,'(a,2g14.4)')'mask min/max on destination grid ',minval(rgmask2d),maxval(rgmask2d)
           call dumpnc(trim(ftype)//'.rgmask2d.'//trim(dstgrid)//'.nc', 'rgmask2d', dims=(/nxr,nyr/),       &
                field=rgmask2d)
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
           if (trim(b2d(n)%output_var_name) == 'speed')idx1 = n
           if (trim(b2d(n)%output_var_name) ==   'SSU')idx2 = n
           if (trim(b2d(n)%output_var_name) ==   'SSV')idx3 = n
        enddo
        where(rgb2d(:,idx1) .ne. vfill)rgb2d(:,idx1) = &
             sqrt(rgb2d(:,idx2)**2 + rgb2d(:,idx3)**2)
     end if

     ! --------------------------------------------------------
     ! write the mapped fields
     ! --------------------------------------------------------

     output_file = trim(ftype)//'.'//trim(dstgrid)//'.nc'
     if (debug) write(logunit, '(a)')'output file: '//trim(output_file)

     rc = nf90_create(trim(output_file), nf90_clobber, ncid)
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
           vname = trim(b2d(n)%output_var_name)
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
           vname = trim(c2d(n)%output_var_name)
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
           vname = trim(b3d(n)%output_var_name)
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
           vname = trim(b2d(n)%output_var_name)
           rc = nf90_inq_varid(ncid, vname, varid)
           rc = nf90_put_var(ncid,   varid, out2d)
        end do
     end if
     if (allocated(rgc2d)) then
        do n = 1,nconsd2d
           out2d(:,:) = reshape(rgc2d(:,n), (/nxr,nyr/))
           out2d(:,nyr) = vfill
           vname = trim(c2d(n)%output_var_name)
           rc = nf90_inq_varid(ncid, vname, varid)
           rc = nf90_put_var(ncid,   varid, out2d)
        end do
     end if
     if (allocated(rgb3d)) then
        do n = 1,nbilin3d
           out3d(:,:,:) = reshape(rgb3d(:,:,n), (/nxr,nyr,nlevs/))
           out3d(:,nyr,:) = vfill
           vname = trim(b3d(n)%output_var_name)
           rc = nf90_inq_varid(ncid, vname, varid)
           rc = nf90_put_var(ncid,   varid, out3d)
        end do
     end if
     rc = nf90_close(ncid)

     if (allocated(rgb2d)) deallocate(rgb2d)
     if (allocated(rgc2d)) deallocate(rgc2d)
     if (allocated(rgb3d)) deallocate(rgb3d)

     if (allocated(out1d)) deallocate(out1d)
     if (allocated(out2d)) deallocate(out2d)
     if (allocated(out3d)) deallocate(out3d)

     if (allocated(dstlon)) deallocate(dstlon)
     if (allocated(dstlat)) deallocate(dstlat)

     if (allocated(rgmask2d)) deallocate(rgmask2d)
     if (allocated(rgmask3d)) deallocate(rgmask3d)

  end do !nd
  write(logunit,'(a)')'all done!'

end program ocnicepost
