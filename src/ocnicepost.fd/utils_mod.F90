module utils_mod

  use netcdf
  use init_mod, only : debug, logunit, vardefs, fsrc, input_file, ftype
  use ieee_arithmetic


  implicit none

  private

  interface getfield
     module procedure getfield2d
     module procedure getfield3d
  end interface getfield

  interface packarrays
     module procedure packarrays2d
     module procedure packarrays3d
  end interface packarrays

  interface remap
     module procedure remap1d
     module procedure remap2d
     module procedure remap3d
  end interface remap

  interface getvecpair
     module procedure getvecpair2d
     module procedure getvecpair3d
  end interface getvecpair

  interface dumpnc
     module procedure dumpnc1d
     module procedure dumpnc2d
     module procedure dumpnc3d
     module procedure dumpnc3dk
  end interface dumpnc

  public getfield
  public packarrays
  public remap
  public dumpnc
  public write_grib2_2d
  public write_grib2_3d
  public nf90_err

contains

  !----------------------------------------------------------
  ! pack 2D fields into arrays by mapping type
  !----------------------------------------------------------
  subroutine packarrays2d(filesrc, wgtsdir, cosrot, sinrot, vars, dims, nflds, fields)

    character(len=*), intent(in)  :: filesrc,wgtsdir
    real,             intent(in)  :: cosrot(:),sinrot(:)
    type(vardefs),    intent(in)  :: vars(:)
    integer,          intent(in)  :: dims(:)
    integer,          intent(in)  :: nflds
    real,             intent(out) :: fields(:,:)

    ! local variables
    integer :: n, nn
    real, allocatable, dimension(:,:) :: vecpair
    character(len=20) :: subname = 'packarrays2d'

    fields=0.0

    if (debug)write(logunit,'(a)')'enter '//trim(subname)
    ! obtain vector pairs
    do n = 1,nflds
       if (trim(vars(n)%var_grid) == 'Cu' .or. trim(vars(n)%var_grid) == 'Bu_x') then
          allocate(vecpair(dims(1)*dims(2),2)); vecpair = 0.0
          call getvecpair(trim(filesrc), trim(wgtsdir), cosrot, sinrot,   &
               trim(vars(n)%var_name), trim(vars(n)%var_grid(1:2)), &
               trim(vars(n)%var_pair), trim(vars(n)%var_pair_grid(1:2)),  &
               dims=(/dims(1),dims(2)/), vecpair=vecpair)
       end if
    end do

    ! create packed array
    nn = 0
    do n = 1,nflds
       if (len_trim(vars(n)%var_pair) == 0) then
          nn = nn + 1
          call getfield(trim(filesrc), trim(vars(n)%var_name), dims=(/dims(1),dims(2)/), &
               field=fields(:,nn))
       else ! fill with vector pairs
          nn = nn+1
          ! ocn vectors
          if (trim(vars(n)%var_grid) == 'Cu')fields(:,nn) = vecpair(:,1)
          if (trim(vars(n)%var_grid) == 'Cv')fields(:,nn) = vecpair(:,2)
          ! ice vectors
          if (trim(vars(n)%var_grid) == 'Bu_x')fields(:,nn) = vecpair(:,1)
          if (trim(vars(n)%var_grid) == 'Bu_y')fields(:,nn) = vecpair(:,2)
       end if
    end do

    if (debug)write(logunit,'(a)')'exit '//trim(subname)
  end subroutine packarrays2d

  !----------------------------------------------------------
  ! pack 3D fields into arrays by mapping type
  !----------------------------------------------------------
  subroutine packarrays3d(filesrc, wgtsdir, cosrot, sinrot, vars, dims, nflds, fields)

    character(len=*), intent(in)  :: filesrc,wgtsdir
    real,             intent(in)  :: cosrot(:),sinrot(:)
    type(vardefs),    intent(in)  :: vars(:)
    integer,          intent(in)  :: dims(:)
    integer,          intent(in)  :: nflds
    real,             intent(out) :: fields(:,:,:)

    ! local variables
    integer :: n, nn
    real, allocatable, dimension(:,:,:) :: vecpair
    character(len=20) :: subname = 'packarrays3d'

    fields=0.0

    if (debug)write(logunit,'(a)')'enter '//trim(subname)
    ! obtain vector pairs
    do n = 1,dims(3)
       if (trim(vars(n)%var_grid) == 'Cu') then
          allocate(vecpair(dims(1)*dims(2),dims(3),2)); vecpair = 0.0
          call getvecpair(trim(filesrc), trim(wgtsdir), cosrot, sinrot, &
               trim(vars(n)%var_name), trim(vars(n)%var_grid),    &
               trim(vars(n)%var_pair), trim(vars(n)%var_pair_grid),     &
               dims=(/dims(1),dims(2),dims(3)/), vecpair=vecpair)
       end if
    end do

    ! create packed array
    nn = 0
    do n = 1,nflds
       if (len_trim(vars(n)%var_pair) == 0) then
          nn = nn + 1
          call getfield(trim(filesrc), trim(vars(n)%var_name), dims=(/dims(1),dims(2),dims(3)/), &
               field=fields(:,:,nn))
       else ! fill with vector pairs
          nn = nn+1
          if (trim(vars(n)%var_grid) == 'Cu')fields(:,:,nn) = vecpair(:,:,1)
          if (trim(vars(n)%var_grid) == 'Cv')fields(:,:,nn) = vecpair(:,:,2)
       end if
    end do

    if (debug)write(logunit,'(a)')'exit '//trim(subname)
  end subroutine packarrays3d

  !----------------------------------------------------------
  ! obtain 2D vector pairs mapped to Ct and rotated to EW
  !----------------------------------------------------------
  subroutine getvecpair2d(fname, wdir, cosrot, sinrot, vname1, vgrid1, &
       vname2, vgrid2, dims, vecpair)

    character(len=*), intent(in)  :: fname
    character(len=*), intent(in)  :: wdir
    real,             intent(in)  :: cosrot(:), sinrot(:)
    character(len=*), intent(in)  :: vname1, vgrid1, vname2, vgrid2
    integer,          intent(in)  :: dims(:)
    real,             intent(out) :: vecpair(:,:)

    ! local variables
    integer :: ii
    real, dimension(dims(1)*dims(2)) :: urot, vrot
    character(len=240) :: wgtsfile
    character(len=20) :: subname = 'getvecpair2d'

    if (debug)write(logunit,'(a)')'enter '//trim(subname)

    wgtsfile = trim(wdir)//'tripole.'//trim(fsrc)//'.'//vgrid1//'.to.Ct.bilinear.nc'
    call getfield(fname, vname1, dims=dims, field=vecpair(:,1), wgts=trim(wgtsfile))
    if (debug)write(logunit,'(a)')'wgtsfile for 2d vector '//trim(vname1)//'   '//trim(wgtsfile)
    wgtsfile = trim(wdir)//'tripole.'//trim(fsrc)//'.'//vgrid2//'.to.Ct.bilinear.nc'
    call getfield(fname, vname2, dims=dims, field=vecpair(:,2), wgts=trim(wgtsfile))
    if (debug)write(logunit,'(a)')'wgtsfile for 2d vector '//trim(vname2)//'   '//trim(wgtsfile)

    urot = 0.0; vrot = 0.0
    do ii = 1,dims(1)*dims(2)
       urot(ii) = vecpair(ii,1)*cosrot(ii) + vecpair(ii,2)*sinrot(ii)
       vrot(ii) = vecpair(ii,2)*cosrot(ii) - vecpair(ii,1)*sinrot(ii)
    end do
    vecpair(:,1) = urot(:)
    vecpair(:,2) = vrot(:)

    if (debug) write(logunit,'(a)')'exit '//trim(subname)
  end subroutine getvecpair2d

  !----------------------------------------------------------
  ! obtain 3D vector pairs, mapped to Ct and rotated to EW
  !----------------------------------------------------------
  subroutine getvecpair3d(fname, wdir, cosrot, sinrot, vname1, vgrid1, &
       vname2, vgrid2, dims, vecpair)

    character(len=*), intent(in)  :: fname
    character(len=*), intent(in)  :: wdir
    real,             intent(in)  :: cosrot(:), sinrot(:)
    character(len=*), intent(in)  :: vname1, vgrid1, vname2, vgrid2
    integer,          intent(in)  :: dims(:)
    real,             intent(out) :: vecpair(:,:,:)

    ! local variables
    integer :: ii,k
    real, dimension(dims(1)*dims(2)) :: urot, vrot
    character(len=240) :: wgtsfile
    character(len=20) :: subname = 'getvecpair3d'

    if (debug)write(logunit,'(a)')'enter '//trim(subname)

    wgtsfile = trim(wdir)//'tripole.'//trim(fsrc)//'.'//vgrid1//'.to.Ct.bilinear.nc'
    call getfield(fname, vname1, dims=dims, field=vecpair(:,:,1), wgts=trim(wgtsfile))
    wgtsfile = trim(wdir)//'tripole.'//trim(fsrc)//'.'//vgrid2//'.to.Ct.bilinear.nc'
    call getfield(fname, vname2, dims=dims, field=vecpair(:,:,2), wgts=trim(wgtsfile))

    do k = 1,dims(3)
       urot = 0.0; vrot = 0.0
       do ii = 1,dims(1)*dims(2)
          urot(ii)= vecpair(ii,k,1)*cosrot(ii) + vecpair(ii,k,2)*sinrot(ii)
          vrot(ii)= vecpair(ii,k,2)*cosrot(ii) - vecpair(ii,k,1)*sinrot(ii)
       end do
       vecpair(:,k,1) = urot(:)
       vecpair(:,k,2) = vrot(:)
    end do

    if (debug) write(logunit,'(a)')'exit '//trim(subname)
  end subroutine getvecpair3d

  !----------------------------------------------------------
  ! obtain a 2D field and return a 1-D vector array
  !----------------------------------------------------------
  subroutine getfield2d(fname, vname, dims, field, wgts)

    character(len=*),           intent(in)  :: fname, vname
    integer,                    intent(in)  :: dims(:)
    real,                       intent(out) :: field(:)
    character(len=*), optional, intent(in)  :: wgts

    ! local variable
    integer           :: ncid, varid, rc
    real              :: fval
    real, allocatable :: a2d(:,:)
    real, allocatable :: atmp(:)
    character(len=20) :: subname = 'getfield2d'

    if (debug)write(logunit,'(a)')'enter '//trim(subname)//' variable '//vname

    allocate(a2d(dims(1),dims(2))); a2d = 0.0
    allocate(atmp(dims(1)*dims(2))); atmp = 0.0

    call nf90_err(nf90_open(fname, nf90_nowrite, ncid), 'nf90_open: '//fname)
    call nf90_err(nf90_inq_varid(ncid, vname, varid), 'get variable ID: '//vname)
    call nf90_err(nf90_get_var(ncid, varid, a2d), 'get variable: '//vname)
    call nf90_err(nf90_get_att(ncid, varid, '_FillValue', fval), 'get attribute FillValue: '//vname)
    call nf90_err(nf90_close(ncid), 'close: '//fname)

    atmp(:) = reshape(a2d, (/dims(1)*dims(2)/))
    where(atmp .eq. fval)atmp = 0.0
    if(present(wgts)) then
       call remap(trim(wgts), src_field=atmp, dst_field=field)
    else
       field = atmp
    end if

    if (debug) write(logunit,'(a)')'exit '//trim(subname)//' variable '//vname
  end subroutine getfield2d

  !----------------------------------------------------------
  ! obtain a 3D field and return a 2-D vector array
  !----------------------------------------------------------
  subroutine getfield3d(fname, vname, dims, field, wgts)

    character(len=*),           intent(in)  :: fname, vname
    integer,                    intent(in)  :: dims(:)
    real,                       intent(out) :: field(:,:)
    character(len=*), optional, intent(in)  :: wgts

    ! local variable
    integer           :: ncid, varid, rc
    real              :: fval
    real, allocatable :: a3d(:,:,:)
    real, allocatable :: atmp(:,:)
    character(len=20) :: subname = 'getfield3d'

    if (debug)write(logunit,'(a)')'enter '//trim(subname)//' variable '//vname

    allocate(a3d(dims(1),dims(2),dims(3))); a3d = 0.0
    allocate(atmp(dims(1)*dims(2),dims(3))); atmp = 0.0

    call nf90_err(nf90_open(fname, nf90_nowrite, ncid), 'nf90_open: '//fname)
    call nf90_err(nf90_inq_varid(ncid, vname, varid), 'get variable ID: '//vname)
    call nf90_err(nf90_get_var(ncid, varid, a3d), 'get variable: '//vname)
    call nf90_err(nf90_get_att(ncid, varid, '_FillValue', fval), 'get attribute FillValue: '//vname)
    call nf90_err(nf90_close(ncid), 'close: '//fname)

    atmp(:,:) = reshape(a3d, (/dims(1)*dims(2),dims(3)/))
    where(atmp .eq. fval)atmp = 0.0
    if(present(wgts)) then
       call remap(trim(wgts), dim2=dims(3), src_field=atmp, dst_field=field)
    else
       field = atmp
    end if

    if (debug) write(logunit,'(a)')'exit '//trim(subname)//' variable '//vname
  end subroutine getfield3d

  !----------------------------------------------------------
  ! remap a 1-D vector array
  !----------------------------------------------------------
  subroutine remap1d(fname, src_field, dst_field)

    character(len=*), intent(in)  :: fname
    real,             intent(in)  :: src_field(:)
    real,             intent(out) :: dst_field(:)

    ! local variables
    integer :: ncid, rc, id
    integer :: i,ii,jj
    integer :: n_a, n_b, n_s
    integer(kind=4), allocatable, dimension(:) :: col, row
    real(kind=8), allocatable, dimension(:)    :: S
    character(len=20) :: subname = 'remap1d'

    if (debug)write(logunit,'(a)')'enter '//trim(subname)

    ! retrieve the weights
    call nf90_err(nf90_open(trim(fname), nf90_nowrite, ncid), 'open: '//fname)
    call nf90_err(nf90_inq_dimid(ncid, 'n_s', id), 'get dimension Id: n_s')
    call nf90_err(nf90_inquire_dimension(ncid, id, len=n_s), 'get dimension: n_s' )
    call nf90_err(nf90_inq_dimid(ncid, 'n_a', id), 'get dimension Id: n_a')
    call nf90_err(nf90_inquire_dimension(ncid, id, len=n_a), 'get dimension: n_a' )
    call nf90_err(nf90_inq_dimid(ncid, 'n_b', id), 'get dimension Id: n_b')
    call nf90_err(nf90_inquire_dimension(ncid, id, len=n_b), 'get dimension: n_b' )

    allocate(col(1:n_s)); col = 0
    allocate(row(1:n_s)); row = 0
    allocate(  S(1:n_s)); S = 0.0

    call nf90_err(nf90_inq_varid(ncid, 'col', id),'get variable Id: col')
    call nf90_err(nf90_get_var(ncid,     id, col),'get variable: col')
    call nf90_err(nf90_inq_varid(ncid, 'row', id),'get variable Id: row')
    call nf90_err(nf90_get_var(ncid,     id, row),'get variable: row')
    call nf90_err(nf90_inq_varid(ncid,   'S', id),'get variable Id: S')
    call nf90_err(nf90_get_var(ncid,      id,  S),'get variable: S')
    call nf90_err(nf90_close(ncid), 'close: '//fname)

    dst_field = 0.0
    do i = 1,n_s
       ii = row(i); jj = col(i)
       dst_field(ii) = dst_field(ii) + S(i)*real(src_field(jj),8)
    enddo

    if (debug) write(logunit,'(a)')'exit '//trim(subname)
  end subroutine remap1d

  !----------------------------------------------------------
  ! remap a packed field of either nflds or nlevs
  !----------------------------------------------------------
  subroutine remap2d(fname, dim2, src_field, dst_field)

    character(len=*), intent(in)  :: fname
    integer,          intent(in)  :: dim2
    real,             intent(in)  :: src_field(:,:)
    real,             intent(out) :: dst_field(:,:)

    ! local variables
    integer :: ncid, rc, id
    integer :: i,ii,jj
    integer :: n_a, n_b, n_s
    integer(kind=4), allocatable, dimension(:) :: col, row
    real(kind=8),    allocatable, dimension(:) :: S
    character(len=20) :: subname = 'remap2d'

    if (debug)write(logunit,'(a)')'enter '//trim(subname)//' weights = '//trim(fname)

    ! retrieve the weights
    call nf90_err(nf90_open(trim(fname), nf90_nowrite, ncid), 'open: '//fname)
    call nf90_err(nf90_inq_dimid(ncid, 'n_s', id), 'get dimension Id: n_s')
    call nf90_err(nf90_inquire_dimension(ncid, id, len=n_s), 'get dimension: n_s')
    call nf90_err(nf90_inq_dimid(ncid, 'n_a', id), 'get dimension Id: n_a')
    call nf90_err(nf90_inquire_dimension(ncid, id, len=n_a), 'get dimension: n_a')
    call nf90_err(nf90_inq_dimid(ncid, 'n_b', id), 'get dimension Id: n_b')
    call nf90_err(nf90_inquire_dimension(ncid, id, len=n_b), 'get dimension: n_b')

    allocate(col(1:n_s)); col = 0
    allocate(row(1:n_s)); row = 0
    allocate(  S(1:n_s)); S = 0.0

    call nf90_err(nf90_inq_varid(ncid, 'col', id),'get variable Id: col')
    call nf90_err(nf90_get_var(ncid,     id, col),'get variable: col')
    call nf90_err(nf90_inq_varid(ncid, 'row', id),'get variable Id: row')
    call nf90_err(nf90_get_var(ncid,     id, row),'get variable: row')
    call nf90_err(nf90_inq_varid(ncid,   'S', id),'get variable Id: S')
    call nf90_err(nf90_get_var(ncid,      id,  S),'get variable: S')
    call nf90_err(nf90_close(ncid), 'close: '//fname)

    dst_field = 0.0
    do i = 1,n_s
       ii = row(i); jj = col(i)
       dst_field(ii,:) = dst_field(ii,:) + S(i)*real(src_field(jj,:),8)
    enddo

    if (debug) write(logunit,'(a)')'exit '//trim(subname)
  end subroutine remap2d

  !----------------------------------------------------------
  ! remap a field packed array of nk levels and nflds fields
  !----------------------------------------------------------
  subroutine remap3d(fname, nk, nflds, src_field, dst_field)

    character(len=*), intent(in)  :: fname
    integer,          intent(in)  :: nk, nflds
    real,             intent(in)  :: src_field(:,:,:)
    real,             intent(out) :: dst_field(:,:,:)

    ! local variables
    integer :: ncid, rc, id
    integer :: i,ii,jj
    integer :: n_a, n_b, n_s
    integer(kind=4), allocatable, dimension(:) :: col, row
    real(kind=8),    allocatable, dimension(:) :: S
    character(len=20) :: subname = 'remap3d'

    if (debug)write(logunit,'(a)')'enter '//trim(subname)//' weights = '//trim(fname)

    ! retrieve the weights
    call nf90_err(nf90_open(trim(fname), nf90_nowrite, ncid), 'open: '//fname)
    call nf90_err(nf90_inq_dimid(ncid, 'n_s', id), 'get dimension Id: n_s')
    call nf90_err(nf90_inquire_dimension(ncid, id, len=n_s), 'get dimension: n_s')
    call nf90_err(nf90_inq_dimid(ncid, 'n_a', id), 'get dimension Id: n_a')
    call nf90_err(nf90_inquire_dimension(ncid, id, len=n_a), 'get dimension: n_a')
    call nf90_err(nf90_inq_dimid(ncid, 'n_b', id), 'get dimension Id: n_b')
    call nf90_err(nf90_inquire_dimension(ncid, id, len=n_b), 'get dimension: n_b')

    allocate(col(1:n_s)); col = 0
    allocate(row(1:n_s)); row = 0
    allocate(  S(1:n_s)); S = 0.0

    call nf90_err(nf90_inq_varid(ncid, 'col', id),'get variable Id: col')
    call nf90_err(nf90_get_var(ncid,     id, col),'get variable: col')
    call nf90_err(nf90_inq_varid(ncid, 'row', id),'get variable Id: row')
    call nf90_err(nf90_get_var(ncid,     id, row),'get variable: row')
    call nf90_err(nf90_inq_varid(ncid,   'S', id),'get variable Id: S')
    call nf90_err(nf90_get_var(ncid,      id,  S),'get variable: S')
    call nf90_err(nf90_close(ncid), 'close: '//fname)

    dst_field = 0.0
    do i = 1,n_s
       ii = row(i); jj = col(i)
       dst_field(ii,:,:) = dst_field(ii,:,:) + S(i)*real(src_field(jj,:,:),8)
    enddo

    if (debug) write(logunit,'(a)')'exit '//trim(subname)
  end subroutine remap3d

  !----------------------------------------------------------
  ! write a bare netcdf file of a 2D packed field
  !----------------------------------------------------------
  subroutine dumpnc2d(fname, vname, dims, nflds, field)

    character(len=*), intent(in) :: fname, vname
    integer,          intent(in) :: dims(:)
    integer,          intent(in) :: nflds
    real,             intent(in) :: field(:,:)

    ! local variable
    integer :: ncid, varid, rc, idimid, jdimid, fdimid
    real, allocatable :: a3d(:,:,:)
    character(len=20) :: subname = 'dumpnc2d'

    if (debug)write(logunit,'(a)')'enter '//trim(subname)//' variable '//vname
    allocate(a3d(dims(1),dims(2),nflds)); a3d = 0.0

    call nf90_err(nf90_create(trim(fname), nf90_clobber, ncid), 'create: '//fname)
    call nf90_err(nf90_def_dim(ncid, 'nx', dims(1), idimid), 'define dimension: nx')
    call nf90_err(nf90_def_dim(ncid, 'ny', dims(2), jdimid), 'define dimension: ny')
    call nf90_err(nf90_def_dim(ncid, 'nf', nflds,   fdimid), 'define dimension: nf')
    call nf90_err(nf90_def_var(ncid, vname, nf90_float, (/idimid,jdimid,fdimid/), varid), 'define variable: '//vname)
    call nf90_err(nf90_enddef(ncid), 'nf90_enddef: '//fname)

    a3d(:,:,:) =  reshape(field(1:dims(1)*dims(2),1:nflds), (/dims(1),dims(2),nflds/))
    call nf90_err(nf90_put_var(ncid, varid, a3d), 'put variable: '//vname)
    call nf90_err(nf90_close(ncid), 'close: '//fname)

    if (debug)write(logunit,'(a)')'exit '//trim(subname)//' variable '//vname
  end subroutine dumpnc2d

  !----------------------------------------------------------
  ! write a bare netcdf file of a packed 3D field
  !----------------------------------------------------------
  subroutine dumpnc3d(fname, vname, dims, nk, nflds, field)

    character(len=*), intent(in) :: fname, vname
    integer,          intent(in) :: dims(:)
    integer,          intent(in) :: nk, nflds
    real,             intent(in) :: field(:,:,:)

    ! local variable
    integer :: n, ncid, varid, rc, idimid, jdimid, kdimid, fdimid
    real, allocatable :: a4d(:,:,:,:)
    character(len=20) :: subname = 'dumpnc3d'

    if (debug)write(logunit,'(a)')'enter '//trim(subname)//' variable '//vname
    allocate(a4d(dims(1),dims(2),dims(3),nflds)); a4d = 0.0

    call nf90_err(nf90_create(trim(fname), nf90_clobber, ncid), 'create: '//fname)
    call nf90_err(nf90_def_dim(ncid, 'nx', dims(1), idimid), 'define dimension: nx')
    call nf90_err(nf90_def_dim(ncid, 'ny', dims(2), jdimid), 'define dimension: ny')
    call nf90_err(nf90_def_dim(ncid, 'nk', dims(3), kdimid), 'define dimension: nk')
    call nf90_err(nf90_def_dim(ncid, 'nf', nflds,   fdimid), 'define dimension: nf')
    call nf90_err(nf90_def_var(ncid, vname, nf90_float, (/idimid,jdimid,kdimid,fdimid/), varid), 'define variable: '//vname)
    call nf90_err(nf90_enddef(ncid), 'nf90_enddef: '//fname)

    do n = 1,nflds
       a4d(:,:,:,n) = reshape(field(1:dims(1)*dims(2),1:dims(3),n), (/dims(1),dims(2),dims(3)/))
    end do
    call nf90_err(nf90_put_var(ncid, varid, a4d), 'put variable: '//vname)
    call nf90_err(nf90_close(ncid), 'close: '//fname)

    if (debug)write(logunit,'(a)')'exit '//trim(subname)//' variable '//vname
  end subroutine dumpnc3d

  !----------------------------------------------------------
  ! write a bare netcdf file of an unpacked 3D field
  !----------------------------------------------------------
  subroutine dumpnc3dk(fname, vname, dims, field)

    character(len=*), intent(in) :: fname, vname
    integer,          intent(in) :: dims(:)
    real,             intent(in) :: field(:,:)

    ! local variable
    integer :: ncid, varid, rc, idimid, jdimid, kdimid
    real, allocatable :: a3d(:,:,:)
    character(len=20) :: subname = 'dumpnc3dk'

    if (debug)write(logunit,'(a)')'enter '//trim(subname)//' variable '//vname
    allocate(a3d(dims(1),dims(2),dims(3))); a3d = 0.0

    call nf90_err(nf90_create(trim(fname), nf90_clobber, ncid), 'nf90_create: '//fname)
    call nf90_err(nf90_def_dim(ncid, 'nx', dims(1), idimid), 'define dimension: nx')
    call nf90_err(nf90_def_dim(ncid, 'ny', dims(2), jdimid), 'define dimension: ny')
    call nf90_err(nf90_def_dim(ncid, 'nk', dims(3), kdimid), 'define dimension: nk')
    call nf90_err(nf90_def_var(ncid, vname, nf90_float, (/idimid,jdimid,kdimid/), varid), 'define variable: '//vname)
    call nf90_err(nf90_enddef(ncid), 'nf90_enddef: '//fname)

    a3d(:,:,:) =  reshape(field(1:dims(1)*dims(2),1:dims(3)), (/dims(1),dims(2),dims(3)/))
    call nf90_err(nf90_put_var(ncid, varid, a3d), 'put variable: '//vname)
    call nf90_err(nf90_close(ncid), 'close: '//fname)

    if (debug)write(logunit,'(a)')'exit '//trim(subname)//' variable '//vname

  end subroutine dumpnc3dk

  !----------------------------------------------------------
  ! write a bare netcdf file of an unpacked 2D field
  !----------------------------------------------------------
  subroutine dumpnc1d(fname, vname, dims, field)

    character(len=*), intent(in) :: fname, vname
    integer,          intent(in) :: dims(:)
    real,             intent(in) :: field(:)

    ! local variable
    integer           :: ncid, varid, rc, idimid, jdimid
    real, allocatable :: a2d(:,:)
    character(len=20) :: subname = 'dumpnc1d'

    if (debug)write(logunit,'(a)')'enter '//trim(subname)//' variable '//vname
    allocate(a2d(dims(1),dims(2))); a2d = 0.0

    call nf90_err(nf90_create(trim(fname), nf90_clobber, ncid), 'nf90_create: '//fname)
    call nf90_err(nf90_def_dim(ncid, 'nx', dims(1), idimid), 'define dimension: nx')
    call nf90_err(nf90_def_dim(ncid, 'ny', dims(2), jdimid), 'define dimension: ny')
    call nf90_err(nf90_def_var(ncid, vname, nf90_float, (/idimid,jdimid/), varid), 'define variable: '//vname)
    call nf90_err(nf90_enddef(ncid), 'nf90_enddef: '//fname)

    a2d(:,:) =  reshape(field(1:dims(1)*dims(2)), (/dims(1),dims(2)/))
    call nf90_err(nf90_put_var(ncid, varid, a2d), 'put variable: '//vname)
    call nf90_err(nf90_close(ncid), 'close: '//fname)

    if (debug)write(logunit,'(a)')'exit '//trim(subname)//' variable '//vname

  end subroutine dumpnc1d



  !-----------------------------------------------------------------------------------
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Write Grib2 2D !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!This subroutine write Grib2 file modified messages!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !-----------------------------------------------------------------------------------

  subroutine write_grib2_2d(fname, gcf, dims, nflds, field, rgmask2d, vfill)
   
       implicit none
   
       character(len=*),    intent(in) :: fname
       type(vardefs),       intent(in) :: gcf(:) 
       integer(4),          intent(in) :: dims(2)
       integer(4),          intent(in) :: nflds
       real(4),             intent(inout) :: field(dims(1)*dims(2),nflds)
       real(4),             intent(in) :: rgmask2d(dims(1) * dims(2))
       real(4),             intent(in) :: vfill

       ! internal variables
       integer(4) :: max_bytes, lengrib
       integer(4) :: ref_time(6)
       integer(4) :: lunout, ierr
       integer(4) :: fortime, dij, npt
       CHARACTER(len=1),allocatable,dimension(:) :: cgrib
       real(4) :: tmpfld(size(field,1))

       ! GRIB2 metadata arrays
       integer(4) :: listsec0(2), listsec1(13)
       integer(4) :: igdtnum, ipdtnum, idrtnum
       integer(4) :: igdtlen, ipdtlen, idrtlen
       integer(4) :: jgdt(19), jpdt(15), idrtmpl(16)
       integer(4) :: igds(5)
       integer(4) :: numcoord, ibmap
       real    :: coordlist
       integer(4) :: n, lon0, lon1, lat0, lat1
       integer(4) :: ideflist, idefnum
       logical :: bmp(dims(1)*dims(2)) 

       real :: max_val, min_val, mean_val, count_val

       npt = dims(1) * dims(2)
   
       max_bytes = npt * 4  ! Estimated max bytes
       bmp=.true.

       call getlun(lunout)
       call baopenw(lunout, trim(fname), ierr)
       if (ierr /= 0) then
           write(0, *) 'Error opening grib2 file ', trim(fname)
           return
       end if
   
       call retrieve_time( fortime , ref_time )

       ! Initialize GRIB2 message sections
       listsec0(1) = gcf(1)%var_g1     ! Discipline - GRIB Master Table Number (Code Table 0.0)
       listsec0(2) = 2                 ! GRIB Edition Number (currently 2)
   
       listsec1(1) = gcf(1)%var_g3     ! Originating Centre (Common Code Table C-1)
       listsec1(2) = 0                 ! Originating Sub-centre (local table) EMC=4
!       listsec1(3) = 32                 ! GRIB Master Tables Version Number (Code Table 1.0)-last one currently 32
       listsec1(3) = gcf(1)%var_g2     ! GRIB Master Tables Version Number (Code Table 1.0)
       listsec1(4) = 1                 ! GRIB Local Tables Version Number (Code Table 1.1)
       listsec1(5) = 1                 ! Significance of Reference Time (Code Table 1.2)
       listsec1(6) = ref_time(1)       ! Reference Time - Year -4digits
       listsec1(7) = ref_time(2)       ! Reference Time - Month
       listsec1(8) = ref_time(3)       ! Reference Time - Day
       listsec1(9) = ref_time(4)       ! Reference Time - Hour
       listsec1(10) = ref_time(5)      ! Reference Time - Minute
       listsec1(11) = ref_time(6)      ! Reference Time - Second
       listsec1(12) = 0                ! Production status of data (Code Table 1.3)
       listsec1(13) = 1                ! Type of processed data (Code Table 1.4)

       ! set grid res
       if (dims(1) == 1440 .and. dims(2) == 721) dij= 250000     ! 1/4deg rectilinear
       if (dims(1) == 720  .and. dims(2) == 361) dij= 500000     ! 1/2deg rectilinear
       if (dims(1) == 360  .and. dims(2) == 181) dij= 1000000     ! 1deg rectilinear
       if (dims(1) == 72   .and. dims(2) == 36) dij= 5000000      ! 5deg rectilinear

       lon0 = 0 
       lon1 = 360000000 - dij
       lat0 = -90000000
       lat1 = 90000000

       ! Populate the jgdt array for Template 3.0 (changed parameters to current grib2 files)
       jgdt(1) = 6              
       jgdt(2) = 0              
       jgdt(3) = 0                
       jgdt(4) = 0                 
       jgdt(5) = 0                  
       jgdt(6) = 0                
       jgdt(7) = 0                 
       jgdt(8) = dims(1)             
       jgdt(9) = dims(2)        
       jgdt(10) = 0
       jgdt(11) = -1    
       jgdt(12) = lat0
       jgdt(13) = lon0
       jgdt(14) = 48   !0
       jgdt(15) = lat1
       jgdt(16) = lon1
       jgdt(17) = dij
       jgdt(18) = dij
       jgdt(19) = 64 

       igdtnum=0
       ! Define igds GRIB2 - SECTION 3
       igds(1) = 0          ! Source of grid definition 
       igds(2) = npt        ! Number of grid points
       igds(3) = 0          ! Number of octets for each additional grid points definition
       igds(4) = 0          ! Interpretation of list for optional points definition
       igds(5) = igdtnum    ! GRIB2 - CODE TABLE 3.1

       igdtlen=size(jgdt)

       if (debug) then
         write(logunit, *) 'listsec0, listsec1: ', listsec0, listsec1
         write(logunit, *) 'igdtnum, igdtlen: ', igdtnum, igdtlen
         write(logunit, *) 'jgdt: ', jgdt
         write(logunit, *) 'igds: ', igds
         write(logunit, *) 'dij: ', dij
         write(logunit, *) 'max_bytes: ', max_bytes
         write(logunit, *) 'forcast time: ', fortime
         write(logunit, *) 'refference time: ', ref_time
       end if

       ideflist=0
       idefnum=0

       do n=1,nflds

         allocate(cgrib(max_bytes))

         listsec0(1) = gcf(n)%var_g1

         call gribcreate(cgrib, max_bytes, listsec0, listsec1, ierr) 
         if (ierr /= 0) then
            write(0, *) 'Error initializing GRIB2 message', ierr
            return
         end if

       ! Compute max, min, and mean
         max_val = maxval(field(:,n), mask = field(:,n) .ne. vfill)
         min_val = minval(field(:,n), mask = field(:,n) .ne. vfill)
         mean_val = sum(field(:,n), mask = field(:,n) .ne. vfill) / count(field(:,n) .ne. vfill)

         if (debug) then
            write(logunit, *) 'var_name, n: ', gcf(n)%var_name, n
            write(logunit,*) 'size_bmp ,size_rgmask2d', size(bmp), size(rgmask2d)
            write(logunit, *) 'Variable_name, max, min, mean: ', gcf(n)%var_name, max_val, min_val, mean_val
         end if

         call addgrid(cgrib, max_bytes, igds, jgdt, igdtlen, ideflist, idefnum, ierr) ! there is an internal error here 
         if (ierr /= 0) then
             write(0, *) 'Error adding grid to GRIB2 message', ierr
             return
         end if

!        Create Section 4 parametrs    
         ipdtnum=0

         jpdt(1)=gcf(n)%var_g5  ! parm number catagory
         jpdt(2)=gcf(n)%var_g6  ! parm number
         jpdt(3)=2              ! (0-analysis, 1-initialazation, 2-forecast, .. GRIB2 - CODE TABLE 4.3 )
         jpdt(4)=0              !  
         jpdt(5)=96             ! Code ON388 Table A- GFS
         jpdt(6)=0              !    
         jpdt(7)=0              ! 
         jpdt(8)=1              ! unit (Hour=1)    6hour=11     (ask later) Table 4.4
         jpdt(9)=fortime        ! forecast time
         jpdt(10)=gcf(n)%var_g7  ! level ID (1-Ground or Water Surface, 101 mean sea level, 160 depth bellow mean sea level , 168-Ocean Model Layer,...)
         jpdt(11)=0              ! 
         jpdt(12)=0             ! 
         jpdt(13)=0
         jpdt(14)=0
         jpdt(15)=0


         if (debug) write(logunit, *) 'ipdtnum=', ipdtnum, ', jpdt= ', jpdt(1:16)

         ipdtlen=size(jpdt)

         numcoord=0
         coordlist=0.  ! needed for hybrid vertical coordinate

         ibmap = 255     ! Bitmap indicator ( see Code Table 6.0 ) -255 no bitmap
         bmp=.true.

         if (trim(gcf(n)%name_gb2) .eq. 'WTMP') then
            where ( field(:,n) .ne. vfill ) field(:,n) = field(:,n) + 273.15
         endif

         where ( field(:,n) .eq. vfill )  field(:,n)= 9999.0

         !        Create Section 5 parametrs   
         idrtnum = 0                            ! Template 5.2 (Grid Point Data - complex Packing)

         idrtmpl(:)=0
         ! Populate idrtmpl
         idrtmpl(1) = 0             ! Reference value (scaled value of the minimum data point)
         idrtmpl(2) = 0             ! Binary scale factor (scale by 2^E)
         idrtmpl(3) = 2            ! Decimal scale factor (scale by 10^D)
         idrtmpl(4) = 0             !
         idrtmpl(5) = 0             ! 
         idrtmpl(6) = 0             ! 
         ! Reserved fields
         idrtmpl(7:16) = 0          ! Reserved for future use 

         idrtlen=size(idrtmpl)

         if (debug) write(logunit, *) 'idrtmpl: ', idrtmpl

         tmpfld=0 
         tmpfld =field(:,n)


         write(logunit, *) 'Variable_name, max, min, mean, count: ', gcf(n)%var_name, max_val, min_val, mean_val, count_val

         call addfield(cgrib, max_bytes, ipdtnum, jpdt, ipdtlen, coordlist, numcoord, &
         idrtnum, idrtmpl, idrtlen, tmpfld, npt, ibmap, bmp, ierr)
         if (ierr /= 0) then
             write(0, *) 'Error adding field to GRIB2 message', ierr
             return
         end if

         call gribend(cgrib, max_bytes, lengrib, ierr)
         if (debug) write(logunit, *) 'gribend status=', ierr
         if (debug) write(logunit, *) 'length of the final GRIB2 message in octets =', lengrib
         call wryte(lunout, lengrib, cgrib)

         deallocate(cgrib)
      
       end do

       call baclose(lunout, ierr)

       return

  end subroutine write_grib2_2d



  !-----------------------------------------------------------------------------------
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Write Grib2 3D!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!This subroutine write Grib2 file modified messages!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !-----------------------------------------------------------------------------------

  subroutine write_grib2_3d(fname, gcf, dims, nflds, field, rgmask3d, vfill)
   
   implicit none

   character(len=*), intent(in) :: fname
   type(vardefs),    intent(in) :: gcf(:)
   integer,          intent(in) :: dims(3)
   integer,          intent(in) :: nflds
   real,             intent(inout) :: field( dims(1) * dims(2) , dims(3) , nflds )
   real,             intent(in) :: rgmask3d(dims(1) * dims(2) , dims(3))
   real,             intent(in) :: vfill

   ! internal variables
   integer :: max_bytes, lengrib
   integer :: ref_time(6)
   integer :: lunout, ierr
   integer :: fortime, dij, npt
   CHARACTER(len=1),allocatable,dimension(:) :: cgrib
   real(4) :: tmpfld(size(field,1))

   ! GRIB2 metadata arrays
   integer :: listsec0(2), listsec1(13)
   integer :: igdtnum, ipdtnum, idrtnum
   integer :: igdtlen, ipdtlen, idrtlen
   integer :: jgdt(19), jpdt(15), idrtmpl(16)
   integer :: igds(5)
   integer :: numcoord, ibmap
   real :: coordlist
   integer :: ideflist, idefnum
   logical :: bmp( dims(1) * dims(2) ) 

   integer :: n, lon0, lon1, lat0, lat1, nlay, lyr
   real, dimension(40) :: dep1
   real, dimension(28) :: dep2
   real, dimension(:), allocatable :: dep
   
   npt = dims(1) * dims(2)

   max_bytes = npt * 4
   bmp=.true.

   call getlun(lunout)
   call baopenw(lunout, trim(fname), ierr)
   if (ierr /= 0) then
       write(0, *) 'Error opening grib2 file ', trim(fname)
       return
   end if

   call retrieve_time( fortime , ref_time )

   ! Initialize GRIB2 message sections
   listsec0(1) = gcf(1)%var_g1     ! Discipline - GRIB Master Table Number (Code Table 0.0)
   listsec0(2) = 2                 ! GRIB Edition Number (currently 2)

   listsec1(1) = gcf(1)%var_g3     ! Originating Centre (Common Code Table C-1)
   listsec1(2) = 0                 ! Originating Sub-centre (local table) EMC=4
   listsec1(3) = gcf(1)%var_g2     ! GRIB Master Tables Version Number (Code Table 1.0)
   listsec1(4) = 1                 ! GRIB Local Tables Version Number (Code Table 1.1)
   listsec1(5) = 1                 ! Significance of Reference Time (Code Table 1.2)
   listsec1(6) = ref_time(1)       ! Reference Time - Year -4digits
   listsec1(7) = ref_time(2)       ! Reference Time - Month
   listsec1(8) = ref_time(3)       ! Reference Time - Day
   listsec1(9) = ref_time(4)       ! Reference Time - Hour
   listsec1(10) = ref_time(5)      ! Reference Time - Minute
   listsec1(11) = ref_time(6)      ! Reference Time - Second
   listsec1(12) = 0                ! Production status of data (Code Table 1.3)
   listsec1(13) = 1                ! Type of processed data (Code Table 1.4)

   dep1=(/ 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115, 125, 135, 145, 155,&
    165, 175, 185, 195, 205, 215, 226, 241, 267, 309, 374, 467, 594, 757, 960,&
     1204, 1490, 1817, 2184, 2587, 3024, 3489, 3977, 4481 /)

   dep2=(/ 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 115, 125, 135, 145, 155,&
    165, 175, 185, 195, 205, 215, 226, 241, 267, 309, 374, 467 /)

   if (dims(1) == 1440 .and. dims(2) == 721) then   ! 1/4deg rectilinear
      dij = 250000
      nlay = 40
      dep = dep1
   end if
  
   if (dims(1) == 720 .and. dims(2) == 361) then   ! 1/2deg rectilinear
      dij = 500000
      nlay = 40
      dep = dep1
   end if
  
   if (dims(1) == 360 .and. dims(2) == 181) then   ! 1deg rectilinear
      dij = 1000000
      nlay = 40
      dep = dep1
   end if
  
   if (dims(1) == 72  .and. dims(2) == 36 ) then   ! 5deg rectilinear
      dij = 5000000
      nlay = 25
      dep = dep2   
   end if

   lon0 = 0
   lon1 = 360000000 - dij
   lat0 = -90000000
   lat1 = 90000000

       ! Populate the jgdt array for Template 3.0 (changed parameters to current grib2 files)
   jgdt(1) = 6              
   jgdt(2) = 0              
   jgdt(3) = 0                
   jgdt(4) = 0                 
   jgdt(5) = 0                  
   jgdt(6) = 0                
   jgdt(7) = 0                 
   jgdt(8) = dims(1)             
   jgdt(9) = dims(2)        
   jgdt(10) = 0
   jgdt(11) = -1   
   jgdt(12) = lat0
   jgdt(13) = lon0
   jgdt(14) = 48   
   jgdt(15) = lat1
   jgdt(16) = lon1
   jgdt(17) = dij
   jgdt(18) = dij
   jgdt(19) = 64 

   igdtnum=0
   ! Define igds GRIB2 - SECTION 3
   igds(1) = 0          ! Source of grid definition 
   igds(2) = npt        ! Number of grid points
   igds(3) = 0          ! Number of octets for each additional grid points definition
   igds(4) = 0          ! Interpretation of list for optional points definition
   igds(5) = igdtnum    ! GRIB2 - CODE TABLE 3.1

   igdtlen=size(jgdt)

   if (debug) then
      write(logunit, *) 'listsec0, listsec1: ', listsec0, listsec1
      write(logunit, *) 'igdtnum, igdtlen: ', igdtnum, igdtlen
      write(logunit, *) 'jgdt: ', jgdt
      write(logunit, *) 'igds: ', igds
      write(logunit, *) 'dij: ', dij
      write(logunit, *) 'max_bytes: ', max_bytes
      write(logunit, *) 'forcast time: ', fortime
      write(logunit, *) 'refference time: ', ref_time
   end if

   ideflist=0
   idefnum=0
   
   do lyr=1,nlay

    do n=1,nflds

     allocate(cgrib(max_bytes))

     listsec0(1) = gcf(n)%var_g1

     call gribcreate(cgrib, max_bytes, listsec0, listsec1, ierr) 
     if (ierr /= 0) then
        write(0, *) 'Error initializing GRIB2 message', ierr
        return
     end if

     if (debug) write(logunit, *) 'n, nflds, npt, lay: ', n, nflds, npt, lyr, gcf(n)%discription_gb2, gcf(n)%var_fillvalue

     call addgrid(cgrib, max_bytes, igds, jgdt, igdtlen, ideflist, idefnum, ierr) ! there is an internal error here 
     if (ierr /= 0) then
         write(0, *) 'Error adding grid to GRIB2 message', ierr
         return
     end if

!        Create Section 4 parametrs    
     ipdtnum=0


     jpdt(1)=gcf(n)%var_g5   ! parm number catagory
     jpdt(2)=gcf(n)%var_g6   ! parm number
     jpdt(3)=2               ! (0-analysis, 1-initialazation, 2-forecast, .. GRIB2 - CODE TABLE 4.3 )
     jpdt(4)=0               !  
     jpdt(5)=96              ! Code ON388 Table A- GFS
     jpdt(6)=0               !    
     jpdt(7)=0               ! 
     jpdt(8)=1               ! unit (Hour=1)    6hour=11     (ask later) Table 4.4
     jpdt(9)=fortime         ! forecast hour
     jpdt(10)=gcf(n)%var_g7  ! level ID (1-Ground or Water Surface, 101 mean sea level, 160 depth bellow mean sea level , 168-Ocean Model Layer,...)
     jpdt(11)=0              ! scale factor
     jpdt(12)=dep(lyr)       ! scale value
     jpdt(13)=255
     jpdt(14)=0
     jpdt(15)=0

     if (debug) write(logunit, *) 'ipdtnum=', ipdtnum, ', jpdt= ', jpdt(1:15)

     ipdtlen=size(jpdt)

     numcoord=0
     coordlist=0.  ! needed for hybrid vertical coordinate

     ibmap=255     ! Bitmap indicator ( see Code Table 6.0 ) -255 no bitmap
     bmp=.true.

     if (trim(gcf(n)%name_gb2) .eq. 'WTMP') then
        where ( field(:,lyr,n) .ne. vfill ) field(:,lyr,n) = field(:,lyr,n) + 273.15
     endif

     ! Assign Template 5

     idrtnum = 0                            ! Template 5.2 (Grid Point Data - complex Packing)

     idrtmpl(:)=0
     ! Populate idrtmpl
     idrtmpl(1) = 0             ! Reference value (scaled value of the minimum data point)
     idrtmpl(2) = 0             ! Binary scale factor (scale by 2^E)
     idrtmpl(3) = 3             ! Decimal scale factor (scale by 10^D)
     idrtmpl(4) = 0             !
     idrtmpl(5) = 0             ! 
     idrtmpl(6) = 0             ! 
     ! Reserved fields
     idrtmpl(7:16) = 0          ! Reserved for future use 

     idrtlen=size(idrtmpl)

     tmpfld=0
     tmpfld =field(:,lyr,n)


     call addfield(cgrib, max_bytes, ipdtnum, jpdt, ipdtlen, coordlist, numcoord, &
     idrtnum, idrtmpl, idrtlen, tmpfld, npt, ibmap, bmp, ierr)
     if (ierr /=- 0) then
         write(0, *) 'Error adding field to GRIB2 message', ierr
         return
     end if

     call gribend(cgrib, max_bytes, lengrib, ierr)
     if (debug) write(logunit, *) 'gribend status=', ierr
     if (debug) write(logunit, *) 'length of the final GRIB2 message in octets =', lengrib
     call wryte(lunout, lengrib, cgrib)

     deallocate(cgrib)
  
    end do
   end do

   call baclose(lunout, ierr)

   return

end subroutine write_grib2_3d


!--------------------------------------------------------------------------------------
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!To get a lun used for bacio!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !--------------------------------------------------------------------------------------
  subroutine getlun(lun)
   integer, intent(out) :: lun
   logical :: is_open
   lun = 50  
   do
       inquire(unit=lun, opened=is_open)
       if (.not. is_open) then
           return  
       else
           lun = lun + 1
       end if
   end do
  end subroutine getlun


  !--------------------------------------------------------------------------------------
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!Retrieve Time From Input File!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !--------------------------------------------------------------------------------------

  subroutine retrieve_time(forecast_hour, ref_time)

   implicit none

   integer, intent(out) :: forecast_hour                  ! Forecast hour as an integer
   integer, dimension(6), intent(out) :: ref_time         ! Array for GRIB2 reference time: [year, month, day, hour, minute, second]

   integer :: ncid, time_varid, T1_varid, T2_varid
   character(len=30) :: units_str
   double precision :: T1, T2

   integer :: ref_year, ref_month, ref_day, ref_hour, ref_min, ref_sec
   integer :: year, month, day, hour
   double precision :: hours_offset

   call nf90_err(nf90_open(trim(input_file), nf90_nowrite, ncid), 'opening '//input_file)
   call nf90_err(nf90_inq_varid(ncid, 'time', time_varid), 'get variable ID: time')
   call nf90_err(nf90_get_var(ncid, time_varid, forecast_hour), 'get variable time')
   call nf90_err(nf90_get_att(ncid, time_varid, 'units', units_str), 'get attribute: units')

   if (trim(ftype) == 'ocean') then
      read(units_str(13:30), '(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)') &          
       ref_year, ref_month, ref_day, ref_hour, ref_min, ref_sec
   else
      read(units_str(12:29), '(I4,1X,I2,1X,I2,1X,I2,1X,I2,1X,I2)') &       ! remove it once ice time unit changed
      ref_year, ref_month, ref_day, ref_hour, ref_min, ref_sec
     forecast_hour=24*forecast_hour ! remove it once ice time unit changed)
   end if

   ref_time(1) = ref_year
   ref_time(2) = ref_month
   ref_time(3) = ref_day
   ref_time(4) = ref_hour
   ref_time(5) = ref_min
   ref_time(6) = ref_sec

  end subroutine retrieve_time


  !----------------------------------------------------------
  ! handle netcdf errors
  !----------------------------------------------------------
  subroutine nf90_err(ierr, string)

    integer ,         intent(in) :: ierr
    character(len=*), intent(in) :: string
    if (ierr /= nf90_noerr) then
      write(0, '(a)') 'FATAL ERROR: ' // trim(string)// ' : ' // trim(nf90_strerror(ierr))
      ! This fails on WCOSS2 with Intel 19 compiler. See
      ! https://community.intel.com/t5/Intel-Fortran-Compiler/STOP-and-ERROR-STOP-with-variable-stop-codes/m-p/1182521#M149254
      ! When WCOSS2 moves to Intel 2020+, uncomment the next line and remove stop 99
      !stop ierr
      stop 99
    end if
  end subroutine nf90_err
  
end module utils_mod
