module utils_mod

  use netcdf
  use outputvars, only : vardefs

  implicit none

  private

  logical, public :: debug
  integer, public :: logunit

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
               trim(vars(n)%input_var_name), trim(vars(n)%var_grid(1:2)), &
               trim(vars(n)%var_pair), trim(vars(n)%var_pair_grid(1:2)),  &
               dims=(/dims(1),dims(2)/), vecpair=vecpair)
       end if
    end do

    ! create packed array
    nn = 0
    do n = 1,nflds
       if (len_trim(vars(n)%var_pair) == 0) then
          nn = nn + 1
          call getfield(trim(filesrc), trim(vars(n)%input_var_name), dims=(/dims(1),dims(2)/), &
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
               trim(vars(n)%input_var_name), trim(vars(n)%var_grid),    &
               trim(vars(n)%var_pair), trim(vars(n)%var_pair_grid),     &
               dims=(/dims(1),dims(2),dims(3)/), vecpair=vecpair)
       end if
    end do

    ! create packed array
    nn = 0
    do n = 1,nflds
       if (len_trim(vars(n)%var_pair) == 0) then
          nn = nn + 1
          call getfield(trim(filesrc), trim(vars(n)%input_var_name), dims=(/dims(1),dims(2),dims(3)/), &
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

    wgtsfile = trim(wdir)//'tripole.mx025.'//vgrid1//'.to.Ct.bilinear.nc'
    call getfield(fname, vname1, dims=dims, field=vecpair(:,1), wgts=trim(wgtsfile))
    if (debug)write(logunit,'(a)')'wgtsfile for 2d vector '//trim(vname1)//'   '//trim(wgtsfile)
    wgtsfile = trim(wdir)//'tripole.mx025.'//vgrid2//'.to.Ct.bilinear.nc'
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
    character(len=20) :: subname = 'getfield3d'

    if (debug)write(logunit,'(a)')'enter '//trim(subname)

    wgtsfile = trim(wdir)//'tripole.mx025.'//vgrid1//'.to.Ct.bilinear.nc'
    call getfield(fname, vname1, dims=dims, field=vecpair(:,:,1), wgts=trim(wgtsfile))
    wgtsfile = trim(wdir)//'tripole.mx025.'//vgrid2//'.to.Ct.bilinear.nc'
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

    rc = nf90_open(fname, nf90_nowrite, ncid)
    call handle_err(rc,' nf90_open '//fname)
    rc = nf90_inq_varid(ncid, vname, varid)
    call handle_err(rc,' get variable ID '// vname)
    rc = nf90_get_var(ncid, varid, a2d)
    call handle_err(rc,' get variable'// vname)
    rc = nf90_get_att(ncid, varid, '_FillValue', fval)
    rc = nf90_close(ncid)

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

    rc = nf90_open(fname, nf90_nowrite, ncid)
    call handle_err(rc,' nf90_open '//fname)
    rc = nf90_inq_varid(ncid, vname, varid)
    call handle_err(rc,' get variable ID '// vname)
    rc = nf90_get_var(ncid, varid, a3d)
    call handle_err(rc,' get variable'// vname)
    rc = nf90_get_att(ncid, varid, '_FillValue', fval)
    rc = nf90_close(ncid)

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
    rc = nf90_open(trim(fname), nf90_nowrite, ncid)
    call handle_err(rc,' nf90_open '//fname)
    rc = nf90_inq_dimid(ncid, 'n_s', id)
    rc = nf90_inquire_dimension(ncid, id, len=n_s)
    rc = nf90_inq_dimid(ncid, 'n_a', id)
    rc = nf90_inquire_dimension(ncid, id, len=n_a)
    rc = nf90_inq_dimid(ncid, 'n_b', id)
    rc = nf90_inquire_dimension(ncid, id, len=n_b)

    allocate(col(1:n_s)); col = 0
    allocate(row(1:n_s)); row = 0
    allocate(  S(1:n_s)); S = 0.0

    rc = nf90_inq_varid(ncid, 'col', id)
    rc = nf90_get_var(ncid,     id, col)
    rc = nf90_inq_varid(ncid, 'row', id)
    rc = nf90_get_var(ncid,     id, row)
    rc = nf90_inq_varid(ncid,   'S', id)
    rc = nf90_get_var(ncid,      id,  S)
    rc = nf90_close(ncid)

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
    rc = nf90_open(trim(fname), nf90_nowrite, ncid)
    call handle_err(rc,' nf90_open '//fname)
    rc = nf90_inq_dimid(ncid, 'n_s', id)
    rc = nf90_inquire_dimension(ncid, id, len=n_s)
    rc = nf90_inq_dimid(ncid, 'n_a', id)
    rc = nf90_inquire_dimension(ncid, id, len=n_a)
    rc = nf90_inq_dimid(ncid, 'n_b', id)
    rc = nf90_inquire_dimension(ncid, id, len=n_b)

    allocate(col(1:n_s)); col = 0
    allocate(row(1:n_s)); row = 0
    allocate(  S(1:n_s)); S = 0.0

    rc = nf90_inq_varid(ncid, 'col', id)
    rc = nf90_get_var(ncid,     id, col)
    rc = nf90_inq_varid(ncid, 'row', id)
    rc = nf90_get_var(ncid,     id, row)
    rc = nf90_inq_varid(ncid,   'S', id)
    rc = nf90_get_var(ncid,      id,  S)
    rc = nf90_close(ncid)

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
    rc = nf90_open(trim(fname), nf90_nowrite, ncid)
    call handle_err(rc,' nf90_open '//fname)
    rc = nf90_inq_dimid(ncid, 'n_s', id)
    rc = nf90_inquire_dimension(ncid, id, len=n_s)
    rc = nf90_inq_dimid(ncid, 'n_a', id)
    rc = nf90_inquire_dimension(ncid, id, len=n_a)
    rc = nf90_inq_dimid(ncid, 'n_b', id)
    rc = nf90_inquire_dimension(ncid, id, len=n_b)

    allocate(col(1:n_s)); col = 0
    allocate(row(1:n_s)); row = 0
    allocate(  S(1:n_s)); S = 0.0

    rc = nf90_inq_varid(ncid, 'col', id)
    rc = nf90_get_var(ncid,     id, col)
    rc = nf90_inq_varid(ncid, 'row', id)
    rc = nf90_get_var(ncid,     id, row)
    rc = nf90_inq_varid(ncid,   'S', id)
    rc = nf90_get_var(ncid,      id,  S)
    rc = nf90_close(ncid)

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

    rc = nf90_create(trim(fname), nf90_clobber, ncid)
    rc = nf90_def_dim(ncid, 'nx', dims(1), idimid)
    rc = nf90_def_dim(ncid, 'ny', dims(2), jdimid)
    rc = nf90_def_dim(ncid, 'nf', nflds,   fdimid)
    rc = nf90_def_var(ncid, vname, nf90_float, (/idimid,jdimid,fdimid/), varid)
    rc = nf90_enddef(ncid)

    a3d(:,:,:) =  reshape(field(1:dims(1)*dims(2),1:nflds), (/dims(1),dims(2),nflds/))
    rc = nf90_put_var(ncid, varid, a3d)
    rc = nf90_close(ncid)

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

    rc = nf90_create(trim(fname), nf90_clobber, ncid)
    rc = nf90_def_dim(ncid, 'nx', dims(1), idimid)
    rc = nf90_def_dim(ncid, 'ny', dims(2), jdimid)
    rc = nf90_def_dim(ncid, 'nk', dims(3), kdimid)
    rc = nf90_def_dim(ncid, 'nf', nflds,   fdimid)
    rc = nf90_def_var(ncid, vname, nf90_float, (/idimid,jdimid,kdimid,fdimid/), varid)
    rc = nf90_enddef(ncid)

    do n = 1,nflds
       a4d(:,:,:,n) = reshape(field(1:dims(1)*dims(2),1:dims(3),n), (/dims(1),dims(2),dims(3)/))
    end do
    rc = nf90_put_var(ncid, varid, a4d)
    rc = nf90_close(ncid)

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
    integer :: n, ncid, varid, rc, idimid, jdimid, kdimid
    real, allocatable :: a3d(:,:,:)
    character(len=20) :: subname = 'dumpnc3dk'

    if (debug)write(logunit,'(a)')'enter '//trim(subname)//' variable '//vname
    allocate(a3d(dims(1),dims(2),dims(3))); a3d = 0.0

    rc = nf90_create(trim(fname), nf90_clobber, ncid)
    rc = nf90_def_dim(ncid, 'nx', dims(1), idimid)
    rc = nf90_def_dim(ncid, 'ny', dims(2), jdimid)
    rc = nf90_def_dim(ncid, 'nk', dims(3), kdimid)
    rc = nf90_def_var(ncid, vname, nf90_float, (/idimid,jdimid,kdimid/), varid)
    rc = nf90_enddef(ncid)

    a3d(:,:,:) =  reshape(field(1:dims(1)*dims(2),1:dims(3)), (/dims(1),dims(2),dims(3)/))
    rc = nf90_put_var(ncid, varid, a3d)
    rc = nf90_close(ncid)

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
    integer           :: n, ncid, varid, rc, idimid, jdimid
    real, allocatable :: a2d(:,:)
    character(len=20) :: subname = 'dumpnc1d'

    if (debug)write(logunit,'(a)')'enter '//trim(subname)//' variable '//vname
    allocate(a2d(dims(1),dims(2))); a2d = 0.0

    rc = nf90_create(trim(fname), nf90_clobber, ncid)
    rc = nf90_def_dim(ncid, 'nx', dims(1), idimid)
    rc = nf90_def_dim(ncid, 'ny', dims(2), jdimid)
    rc = nf90_def_var(ncid, vname, nf90_float, (/idimid,jdimid/), varid)
    rc = nf90_enddef(ncid)

    a2d(:,:) =  reshape(field(1:dims(1)*dims(2)), (/dims(1),dims(2)/))
    rc = nf90_put_var(ncid, varid, a2d)
    rc = nf90_close(ncid)

    if (debug)write(logunit,'(a)')'exit '//trim(subname)//' variable '//vname

  end subroutine dumpnc1d

  !----------------------------------------------------------
  ! handle netcdf errors
  !----------------------------------------------------------
  subroutine handle_err(ierr,string)

    integer ,         intent(in) :: ierr
    character(len=*), intent(in) :: string
    if (ierr /= nf90_noerr) then
      write(logunit,'(a)') '*** ERROR ***: '//trim(string)//':'//trim(nf90_strerror(ierr))
      stop
    end if
  end subroutine handle_err
end module utils_mod
