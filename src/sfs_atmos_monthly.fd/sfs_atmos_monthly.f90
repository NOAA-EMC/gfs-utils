program sfs_atmos_monthly
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -+ + + ++
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! MAIN PROGRAM:  sfs_atmos_monthly    calculate monthly mean.
!   PRGMMR: K. ASMAR        ORG: NCEP/EMC     DATE: 05-08-2025
!
! ABSTRACT: This program takes as input 31 grib2 files of SFS
!            post-processed atmospheric products at 24-hrly
!            forcasts and calculates the monthly mean.
!
! PROGRAM HISTORY LOG:
!   05-08-25  K. Asmar      Origination and implement at NCEP.
!
! USAGE:
!**   Interface.
!     ----------
!       The program runs independently and must run after the
!       completion of all individual POST runs 
!
!     Input Files: 
!     ------------
!     master grib2: Post-processed atmospheric products at 24 hour intervals
!
!     Output File: 
!     -------------
!     outfile: grib2 file containing monthly mean for atmospheric products
!
!     Method.
!     -------
!       Monthly mean. Mean per gridpoint at each message across 31 days.
!
!     Programs called.
!     ----------
!       baopenwt
! 	baopenr
!       skgb
!       baread
!       gb_info
!       gf_getfld
!	getgb2
!       gf_free
!	gribcreate
!	addgrid
!	addfield
!	gribend
!	wryte
!	baclose
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!
! - - - - - - - - - - - - - 
  use grib_mod
  use params

  implicit none 

!*********************************************************************
  integer :: ierr,k,n,m,i,lu
  integer :: lengrib_in,lengrib_out
  integer :: iseek,lskip,lgrib,currlen=0
  integer :: numfields,numlocal,maxlocal
  integer :: nfiles,npts,icount,fcount
  integer(4) :: max_bytes,igds(5)
  integer :: yy,mm,dd,fhr
  integer :: listsec0(3),listsec1(13)
  integer, parameter :: mseek=32000
  real :: spval=9.99e20  
  integer :: ipdtmpl(29),idrstmpl(18)
  integer,dimension(200) :: jids,jpdt,jgdt
  integer :: jpdtn,jgdtn
  integer :: ibin_scl,idec_scl,inumbits
  real(8),allocatable :: datavalues(:,:),means(:,:),&
  			dummy(:),fout(:)
  type(gribfield) :: gfld,gfld2
  character(len=1),allocatable,dimension(:) :: cgrib_in,cgrib_out
  character(len=255),dimension(50) :: ingrib   
  character*255 :: outfile
  namelist /namens/ ingrib,outfile,nfiles
!**********************************************************************
  read(5,namens) 

  call baopenwt(91,outfile,ierr)  ! open output grib2 file
        if(ierr.ne.0) then
          print *,'error writing file ',outfile
          call abort
        endif

! I. open all files and store in unit value
  do n=1,nfiles
    call baopenr(n+10,ingrib(n),ierr)  
        if(ierr.ne.0) then
          print *,'error opening file ',ingrib(n)
          call abort
        endif
  end do

! II. open only first file to get each message/field
  ! Unpack GRIB2 fields
  iseek=0  ! no. of bytes to skip before search of message, for skgb routine
  icount=0 ! count no. of each grib2 message, starts at lskip+1
  fcount=0 ! count no. of each grib2 field (some messages have more than one)
  do  ! start grib message unpacking for input file
    call skgb(11,iseek,mseek,lskip,lgrib)  ! w3 routine to search for next grib message
         if (lgrib==0) exit    ! end loop at EOF or problem
         if (lgrib>currlen) then
            if (allocated(cgrib_in)) deallocate(cgrib_in)
            allocate(cgrib_in(lgrib),stat=ierr)
            currlen=lgrib
         endif
       
    call baread(11,lskip,lgrib,lengrib_in,cgrib_in)
    iseek=lskip+lgrib  ! for next message in loop
    icount=icount+1      
    
    ! unpack the icount grib2 message 
    call gb_info(cgrib_in,lengrib_in,listsec0,listsec1,&
                  numfields,numlocal,maxlocal,ierr)

    do m=1,numfields
      fcount=fcount+1
      call gf_getfld(cgrib_in,lengrib_in,m,.true.,.true.,gfld,ierr)

      ! only do this once: get no. of grid points and allocate means array
      if((icount==1).and.(m==1))then
      	npts=gfld%ngrdpts ! do this once
        allocate(means(5000,npts))  ! enough rows to get all fields
      endif
      
! III. loop through ALL files to get this same message field
! 1)file no.,2)data values at grid points
      allocate(datavalues(nfiles,npts)) 
      do n=1,nfiles
        lu=n+10
	! set field identifiers, fhr will be different for each file
          jids= -9999
          jids(6)=listsec1(6) ! year
          jids(7)=listsec1(7) ! mon
          jids(8)=listsec1(8) ! day
          jpdtn=gfld%ipdtnum
          jpdt=-9999
          jpdt(1)=gfld%ipdtmpl(1) ! cat number
          jpdt(2)=gfld%ipdtmpl(2) ! parm number
          jpdt(3)=gfld%ipdtmpl(3) ! (0-analysis, 1-forecast, or 2-analysis error)
          jpdt(10)=gfld%ipdtmpl(10) ! level ID
          jpdt(12)=gfld%ipdtmpl(12) ! level value
          jgdtn=gfld%igdtnum
          jgdt=-9999
          jgdt(8)=gfld%igdtmpl(8) ! im
          jgdt(9)=gfld%igdtmpl(9) ! jm
          call getgb2(lu,0,0,listsec0(1),jids,jpdtn,jpdt, &
               jgdtn,jgdt,.true.,k,gfld2,ierr)

! store the datavalues, indexed at the file no.
          do k=1,npts
  	    if (gfld2%fld(k) == spval) then
    	      datavalues(n,k) = 0.0  ! replace missing values with 0 for getting means
  	    else
    	      datavalues(n,k)=gfld2%fld(k)
  	    end if
	  end do
          call gf_free(gfld2)
        end do  ! end nfiles loop
        call gf_free(gfld)

! IV. get the mean for each message field across all files
	do k=1,npts
	  allocate(dummy(nfiles))
	  do i=1,nfiles
	    dummy(i)=datavalues(i,k)
	  end do
	  means(fcount,k)=sum(dummy)/nfiles
	  deallocate(dummy)
	end do  	
	deallocate(datavalues)

      end do  ! end m loop for numfields
    end do  ! end of grib message unpacking for input file

! ********************** WRITE OUTPUT ************************************************
! open and write only last file

  ! Unpack GRIB2 fields
  iseek=0  ! no. of bytes to skip before search of message, for skgb routine
  icount=0 ! count no. of each grib2 message, starts at lskip+1
  fcount=0 ! count no. of each grib2 field (some messages have more than one)
  do  ! start grib message unpacking for input file
    call skgb(nfiles+10,iseek,mseek,lskip,lgrib)  ! w3 routine to search for next grib message
         if (lgrib==0) exit    ! end loop at EOF or problem
         if (lgrib>currlen) then
            if (allocated(cgrib_in)) deallocate(cgrib_in)
            allocate(cgrib_in(lgrib),stat=ierr)
            currlen=lgrib
         endif
       
    call baread(nfiles+10,lskip,lgrib,lengrib_in,cgrib_in)
    iseek=lskip+lgrib  ! for next message in loop
    icount=icount+1      
    
    ! unpack the icount grib2 message 
    call gb_info(cgrib_in,lengrib_in,listsec0,listsec1,&
                  numfields,numlocal,maxlocal,ierr)

    do m=1,numfields
      fcount=fcount+1
      call gf_getfld(cgrib_in,lengrib_in,m,.true.,.true.,gfld,ierr)
        if (gfld%ipdtnum==0) fhr=gfld%ipdtmpl(9)
 
        max_bytes=gfld%ngrdpts*4
        allocate(cgrib_out(max_bytes))
        call gribcreate(cgrib_out,max_bytes,listsec0,listsec1,ierr)
            if (ierr.ne.0)print*,'error with gribcreate'
      
        igds(1)=gfld%griddef
      	igds(2)=gfld%ngrdpts
      	igds(3)=gfld%numoct_opt 
      	igds(4)=gfld%interp_opt
      	igds(5)=gfld%igdtnum
       
      	call addgrid(cgrib_out,max_bytes,igds,gfld%igdtmpl,gfld%igdtlen,0,1,ierr)
        	if (ierr.ne.0)print*,'error with addgrid'

! Apply Product Template 4.8 for monthly mean
        ipdtmpl(1) = gfld%ipdtmpl(1)              ! Parameter category (see Code table 4.1)
  	ipdtmpl(2) = gfld%ipdtmpl(2)              ! Parameter number (see Code table 4.2)
  	ipdtmpl(3) = gfld%ipdtmpl(3)              ! Type of generating process (see Code table 4.3)
  	ipdtmpl(4) = gfld%ipdtmpl(4)              ! Background generating process identifier
  	ipdtmpl(5) = gfld%ipdtmpl(5)              ! Analysis or forecast generating process identified (see Code ON388 Table A)
  	ipdtmpl(6) = gfld%ipdtmpl(6)              ! Hours of observational data cutoff after reference time (see Note)
  	ipdtmpl(7) = gfld%ipdtmpl(7)              ! Minutes of observational data cutoff after reference time (see Note)
  	ipdtmpl(8) = 3                        ! Indicator of unit of time range (see Code table 4.4)

  	! Establish 0-1, 1-2, 2-3. or 3-4 monthy ave based on fhr of this last file
  	! ipdtmpl(9) -- Forecast time in units defined by octet 18
 	if (fhr==744) then
    	   ipdtmpl(9)=0
  	else if (fhr==1488) then
    	   ipdtmpl(9)=1
  	else if (fhr==2232) then
    	   ipdtmpl(9)=2
  	else if (fhr==2976) then
    	   ipdtmpl(9)=3
  	else
    	   ipdtmpl(9)=100   !!! TODO need a better way to deal with wrong files
   	print*,'fhr not found'
  	endif

      	yy = listsec1(6)                  
      	mm = listsec1(7)                 
      	dd = listsec1(8)  
  
  	ipdtmpl(10) = gfld%ipdtmpl(10)            ! Type of first fixed surface (see Code table 4.5)
  	ipdtmpl(11) = gfld%ipdtmpl(11)            ! Scale factor of first fixed surface
  	ipdtmpl(12) = gfld%ipdtmpl(12)            ! Scaled value of first fixed surface
  	ipdtmpl(13) = gfld%ipdtmpl(13)            ! Type of second fixed surfaced (see Code table 4.5)
  	ipdtmpl(14) = gfld%ipdtmpl(14)            ! Scale factor of second fixed surface
  	ipdtmpl(15) = gfld%ipdtmpl(15)            ! Scaled value of second fixed surfaces
  	ipdtmpl(16) = yy                ! Year  - Time of end of overall time interval 
  	ipdtmpl(17) = mm                 ! Month  - Time of end of overall time interval
  	ipdtmpl(18) = dd                 ! Day  - Time of end of overall time interval
  	ipdtmpl(19) = 0                           ! Hour  - Time of end of overall time interval
  	ipdtmpl(20) = 0                           ! Minute  - Time of end of overall time interval
  	ipdtmpl(21) = 0                           ! Second  - Time of end of overall time interval
  	ipdtmpl(22) = 1                           ! n - number of time ranges specifications describing the time intervals used to calculate the statistically-processed field
        ipdtmpl(23) = 0                           ! Total number of data values missing in statistical process
  	ipdtmpl(24) = 0                           ! Statistical process used to calculate the processed field from the field at each time increment during the time range (see Code Table 4.10)
  	ipdtmpl(25) = 2                           ! Type of time increment between successive fields used in the statistical processing (see Code Table 4.11)
  	ipdtmpl(26) = 3                           ! Indicator of unit of time for time range over which statistical processing is done (see Code Table 4.4)
  	ipdtmpl(27) = 1                           ! Length of the time range over which statistical processing is done, in units defined by the previous octet 
  	ipdtmpl(28) = 255                           ! Indicator of unit of time for the increment between the successive fields used (see Code Table 4.4)
  	ipdtmpl(29) = 0                          ! Time increment between successive fields, in units defined by the previous octet (see Notes 3 and 4) 

	allocate(fout(npts))
	fout=real(means(fcount,:))

	if (gfld%idrtnum==3) then ! complex packing and spatial differencing
	  idrstmpl(:)=0
	  if(gfld%idrtmpl(3)>6)then  ! large decimal scale factor when needed
	    idrstmpl(3)=gfld%idrtmpl(3)
	  else
            idrstmpl(3)=3
	  endif
	  idrstmpl(17)=2  ! second-order differencing
          call addfield (cgrib_out,max_bytes,8,ipdtmpl, &
                    29,0.0,0, &
                    3 ,idrstmpl,18, &   
                    fout,npts,gfld%ibmap,gfld%bmap,ierr)		
             if(ierr.ne.0)print*,'error with addfield'
	else  ! simple packing
          call addfield (cgrib_out,max_bytes,8,ipdtmpl, &
                    29,0.0,0, &
                    gfld%idrtnum,gfld%idrtmpl,gfld%idrtlen, &   
                    fout,npts,gfld%ibmap,gfld%bmap,ierr)		
             if(ierr.ne.0)print*,'error with addfield'
	endif  ! complex/simple packing if block

        call gribend(cgrib_out,max_bytes,lengrib_out,ierr)
             if(ierr.ne.0)print*,'error with gribend'
	call wryte(91,lengrib_out,cgrib_out)
       	deallocate(cgrib_out)
	deallocate(fout)
        call gf_free(gfld)
      end do  ! end m loop for numfields
    end do  ! end of grib message unpacking for input file

! close all files
  do n=1,nfiles
    call baclose(n+10,ierr)
  end do
  call baclose(91, ierr)

  stop
end program sfs_atmos_monthly




