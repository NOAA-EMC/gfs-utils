program gefs_6h_ave_1mem
! main program: gefs_6h_ave_1mem
! Author: Hong Guan :2018-12-17
! REF: Eric Sinsky, Wei Li, Yali Mao, Bo Cui
! Purpose: calculate accumulation or avrage (03Z,06Z)
! 
! usage: daily_ave_acc.exe yyyymmdd
!
!   input file: control f00 and f03 reanalysis data and ensemble forecast (master file)                                      
!   output file: accumulation

! programs called:
!   baopenr          grib i/o
!   baopenw          grib i/o
!   baclose          grib i/o
!   getgb2           grib reader
!   putgb2           grib writer
!   init_parm        define grid definition and product definition
!   printinfr        print grib2 data information
!   change_template4 change data values for specified Product Definition Template

! exit states:
!   cond =   0 - successful run
!   cond =   1 - I/O abort
!
! attributes:
!   language: fortran 90
!
!$$$

      use grib_mod
      use params
      implicit none

      type(gribfield) :: gfld,gfldo

!type pdt_t0 
!    integer :: npdt0   ! Product Definition Template Number
!    integer :: icat0   ! Parameter Category by Product Discipline 
!    integer :: iprm0   ! Parameter Number by Product Discipline and Parameter Category
!    integer :: igp0    ! Type of Generating Process
!    integer :: iffs0   ! Type of first fixed surface 
!    integer :: isf0    ! Scale factor of first fixed surface
!    integer :: isv0    !Scaled value of first fixed surface
!end type pdt_t0
!
!! PDT parameters in the input GRIB2 file (template 4 number, category, parameter, type of level)
!type(pdt_t0), parameter :: &
!in pgrb2a
!    pdt_hgt500   = pdt_t0(1, 3, 5, 4, 100, 0, 50000), &
!    pdt_hgt200   = pdt_t0(1, 3, 5, 4, 100, 0, 20000), &
!    pdt_ugrd200  = pdt_t0(1, 2, 2, 4, 100, 0, 20000), &
!    pdt_ugrd850  = pdt_t0(1, 2, 2, 4, 100, 0, 85000), &
!    pdt_vgrd200  = pdt_t0(1, 2, 3, 4, 100, 0, 20000), &
!    pdt_vgrd850  = pdt_t0(1, 2, 3, 4, 100, 0, 85000), &
!    pdt_t2m      = pdt_t0(1, 0, 0, 4, 103, 0, 2), &
!    pdt_apcp     = pdt_t0(11,1, 8, 4,   1, 0, 0), &
!    pdt_ulwrf_top= pdt_t0(11,5,193,4,   8, 0, 0), &
!now in pgrb2a
!    pdt_tsfc     = pdt_t0(1, 0, 0, 4, 1,   0, 0)

      integer :: nfield,nfield1
      parameter(nfield=42)
      parameter(nfield1=39)

      type pdt_t
          integer,dimension(nfield):: npdt  ! Product Definition Template Number
          integer,dimension(nfield):: icat  ! Parameter Category by Product Discipline
          integer,dimension(nfield):: iprm  ! Parameter Number by Product Discipline and Parameter Category
          integer,dimension(nfield):: igp   ! Type of Generating Process
          integer,dimension(nfield):: iffs  ! Type of first fixed surface 
          integer,dimension(nfield):: isf   ! Scale factor of first fixed surface
          integer,dimension(nfield):: isv   !Scaled value of first fixed surface
      end type pdt_t

      type(pdt_t) :: pdt

!      data pdt%npdt/   8,  8,  8,  8,  8,  8, 8, 8, 8, 8, 8,  8, 8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,  8, 8/
!      data pdt%icat/   5,  5,  4,  4,  4,  5, 2, 2, 0, 0, 1,  1,19,  6,  6,  6,  6,  6,  0,  3,  3,  4,  4,  0,  0,  3,  3,  0,  3,  3,  0,  3,  3,  0,  6, 0, 1,  0,  1,  1, 1/!Parameter Category by Product Discipline
!      data pdt%iprm/ 192,193,192,193,193,193,17,18,11,10, 7,196, 1,  1,  1,  5,  4,  3,193,194,195,194,195,  5,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,193,16,42,  5, 8, 10, 9/!ParameterNumberbyProductDiscipline and Parameter Category
!      data pdt%iffs/   1,  1,  1,  1,  8,  8, 1, 1, 1, 1, 1,  1, 1,200,211,234,224,214,  1,  1,  1,  1,  1,103,103,233,232,233,223,222,223,213,212,213,200, 1, 1,  1,  1,  1,  1/
!      integer,dimension(nfield)::disc
!      data disc/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0/


      data pdt%npdt/   8,  8,  8,  8,  8,  8, 8, 8, 8, 8, 8,  8, 8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8/
      data pdt%icat/   5,  5,  4,  4,  4,  5, 2, 2, 0, 0, 1,  1,19,  6,  6,  6,  6,  6,  0,  3,  3,  4,  4,  0,  0,  3,  3,  0,  3,  3,  0,  3,  3,  0,  6, 0, 1,  0, 1,  1, 1, 1/!Parameter Category by Product Discipline
      data pdt%iprm/ 192,193,192,193,193,193,17,18,11,10, 7,196, 1,  1,  1,  5,  4,  3,193,194,195,194,195,  5,  4,  0,  0,  0,  0,  0,  0,  0,  0,  0,193,16,42,  5, 50, 8, 10, 9/!ParameterNumberbyProductDiscipline and Parameter Category
      data pdt%iffs/   1,  1,  1,  1,  8,  8, 1, 1, 1, 1, 1,  1, 1,200,211,234,224,214,  1,  1,  1,  1,  1,103,103,233,232,233,223,222,223,213,212,213,200, 1, 1,  1, 1, 1,  1,  1/
      integer,dimension(nfield)::disc
      data disc/0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0/


      integer :: n_time
      parameter(n_time=3) !from fhr 00 to 06
      integer :: j,jpdtn,jgdtn
      integer,dimension(200) :: jids,jpdt,jgdt
      logical :: unpack=.true.
      integer :: jdisc      ! discipline#, table 0.0(met:0 hydro:1 land:2)
      integer :: day,month,year,hour,fhour
      character(len=256) :: datafile(n_time),outfile,outfile03,outfile006
      character(len=8) :: file_date
      integer :: unit, ifid,ifid1,nx,ny,iret,jret,i,k,ifh,nfi,maxgrd,ifd,ind,ens_id
      character(len=3) ::sfh
      character(len=2) ::sdy
      character(len=2) ::smonth
      character(len=4) ::syr
      character(len=8) :: pabbrev
      character(len=30) :: labbrev
      character(len=30) :: labbrev_short
      character(len=500) :: datapath = './'

      real, allocatable :: var_save(:,:) ! (maxgrd,nfi)
      real, allocatable :: var_save_acc(:) ! (maxgrd)
      real, allocatable :: var_save_DSWRF(:) ! (maxgrd)
      real, allocatable :: var_save_USWRF(:) ! (maxgrd)
      real, allocatable :: apcp_6h(:) ! (maxgrd)
      real, allocatable :: acpcp_6h(:) ! (maxgrd)
      real, allocatable :: apcp_3h(:) ! (maxgrd)
      real, allocatable :: acpcp_3h(:) ! (maxgrd)
      real, allocatable :: dpcp(:) ! (maxgrd)


      call GET_COMMAND_ARGUMENT(1, file_date)
!      call get_environment_variable("ens_mem", ens_mem)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!open files for output grib2 (f03 and f006)
       outfile03=trim(datapath)//'gefs'//'.t00z.'//'pgrb2af003'
       print *,outfile03
       call baopenwa(300,outfile03,iret) ! for add more than 1 members
       outfile006=trim(datapath)//'gefs'//'.t00z.'//'pgrb2af006'
       print *,outfile006
       call baopenwa(200,outfile006,iret) ! for add more than 1 members
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      unit=10
      do ifid=1,nfield1
      nfi=1
      do ifh=003,006,3  !foercast hours

       nfi=nfi+1
       unit=unit+1
       write(sfh,'(i3.3)') ifh

!search in pgrb2a

       datafile(nfi)=trim(datapath)//'gefs.t00z.master.grb2f'//trim(sfh)

       call BAOPENR(unit, datafile(nfi), iret)
       if(iret /= 0 ) then
        write(*,*) "there is no GEFS forecast",datafile(nfi)
       end if
       write(*,*) unit, datafile(nfi), iret

       j = 0
       jids=-9999;jpdt=-9999; jgdt=-9999
       jdisc=-1; jgdtn=-1
       jpdt(1)  = pdt%icat(ifid)
       jpdt(2)  = pdt%iprm(ifid)
       jpdt(10) = pdt%iffs(ifid)
       jdisc   = disc(ifid)

       if (nfi /= 3 ) then
          jpdtn = pdt%npdt(ifid)  !template version num. 
       else
          jpdtn =11  !template version num.
       endif

       call getgb2(unit,0,j,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,unpack,j,gfld,iret)
       if (iret /= 0) then
        write(*,*) "reading file iret=", iret,"j=",j,"nfi=",nfi
        stop
       end if
       call baclose(unit, iret)
       write(*,*) 'after getgb2, iret=', iret

! assign gfldo same arribute for gfld
         
       maxgrd=gfld%ngrdpts
       print*,maxgrd
       nx = gfld%igdtmpl(8)
       ny = gfld%igdtmpl(9)
! assign gfldo same arribute for gfld

       gfldo=gfld

       print*,nfi,' ',ifh
       print*,maxgrd,' ',maxgrd

      if(.not. allocated(apcp_3h)) allocate(apcp_3h(maxgrd))
      if(.not. allocated(acpcp_3h)) allocate(acpcp_3h(maxgrd))
      if(.not. allocated(apcp_6h)) allocate(apcp_6h(maxgrd))
      if(.not. allocated(acpcp_6h)) allocate(acpcp_6h(maxgrd))
      if(.not. allocated(dpcp)) allocate(dpcp(maxgrd))
      if(.not. allocated(var_save)) allocate(var_save(maxgrd,3))

       var_save(:,nfi) = gfld%fld
       year  = gfld%idsect(6)
       month = gfld%idsect(7)
       day   = gfld%idsect(8)
       hour  = gfld%idsect(9)

       write(syr,'(i4.4)') year
       write(smonth,'(i2.2)') month
       write(sdy,'(i2.2)') day

!- forecast hour
       fhour=gfld%ipdtmpl(9)
       ens_id=gfld%ipdtmpl(17)
       pabbrev=param_get_abbrev(gfld%discipline,gfld%ipdtmpl(1),gfld%ipdtmpl(2))
       call prlevel(gfld%ipdtnum,gfld%ipdtmpl,labbrev)
       if ( nfi /= 3) then
       call gf_free(gfld)
       endif
       
  201 end do !ifh

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      labbrev_short=trim(labbrev)
      print *,pabbrev,trim(labbrev_short(1:4))

      if(.not. allocated(var_save_acc)) allocate(var_save_acc(maxgrd))
      if(.not. allocated(var_save_USWRF)) allocate(var_save_USWRF(maxgrd))
      if(.not. allocated(var_save_DSWRF)) allocate(var_save_DSWRF(maxgrd))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!calculate the 00-6h ave/acc
       if(pabbrev=='PRATE' .or. pabbrev=='CPRAT') then
            var_save_acc(:)=(var_save(:,2)+var_save(:,3)*6./3.)/2.

            if(pabbrev=='PRATE') then
               apcp_6h(:)=var_save_acc(:)*6.*3600.
            else
               acpcp_6h(:)=var_save_acc(:)*6.*3600.
            endif
            print*,'For 0-6h precip. and precip. rate'
            print*,''
            
        else
!         var_save_acc(:)=(var_save(:,2)+var_save(:,3))/2.
!        rate for 6, 12, 18,... hours is accumulated values/(current time - diagnosis time)

         var_save_acc(:)=(var_save(:,2)+var_save(:,3)*6./3.)/2.

         if (pabbrev=='DSWRF' ) then
                 var_save_DSWRF(:)=var_save_acc(:)
         endif
         if (ifid==4 ) then
                 var_save_USWRF(:)=var_save_acc(:)
         endif

         if (pabbrev=='PRES' .or. pabbrev=='TMP') then
           do i=1,maxgrd
           if (var_save(i,2).eq.0.0.and.var_save(i,3).eq.0.0) var_save_acc(i)=var_save(i,3)
           if (var_save(i,2).eq.0.0.and.var_save(i,3).gt.0.0) var_save_acc(i)=var_save(i,3)
           if (var_save(i,2).gt.0.0.and.var_save(i,3).eq.0.0) var_save_acc(i)=var_save(i,2)
           if (var_save(i,2).gt.0.0.and.var_save(i,3).gt.0.0) var_save_acc(i)=(var_save(i,2)+var_save(i,3))/2.
           enddo
         endif

         if (pabbrev=='ALBDO') then
           do i=1,maxgrd
           var_save_acc(i)=0.0
           if (var_save_DSWRF(i) .gt. 0.01 .and. var_save_USWRF(i).gt.0.01) then
                var_save_acc(i)=100.*var_save_USWRF(i)/var_save_DSWRF(i)
                if(var_save_acc(i) .gt. 100) var_save_acc(i)=0.0
           endif
           enddo
         endif

        if(pabbrev=='WATR' .or. pabbrev=='TSNOWP') then
           do i=1,maxgrd
                 var_save_acc(i)=var_save(i,2)+var_save(i,3)
           enddo
        endif

        if(pabbrev=='TMIN') then
         do i =1,maxgrd
         if(var_save(i,3).gt.var_save(i,2)) then
            var_save_acc(i)=var_save(i,2)
         else
            var_save_acc(i)=var_save(i,3)
         endif
         enddo
        endif

        if(pabbrev=='TMAX') then
         do i =1,maxgrd
          if(var_save(i,3).gt.var_save(i,2)) then
            var_save_acc(i)=var_save(i,3)
          else
            var_save_acc(i)=var_save(i,2)
          endif
          enddo
       endif
       endif

!output 6h variables

        gfld%fld=var_save_acc(:)
       gfld%ipdtmpl(22)=6 !forecast time
       gfld%ipdtmpl(30)=6 !forecast time
       jret=0
       
!       call putgb2(200,gfldo,jret)
       call putgb2(200,gfld,jret)
       write(*,*) '200 put',jret,gfld%discipline,gfld%ipdtmpl

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       write(*,'(a10,a20,4f15.2)') pabbrev,trim(labbrev),var_save(100,2),var_save(100,2),var_save(100,3),var_save_acc(100)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!calculate the 3h ave/acc
!3h accumulation 00-03z
       if(pabbrev=='PRATE' .or. pabbrev=='CPRAT') then

        var_save_acc(:)=var_save(:,2)

        print*,'For 0-3h precip. rate'
        print*,''

        if(pabbrev=='PRATE') then
           apcp_3h(:)=var_save(:,2)*3600.*3
        else
           acpcp_3h(:)=var_save(:,2)*3600.*3
        endif
        print*,'For 0-3h precip.'
        print*,''

       else
       
        var_save_acc(:)=var_save(:,2)

       end if

!output 3-h variables

       gfld%fld=var_save_acc(:)
!reassign the ipdtmpl
       gfld%ipdtmpl(9)=0 !forecast time
       gfld%ipdtmpl(22)=3 !forecast time
       gfld%ipdtmpl(30)=3 !forecast time
       jret=0
       call putgb2(300,gfld,jret)

       write(*,*) '300 put',jret,gfldo%ipdtmpl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      call gf_free(gfldo)

      deallocate(var_save_acc)
      deallocate(var_save)
       if(ifid.eq.38) then
          deallocate(var_save_DSWRF)
          deallocate(var_save_USWRF)
      endif
      enddo


!      output accumulated precipitations

       unit=100
       call BAOPENR(unit, datafile(3), iret)
       if(iret /= 0 ) then
        write(*,*) "there is no GEFS forecast",datafile(nfi)
       end if

      do ifid=40,42
       j = 0
       jids=-9999
       jids=-9999;jpdt=-9999; jgdt=-9999
       jdisc=-1; jgdtn=-1

       jpdtn = pdt%npdt(ifid)  !template version num.
       jpdtn =11 !template version num.
       jpdt(1)  = pdt%icat(ifid)
       jpdt(2)  = pdt%iprm(ifid)
       jpdt(10) = pdt%iffs(ifid)
!
        write(*,*) unit,datafile(3),jpdt(1),jpdt(2),jpdt(10)
       call getgb2(unit,0,j,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,unpack,j,gfld,iret)
       if (iret /= 0) then
        write(*,*) "reading file iret=", iret
        stop
       end if

       gfldo=gfld
       fhour=gfld%ipdtmpl(9)
       ens_id=gfld%ipdtmpl(17)
       pabbrev=param_get_abbrev(gfld%discipline,gfld%ipdtmpl(1),gfld%ipdtmpl(2))
       call prlevel(gfld%ipdtnum,gfld%ipdtmpl,labbrev)



! assign gfldo same arribute for gfld
       if(ifid.eq.40) then

         gfld%ipdtmpl(22)=6 !forecast time
         gfld%ipdtmpl(30)=6 !forecast time
         gfld%fld=apcp_6h(:)
         jret=0
         call putgb2(200,gfld,jret)
         write(*,*) '0-6h apcp put',jret

         gfld%fld=apcp_3h(:)
         gfld%ipdtmpl(22)=3 !forecast time
         gfld%ipdtmpl(30)=3 !forecast time
         jret=0
         call putgb2(300,gfld,jret)
         write(*,*) '0-3h apcp put',jret

       endif

       if(ifid.eq.41) then

         gfld%ipdtmpl(22)=6 !forecast time
         gfld%ipdtmpl(30)=6 !forecast time
         gfld%fld=acpcp_6h(:)
         jret=0
         call putgb2(200,gfld,jret)
         write(*,*) '0-6h acpcp put',jret

         gfld%ipdtmpl(22)=3 !forecast time
         gfld%ipdtmpl(30)=3 !forecast time
         gfld%fld=acpcp_3h(:)
         jret=0
         call putgb2(300,gfld,jret)
         write(*,*) '0-3h acpcp put',jret

       endif

       if(ifid.eq.42) then
         gfld%ipdtmpl(22)=6 !forecast time
         gfld%ipdtmpl(30)=6 !forecast time
         gfld%fld=apcp_6h(:)-acpcp_6h(:)
        do i=1,maxgrd
          if(gfld%fld(i).lt.0.0) then
            gfld%fld(i)=0.0
          endif
        enddo
         jret=0
         call putgb2(200,gfld,jret)
         write(*,*) '0-6h ncpcp put',jret

         gfld%ipdtmpl(22)=3 !forecast time
         gfld%ipdtmpl(30)=3 !forecast time
         gfld%fld=apcp_3h(:)-acpcp_3h(:)
        do i=1,maxgrd
          if(gfld%fld(i).lt.0.0) then
            gfld%fld(i)=0.0
          endif
        enddo
         jret=0
         call putgb2(300,gfld,jret)
         write(*,*) '0-3h ncpcp put',jret


       endif

      enddo

      deallocate(apcp_6h)
      deallocate(acpcp_6h)
      deallocate(apcp_3h)
      deallocate(acpcp_3h)
      deallocate(dpcp)

      call baclose(100,iret)
      call baclose(200,iret)
      call baclose(300,iret)

      end program gefs_6h_ave_1mem
