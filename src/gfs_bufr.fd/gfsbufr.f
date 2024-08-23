      program meteormrf
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C  
C MAIN PROGRAM: METEOMRF
C   PRGMMR: PAN              ORG: NP23        DATE: 1999-07-21
C
C ABSTRACT: Creates BUFR meteogram files for the AVN and MRF.
C
C PROGRAM HISTORY LOG:
C   99-07-21  Hualu Pan
C   16-09-27  HUIYA CHUANG  MODIFY TO READ GFS NEMS OUTPUT ON GRID SPACE
C   16-10-15  HUIYA CHUANG: CONSOLIDATE TO READ FLUX FIELDS IN THIS
C             PACKAGE TOO AND THIS SPEEDS UP BFS BUFR BY 3X
C   17-02-27  GUANG PING LOU: CHANGE MODEL OUTPUT READ-IN TO HOURLY
C             TO 120 HOURS AND 3 HOURLY TO 180 HOURS.
C   19-07-16  GUANG PING LOU: CHANGE FROM NEMSIO TO GRIB2.
C   24-08-08  Bo Cui: UPDATE TO HANDLE ONE FORECAST AT A TIME
C                     REMOVE NEMSIO INPUT FILES
C   
C
C USAGE:
C   INPUT FILES:
C     FTxxF001 - UNITS 11 THRU 49
C     PARM     - UNIT 5 (STANDARD READ)
C
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
C     FTxxF001 - UNITS 51 THRU 79
C     FTxxF001 - UNIT 6 (STANDARD PRINTFILE)
C
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)
C     UNIQUE:    - ROUTINES THAT ACCOMPANY SOURCE FOR COMPILE
C     LIBRARY:
C       W3LIB    -
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =NNNN - TROUBLE OR SPECIAL FLAG - SPECIFY NATURE
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS
C   MACHINE:  IBM SP
C
C$$$
      use netcdf
      implicit none
      integer,parameter:: nsta=3000
      integer,parameter:: ifile=11
      integer,parameter:: levso=64
      integer ncfsig, nsig
      integer istat(nsta), idate(4), jdate, nfhour
      integer :: levs,nstart,nend,nint,nsfc,levsi,im,jm
      integer :: npoint,np,ist,is,iret,lss,nss,nf,nsk,nfile
      integer :: ielev
      integer :: lsfc
      real :: alat,alon,rla,rlo
      real :: wrkd(1),dummy
      real rlat(nsta), rlon(nsta), elevstn(nsta), fhour
      integer iidum(nsta),jjdum(nsta)
      integer nint1, nend1, nint3, nend3, np1
      integer landwater(nsta)
      character*1 ns, ew
      character*4 t3
      character*4 cstat(nsta)
      character*32 desc
      character*512 dird, fnsig,fngrib,fngrib2
      logical f00, makebufr
      CHARACTER*8      SBSET
      LOGICAL         SEQFLG(4)
      CHARACTER*80     CLIST(4)
      INTEGER            NPP(4)
      CHARACTER*8     SEQNAM(4)
      integer ierr, mrank, msize,ntask
      integer n0, ntot
      integer  :: error, ncid, id_var,dimid
      character(len=10) :: dim_nam
      character(len=6) :: fformat
      character(len=100) :: long_name
      !added from Cory
      integer :: iope, ionproc
      integer, allocatable  :: iocomms(:)
C
      DATA             SBSET / 'ABCD1234' /
C
      DATA            SEQFLG / .FALSE., .TRUE., .FALSE.,  .FALSE. /
C
      DATA            SEQNAM / 'HEADR', 'PROFILE', 'CLS1' ,'D10M' /
c      DATA         SEQNAM / 'HEADR', 'PRES TMDB UWND VWND SPFH OMEG',
c     &                      'CLS1' ,'D10M' /
C
      namelist /nammet/ levs, makebufr, dird,
     &                  nstart, nend, nint, nend1, nint1, 
     &                  nint3, nsfc, f00, fformat, np1,
     &                  fnsig,fngrib,fngrib2                          

      CALL W3TAGB('METEOMRF',1999,0202,0087,'NP23')
      open(5,file='gfsparm')
      read(5,nammet)
      write(6,nammet)
      npoint = 0
   99 FORMAT (I6, F6.2,A1, F7.2,A1,1X,A4,1X,I2, A28, I4)
      do np = 1, nsta+2
        read(8,99,end=200) IST,ALAT,NS,ALON,EW,T3,lsfc,DESC,IELEV
CC        print*," IST,ALAT,NS,ALON,EW,T3,lsfc,DESC,IELEV= "
CC        print*, IST,ALAT,NS,ALON,EW,T3,lsfc,DESC,IELEV
        if(alat.lt.95.) then
          npoint = npoint + 1
                            RLA = 9999.
          IF (NS .EQ. 'N')  RLA =  ALAT
          IF (NS .EQ. 'S')  RLA = -ALAT
                            RLO = 9999.
          IF (EW .EQ. 'E')  RLO =  ALON
          IF (EW .EQ. 'W')  RLO = -ALON
          rlat(npoint) = rla
          rlon(npoint) = rlo
          istat(npoint) = ist
          cstat(npoint) = T3
          elevstn(npoint) = ielev
           
        if(lsfc .le. 9) then
          landwater(npoint) = 2    !!nearest
         else if(lsfc .le. 19) then
          landwater(npoint) = 1    !!land
         else if(lsfc .ge. 20) then
          landwater(npoint) = 0    !!water
        endif
        endif
      enddo
 200  continue
      if(npoint.le.0) then
        print *, ' station list file is empty, abort program'
        call abort
      elseif(npoint.gt.nsta) then
        print *, ' number of station exceeds nsta, abort program'
        call abort
      endif
!          print*,'npoint= ', npoint
!          print*,'np,IST,idum,jdum,rlat(np),rlon(np)= '
      if(np1 == 0) then
          do np = 1, npoint
          read(7,98) IST, iidum(np), jjdum(np), ALAT, ALON
          enddo
      endif
  98     FORMAT (3I6, 2F9.2) 
      if (makebufr) then
        REWIND 1
        READ (1,100) SBSET
  100   FORMAT ( ////// 2X, A8 )
        PRINT 120, SBSET
  120   FORMAT ( ' SBSET=#', A8, '#' )
        REWIND 1
C
C     READ PARM NAMES AND NUMBER OF PARM NAMES FROM BUFR TABLE.
        DO IS = 1,4
           CALL BFRHDR ( 1, SEQNAM(IS), SEQFLG(IS),
     X                   CLIST(IS), NPP(IS), IRET )
           IF ( IRET .NE. 0 ) THEN
              PRINT*, ' CALL BFRHDR  IRET=', IRET
           ENDIF
        ENDDO
      lss = len ( dird )
      DO WHILE ( dird (lss:lss) .eq. ' ' )
        lss = lss - 1
      END DO
C
      else   ! else of makebufr

!!   nfile - output data file channel, start from fort.21
!!   nf  - forecast hour 

        nf=nstart
        if(nf .le. nend1) then
        nfile = 21 + (nf / nint1)
         else
        nfile = 21 + (nend1/nint1) + (nf-nend1)/nint3
        endif
        print*, 'nf,nint,nfile = ',nf,nint,nfile
        print *, 'Opening atmos file : ',trim(fnsig)
        print *, 'Opening surface file : ',trim(fngrib)
        print *, 'Opening surface file 2 : ',trim(fngrib2)

!! read in NetCDF files
       if (fformat == 'netcdf') then
          error=nf90_open(trim(fnsig),nf90_nowrite,ncid)
          error=nf90_inq_dimid(ncid,"grid_xt",dimid)
          error=nf90_inquire_dimension(ncid,dimid,dim_nam,im)
          error=nf90_inq_dimid(ncid,"grid_yt",dimid)
          error=nf90_inquire_dimension(ncid,dimid,dim_nam,jm)
          error=nf90_inq_dimid(ncid,"pfull",dimid)
          error=nf90_inquire_dimension(ncid,dimid,dim_nam,levsi)
          error=nf90_close(ncid)
!         print*,'NetCDF file im,jm,lm= ',im,jm,levs,levsi

          call meteorg(npoint,rlat,rlon,istat,cstat,elevstn,
     &             nf,nfile,fnsig,fngrib,fngrib2,jdate,idate,
     &      levsi,im,jm,nsfc,
     &      landwater,nend1, nint1, nint3, iidum,jjdum,np1,
     &      fformat)
        endif  ! end of process

      endif  ! endif of makebufr

      if(makebufr) then

!  read in NetCDF file header info
!  sample of idate and jdate
!  idate =  0  7  1 2019
!  jdate = 2019070100

        if (fformat == 'netcdf') then
          error=nf90_open(trim(fnsig),nf90_nowrite,ncid)
          error=nf90_inq_varid(ncid, "time", id_var)
          error=nf90_get_var(ncid, id_var, nfhour)
          error=nf90_get_att(ncid,id_var,"units",long_name)
          error=nf90_close(ncid)
        endif

        read(long_name(13:16),"(i4)")idate(4)
        read(long_name(18:19),"(i2)")idate(2)
        read(long_name(21:22),"(i2)")idate(3)
        read(long_name(24:25),"(i2)")idate(1)
        fhour=float(nfhour)
        jdate = idate(4)*1000000 + idate(2)*10000+
     &        idate(3)*100 + idate(1)

        print *, ' starting to make bufr files'
        print *, ' makebufr= ', makebufr
        print *, ' processing forecast hour ', fhour
        print *, 'nint1,nend1,nint3,nend= ',nint1,nend1,nint3,nend
        print *, 'idate,jdate=',idate,jdate                           

        nend3 = nend
        call buff(nint1,nend1,nint3,nend3,
     &        npoint,idate,jdate,levso,
     &        dird,lss,istat,sbset,seqflg,clist,npp,wrkd)
        CALL W3TAGE('METEOMRF')

      endif  ! end of makebufr

      end
