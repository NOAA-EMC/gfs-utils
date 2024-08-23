!-------------------------------------------------------------------------------
module modpr_module
!$$$  Module Documentation Block
!
! Original module sourced from: /apps/ops/prod/libs/build/v1.1.0/pkg/sigio-v2.3.2/src/sigio_module.f
! 
! Modifications:
! - Copied the original sigio_module.f to the folder gfs_bufr.fd and renamed it to modpr_module.f90
! - Retained only the sigio_modpr subroutine and its related code
! - Removed all other subroutines and associated code that were present in sigio_module.f
! 
! Reason for modification:
! - To simplify the module by including only the necessary subroutines
! 
!  Program History Log:
!   2024-08-23  Bo Cui: Simplified the module by retaining only sigio_modpr subroutine
! 
! Below are Original Module Documentation Block from sigio_module.f
!
! Module:    sigio_module    API for global spectral sigma file I/O
!   Prgmmr: iredell          Org: w/nx23     date: 1999-01-18
!
! Abstract: This module provides an Application Program Interface
!   for performing I/O on the sigma restart file of the global spectral model.
!   Functions include opening, reading, writing, and closing as well as
!   allocating and deallocating data buffers used in the transfers.
!   The I/O performed here is sequential.
!   The transfers are limited to header records or data records.
!   
! Program History Log:
!   1999-01-18  Mark Iredell
!   2013-10-14  Fanglin Yang: Added dynamics restart fields (ixga etc)
!                 and restructureed physics restart fields (ixgr etc).
!   2018-05-11  Mark Iredell: Added error check for NEMSIO file.
!
! Public Variables:
!   sigio_lhead1      Integer parameter length of first header record (=32)
!   sigio_charkind    Integer parameter kind or length of passed characters (=8)
!   sigio_intkind     Integer parameter kind or length of passed integers (=4)
!   sigio_realkind    Integer parameter kind or length of passed reals (=4)
!   sigio_dblekind    Integer parameter kind or length of passed longreals (=8)
!   sigio_realfill    Real(sigio_realkind) parameter fill value (=-9999.)
!   sigio_dblefill    Real(sigio_dblekind) parameter fill value (=-9999.)
!
! Public Defined Types:
!   sigio_head        Sigma file header information
!     clabsig           Character(sigio_lhead1) ON85 label
!                       (obsolescent)
!     fhour             Real(sigio_realkind) forecast hour
!     idate             Integer(sigio_intkind)(4) initial date
!                       (hour, month, day, 4-digit year)
!     si                Real(sigio_realkind)(101) sigma interfaces
!                       (obsolescent)
!     sl                Real(sigio_realkind)(100) sigma levels
!                       (obsolescent)
!     ak                Real(sigio_realkind)(101) hybrid interface a
!                       (obsolescent)
!     bk                Real(sigio_realkind)(101) hybrid interface b
!                       (obsolescent)
!     jcap              Integer(sigio_intkind) spectral truncation
!     levs              Integer(sigio_intkind) number of levels
!     itrun             Integer(sigio_intkind) truncation flag
!                       (=1 for triangular)
!     iorder            Integer(sigio_intkind) coefficient order flag
!                       (=2 for ibm order)
!     irealf            Integer(sigio_intkind) floating point flag
!                       (=1 for 4-byte ieee, =2 for 8-byte ieee)
!     igen              Integer(sigio_intkind) model generating flag
!     latf              Integer(sigio_intkind) latitudes in dynamics
!                       (=(jcap+1)*3/2)
!     lonf              Integer(sigio_intkind) longitudes in dynamics
!                       (>=(jcap+1)*3 appropriate for fft)
!     latb              Integer(sigio_intkind) latitudes in physics
!     lonb              Integer(sigio_intkind) longitudes in physics
!     latr              Integer(sigio_intkind) latitudes in radiation
!     lonr              Integer(sigio_intkind) longitudes in radiation
!     ntrac             Integer(sigio_intkind) number of tracers
!     icen2             Integer(sigio_intkind) subcenter id
!     iens              Integer(sigio_intkind)(2) ensemble ids
!     idpp              Integer(sigio_intkind) processing id
!     idsl              Integer(sigio_intkind) semi-lagrangian id
!     idvc              Integer(sigio_intkind) vertical coordinate id
!                       (=1 for sigma, =2 for ec-hybrid, =3 for ncep hybrid)
!     idvm              Integer(sigio_intkind) mass variable id
!     idvt              Integer(sigio_intkind) tracer variable id
!     idrun             Integer(sigio_intkind) run id
!     idusr             Integer(sigio_intkind) user-defined id
!     pdryini           Real(sigio_realkind) global mean dry air pressure (kPa)
!                       (obsolescent)
!     ncldt             Integer(sigio_intkind) number of cloud types
!     ixgr              Integer(sigio_intkind) extra fileds for physics.
!                         ixgr=00000000  no extra fields                                      
!                         ixgr=0000000a  zhao micro,    a=1: zhao1, two 3d, one 2d, and nxss=0            
!                                                       a=2: zhao2, four 3d, three 2d, and nxss=0            
!                                                       a=3: zhao2, four 3d, three 2d, and nxss=1            
!                         ixgr=000000b0  ferrier micro, b=1: three 3d, one 2d, and nxss=0          
!                                        ferrier micro, b=2: three 3d, one 2d, and nxss=1           
!                         ixgr=00000c00  c=1, pdf cld, three 3d                      
!     ixga              Integer(sigio_intkind) extra fileds for dynamics. 
!                         ixga=00000000  no extra fields                                      
!                         ixga=0000000a  zflxtvd micro,   ntrac 3d, zero 2d            
!                         ixga=000000b0  (reserved for) joe-sela semi-lag gfs
!     ivs               Integer(sigio_intkind) version number
!     nvcoord           Integer(sigio_intkind) number of vcoord profiles
!  The following variables should be allocated with sigio_alhead:
!     vcoord            Real(sigio_realkind)((levs+1),nvcoord) vcoord profiles
!     cfvars            Character(8)(5+ntrac) field variable names
!  The following variables should not be modified by the user:
!     nxgr              Integer(sigio_intkind) number of extra physics grid fields
!     nxss              Integer(sigio_intkind) number of extra scalars
!     nxga              Integer(sigio_intkind) number of extra dynamics grid fields
!     nhead             Integer(sigio_intkind) number of header records
!     ndata             Integer(sigio_intkind) number of data records
!     lhead             Integer(sigio_intkind)(nhead) header record lengths
!     ldata             Integer(sigio_intkind)(ndata) data record lengths
!
!   sigio_data        Sigma file data fields
!     hs                Real(sigio_realkind)(:) pointer to spectral
!                       coefficients of surface height in m
!     ps                Real(sigio_realkind)(:) pointer to spectral
!                       coefficients of log of surface pressure over 1 kPa
!     t                 Real(sigio_realkind)(:,:) pointer to spectral
!                       coefficients of virtual temperature by level in K
!     d                 Real(sigio_realkind)(:,:) pointer to spectral
!                       coefficients of divergence by level in 1/second
!     z                 Real(sigio_realkind)(:,:) pointer to spectral
!                       coefficients of vorticity by level in 1/second
!     q                 Real(sigio_realkind)(:,:,:) pointer to spectral
!                       coefficients of tracers by level and tracer number
!                       in specific densities
!     xgr               Real(sigio_realkind)(:,:,:) pointer to extra grid fields
!                       by longitude, latitude and number of extra physics grid fields
!     xss               Real(sigio_realkind)(:) pointer to scalar array
!     xga               Real(sigio_realkind)(:,:,:) pointer to extra dynamics grid fields
!                       by longitude, latitude and number of extra grid fields
!                       
!   sigio_dbta        Sigma file longreal data fields
!     hs                Real(sigio_dblekind)(:) pointer to spectral
!                       coefficients of surface height in m
!     ps                Real(sigio_dblekind)(:) pointer to spectral
!                       coefficients of log of surface pressure over 1 kPa
!     t                 Real(sigio_dblekind)(:,:) pointer to spectral
!                       coefficients of virtual temperature by level in K
!     d                 Real(sigio_dblekind)(:,:) pointer to spectral
!                       coefficients of divergence by level in 1/second
!     z                 Real(sigio_dblekind)(:,:) pointer to spectral
!                       coefficients of vorticity by level in 1/second
!     q                 Real(sigio_dblekind)(:,:,:) pointer to spectral
!                       coefficients of tracers by level and tracer number
!                       in specific densities
!     xgr               Real(sigio_dblekind)(:,:,:) pointer to extra physics grid fields
!                       by longitude, latitude and number of extra grid fields
!     xss               Real(sigio_dblekind)(:) pointer to scalar array
!     xga               Real(sigio_dblekind)(:,:,:) pointer to extra dynamics grid fields
!                       by longitude, latitude and number of extra grid fields
!                       
! Public Subprograms:
!   sigio_modpr        Compute model pressures
!     im                Integer(sigio_intkind) input number of points
!     ix                Integer(sigio_intkind) input first dimension
!     km                Integer(sigio_intkind) input number of levels
!     nvcoord           Integer(sigio_intkind) input number of vertical coords
!     idvc              Integer(sigio_intkind) input vertical coordinate id
!                       (1 for sigma and 2 for hybrid)
!     idsl              Integer(sigio_intkind) input type of sigma structure
!                       (1 for phillips or 2 for mean)
!     vcoord            Real(sigio_realkind)(km+1,nvcoord) input vertical coords
!                       for idvc=1, nvcoord=1: sigma interface
!                       for idvc=2, nvcoord=2: hybrid interface a and b
!     iret              Integer(sigio_intkind) output return code
!     ps                Real(sigio_realkind)(ix) input optional surface pressure (Pa)
!     tv                Real(sigio_realkind)(ix,km) input optional virtual temperature (K)
!     pd                Real(sigio_realkind)(ix,km) output optional delta pressure (Pa)
!     pm                Real(sigio_realkind)(ix,km) output optional layer pressure (Pa)
!
!
! Remarks:
!   (1) The sigma file format follows:
!       For ivs=198410:
!         ON85 label (32 bytes)
!         Header information record containing
!           real forecast hour, initial date, sigma interfaces, sigma levels,
!           padding to allow for 100 levels, and finally 44 identifier words
!           containing JCAP, LEVS, NTRAC, IREALF, etc. (250 4-byte words)
!           (word size in the remaining records depends on the value of IREALF)
!         Orography (NC words, where NC=(JCAP+1)*(JCAP+2))
!         Log surface pressure (NC words)
!         Temperature (LEVS records of NC words)
!         Divergence & Vorticity interleaved (2*LEVS records of NC words)
!         Tracers (LEVS*NTRAC records of NC words)
!         Extra grid fields (NXGR records of LONB*LATB words)
!       For ivs=200509:
!         Label containing
!           'GFS ','SIG ',ivs,nhead,ndata,reserved(3) (8 4-byte words)
!         Header records
!           lhead(nhead),ldata(ndata) (nhead+ndata 4-byte words)
!           fhour, idate(4), jcap, levs, itrun, iorder, irealf, igen,
!             latf, lonf, latb, lonb, latr, lonr, ntrac, nvcoord, 
!             icen2, iens(2), idpp, idsl, idvc, idvm, idvt, idrun, idusr,
!             pdryini, ncldt, ixgr, ixga,reserved(17) (50 4-byte words)
!           vcoord((levs+1)*nvcoord 4-byte words)
!           cfvars(5+ntrac 8-byte character words)
!         Data records (word size depends on irealf)
!           orography (nc words, where nc=(jcap+1)*(jcap+2))
!           log surface pressure (nc words)
!           temperature (levs records of nc words)
!           divergence (levs records of nc words)
!           vorticity (levs records of nc words)
!           tracers (levs*ntrac records of nc words)
!           scalars (nxss words)
!           extra physics grid fields (nxgr records of lonb*latb words)
!           extra scalars (nxss words)
!           extra dynamics grid fields (nxga records of lonf*latf words)
!
!   (2) Possible return codes:
!          0   Successful call
!         -1   Open or close I/O error
!         -2   Header record I/O error (possible EOF)
!         -3   Allocation or deallocation error
!         -4   Data record I/O error
!         -5   Insufficient data dimensions allocated
!         -6   Attempted to read a NEMSIO file
!
! Examples:
!   (1) Read the entire sigma file 'sigf24' and
!       print out the global mean temperature profile.
!
!     use sigio_module
!     type(sigio_head):: head
!     type(sigio_data):: data
!     call sigio_srohdc(11,'sigf24',head,data,iret)
!     print '(f8.2)',data%t(1,head%levs:1:-1)/sqrt(2.)
!     end
!
! Attributes:
!   Language: Fortran 90
!
!$$$
  implicit none
  private
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Public Variables
  integer,parameter,public:: sigio_lhead1=32
  integer,parameter,public:: sigio_intkind=4,sigio_realkind=4,sigio_dblekind=8
  integer,parameter,public:: sigio_charkind=8
  real(sigio_intkind),parameter,public:: sigio_intfill=-9999_sigio_intkind
  real(sigio_realkind),parameter,public:: sigio_realfill=-9999._sigio_realkind
  real(sigio_dblekind),parameter,public:: sigio_dblefill=-9999._sigio_dblekind
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Public Types
  type,public:: sigio_head
    character(sigio_lhead1):: clabsig=' '
    real(sigio_realkind):: fhour=sigio_realfill
    integer(sigio_intkind):: idate(4)=sigio_intfill
    real(sigio_realkind):: si(101)=sigio_realfill
    real(sigio_realkind):: sl(100)=sigio_realfill
    real(sigio_realkind):: ak(101)=sigio_realfill
    real(sigio_realkind):: bk(101)=sigio_realfill
    integer(sigio_intkind):: jcap=sigio_intfill
    integer(sigio_intkind):: levs=sigio_intfill
    integer(sigio_intkind):: itrun=sigio_intfill
    integer(sigio_intkind):: iorder=sigio_intfill
    integer(sigio_intkind):: irealf=sigio_intfill
    integer(sigio_intkind):: igen=sigio_intfill
    integer(sigio_intkind):: latf=sigio_intfill
    integer(sigio_intkind):: lonf=sigio_intfill
    integer(sigio_intkind):: latb=sigio_intfill
    integer(sigio_intkind):: lonb=sigio_intfill
    integer(sigio_intkind):: latr=sigio_intfill
    integer(sigio_intkind):: lonr=sigio_intfill
    integer(sigio_intkind):: ntrac=sigio_intfill
    integer(sigio_intkind):: icen2=sigio_intfill
    integer(sigio_intkind):: iens(2)=sigio_intfill
    integer(sigio_intkind):: idpp=sigio_intfill
    integer(sigio_intkind):: idsl=sigio_intfill
    integer(sigio_intkind):: idvc=sigio_intfill
    integer(sigio_intkind):: idvm=sigio_intfill
    integer(sigio_intkind):: idvt=sigio_intfill
    integer(sigio_intkind):: idrun=sigio_intfill
    integer(sigio_intkind):: idusr=sigio_intfill
    real(sigio_realkind):: pdryini=sigio_realfill
    integer(sigio_intkind):: ncldt=sigio_intfill
    integer(sigio_intkind):: ixgr=sigio_intfill
    integer(sigio_intkind):: ixga=sigio_intfill
    integer(sigio_intkind):: ivs=sigio_intfill
    integer(sigio_intkind):: nvcoord=sigio_intfill
    real(sigio_realkind),allocatable:: vcoord(:,:)
    character(sigio_charkind),allocatable:: cfvars(:)
    integer(sigio_intkind):: nxgr=sigio_intfill
    integer(sigio_intkind):: nxss=sigio_intfill
    integer(sigio_intkind):: nxga=sigio_intfill
    integer(sigio_intkind):: nhead=sigio_intfill
    integer(sigio_intkind):: ndata=sigio_intfill
    integer(sigio_intkind),allocatable:: lhead(:)
    integer(sigio_intkind),allocatable:: ldata(:)
    real(sigio_realkind), allocatable :: cpi(:), ri(:)
!   real(sigio_realkind):: cpi(100)=sigio_realfill
!   real(sigio_realkind):: ri(100)=sigio_realfill
  end type
  type,public:: sigio_data
    real(sigio_realkind),pointer:: hs(:)=>null()
    real(sigio_realkind),pointer:: ps(:)=>null()
    real(sigio_realkind),pointer:: t(:,:)=>null()
    real(sigio_realkind),pointer:: d(:,:)=>null()
    real(sigio_realkind),pointer:: z(:,:)=>null()
    real(sigio_realkind),pointer:: q(:,:,:)=>null()
    real(sigio_realkind),pointer:: xgr(:,:,:)=>null()
    real(sigio_realkind),pointer:: xss(:)=>null()
    real(sigio_realkind),pointer:: xga(:,:,:)=>null()
  end type
  type,public:: sigio_dbta
    real(sigio_dblekind),pointer:: hs(:)=>null()
    real(sigio_dblekind),pointer:: ps(:)=>null()
    real(sigio_dblekind),pointer:: t(:,:)=>null()
    real(sigio_dblekind),pointer:: d(:,:)=>null()
    real(sigio_dblekind),pointer:: z(:,:)=>null()
    real(sigio_dblekind),pointer:: q(:,:,:)=>null()
    real(sigio_dblekind),pointer:: xgr(:,:,:)=>null()
    real(sigio_dblekind),pointer:: xss(:)=>null()
    real(sigio_dblekind),pointer:: xga(:,:,:)=>null()
  end type
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Public Subprograms
  public sigio_modpr
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Private Variables
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Private Types
  type sigio_head2
    sequence
    real(sigio_realkind):: fhour
    integer(sigio_intkind):: idate(4)
    real(sigio_realkind):: sisl(2*100+1)
    real(sigio_realkind):: ext(44)
  end type
contains
!-------------------------------------------------------------------------------
  subroutine sigio_modpr(im,ix,km,nvcoord,idvc,idsl,vcoord,iret,&
                         ps,t,pd,dpddps,dpddt,pm,dpmdps,dpmdt)
    implicit none
    integer,intent(in):: im,ix,km,nvcoord,idvc,idsl
    real,intent(in):: vcoord(km+1,nvcoord)
    integer,intent(out):: iret
    real,intent(in),optional:: ps(ix),t(ix,km)
    real,intent(out),optional:: pd(ix,km),pm(ix,km)
    real,intent(out),optional:: dpddps(ix,km),dpddt(ix,km)
    real,intent(out),optional:: dpmdps(ix,km),dpmdt(ix,km)
    real(sigio_dblekind),parameter:: rocp=287.05/1004.6,rocpr=1/rocp
    real(sigio_dblekind),parameter:: t00=300.
    integer id1,id2
    real(sigio_dblekind) pid(im),dpiddps(im),dpiddt(im),tid(im),pidk(im)
    real(sigio_dblekind) piu,dpiudps,dpiudt,tiu,piuk
    real(sigio_dblekind) pmm,dpmdpid,dpmdpiu
    real(sigio_dblekind) pmk
    integer i,k
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if((idvc.eq.0.or.idvc.eq.1).and.nvcoord.eq.1.and.present(ps)) then
      id1=11
    elseif(idvc.eq.2.and.nvcoord.eq.2.and.present(ps)) then
      id1=22
    elseif(idvc.eq.3.and.nvcoord.eq.3.and.all(vcoord(:,3).eq.0).and.present(ps)) then
      id1=22
    elseif(idvc.eq.3.and.nvcoord.eq.2.and.present(ps).and.present(t)) then
      id1=32
    elseif(idvc.eq.3.and.nvcoord.eq.3.and.present(ps).and.present(t)) then
      id1=33
    else
      id1=0
    endif
    if(idsl.eq.0.or.idsl.eq.1) then
      id2=1
    elseif(idsl.eq.2) then
      id2=2
    else
      id2=0
    endif
    iret=0
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(id1.gt.0.and.id2.gt.0) then
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i)
      do i=1,im
        pid(i)=ps(i)
        dpiddps(i)=1
        dpiddt(i)=0
        tid(i)=0
        if(id2.eq.1) pidk(i)=pid(i)**rocp
      enddo
!$OMP END PARALLEL DO

!!$OMP PARALLEL DO DEFAULT(SHARED) &
!!$OMP& PRIVATE(i,k,piu,dpiudps,dpiudt,tiu,piuk,pmk,pmm,dpmdpid,dpmdpiu) &
!!$OMP& PRIVATE(pid,dpiddps,dpiddt,tid,pidk)

      do k=1,km
!$OMP PARALLEL DO DEFAULT(SHARED) &
!$OMP& PRIVATE(i,piu,dpiudps,dpiudt,tiu,piuk,pmk,pmm,dpmdpid,dpmdpiu)
        do i=1,im
          select case(id1)
          case(11)
            piu=vcoord(k+1,1)*ps(i)
            dpiudps=vcoord(k+1,1)
            dpiudt=0
          case(22)
            piu=vcoord(k+1,1)+vcoord(k+1,2)*ps(i)
            dpiudps=vcoord(k+1,2)
            dpiudt=0
          case(32)
            tiu=(t(i,k)+t(i,min(k+1,km)))/2
            piu=vcoord(k+1,2)*ps(i)+vcoord(k+1,1)*(tiu/t00)**rocpr
            dpiudps=vcoord(k+1,2)
            dpiudt=vcoord(k+1,1)*(tiu/t00)**rocpr*rocpr/tiu
            if(k.lt.km) dpiudt=dpiudt/2
          case(33)
            tiu=(t(i,k)+t(i,min(k+1,km)))/2
            piu=vcoord(k+1,1)+vcoord(k+1,2)*ps(i)+vcoord(k+1,3)*(tiu/t00)**rocpr
            dpiudps=vcoord(k+1,2)
            dpiudt=vcoord(k+1,3)*(tiu/t00)**rocpr*rocpr/tiu
            if(k.lt.km) dpiudt=dpiudt/2
          end select
          if(present(pd)) pd(i,k)=pid(i)-piu
          if(present(dpddps)) dpddps(i,k)=dpiddps(i)-dpiudps
          if(present(dpddt)) dpddt(i,k)=dpiddt(i)-dpiudt
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          select case(id2)
          case(1)
            piuk=piu**rocp
            pmk=(pid(i)*pidk(i)-piu*piuk)/((rocp+1)*(pid(i)-piu))
            pmm=pmk**rocpr
            dpmdpid=rocpr*pmm/(pid(i)-piu)*(pidk(i)/pmk-1)
            dpmdpiu=rocpr*pmm/(pid(i)-piu)*(1-piuk/pmk)
          case(2)
            pmm=(pid(i)+piu)/2
            dpmdpid=0.5
            dpmdpiu=0.5
          end select
          if(present(pm)) pm(i,k)=pmm
          if(present(dpmdps)) dpmdps(i,k)=dpmdpid*dpiddps(i)+dpmdpiu*dpiudps
          if(present(dpmdt)) dpmdt(i,k)=dpmdpid*dpiddt(i)+dpmdpiu*dpiudt
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          pid(i)=piu
          dpiddps(i)=dpiudps
          dpiddt(i)=dpiudt
          tid(i)=tiu
          if(id2.eq.1) pidk(i)=piuk
        enddo
!$OMP END PARALLEL DO
      enddo
!!$OMP END PARALLEL DO
    else
      if(id1.le.0) iret=iret+1
      if(id2.le.0) iret=iret+2
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
end module
