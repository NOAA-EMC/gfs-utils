 module setup

 implicit none

 private

 character(len=300), public       :: sfcanl_file
 character(len=300), public       :: sfcf006_file
 character(len=300), public       :: output_file

 integer, public  :: i_target
 integer, public  :: j_target
 integer, public  :: ij_target

 integer, public  :: i_source
 integer, public  :: j_source
 integer, public  :: ij_source

 public                           :: program_setup

 contains

 subroutine program_setup

 implicit none

 integer                           :: istat
 character(len=500)                :: filenamelist

 namelist /tref_calc_setup/ sfcanl_file, sfcf006_file, output_file

 print*
 call getarg(1,filenamelist)
 print*,"OPEN SETUP NAMELIST ",trim(filenamelist)
 open(43, file=filenamelist, iostat=istat)
 if (istat /= 0) then
   print*,"FATAL ERROR OPENING NAMELIST FILE. ISTAT IS: ",istat
   stop 
 endif

 print*,"READ SETUP NAMELIST."
 read(43, nml=tref_calc_setup, iostat=istat)
 if (istat /= 0) then
   print*,"FATAL ERROR READING NAMELIST FILE. ISTAT IS: ",istat
   stop
 endif

 close(43)

 end subroutine program_setup

 end module setup
