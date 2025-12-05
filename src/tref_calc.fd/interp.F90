 module interp

 implicit none

 private

 public                 :: gaus_to_gaus

 contains

 subroutine gaus_to_gaus

!----------------------------------------------------------------------------------
! Interpolate data from the input to output grid using IPOLATES library.
!----------------------------------------------------------------------------------

 use output_data
 use input_data
 use setup
#ifdef IP_V4
 use ip_mod, only: ipolates
#endif

 implicit none

 integer                 :: ip, ipopt(20), i
 integer                 :: num_fields
 integer                 :: iret, numpts
 integer, allocatable    :: ibi(:), ibo(:)

 logical*1, allocatable  :: bitmap_input(:,:), bitmap_output(:,:)
 logical                 :: same_grid

 real, allocatable       :: data_input(:,:)
 real, allocatable       :: data_output(:,:)

 same_grid=.true.
 do i = 1, 11
   if (kgds_input(i) /= kgds_output(i)) then
     same_grid=.false.
     exit
   endif
 enddo

 if (same_grid) then

   print*
   print*,'INPUT AND OUTPUT GRIDS ARE THE SAME.'
   print*,'NO HORIZ INTERPOLATION REQUIRED.'

   allocate(tref_interp(ij_output))
   tref_interp = tref_highres
   
   
   deallocate(tref_highres)

 else

   print*
   print*,'INTERPOLATE DATA TO OUTPUT GRID'


 ip    = 0   ! bilinear
 ipopt = 0

!----------------------------------------------------------------------------------
! Do 2-D fields first
!----------------------------------------------------------------------------------

 num_fields = 1

 allocate(ibi(num_fields))
 ibi = 0 ! no bitmap
 allocate(ibo(num_fields))
 ibo = 0 ! no bitmap

 allocate(bitmap_input(ij_input,num_fields))
 bitmap_input = .true.
 allocate(bitmap_output(ij_output,num_fields))
 bitmap_output = .true.

 allocate(rlat_output(ij_output))
 rlat_output = 0.0
 allocate(rlon_output(ij_output))
 rlon_output = 0.0

!----------------
! Tref
!----------------

 allocate(data_input(ij_input,num_fields))
 data_input(:,num_fields) = tref_highres(:)
 deallocate(tref_highres)

 allocate(data_output(ij_output,num_fields))
 data_output = 0

 print*,"INTERPOLATE TREF"
 call ipolates(ip, ipopt, kgds_input, kgds_output, ij_input, ij_output,&
               num_fields, ibi, bitmap_input, data_input,  &
               numpts, rlat_output, rlon_output, ibo, bitmap_output, &
               data_output, iret)
 if (iret /= 0)
   print*,"FATAL ERROR IN IPOLATES. IRET IS: ", iret
   call errexit(23)
 end if 

 allocate(tref_interp(ij_output))
 tref_interp = data_output(:,num_fields) - tref_lowres(:)
 

 deallocate (ibi, ibo, bitmap_input, bitmap_output)

 endif

 return

 end subroutine gaus_to_gaus

 end module interp
