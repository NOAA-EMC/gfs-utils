module outputvars

  implicit none

  integer, parameter :: maxvars = 40          !< The maximum number of variables written to a file

  type :: vardefs
     character(len= 10)   :: input_var_name    !< A variable's  input variable name
     character(len= 10)   :: output_var_name   !< A variable's output variable name
     character(len=120)   :: long_name         !< A variable's long name
     character(len= 20)   :: units             !< A variable's unit
     character(len= 10)   :: var_remapmethod   !< A variable's mapping method
     integer              :: var_dimen         !< A variable's dimensionality
     character(len=  4)   :: var_grid          !< A variable's input grid location; all output locations are on cell centers
     character(len= 10)   :: var_pair          !< A variable's pair
     character(len=  4)   :: var_pair_grid     !< A pair variable grid
     real                 :: var_fillvalue     !< A variable's fillvalue
  end type vardefs

  type(vardefs), public :: outvars(maxvars)    !< Attribute definitions for the variables

contains

  subroutine ocnvars_typedefine

    ! local variables
    integer :: ii = 0

    !set defaults
    outvars(:)%input_var_name=''
    outvars(:)%var_grid  = 'Ct'
    outvars(:)%var_remapmethod  = 'bilinear'
    outvars(:)%var_dimen = 2
    outvars(:)%var_pair = ''
    outvars(:)%var_pair_grid = ''
    outvars(:)%long_name = ''          ! obtained from input file
    outvars(:)%units = ''              ! obtained from input file
    outvars(:)%var_fillvalue = -1.0    ! obtained from input file

    ! 2D states with native grid location on cell centers; remapped bilinearly
    ii = ii + 1; outvars(ii)%input_var_name  = 'SSH'
    ii = ii + 1; outvars(ii)%input_var_name  = 'SST'
    ii = ii + 1; outvars(ii)%input_var_name  = 'SSS'
    ii = ii + 1; outvars(ii)%input_var_name  = 'speed'
    !ii = ii + 1; outvars(ii)%input_var_name  = 'mld'
    ii = ii + 1; outvars(ii)%input_var_name  = 'ePBL'
    ii = ii + 1; outvars(ii)%input_var_name  = 'MLD_003'
    ii = ii + 1; outvars(ii)%input_var_name  = 'MLD_0125'

    ! 2D fluxes with native grid location on cell centers; remapped conservatively
    ii = ii + 1; outvars(ii)%input_var_name  = 'latent'    ; outvars(ii)%var_remapmethod  = 'conserve'
    ii = ii + 1; outvars(ii)%input_var_name  = 'sensible'  ; outvars(ii)%var_remapmethod  = 'conserve'
    ii = ii + 1; outvars(ii)%input_var_name  = 'SW'        ; outvars(ii)%var_remapmethod  = 'conserve'
    ii = ii + 1; outvars(ii)%input_var_name  = 'LW'        ; outvars(ii)%var_remapmethod  = 'conserve'
    ii = ii + 1; outvars(ii)%input_var_name  = 'evap'      ; outvars(ii)%var_remapmethod  = 'conserve'
    ii = ii + 1; outvars(ii)%input_var_name  = 'lprec'     ; outvars(ii)%var_remapmethod  = 'conserve'
    ii = ii + 1; outvars(ii)%input_var_name  = 'fprec'     ; outvars(ii)%var_remapmethod  = 'conserve'
    ii = ii + 1; outvars(ii)%input_var_name  = 'LwLatSens' ; outvars(ii)%var_remapmethod  = 'conserve'
    ii = ii + 1; outvars(ii)%input_var_name  = 'Heat_PmE'  ; outvars(ii)%var_remapmethod  = 'conserve'

    ! 2D vector states on stagger locations; remapped bilinearly
    ii = ii + 1; outvars(ii)%input_var_name  = 'SSU'
                 outvars(ii)%var_grid = 'Cu'
                 outvars(ii)%var_pair = 'SSV'
                 outvars(ii)%var_pair_grid = 'Cv'

    ii = ii + 1; outvars(ii)%input_var_name  = 'SSV'
                 outvars(ii)%var_grid = 'Cv'
                 outvars(ii)%var_pair = 'SSU'
                 outvars(ii)%var_pair_grid = 'Cu'

    ! 2D vector fluxes on stagger locations; remapped conservatively
    ii = ii + 1; outvars(ii)%input_var_name = 'taux'
                 outvars(ii)%var_grid = 'Cu'
                 outvars(ii)%var_pair = 'tauy'
                 outvars(ii)%var_pair_grid = 'Cv'
                 outvars(ii)%var_remapmethod = 'conserve'

    ii = ii + 1; outvars(ii)%input_var_name = 'tauy'
                 outvars(ii)%var_grid = 'Cv'
                 outvars(ii)%var_pair = 'taux'
                 outvars(ii)%var_pair_grid = 'Cu'
                 outvars(ii)%var_remapmethod = 'conserve'

    ! 3D scalars with native grid location on cell centers; remapped bilinearly
    ii = ii + 1; outvars(ii)%input_var_name  = 'temp' ; outvars(ii)%var_dimen = 3
    ii = ii + 1; outvars(ii)%input_var_name  = 'so'   ; outvars(ii)%var_dimen = 3

    ! 3D vectors on stagger locations; remapped bilinearly
    ii = ii + 1; outvars(ii)%input_var_name  = 'uo'
                 outvars(ii)%var_grid = 'Cu'
                 outvars(ii)%var_pair = 'vo'
                 outvars(ii)%var_pair_grid = 'Cv'
                 outvars(ii)%var_dimen = 3

    ii = ii + 1; outvars(ii)%input_var_name  = 'vo'
                 outvars(ii)%var_grid = 'Cv'
                 outvars(ii)%var_pair = 'uo'
                 outvars(ii)%var_pair_grid = 'Cu'
                 outvars(ii)%var_dimen = 3

    ! set default output name
    outvars(:)%output_var_name = outvars(:)%input_var_name

  end subroutine ocnvars_typedefine

  subroutine icevars_typedefine

    ! local variables
    integer :: ii = 0

    !set defaults
    outvars(:)%input_var_name=''
    outvars(:)%var_grid  = 'Ct'
    outvars(:)%var_remapmethod  = 'bilinear'
    outvars(:)%var_dimen = 2
    outvars(:)%var_pair = ''
    outvars(:)%var_pair_grid = ''
    outvars(:)%long_name = ''          ! obtained from input file
    outvars(:)%units = ''              ! obtained from input file
    outvars(:)%var_fillvalue = -1.0    ! obtained from input file

    ! 2D states with native grid location on cell centers; remapped bilinearly
    ii = ii + 1; outvars(ii)%input_var_name  = 'hi_h'
    ii = ii + 1; outvars(ii)%input_var_name  = 'hs_h'
    ii = ii + 1; outvars(ii)%input_var_name  = 'aice_h'
    ii = ii + 1; outvars(ii)%input_var_name  = 'sst_h'
    ii = ii + 1; outvars(ii)%input_var_name  = 'Tsfc_h'

    ! 2D vector states on stagger locations; remapped bilinearly
    ii = ii + 1; outvars(ii)%input_var_name  = 'uvel_h'
                 outvars(ii)%var_grid = 'Bu_x'
                 outvars(ii)%var_pair = 'vvel_h'
                 outvars(ii)%var_pair_grid = 'Bu_y'

    ii = ii + 1; outvars(ii)%input_var_name  = 'vvel_h'
                 outvars(ii)%var_grid = 'Bu_y'
                 outvars(ii)%var_pair = 'uvel_h'
                 outvars(ii)%var_pair_grid = 'Bu_x'

    ! set default output name
    outvars(:)%output_var_name = outvars(:)%input_var_name

  end subroutine icevars_typedefine

end module outputvars
