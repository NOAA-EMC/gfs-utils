# Find the gempak headers, library and executable
#
# This module defines:
#
#   - gempak::gempak        - library and include directory, all in a single target.
#   - GEMPAK_INCLUDE_DIR    - include directory
#   - OS_GEMPAK_INCLUDE_DIR - include directory
#   - GEMPAK_LIBRARIES      - gempak library
#
# The following paths will be searched in order:
#
#   - GEMPAK_INCLUDE_DIRS - folders containing GEMPRM.PRM BRIDGE.PRM MCHPRM.PRM.
#   - GEMPAK_LIBRARY_DIRS - folders containing appl.a bridge.a cgemlib.a gemlib.a syslib.a
#   - GEMPAK              - root of gempak installation
#   - OS_ROOT             - root of gempak os installation
#

# First find the INCLUDE_DIRS
find_path(
  GEMPAK_INCLUDE_DIR
  NAMES GEMPRM.PRM BRIDGE.PRM
  HINTS ${GEMPAK_INCLUDE_DIRS}
        ${GEMPAK} $ENV{GEMPAK}
        ${GEMINC} $ENV{GEMINC}
  PATH_SUFFIXES include
  DOC "Path to GEMPRM.PRM, BRIDGE.PRM"
  )

# FIXME: On Hera, MCHPRM.PRM is linked to GEMINC directory so may cause issues
find_path(
  OS_GEMPAK_INCLUDE_DIR
  NAMES MCHPRM.PRM
  HINTS ${GEMPAK_INCLUDE_DIRS}
        ${GEMPAK} $ENV{GEMPAK}
        ${GEMINC} $ENV{GEMINC}
        ${OS_ROOT} $ENV{OS_ROOT}
  PATH_SUFFIXES include
  DOC "Path to MCHPRM.PRM"
)

# if(NOT TARGET gempak::gempak)
  # message(DEBUG "[FindGempak.cmake]: creating target gempak::gempak")
  # add_library(gempak::gempak UNKNOWN IMPORTED)
#   set_target_properties(gempak::gempak PROPERTIES INTERFACE_INCLUDE_DIRECTORIES "${GEMPAK_INCLUDE_DIR};${OS_GEMPAK_INCLUDE_DIR}")
# endif()

# Next find the LIBRARY_DIRS and LIBRARIES
list( APPEND _libraries gemlib appl syslib cgemlib bridge )

foreach( _lib IN LISTS _libraries )
  find_library(
    GEMPAK_${_lib}_LIBRARY
    NAMES ${_lib}.a lib${_lib}.a
    HINTS ${GEMPAK_LIBRARY_DIRS}
          ${GEMPAK} $ENV{GEMPAK}
          ${GEMLIB} $ENV{GEMLIB}
          ${GEMOLB} $ENV{GEMOLB}
    PATH_SUFFIXES lib lib64
    DOC "Path to GEMPAK_${_lib}_LIBRARY"
  )
endforeach()

mark_as_advanced(GEMPAK_INCLUDE_DIR OS_GEMPAK_INCLUDE_DIR GEMPAK_gemlib_LIBRARY GEMPAK_appl_LIBRARY GEMPAK_syslib_LIBRARY GEMPAK_cgemlib_LIBRARY GEMPAK_bridge_LIBRARY)

message(DEBUG "[Findgempak.cmake]: creating target gempak::gempak")

foreach( _lib IN LISTS _libraries )
  list( APPEND GEMPAK_LIBRARIES "${GEMPAK_${_lib}_LIBRARY}" )
  add_library(gempak::${_lib} UNKNOWN IMPORTED)
  set_target_properties(gempak::${_lib} PROPERTIES IMPORTED_LOCATION "${GEMPAK_${_lib}_LIBRARY}"
                                                   INTERFACE_INCLUDE_DIRECTORIES "${GEMPAK_INCLUDE_DIR};${OS_GEMPAK_INCLUDE_DIR}")
endforeach()

add_library(gempak::gempak INTERFACE IMPORTED)
target_link_libraries(gempak::gempak INTERFACE gempak::gemlib gempak::appl gempak::syslib gempak::cgemlib gempak::bridge)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(gempak
  REQUIRED_VARS GEMPAK_LIBRARIES GEMPAK_INCLUDE_DIR OS_GEMPAK_INCLUDE_DIR
  )

if(gempak_FOUND AND NOT gempak_FIND_QUIETLY)
  message(STATUS "Findgempak:")
  message(STATUS "  - GEMPAK_INCLUDE_DIR: ${GEMPAK_INCLUDE_DIR}")
  message(STATUS "  - OS_GEMPAK_INCLUDE_DIR: ${OS_GEMPAK_INCLUDE_DIR}")
  message(STATUS "  - GEMPAK_LIBRARIES: ${GEMPAK_LIBRARIES}")
endif()
