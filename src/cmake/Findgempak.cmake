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

list( APPEND _libraries appl bridge cgemlib gemlib syslib )

find_path(
  GEMPAK_INCLUDE_DIR
  NAMES GEMPRM.PRM BRIDGE.PRM
  HINTS ${GEMPAK_INCLUDE_DIRS}
        ${GEMPAK} $ENV{GEMPAK}
        ${GEMINC} $ENV{GEMINC}
  PATH_SUFFIXES include
  DOC "Path to GEMPRM.PRM, BRIDGE.PRM"
  )

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

mark_as_advanced(GEMPAK_INCLUDE_DIR OS_GEMPAK_INCLUDE_DIR)

# if(NOT TARGET gempak::gempak)
  # message(DEBUG "[FindGempak.cmake]: creating target gempak::gempak")
  # add_library(gempak::gempak UNKNOWN IMPORTED)
#   set_target_properties(gempak::gempak PROPERTIES INTERFACE_INCLUDE_DIRECTORIES "${GEMPAK_INCLUDE_DIR};${OS_GEMPAK_INCLUDE_DIR}")
# endif()

foreach( _lib IN LISTS _libraries )
  find_library(
    GEMPAK_${_lib}_LIBRARY
    NAMES ${_lib}.a
    HINTS ${GEMPAK_LIBRARY_DIRS}
          ${GEMPAK} $ENV{GEMPAK}
          ${OS_ROOT} $ENV{OS_ROOT}
    PATH_SUFFIXES lib lib64
    DOC "Path to GEMPAK_${_lib}_LIBRARY"
  )

#  target_link_libraries(gempak::gempak INTERFACE ${GEMPAK_${_lib}_LIBRARY})

  if(NOT TARGET gempak::gempak_${_lib})
    add_library(gempak::gempak_${_lib} UNKNOWN IMPORTED)
    set_target_properties(gempak::gempak_${_lib} PROPERTIES
      IMPORTED_LOCATION "${GEMPAK_${_lib}_LIBRARY}"
      )
    if(EXISTS "${GEMPAK_${_lib}_LIBRARY}")
      set_target_properties(gempak::gempak_${_lib} PROPERTIES
        IMPORTED_LOCATION "${GEMPAK_${_lib}_LIBRARY}")
    endif()
  endif()

  if(NOT TARGET gempak::gempak)
    add_library(gempak::gempak UNKNOWN IMPORTED)
    set_target_properties(gempak::gempak PROPERTIES
      IMPORTED_LOCATION "${GEMPAK_${_lib}_LIBRARY}"
      INTERFACE_INCLUDE_DIRECTORIES "${GEMPAK_INCLUDE_DIR};${OS_GEMPAK_INCLUDE_DIR}"
      )
    if(EXISTS "${GEMPAK_${_lib}_LIBRARY}")
      set_target_properties(gempak::gempak PROPERTIES
        IMPORTED_LOCATION "${GEMPAK_${_lib}_LIBRARY}")
    endif()
  endif()
  list(APPEND GEMPAK_LIBRARIES "${GEMPAK_${_lib}_LIBRARY}")

endforeach()

message(STATUS "GEMPAK_LIBRARIES = ${GEMPAK_LIBRARIES}")
set(GEMPAK_LIBRARIES "${GEMPAK_LIBRARIES}" CACHE STRING "gempak library targets" FORCE)
mark_as_advanced(GEMPAK_LIBRARIES)

message(DEBUG "[FindGempak.cmake]: linking with appl.a bridge.a cgemlib.a gemlib.a syslib.a")
set_target_properties(gempak::gempak PROPERTIES
  INTERFACE_LINK_LIBRARIES "${GEMPAK_LIBRARIES}")

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(gempak
  REQUIRED_VARS GEMPAK_INCLUDE_DIR OS_GEMPAK_INCLUDE_DIR
  )

if(gempak_FOUND AND NOT gempak_FIND_QUIETLY)
  message(STATUS "FindGempak:")
  message(STATUS "  - GEMPAK_INCLUDE_DIR: ${GEMPAK_INCLUDE_DIR}")
  message(STATUS "  - OS_GEMPAK_INCLUDE_DIR: ${OS_GEMPAK_INCLUDE_DIR}")
#  message(STATUS "  - GEMPAK_LIBRARIES: ${GEMPAK_LIBRARIES}")
endif()

