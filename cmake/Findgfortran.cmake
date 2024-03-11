find_library(
  GFORTRAN_LIBRARY
  NAMES libgfortran.so
  HINTS ${GFORTRAN_LIBRARY_DIRS}
        /apps/gnu/gcc-9.2.0
        /apps/gcc-8/gcc-8.3.0
        /opt/cray/pe/gcc/10.3.0/snos
  PATH_SUFFIXES lib lib64
  DOC "Path to GFORTRAN_LIBRARY"
  )

mark_as_advanced(GFORTRAN_LIBRARY)

message(DEBUG "[Findgfortran.cmake]: creating target gfortran::gfortran")
add_library(gfortran::gfortran UNKNOWN IMPORTED)

message(DEBUG "GFORTRAN_LIBRARY = ${GFORTRAN_LIBRARY}")

message(DEBUG "[Findgfortran.cmake]: linking with gfortran.a")
set_target_properties(gfortran::gfortran PROPERTIES IMPORTED_LOCATION ${GFORTRAN_LIBRARY})

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(gfortran
  REQUIRED_VARS GFORTRAN_LIBRARY
  )

if(gfortran_FOUND AND NOT gfortran_FIND_QUIETLY)
  message(STATUS "Findgfortran:")
  message(STATUS "  - GFORTRAN_LIBRARY: ${GFORTRAN_LIBRARY}")
endif()
