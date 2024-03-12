cmake_minimum_required(VERSION 3.19)

project(gfsutils
        VERSION 1.0.0
        LANGUAGES Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
set(CMAKE_DIRECTORY_LABELS ${PROJECT_NAME})

include(GNUInstallDirs)

# Build type
if(NOT CMAKE_BUILD_TYPE MATCHES "^(Debug|Release|RelWithDebInfo|MinSizeRel)$")
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE
      "Release"
      CACHE STRING "Choose the type of build." FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
                                               "MinSizeRel" "RelWithDebInfo")
endif()

if(NOT CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU|Intel)$")
  message(WARNING "${CMAKE_Fortran_COMPILER_ID} is not supported.")
endif()

# User options.
option(OPENMP "use OpenMP threading" ON)

# Find packages.
find_package(MPI REQUIRED)
find_package(NetCDF REQUIRED Fortran)
if(OPENMP)
  find_package(OpenMP REQUIRED COMPONENTS Fortran)
endif()

# NCEPLibs dependencies.
find_package(bacio REQUIRED)
find_package(w3emc REQUIRED)
find_package(sp REQUIRED)
find_package(ip REQUIRED)
find_package(ncio REQUIRED)
find_package(nemsio REQUIRED)
find_package(sigio REQUIRED)
find_package(g2 REQUIRED)
find_package(bufr REQUIRED)
find_package(landsfcutil REQUIRED)
find_package(wgrib2 REQUIRED)
# rdbfmsua.fd needs gempak and gfortran, but
# make them optional and skip rdbfmsua building if they are not found.
find_package(gempak)
find_package(gfortran)

# See https://github.com/NOAA-EMC/NCEPLIBS-nemsio/pull/22
target_link_libraries(nemsio::nemsio INTERFACE w3emc::w3emc_d bacio::bacio_4)

# Set compiler flags.
if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -traceback")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -check all -ftrapuv")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -fbacktrace")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -ggdb -fno-unsafe-math-optimizations -frounding-math -fsignaling-nans -ffpe-trap=invalid,zero,overflow -fbounds-check")
endif()

# Build and install code.
add_subdirectory(src)

# Install utility scripts.
add_subdirectory(ush)
