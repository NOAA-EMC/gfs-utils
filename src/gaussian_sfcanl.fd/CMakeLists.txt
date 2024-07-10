list(APPEND fortran_src
  gaussian_sfcanl.f90
)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O0 -r8 -i4 -g -traceback -ftrapuv")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-real-8")
endif()

set(exe_name gaussian_sfcanl.x)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} PRIVATE NetCDF::NetCDF_Fortran
                                          bacio::bacio_4
                                          sp::sp_4
                                          w3emc::w3emc_d)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
