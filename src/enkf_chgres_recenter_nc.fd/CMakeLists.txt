list(APPEND fortran_src
driver.f90
input_data.f90
interp.F90
output_data.f90
setup.f90
utils.f90
)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model precise")
endif()

set(exe_name enkf_chgres_recenter_nc.x)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} PRIVATE NetCDF::NetCDF_Fortran
                                          bacio::bacio_4
                                          ip::ip_4
                                          sp::sp_4
                                          w3emc::w3emc_4
                                          ncio::ncio)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(${exe_name} PRIVATE OpenMP::OpenMP_Fortran)
endif()

if(ip_VERSION VERSION_GREATER_EQUAL 4.0.0)
   target_compile_definitions(${exe_name} PRIVATE "IP_V4")
endif()

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
