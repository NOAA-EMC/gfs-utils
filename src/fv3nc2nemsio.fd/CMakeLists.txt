list(APPEND fortran_src
constants.f90
fv3_main.f90
fv3_module.f90
kinds.f90
)

set(exe_name fv3nc2nemsio.x)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} PRIVATE NetCDF::NetCDF_Fortran
                                          bacio::bacio_4
                                          w3emc::w3emc_d
                                          nemsio::nemsio)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
