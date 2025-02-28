list(APPEND fortran_src
  init_mod.F90
  arrays_mod.F90
  utils_mod.F90
  masking_mod.F90
  ocnicepost.F90

)

set(exe_name ocnicepost.x)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} PRIVATE NetCDF::NetCDF_Fortran
                                          g2::g2_d
                                          bacio::bacio_4)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
