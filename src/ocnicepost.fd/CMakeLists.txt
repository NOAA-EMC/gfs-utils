list(APPEND fortran_src
  arrays_mod.F90
  init_mod.F90
  masking_mod.F90
  ocnicepost.F90
  utils_mod.F90
)

set(exe_name ocnicepost.x)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} PRIVATE NetCDF::NetCDF_Fortran)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
