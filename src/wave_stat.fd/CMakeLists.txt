list(APPEND fortran_src
  grbit2.f90
  wave_stat.f90
)

set(exe_name wave_stat.x)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} PRIVATE bacio::bacio_4
                                          w3emc::w3emc_4
                                          g2::g2_d)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
