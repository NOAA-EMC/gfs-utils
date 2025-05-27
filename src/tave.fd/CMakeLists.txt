list(APPEND fortran_src
  tave.f
)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -i4 -r8")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-real-8")
endif()

set(exe_name tave.x)
add_executable(${exe_name} ${fortran_src})
if(g2_VERSION VERSION_LESS 3.4.9)
  target_link_libraries(${exe_name} PRIVATE bacio::bacio_4
                                          w3emc::w3emc_d
                                          g2::g2_d)
else()
  target_link_libraries(${exe_name} PRIVATE bacio::bacio_4
                                          g2::g2_d)
endif()

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
