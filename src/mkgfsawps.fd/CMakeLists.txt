list(APPEND fortran_src
  mkgfsawps.F
)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -convert big_endian -fp-model source")
  set_source_files_properties(${fortran_src_free} PROPERTIES COMPILE_FLAGS "-free")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian")
  set_source_files_properties(${fortran_src_free} PROPERTIES COMPILE_FLAGS "-ffree-form")
endif()

if(ip_VERSION VERSION_GREATER_EQUAL 4.0.0)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DIP_V4")
endif()

set(exe_name mkgfsawps.x)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} PRIVATE bacio::bacio_4
                                          ip::ip_4
                                          sp::sp_d
                                          w3emc::w3emc_4)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(${exe_name} PRIVATE OpenMP::OpenMP_Fortran)
endif()

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
