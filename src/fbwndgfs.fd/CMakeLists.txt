list(APPEND fortran_src
  fbwndgfs.F
)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -r8 -i8 -convert big_endian -assume byterecl")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-real-8 -fdefault-integer-8 -fconvert=big-endian")
endif()

if(w3emc_VERSION VERSION_GREATER_EQUAL 2.10)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DW3EMC_210")
endif()

set(exe_name fbwndgfs.x)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} PRIVATE bacio::bacio_8
                                          ip::ip_8
                                          sp::sp_8
                                          w3emc::w3emc_8)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
