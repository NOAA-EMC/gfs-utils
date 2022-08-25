list(APPEND fortran_src
  mkgfsawps.f
)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -convert big_endian -fp-model source")
  set_source_files_properties(${fortran_src_free} PROPERTIES COMPILE_FLAGS "-free")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian")
  set_source_files_properties(${fortran_src_free} PROPERTIES COMPILE_FLAGS "-ffree-form")
endif()

set(exe_name mkgfsawps.x)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(
  ${exe_name}
  bacio::bacio_4
  ip::ip_d
  sp::sp_d
  w3nco::w3nco_4
  w3nco::w3nco_d)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(${exe_name} OpenMP::OpenMP_Fortran)
endif()

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})