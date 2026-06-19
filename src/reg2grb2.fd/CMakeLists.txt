list(APPEND fortran_src
reg2grb2.f
regdiag.f
)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -free -convert big_endian -fp-model precise -assume byterecl")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fconvert=big-endian")
endif()

set(exe_name reg2grb2.x)
add_executable(${exe_name} ${fortran_src})

if(TARGET wgrib2::wgrib2_ftn_api)
  set(wgrib2_target wgrib2::wgrib2_ftn_api)
elseif(TARGET wgrib2::wgrib2)
  set(wgrib2_target wgrib2::wgrib2)
else()
  message(FATAL_ERROR "No target for wgrib2 found")
endif()

target_link_libraries(${exe_name} PRIVATE NetCDF::NetCDF_Fortran
                                          landsfcutil::landsfcutil_d
                                          ip::ip_d
                                          bacio::bacio_4
                                          w3emc::w3emc_4
                                          ${wgrib2_target})

if(OpenMP_Fortran_FOUND)
  target_link_libraries(${exe_name} PRIVATE OpenMP::OpenMP_Fortran)
endif()

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
