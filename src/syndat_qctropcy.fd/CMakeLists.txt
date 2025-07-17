list(APPEND fortran_src
  qctropcy.f
)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -i4 -r8 -assume byterecl -assume noold_ldout_format")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-integer-4 -fdefault-real-8")
endif()

set(exe_name syndat_qctropcy.x)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} PRIVATE w3emc::w3emc_4)

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
