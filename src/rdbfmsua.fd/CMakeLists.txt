list(APPEND fortran_src
  rdbfmsua.f
)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^(Intel)$")
  set_source_files_properties(${fortran_src_free} PROPERTIES COMPILE_FLAGS "-free")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^(GNU)$")
  set_source_files_properties(${fortran_src_free} PROPERTIES COMPILE_FLAGS "-ffree-form")
endif()

set(exe_name rdbfmsua.x)
add_executable(${exe_name} ${fortran_src})
target_link_libraries(${exe_name} PRIVATE bufr::bufr_4
                                          w3emc::w3emc_4
                                          gempak::gempak
                                          gfortran::gfortran)
#target_link_libraries(${exe_name} PRIVATE "-lgfortran")

install(TARGETS ${exe_name} RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
