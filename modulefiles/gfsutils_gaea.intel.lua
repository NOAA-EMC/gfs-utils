help([[
  This module loads libraries required for building and running GFS UTILS 
  on the NOAA RDHPC machine Gaea C5 using Intel-2023.1.0.
]])

whatis([===[Loads libraries needed for building the UFS Weather Model on Gaea ]===])

--Compiler and MPI versions
stack_intel_ver=os.getenv("stack_intel_ver") or "2023.1.0"
stack_cray_mpich_ver=os.getenv("stack_cray_mpich_ver") or "8.1.25"

--Spack-stack root path and environment name
stack_root=os.getenv("stack_root") or "/usw/spack-stack/c5"
stack_env=os.getenv(stack_env) or "ue-intel-" .. stack_intel_ver

--Stack compiler and MPI modules to load
stack_compiler=os.getenv("stack_compiler") or pathJoin("stack-intel", stack_intel_ver)
stack_mpi=os.getenv("stack_mpi") or pathJoin("stack-cray-mpich", stack_cray_mpich_ver)

load("gfsutils_common")

unload("darshan-runtime")
unload("cray-libsci")

setenv("CC","cc")
setenv("CXX","CC")
setenv("FC","ftn")
setenv("CMAKE_Platform","gaea.intel")
