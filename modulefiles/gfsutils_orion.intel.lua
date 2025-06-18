help([[
Build environment for GFS utilities on Orion
]])

prepend_path("MODULEPATH", "/apps/contrib/spack-stack/spack-stack-1.9.1/envs/ue-oneapi-2024.1.0/install/modulefiles/Core")

local stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2024.1.0"
local stack_impi_ver=os.getenv("stack_impi_ver") or "2021.13"

load(pathJoin("stack-oneapi", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))

load("gfsutils_common")

setenv("CC","mpiicc")
setenv("CXX","mpiicpc")
setenv("FC","mpiifort")

whatis("Description: GFS utilities environment on Orion with Intel Compilers")
