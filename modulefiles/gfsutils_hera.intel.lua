help([[
Build environment for GFS utilities on Hera
]])

prepend_path("MODULEPATH", '/scratch1/NCEPDEV/nems/Alexander.Richert/spack-stack-py311-aug24/envs/test/install/modulefiles/Core')

local stack_intel_ver=os.getenv("stack_intel_ver") or "2021.5.0"
local stack_impi_ver=os.getenv("stack_impi_ver") or "2021.5.1"
local cmake_ver=os.getenv("cmake_ver") or "3.27.9"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
load(pathJoin("cmake", cmake_ver))

load("gfsutils_common")

whatis("Description: GFS utilities environment on Hera with Intel Compilers")
