help([[
Build environment for GFS utilities on NOAA Cloud
]])

prepend_path("MODULEPATH", "/contrib/spack-stack-rocky8/spack-stack-1.6.0/envs/gsi-addon-env/install/modulefiles/Core")
prepend_path("MODULEPATH", "/apps/modules/modulefiles")

local stack_intel_ver=os.getenv("stack_intel_ver") or "2021.10.0"
local stack_impi_ver=os.getenv("stack_impi_ver") or "2021.10.0"
local cmake_ver=os.getenv("cmake_ver") or "3.23.1"

load("gnu")
load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
unload("gnu")

load(pathJoin("cmake", cmake_ver))

load("gfsutils_common")

whatis("Description: GFS utilities environment on NOAA Cloud with Intel Compilers")
