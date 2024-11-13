help([[
Build environment for GFS utilities in a container 
]])

prepend_path("MODULEPATH", "/opt/spack-stack/spack-stack-1.8.0/envs/unified-env/install/modulefiles/Core")

local stack_intel_ver=os.getenv("stack_intel_ver") or "2021.10.0"
local stack_impi_ver=os.getenv("stack_impi_ver") or "2021.12.1"
local cmake_ver=os.getenv("cmake_ver") or "3.27.9"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
load(pathJoin("cmake", cmake_ver))

load("gfsutils_common")

whatis("Description: GFS utilities environment in container with Intel Compilers")
