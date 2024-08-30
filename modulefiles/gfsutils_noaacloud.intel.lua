help([[
Build environment for GFS utilities on NOAA Cloud
]])

prepend_path("MODULEPATH", "/contrib/spack-stack-rocky8/spack-stack-1.6.0/envs/gsi-addon-env/install/modulefiles/Core")
prepend_path("MODULEPATH", "/apps/modules/modulefiles")
prepend_path("PATH", "/contrib/EPIC/bin")
load("gnu")
load("stack-intel")
load("stack-intel-oneapi-mpi")

stack_intel_ver=os.getenv("stack_intel_ver") or "2021.10.0"
load(pathJoin("stack-intel", stack_intel_ver))

stack_impi_ver=os.getenv("stack_impi_ver") or "2021.10.0"
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
unload("gnu")

local cmake_ver=os.getenv("cmake_ver") or "3.23.1"

load(pathJoin("cmake", cmake_ver))

load("gfsutils_common")

whatis("Description: GFS utilities environment on NOAA Cloud with Intel Compilers")
