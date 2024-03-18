help([[
Build environment for GFS utilities on Hera
]])

prepend_path("MODULEPATH", "/scratch1/NCEPDEV/nems/role.epic/spack-stack/spack-stack-1.6.0/envs/gsi-addon-dev/install/modulefiles/Core")

local stack_intel_ver=os.getenv("stack_intel_ver") or "2021.5.0"
local stack_impi_ver=os.getenv("stack_impi_ver") or "2021.5.1"
local cmake_ver=os.getenv("cmake_ver") or "3.23.1"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
load(pathJoin("cmake", cmake_ver))

load("gfsutils_common")

local gempak_ver=os.getenv("gempak_ver") or "7.4.2"
-- load(pathJoin("gempak", gempak_ver))

-- Used in rdbfmsua.f
setenv("gfortran_ROOT", "/apps/gnu/gcc-9.2.0")

whatis("Description: GFS utilities environment on Hera with Intel Compilers")
