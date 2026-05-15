help([[
Build environment for GFS utilities on NOAA Cloud
]])

prepend_path("MODULEPATH", "/opt/spack-stack/envs/ue-oneapi-2024.2.1/install/modulefiles/Core")
prepend_path("MODULEPATH", "/opt/spack-stack/envs/ue-oneapi-2024.2.1/install/modulefiles/intel-oneapi-mpi/2021.13-dsdmcwn/gcc/11.4.0")
prepend_path("MODULEPATH", "/opt/modulefiles")

stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2024.2.1"
stack_impi_ver=os.getenv("stack_impi_ver") or "2021.13"

load(pathJoin("stack-oneapi", stack_oneapi_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))

local bufr_ver=os.getenv("bufr_ver") or "12.1.0"
local bacio_ver=os.getenv("bacio_ver") or "2.4.1"
local w3emc_ver=os.getenv("w3emc_ver") or "2.10.0"
local gempak_ver=os.getenv("gempak_ver") or "7.4.2"

load(pathJoin("bufr", bufr_ver))
load(pathJoin("bacio", bacio_ver))
load(pathJoin("w3emc", w3emc_ver))
load(pathJoin("gempak", gempak_ver))

-- setenv("gfortran_ROOT", "/apps/gnu/gcc-9.2.0")

whatis("Description: GFS utilities environment on NOAA Cloud with Intel Compilers")
