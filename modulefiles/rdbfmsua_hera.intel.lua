help([[
Build environment for GFS utilities on Hera
]])

prepend_path("MODULEPATH", "/contrib/spack-stack/spack-stack-1.9.1/envs/ue-oneapi-2024.2.1/install/modulefiles/Core")

local cmake_ver=os.getenv("cmake_ver") or "3.27.9"

local stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2024.2.1"
local stack_impi_ver=os.getenv("stack_impi_ver") or "2021.13"

local bufr_ver=os.getenv("bufr_ver") or "12.1.0"
local bacio_ver=os.getenv("bacio_ver") or "2.4.1"
local w3emc_ver=os.getenv("w3emc_ver") or "2.10.0"
local gempak_ver=os.getenv("gempak_ver") or "7.4.2"

load(pathJoin("bufr", bufr_ver))
load(pathJoin("bacio", bacio_ver))
load(pathJoin("w3emc", w3emc_ver))
load(pathJoin("gempak", gempak_ver))

setenv("gfortran_ROOT", "/apps/gnu/gcc-9.2.0")

whatis("Description: GFS utilities environment on Hera with Intel Compilers")
