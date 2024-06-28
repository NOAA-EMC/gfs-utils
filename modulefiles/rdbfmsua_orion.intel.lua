help([[
Build environment for GFS utilities on Orion
]])

-- Spack Stack installation specs
local ss_dir="/work/noaa/epic/role-epic/spack-stack/orion"
local ss_ver=os.getenv("stack_ver") or "1.6.0"
local ss_env=os.getenv("stack_env") or "gsi-addon-env-rocky9"

prepend_path("MODULEPATH", pathJoin(ss_dir, "spack-stack-" .. ss_ver, "envs", ss_env, "install/modulefiles/Core"))

local stack_intel_ver=os.getenv("stack_intel_ver") or "2021.9.0"
local stack_impi_ver=os.getenv("stack_impi_ver") or "2021.9.0"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))

local bufr_ver=os.getenv("bufr_ver") or "11.7.0"
local bacio_ver=os.getenv("bacio_ver") or "2.4.1"
local w3emc_ver=os.getenv("w3emc_ver") or "2.10.0"
local gempak_ver=os.getenv("gempak_ver") or "7.5.1"

load(pathJoin("bufr", bufr_ver))
load(pathJoin("bacio", bacio_ver))
load(pathJoin("w3emc", w3emc_ver))
load(pathJoin("gempak", gempak_ver))

setenv("gfortran_ROOT", "/apps/gcc-8/gcc-8.3.0")

whatis("Description: GFS utilities environment on Orion with Intel Compilers")
