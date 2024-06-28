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
local cmake_ver=os.getenv("cmake_ver") or "3.23.1"

load(pathJoin("stack-intel", stack_intel_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
load(pathJoin("cmake", cmake_ver))

load("gfsutils_common")

whatis("Description: GFS utilities environment on Orion with Intel Compilers")
