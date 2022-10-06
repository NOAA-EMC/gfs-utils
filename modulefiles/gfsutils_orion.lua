help([[
Build environment for GFS utilities on Orion
]])

prepend_path("MODULEPATH", "/apps/contrib/NCEP/libs/hpc-stack/modulefiles/stack")

local hpc_ver=os.getenv("hpc_ver") or "1.1.0"
local hpc_intel_ver=os.getenv("hpc_intel_ver") or "2018.4"
local hpc_impi_ver=os.getenv("hpc_impi_ver") or "2018.4"
local cmake_ver=os.getenv("cmake_ver") or "3.22.1"

local jasper_ver=os.getenv("jasper_ver") or "2.0.25"
local zlib_ver=os.getenv("zlib_ver") or "1.2.11"
local libpng_ver=os.getenv("libpng_ver") or "1.6.35"

load(pathJoin("hpc", hpc_ver))
load(pathJoin("hpc-intel", hpc_intel_ver))
load(pathJoin("hpc-impi", hpc_impi_ver))
load(pathJoin("cmake", cmake_ver))

load(pathJoin("jasper", jasper_ver))
load(pathJoin("zlib", zlib_ver))
load(pathJoin("png", libpng_ver))

load("gfsutils_common")

whatis("Description: GFS utilities environment on Orion with Intel Compilers")
