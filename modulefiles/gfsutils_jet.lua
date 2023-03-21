help([[
Build environment for GFS utilities on Jet
]])

prepend_path("MODULEPATH", "/lfs4/HFIP/hfv3gfs/role.epic/hpc-stack/libs/intel-18.0.5.274/modulefiles/stack")

local hpc_ver=os.getenv("hpc_ver") or "1.2.0"
local hpc_intel_ver=os.getenv("hpc_intel_ver") or "18.0.5.274"
local hpc_impi_ver=os.getenv("hpc_impi_ver") or "2018.4.274"
local cmake_ver=os.getenv("cmake_ver") or "3.20.1"

local jasper_ver=os.getenv("jasper_ver") or "2.0.25"
local zlib_ver=os.getenv("zlib_ver") or "1.2.11"
local libpng_ver=os.getenv("libpng_ver") or "1.6.35"

load(pathJoin("hpc", hpc_ver))
load(pathJoin("hpc-intel", hpc_intel_ver))
load(pathJoin("hpc-impi", hpc_impi_ver))
load(pathJoin("cmake", cmake_ver))

load(pathJoin("jasper", jasper_ver))
load(pathJoin("zlib", zlib_ver))
load(pathJoin("libpng", libpng_ver))

load("gfsutils_common")

whatis("Description: GFS utilities environment on Jet with Intel Compilers")
