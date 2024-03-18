help([[
Build environment for GFS utilities on WCOSS2
]])

local PrgEnv_intel_ver=os.getenv("PrgEnv_intel_ver") or "8.1.0"
local intel_ver=os.getenv("intel_ver") or "19.1.3.304"
local craype_ver=os.getenv("craype_ver") or "2.7.10"
local cray_mpich_ver=os.getenv("cray_mpich_ver") or "8.1.9"

local jasper_ver=os.getenv("jasper_ver") or "2.0.25"
local zlib_ver=os.getenv("zlib_ver") or "1.2.11"
local libpng_ver=os.getenv("libpng_ver") or "1.6.37"
local netcdf_ver=os.getenv("netcdf_ver") or "4.7.4"

local bufr_ver=os.getenv("bufr_ver") or "11.7.0"
local bacio_ver=os.getenv("bacio_ver") or "2.4.1"
local w3emc_ver=os.getenv("w3emc_ver") or "2.9.2"
local gempak_ver=os.getenv("gempak_ver") or "7.14.1"

load(pathJoin("PrgEnv-intel", PrgEnv_intel_ver))
load(pathJoin("intel", intel_ver))
load(pathJoin("craype", craype_ver))
load(pathJoin("cray-mpich", cray_mpich_ver))

load(pathJoin("jasper", jasper_ver))
load(pathJoin("zlib", zlib_ver))
load(pathJoin("libpng", libpng_ver))

load(pathJoin("bufr", bufr_ver))
load(pathJoin("bacio", bacio_ver))
load(pathJoin("w3emc", w3emc_ver))
load(pathJoin("gempak", gempak_ver))

setenv("gfortran_ROOT", "//opt/cray/pe/gcc/10.3.0/snos")

whatis("Description: GFS utilities environment on WCOSS2 with Intel Compilers")
