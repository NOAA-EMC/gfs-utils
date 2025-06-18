help([[
Build environment for GFS utilities on WCOSS2
]])

local PrgEnv_intel_ver=os.getenv("PrgEnv_intel_ver") or "8.1.0"
local intel_ver=os.getenv("intel_ver") or "19.1.3.304"
local craype_ver=os.getenv("craype_ver") or "2.7.10"
local cray_mpich_ver=os.getenv("cray_mpich_ver") or "8.1.9"
local cmake_ver= os.getenv("cmake_ver") or "3.20.2"

local jasper_ver=os.getenv("jasper_ver") or "2.0.25"
local zlib_ver=os.getenv("zlib_ver") or "1.2.11"
local libpng_ver=os.getenv("libpng_ver") or "1.6.37"
local netcdf_ver=os.getenv("netcdf_ver") or "4.9.2"

local bufr_ver=os.getenv("bufr_ver") or "12.1.0"
local bacio_ver=os.getenv("bacio_ver") or "2.4.1"
local w3emc_ver=os.getenv("w3emc_ver") or "2.9.2"
local ip_ver=os.getenv("ip_ver") or "5.2.0"
local sigio_ver=os.getenv("sigio_ver") or "2.3.2"
local nemsio_ver=os.getenv("nemsio_ver") or "2.5.2"
local nemsiogfs_ver=os.getenv("nemsiogfs_ver") or "2.5.3"
local wrf_io_ver=os.getenv("wrf_io_ver") or "1.2.0"
local g2_ver=os.getenv("g2_ver") or "3.4.5"
local landsfcutil_ver=os.getenv("landsfcutil_ver") or "2.4.1"
local wgrib2_ver=os.getenv("wgrib2_ver") or "2.0.8"
local wgrib2_ver=os.getenv("ncio_ver") or "1.1.2"

load(pathJoin("PrgEnv-intel", PrgEnv_intel_ver))
load(pathJoin("intel", intel_ver))
load(pathJoin("craype", craype_ver))
load(pathJoin("cray-mpich", cray_mpich_ver))
load(pathJoin("cmake", cmake_ver))

load(pathJoin("jasper", jasper_ver))
load(pathJoin("zlib", zlib_ver))
load(pathJoin("libpng", libpng_ver))

load(pathJoin("netcdf-B", netcdf_ver))

load(pathJoin("bufr", bufr_ver))
load(pathJoin("bacio", bacio_ver))
load(pathJoin("w3emc", w3emc_ver))
--load(pathJoin("ip", ip_ver))
-- Temporarily define IP's paths here.
-- TODO: when testing is complete, request an official installation in https://github.com/NOAA-EMC/WCOSS2-requests/issues/11
pushenv("ip_ROOT", pathJoin("/apps/ops/para/libs/intel/19.1.3.304/ip", ip_ver))
pushenv("IP_INC4", pathJoin("/apps/ops/para/libs/intel/19.1.3.304/ip", ip_ver, "include_4"))
pushenv("IP_INCd", pathJoin("/apps/ops/para/libs/intel/19.1.3.304/ip", ip_ver, "include_d"))
pushenv("IP_LIB4", pathJoin("/apps/ops/para/libs/intel/19.1.3.304/ip", ip_ver, "lib64/libip_4.a"))
pushenv("IP_LIBd", pathJoin("/apps/ops/para/libs/intel/19.1.3.304/ip", ip_ver, "lib64/libip_d.a"))
pushenv("ip_VERSION", ip_ver)
load(pathJoin("sigio", sigio_ver))
load(pathJoin("sfcio", sfcio_ver))
load(pathJoin("nemsio", nemsio_ver))
load(pathJoin("wrf_io", wrf_io_ver))
load(pathJoin("g2", g2_ver))
load(pathJoin("landsfcutil", landsfcutil_ver))
load(pathJoin("wgrib2", wgrib2_ver))
load(pathJoin("ncio-A", ncio_ver))

whatis("Description: GFS utilities environment on WCOSS2 with Intel Compilers")
