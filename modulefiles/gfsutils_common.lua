help([[
Load common modules to build GFS utilities on all machines
]])

local netcdf_c_ver=os.getenv("netcdf_c_ver") or "4.9.2"
local netcdf_fortran_ver=os.getenv("netcdf_fortran_ver") or "4.6.1"

local bufr_ver=os.getenv("bufr_ver") or "11.7.0"
local bacio_ver=os.getenv("bacio_ver") or "2.4.1"
local w3emc_ver=os.getenv("w3emc_ver") or "2.10.0"
local sp_ver=os.getenv("sp_ver") or "2.5.0"
local ip_ver=os.getenv("ip_ver") or "4.3.0"
local sigio_ver=os.getenv("sigio_ver") or "2.3.2"
local nemsio_ver=os.getenv("nemsio_ver") or "2.5.4"
local nemsiogfs_ver=os.getenv("nemsiogfs_ver") or "2.5.3"
local wrf_io_ver=os.getenv("wrf_io_ver") or "1.2.0"
local ncio_ver=os.getenv("ncio_ver") or "1.1.2"
local g2_ver=os.getenv("g2_ver") or "3.4.5"
local landsfcutil_ver=os.getenv("landsfcutil_ver") or "2.4.1"
local wgrib2_ver=os.getenv("wgrib2_ver") or "2.0.8"
local libpng_ver=os.getenv("libpng_ver") or "1.6.37"

load(pathJoin("libpng", libpng_ver))

load(pathJoin("netcdf-c", netcdf_c_ver))
load(pathJoin("netcdf-fortran", netcdf_fortran_ver))

load(pathJoin("bufr", bufr_ver))
load(pathJoin("bacio", bacio_ver))
load(pathJoin("w3emc", w3emc_ver))
load(pathJoin("sp", sp_ver))
load(pathJoin("ip", ip_ver))
load(pathJoin("sigio", sigio_ver))
load(pathJoin("sfcio", sfcio_ver))
load(pathJoin("nemsio", nemsio_ver))
load(pathJoin("wrf-io", wrf_io_ver))
load(pathJoin("ncio", ncio_ver))
load(pathJoin("g2", g2_ver))
load(pathJoin("landsfcutil", landsfcutil_ver))
load(pathJoin("wgrib2", wgrib2_ver))
