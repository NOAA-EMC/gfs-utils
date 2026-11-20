help([[
Build environment for GFS utilities on Nimbus with Intel oneAPI (ifx) compilers
]])

-- Module paths and versions default to the Nimbus Spack stack.
prepend_path("MODULEPATH", os.getenv("intel_modulepath") or "/opt/intel/oneapi/modulefiles")
prepend_path("MODULEPATH", os.getenv("stack_core_path") or "/lfs/work/alexander_richert/stack/spack-stack/envs/nco-sci-oneapi-2026.1.0/modules_flat/Core")

local stack_intel_ver=os.getenv("stack_intel_ver") or "2026.1.1"
local stack_intel_mpi_ver=os.getenv("stack_intel_mpi_ver") or "2021.18"
local cmake_ver=os.getenv("cmake_ver") or "3.31.11"
local libpng_ver=os.getenv("libpng_ver") or "1.6.55"
local netcdf_c_ver=os.getenv("netcdf_c_ver") or "4.9.2"
local netcdf_fortran_ver=os.getenv("netcdf_fortran_ver") or "4.6.1"
local bufr_ver=os.getenv("bufr_ver") or "12.3.0"
local bacio_ver=os.getenv("bacio_ver") or "2.4.1"
local w3emc_ver=os.getenv("w3emc_ver") or "2.13.0"
local ip_ver=os.getenv("ip_ver") or "5.4.0"
local sigio_ver=os.getenv("sigio_ver") or "2.3.3"
local sfcio_ver=os.getenv("sfcio_ver") or "1.4.2"
local nemsio_ver=os.getenv("nemsio_ver") or "2.5.5"
local wrf_io_ver=os.getenv("wrf_io_ver") or "1.3.0"
local ncio_ver=os.getenv("ncio_ver") or "1.1.2"
local g2_ver=os.getenv("g2_ver") or "3.5.1"
local landsfcutil_ver=os.getenv("landsfcutil_ver") or "2.4.2"
local wgrib2_ver=os.getenv("wgrib2_ver") or "3.8.0"

setenv("cmake_ver", cmake_ver)
setenv("libpng_ver", libpng_ver)
setenv("netcdf_c_ver", netcdf_c_ver)
setenv("netcdf_fortran_ver", netcdf_fortran_ver)
setenv("bufr_ver", bufr_ver)
setenv("bacio_ver", bacio_ver)
setenv("w3emc_ver", w3emc_ver)
setenv("ip_ver", ip_ver)
setenv("sigio_ver", sigio_ver)
setenv("sfcio_ver", sfcio_ver)
setenv("nemsio_ver", nemsio_ver)
setenv("wrf_io_ver", wrf_io_ver)
setenv("ncio_ver", ncio_ver)
setenv("g2_ver", g2_ver)
setenv("landsfcutil_ver", landsfcutil_ver)
setenv("wgrib2_ver", wgrib2_ver)

load("tbb")
load("compiler-rt")
load("umf")
load("tcm/1.5")
load(pathJoin("compiler", stack_intel_ver))
load(pathJoin("mpi", stack_intel_mpi_ver))
load(pathJoin("python", "3.11.15"))
load("python-venv/1.0")
load("py-setuptools/73.0.1")
load("py-numpy/1.26.4")
load(pathJoin("openblas", os.getenv("openblas_ver") or "0.3.33"))

load("gfsutils_common")

setenv("CC", "mpiicx")
setenv("CXX", "mpiicpx")
setenv("FC", "mpiifx")

whatis("Description: GFS utilities environment on Nimbus with Intel Compilers")
