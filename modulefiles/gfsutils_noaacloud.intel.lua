help([[
Build environment for GFS utilities on NOAA Cloud
]])

prepend_path("MODULEPATH", "/contrib/spack-stack-rocky8/spack-stack-1.9.2/envs/ue-oneapi-2024.2.1/install/modulefiles/Core")
prepend_path("MODULEPATH", "/contrib/spack-stack-rocky8/spack-stack-1.9.2/envs/ue-oneapi-2024.2.1/install/modulefiles/intel-oneapi-mpi/2021.13-mg3hegm/gcc/13.2.0")
prepend_path("MODULEPATH", "/apps/modules/modulefiles")

gnu_ver=os.getenv("gnu_ver") or "14.2.0"
stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2024.2.1"
stack_impi_ver=os.getenv("stack_impi_ver") or "2021.13"
cmake_ver=os.getenv("cmake_ver") or "3.28.1"

load(pathJoin("gnu", gnu_ver))
load(pathJoin("stack-oneapi", stack_oneapi_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
load(pathJoin("cmake", cmake_ver))

setenv("sigio_ver", "2.3.3")
setenv("landsfcutil_ver", "2.4.2")
setenv("bufr_ver", "12.1.0")
setenv("ip_ver", "5.1.0")
setenv("g2_ver", "3.5.1")
setenv("wgrib2_ver", "3.6.0")

load("gfsutils_common")

whatis("Description: GFS utilities environment on NOAA Cloud with Intel Compilers")
