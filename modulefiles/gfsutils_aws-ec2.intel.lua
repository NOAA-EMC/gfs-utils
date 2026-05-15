help([[
Build environment for GFS utilities on NOAA Cloud
]])

prepend_path("MODULEPATH", "/opt/spack-stack/envs/ue-oneapi-2024.2.1/install/modulefiles/Core")
prepend_path("MODULEPATH", "/opt/spack-stack/envs/ue-oneapi-2024.2.1/install/modulefiles/intel-oneapi-mpi/2021.13-dsdmcwn/gcc/11.4.0")
prepend_path("MODULEPATH", "/opt/modulefiles")

stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2024.2.1"
stack_impi_ver=os.getenv("stack_impi_ver") or "2021.13"

load(pathJoin("stack-oneapi", stack_oneapi_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))

load("gfsutils_common")

whatis("Description: GFS utilities environment on NOAA Cloud with Intel Compilers")
