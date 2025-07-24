help([[
Build environment for GFS utilities in a container 
]])

prepend_path("MODULEPATH", "/opt/spack-stack/spack-stack-1.9.1/envs/unified-env/install/modulefiles/Core")
prepend_path("MODULEPATH", "/opt/spack-stack/spack-stack-1.9.1/envs/unified-env/install/modulefiles/intel-oneapi-mpi/2021.13-argr3sd/oneapi/2024.2.0")
prepend_path("MODULEPATH", "/opt/spack-stack/spack-stack-1.9.1/envs/unified-env/install/modulefiles/oneapi/2024.2.0")

stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2024.2.0"
stack_impi_ver=os.getenv("stack_impi_ver") or "2021.13"

load(pathJoin("stack-oneapi", stack_oneapi_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))

load("gfsutils_common")

whatis("Description: GFS utilities environment in container with Intel Compilers")
