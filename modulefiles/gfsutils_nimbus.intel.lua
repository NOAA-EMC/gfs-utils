help([[
Build environment for GFS utilities on Nimbus with Intel oneAPI (ifx) compilers
]])

-- Module paths and versions come from global-workflow versions/build.nimbus.ver
prepend_path("MODULEPATH", os.getenv("intel_modulepath") or "/opt/intel/oneapi/modulefiles")
prepend_path("MODULEPATH", os.getenv("stack_core_path") or "/lfs/work/alexander_richert/stack/spack-stack/envs/nco-sci-oneapi-2026.1.0/modules_flat/Core")

local stack_intel_ver=os.getenv("stack_intel_ver") or "2026.1.1"
local stack_intel_mpi_ver=os.getenv("stack_intel_mpi_ver") or "2021.18"

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
