help([[
Build environment for GFS utilities on Derecho
]])

setenv("LMOD_TMOD_FIND_FIRST","yes")
prepend_path("MODULEPATH", "/lustre/desc1/scratch/epicufsrt/contrib/modulefiles_extra")
prepend_path("MODULEPATH", "/glade/work/epicufsrt/contrib/spack-stack/derecho/spack-stack-1.9.2/envs/ue-oneapi-2024.2.1/install/modulefiles/Core")

local stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2024.2.1"
load(pathJoin("stack-oneapi", stack_oneapi_ver))


load("gfsutils_common")

setenv("CC","mpiicc")
setenv("CXX","mpiicpc")
setenv("FC","mpiifort")

whatis("Description: GFS utilities environment on Derecho with oneapi Compilers")
