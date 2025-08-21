#!/bin/bash

set -eu
# Get the root of the cloned GFS-utils directory
#shellcheck disable=SC2155
readonly DIR_ROOT=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )/.." && pwd -P)

# User Options
BUILD_TYPE=${BUILD_TYPE:-"Release"}
CMAKE_OPTS=${CMAKE_OPTS:-}
COMPILER=${COMPILER:-"intel"}
BUILD_DIR=${BUILD_DIR:-"${DIR_ROOT}/build"}
INSTALL_PREFIX=${INSTALL_PREFIX:-"${DIR_ROOT}/install"}

#==============================================================================#

# Detect machine (sets MACHINE_ID)
source "${DIR_ROOT}/ush/detect_machine.sh"

# Load modules
source "${DIR_ROOT}/ush/module-setup.sh"
module use "${DIR_ROOT}/modulefiles"
module load "gfsutils_${MACHINE_ID}.${COMPILER}"
module list

# Collect BUILD Options
CMAKE_OPTS+=" -DCMAKE_BUILD_TYPE=${BUILD_TYPE}"

# Install destination for built executables, libraries, CMake Package config
CMAKE_OPTS+=" -DCMAKE_INSTALL_PREFIX=${INSTALL_PREFIX}"

# Re-use or create a new BUILD_DIR (Default: create new BUILD_DIR)
[[ ${BUILD_CLEAN:-"YES"} =~ [yYtT] ]] && rm -rf "${BUILD_DIR}"
mkdir -p "${BUILD_DIR}" && cd "${BUILD_DIR}"

# Configure, build, install
set -x
# shellcheck disable=SC2086
cmake ${CMAKE_OPTS} "${DIR_ROOT}"
make -j "${BUILD_JOBS:-8}" VERBOSE="${BUILD_VERBOSE:-}"
make install
set +x

# Build rdbfmsua.x with makefile separately due to GEMPAK issues in cmake
# Load modules
source "${DIR_ROOT}/ush/module-setup.sh"
module use "${DIR_ROOT}/modulefiles"
if module load "rdbfmsua_${MACHINE_ID}.${COMPILER}" &> /dev/null; then
  module list
else
  if [[ -f "${DIR_ROOT}/modulefiles/rdbfmsua_${MACHINE_ID}.${COMPILER}.lua" ]]; then
    echo "WARNING: Unable to load modulefile for 'rdbfmsua' on '${MACHINE_ID}'."
    set +e
    module load "rdbfmsua_${MACHINE_ID}.${COMPILER}"
    set -e
  else
    echo "${DIR_ROOT}/modulefiles/rdbfmsua_${MACHINE_ID}.${COMPILER}.lua does not exist!"
  fi
  echo "  Skipping building 'rdbfmsua.x'"
  exit 0
fi

cd "${DIR_ROOT}/src/rdbfmsua.fd"
export MACHINE_ID
export COMPILER
export PREFIX="${INSTALL_PREFIX}"
make clean
make VERBOSE="${BUILD_VERBOSE:-}"
make install VERBOSE="${BUILD_VERBOSE:-}"

exit
