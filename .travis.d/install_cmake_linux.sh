#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o verbose
set -o nounset

CMAKE_VER="${1:-${CMAKE_VER:-3.10.1}}"
CACHE="${2:-${CACHE:-${HOME/.local}}}"
CMAKE_URL_HEAD="https://cmake.org/files/v${CMAKE_VER%.*}"
CMAKE_INSTALLER="cmake-${CMAKE_VER}-Linux-x86_64.sh"

if ! [[ -x "${HOME}/.local/bin/cmake" ]] ; then
    wget "${CMAKE_URL_HEAD}/${CMAKE_INSTALLER}"
    chmod +x "${CMAKE_INSTALLER}"
    "./${CMAKE_INSTALLER}" --prefix="${HOME}/.local" --skip-license accept
    rm "${CMAKE_INSTALLER}"
fi
