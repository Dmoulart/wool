#!/usr/bin/env bash

set -ex

DIR=$PWD/binaryen
JOBS=8

# just use zig oh god
export CC="$(which zig11) cc"
export CXX="$(which zig11) c++"

pushd $DIR
  cmake . -DBUILD_STATIC_LIB=on -DBUILD_TESTS=off -DBUILD_TOOLS=off
  make -j $JOBS
popd