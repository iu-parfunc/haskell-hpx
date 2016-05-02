#!/bin/bash

# This is just a trivial script to switch into the right directory.

source $stdenv/setup
echo "Building HPX runtime."
set -xe

tar xf $src
cd ./hpx-2.2.0/hpx/

./configure --prefix=$out --enable-shared CFLAGS="-O0 -g" --enable-debug
make -j
make install
