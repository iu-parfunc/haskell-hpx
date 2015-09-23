#!/bin/bash

# Takes PREFIX for where to install as the first argument:

PREFIX=$1

set -xe

wget http://hpx.crest.iu.edu/release/HPX_Release_v1.0.0.tar.gz
tar xf HPX_Release_v1.0.0.tar.gz
rm HPX_Release_v1.0.0.tar.gz

cd HPX_Release_v1.0.0/hpx
./configure --prefix=$PREFIX
make -j
make install
cd ../../

rm -rf HPX_Release_v1.0.0/
