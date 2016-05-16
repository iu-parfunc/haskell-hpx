#!/bin/bash

# Takes PREFIX for where to install as the first argument:

PREFIX=$1

if [ "$PREFIX" == "" ]; then
    echo "Must provide prefix as first argument"
    exit 1
fi

set -xe

HPX_VER=3.0.0
HPX_DIR=hpx-${HPX_VER}
HPX_TGZ=${HPX_DIR}.tar.gz

wget http://hpx.crest.iu.edu/release/${HPX_TGZ}
tar xf ${HPX_TGZ}
rm ${HPX_TGZ}

cd ${HPX_DIR}/hpx
./configure --prefix=$PREFIX --enable-shared --enable-debug
make -j
make install
cd ../../

rm -rf ${HPX_DIR}/
