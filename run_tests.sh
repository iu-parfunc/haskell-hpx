#!/bin/bash

set -xe

STACK=stack

docker build -t haskell-hpx:1.0 .

$STACK build
