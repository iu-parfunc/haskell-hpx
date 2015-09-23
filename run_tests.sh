#!/bin/bash

set -xe

docker build -t haskell-hpx:1.0 .

# This would use stack's docker integration, which is a little bit
# sketchy and doesn't work on mac os:
# stack build


# Enable when ready:

# docker run -it haskell-hpx:1.0 bash -c "cd haskell-hpx-src/ && stack test"
