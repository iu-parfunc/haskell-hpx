#!/bin/bash

# This is a good test script to run locally to validate before committing.
# (It is also the one currently run by our Jenkins CI instance.)

set -xe

docker build -t haskell-hpx:1.0 .

# This would use stack's docker integration, which is a little bit
# sketchy and doesn't work on mac os:
# stack build


# Enable when ready:

# docker run -it haskell-hpx:1.0 bash -c "cd haskell-hpx-src/ && stack test"
