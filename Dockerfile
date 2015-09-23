# FROM ubuntu:15.10
FROM fpco/stack-build:lts-3.4

RUN apt-get update && apt-get install -y \
     gcc g++ make wget

# ADD http://hpx.crest.iu.edu/release/HPX_Release_v1.3.0.tar.gz ./

COPY .grab_hpx_1.3.sh haskell-hpx-src/
RUN cd haskell-hpx-src/ && ./.grab_hpx_1.3.sh /usr

# First, build the dependencies and cache them in a separate layer.
# Changing the source code under this directory should not trigger
# a reexecution at this layer.
COPY haskell-hpx.cabal stack.yaml haskell-hpx-src/
RUN cd haskell-hpx-src/ && \
    stack install --only-dependencies


# Second, build the actual project:
COPY . haskell-hpx-src/
# Here we do the build directly INSIDE the container.
RUN cd haskell-hpx-src/ && \
    stack build
