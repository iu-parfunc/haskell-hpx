# FROM ubuntu:15.10
FROM fpco/stack-build:lts-3.4

RUN apt-get update && apt-get install -y \
     gcc g++ make wget

# ADD http://hpx.crest.iu.edu/release/HPX_Release_v1.0.0.tar.gz ./

RUN wget http://hpx.crest.iu.edu/release/HPX_Release_v1.0.0.tar.gz && \
    tar xvf HPX_Release_v1.0.0.tar.gz && \
    rm HPX_Release_v1.0.0.tar.gz && \
    cd HPX_Release_v1.0.0/hpx && \
    ./configure --prefix=/usr && \
    make -j && \
    make install && \
    cd / && rm -rf /HPX_Release_v1.0.0/


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
