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
