# syntax=docker/dockerfile:1

FROM ocaml/opam:debian

# install dependencies

RUN sudo apt-get install liblapacke-dev libopenblas-dev pkg-config zlib1g-dev -y

# install owl with arm specific flags

RUN opam install dune
RUN opam install ctypes
RUN EIGEN_FLAGS="-O3 -Ofast -march=native -funroll-loops -ffast-math" EIGENCPP_OPTFLAGS="-O3 -Ofast -march=native -funroll-loops -ffast-math" opam install eigen
RUN opam install npy
RUN opam install dune-configurator
RUN opam install base
RUN opam install conf-openblas
RUN opam install stdio
RUN OWL_CFLAGS="-g -O3 -Ofast -march=native -funroll-loops -ffast-math -DSFMT_MEXP=19937 -fno-strict-aliasing -Wno-tautological-constant-out-of-range-compare" opam install owl

# setup entry point

WORKDIR /app
ENTRYPOINT /bin/bash