#!/usr/bin/env bash

# Download and extract boost. We need only
# the headers, so there's no need to build it
wget https://dl.bintray.com/boostorg/release/1.75.0/source/boost_1_75_0.tar.gz
tar -xf boost_1_75_0.tar.gz
rm boost_1_75_0.tar.gz

# Download odeint-v2. There's no need to build
# it because odeint is a header-only library
git clone git://github.com/headmyshoulder/odeint-v2

# Download and install emscripten
git clone https://github.com/emscripten-core/emsdk.git
cd emsdk
git pull
./emsdk install latest
./emsdk activate latest
cd ..
