#!/usr/bin/env bash

# Command from
# https://developer.mozilla.org/en-US/docs/WebAssembly/existing_C_to_wasm
# to create a .js and .wasm file
emsdk/upstream/emscripten/em++ \
    -O3 -s WASM=1 -s EXTRA_EXPORTED_RUNTIME_METHODS='["cwrap"]' \
    -Iboost_1_75_0 -Iodeint-v2/include -o solver.js solver.cpp
