#!/usr/bin/env bash

zig11 build && ./zig-out/bin/wool ./test.wool && wat2wasm out.wat   && mv out.wasm test/test.wasm  && bun run ./test/index.js