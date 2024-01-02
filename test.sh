#!/usr/bin/env bash

zig11 build && ./zig-out/bin/wool ./test.wool && wat2wasm out.wat