#!/usr/bin/env bash

make
./bin/bats --jobs 16 tests/generated/
