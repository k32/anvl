#!/bin/sh
set -xe

## Stage 1:
# Build the most essential modules:
export STAGE1_DIR=_anvl_build/stage1
mkdir -p "${STAGE1_DIR}/ebin"
erlc -o "${STAGE1_DIR}/ebin" src/anvl_condition.erl src/anvl_erlc.erl src/anvl_sup.erl src/anvl_app.erl

## Stage 2:
# Build the modules that are needed for compiling plugins:
erl -config bootstrap -noshell -pz "${STAGE1_DIR}/ebin" -run anvl_app bootstrap 2
