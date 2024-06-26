#!/bin/sh
set -xe

case "${1:-}" in
    clean)
        rm -rf _anvl_build
        ;;
    *)
        ;;
esac

## Stage 1:
# Build the most essential modules:
export STAGE1_DIR=_anvl_build/stage1
mkdir -p "${STAGE1_DIR}/ebin"
erlc -D BOOTSTRAP -o "${STAGE1_DIR}/ebin" -I include vendor/typerefl/src/typerefl.erl src/anvl_resource.erl src/anvl_condition.erl src/anvl_lib.erl src/anvl_erlc.erl src/anvl_sup.erl src/anvl_app.erl

## Stage 2:
# Build the modules that are needed for compiling the escript:
erl -config bootstrap -noshell -pz "${STAGE1_DIR}/ebin" -run anvl_app bootstrap

## Stage 3:
time _anvl_build/stage2/anvl @escript
