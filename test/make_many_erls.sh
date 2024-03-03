#!/bin/sh
export N="${1:-1000}"

mkdir "src/dummy"

for i in $(seq 1 $N); do
    cat > "src/dummy/dummy_$i.erl" <<-EOF
-module(dummy_$i).
EOF
done
