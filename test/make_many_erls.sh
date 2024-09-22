#!/bin/sh
export N="${1:-10000}"
export DIR="dummy/src"

mkdir -p "${DIR}"

for i in $(seq 1 $N); do
    cat > "${DIR}/dummy_$i.erl" <<-EOF
-module(dummy_$i).
EOF
done

cat > "${DIR}/dummy.app.src" <<-EOF
{application, dummy, []}.
EOF
