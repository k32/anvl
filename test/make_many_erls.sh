#!/bin/sh
export N="${1:-1000}"
export DIR="test/dummy/src"

mkdir -p "${DIR}"

for i in $(seq 1 $N); do
    cat > "${DIR}/dummy_$i.erl" <<-EOF
-module(dummy_$i).
EOF
done

cat > "${DIR}/dummy.app.src" <<-EOF
{application, dummy, []}.
EOF
