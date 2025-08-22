#!/bin/bash
set -e

cd "$(dirname "$0")"

echo "[*] Building parser..."
make clean >/dev/null 2>&1 || true
make

mkdir -p output

for testfile in test/*; do
    if [[ -f "$testfile" ]]; then
        base=$(basename "$testfile")
        outfile="output/${base%.*}.txt"
        echo "[*] Running on $base -> $outfile"
        ./parser < "$testfile" > "$outfile" 2>&1
    fi
done

echo "[*] All testcases executed. Outputs are in ./output/"
