#!/bin/sh
# Compares build/leu against recorded output for the files in test_data.
#
# The *.haskell.txt files were produced by the Haskell binary.  It decoded the
# response twice and turned every non-ASCII character into U+FFFD, so the C
# output is folded the same way before comparing: one replacement character per
# codepoint means the column arithmetic is unaffected, and the comparison still
# proves both versions agree on every space, line break and ordering.
#
# similar_promiss.txt has no Haskell counterpart - that file made the Haskell
# version abort with "content is not a CElem".
set -eu

cd "$(dirname "$0")/.."

leu=build/leu
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
status=0

fold_to_ascii() {
    python3 -c 'import sys
data = sys.stdin.buffer.read().decode("utf-8", "replace")
folded = "".join(c if ord(c) < 128 else "�" for c in data)
sys.stdout.buffer.write(folded.encode("utf-8"))'
}

compare() {
    name=$1
    golden=$2
    filter=$3

    if [ "$filter" = fold ]; then
        $leu -f "test_data/$name.xml" | fold_to_ascii >"$tmp/$name"
    else
        $leu -f "test_data/$name.xml" >"$tmp/$name"
    fi

    if diff -u "$golden" "$tmp/$name" >"$tmp/diff"; then
        echo "ok   golden $name"
    else
        echo "FAIL golden $name"
        cat "$tmp/diff"
        status=1
    fi
}

compare query_for_hello tests/golden/query_for_hello.haskell.txt fold
compare offspring       tests/golden/offspring.haskell.txt       fold
compare similar_promiss tests/golden/similar_promiss.txt         exact

echo
if [ $status -eq 0 ]; then
    echo "all golden files match"
else
    echo "GOLDEN FILES DIFFER"
fi
exit $status
