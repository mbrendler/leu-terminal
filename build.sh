#!/bin/sh
# Build leu.  The whole program compiles in well under a second, so there is
# no object cache and no incremental build - just one compiler call.
#
#   ./build.sh        build build/leu
#   ./build.sh test   build and run the tests
#
# macOS needs nothing but the Xcode Command Line Tools: libcurl and libxml2
# are part of the SDK.  Linux needs libcurl and libxml2 development packages.
set -eu

cd "$(dirname "$0")"

CC=${CC:-cc}
CFLAGS="-std=c23 -O2 -Wall -Wextra -Wpedantic"

# Apple clang before Xcode 16 and gcc before 14 only know the draft name.
if ! $CC $CFLAGS -E -x c /dev/null >/dev/null 2>&1; then
    CFLAGS="-std=c2x -O2 -Wall -Wextra -Wpedantic"
fi

case "$(uname -s)" in
Darwin)
    SDK=$(xcrun --show-sdk-path)
    INCLUDES="-I$SDK/usr/include/libxml2"
    LIBS="-lcurl -lxml2"
    ;;
*)
    INCLUDES=$(pkg-config --cflags libcurl libxml-2.0)
    LIBS=$(pkg-config --libs libcurl libxml-2.0)
    ;;
esac

mkdir -p build

case "${1:-}" in
test)
    # Everything but the real main, so the tests can link their own.
    SOURCES=$(ls src/*.c | grep -v 'src/leu\.c$')
    # shellcheck disable=SC2086
    $CC $CFLAGS $INCLUDES -o build/test tests/test.c $SOURCES $LIBS
    ./build/test
    # shellcheck disable=SC2086
    $CC $CFLAGS $INCLUDES -o build/leu src/*.c $LIBS
    ./tests/golden.sh
    ;;
*)
    # shellcheck disable=SC2086
    $CC $CFLAGS $INCLUDES -o build/leu src/*.c $LIBS
    echo "built build/leu"
    ;;
esac
