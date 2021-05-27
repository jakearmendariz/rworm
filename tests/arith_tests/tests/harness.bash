#!/usr/bin/env bash

check() {
    run sh -c "echo '$1' |  ./worm-pl worm/arith.c"
    echo "$1 = $2, your code outputs $output"
    [ "$output" = "$2" ]
}
