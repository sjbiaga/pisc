#!/bin/bash

function ma() {
    local srcs args
    while [ $# -gt 0 ]
    do
        if [ "$1" = '--' ]
        then
            break
        fi
        srcs="$srcs $1"
        shift
    done
    while [ $# -gt 0 ]
    do
        args="$args $1"
        shift
    done
    set ../ma.scala ${srcs#?}
    scala-cli run "$@" \
                  -q -O -nowarn -S 3.5.1-RC2 \
                  --dep org.typelevel::cats-effect:3.6-0142603 \
                  ${args#?} \
                  2>&1
}

function maio() {
    while [ $# -gt 0 ]
    do
        { cat ../main.scala.in; cat in/"$1".scala.in | sed -e 's/^/  /'; } >| out/"$1".scala.out
        cat out/"$1".scala.out |
        scalafmt --non-interactive --stdin >| "$1".scala
        shift
    done
}

export -f maio ma