#!/bin/bash

function ppi() {
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
    set ../ppi.scala ${srcs#?}
    scala-cli run "$@" \
                  -q -O -nowarn -S 3.6.4-RC1 \
                  --dep org.typelevel::cats-effect:3.6-28f8f29 \
                  -Dcats.effect.warnOnNonMainThreadDetected=false \
                  ${args#?} \
                  2>&1
}

function ppi_() {
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
    set ../ppi_.scala ${srcs#?}
    scala-cli run "$@" \
                  -q -O -nowarn -S 3.6.4-RC1 \
                  --dep org.typelevel::cats-effect:3.6-28f8f29 \
                  -Dcats.effect.warnOnNonMainThreadDetected=false \
                  ${args#?} \
                  2>&1
}

function ppio() {
    while [ $# -gt 0 ]
    do
        { cat ../main.scala.in; cat in/"$1".scala.in | sed -e 's/^/  /'; } >| out/"$1".scala.out
        cat out/"$1".scala.out |
        scalafmt --quiet --non-interactive --stdin >| "$1".scala
        shift
    done
}

export -f ppio ppi ppi_
