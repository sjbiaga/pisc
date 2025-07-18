#!/bin/bash

function pi() {
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
    set ../pi.scala ${srcs#?}
    scala-cli run "$@" \
                  -q -O -nowarn -S 3.7.2-RC1 \
                  --dep org.typelevel::cats-effect:3.6.2 \
                  --dep eu.timepit::refined:0.11.3 \
                  -Dcats.effect.warnOnNonMainThreadDetected=false \
                  ${args#?} \
                  2>&1
}

function pi_() {
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
    set ../pi_.scala ${srcs#?}
    scala-cli run "$@" \
                  -q -O -nowarn -S 3.7.2-RC1 \
                  --dep org.typelevel::cats-effect:3.6.2 \
                  --dep eu.timepit::refined:0.11.3 \
                  -Dcats.effect.warnOnNonMainThreadDetected=false \
                  ${args#?} \
                  2>&1
}

function pio() {
    while [ $# -gt 0 ]
    do
        { cat ../main.scala.in; cat in/"$1".scala.in | sed -e 's/^/  /'; } >| out/"$1".scala.out
        cat out/"$1".scala.out |
        scalafmt --quiet --non-interactive --stdin >| "$1".scala || cp out/"$1".scala.out "$1".scala
        shift
    done
}

export -f pio pi pi_
