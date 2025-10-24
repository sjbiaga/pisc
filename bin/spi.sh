#!/bin/bash

function spi() {
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
    set ${srcs#?} ../dump.scala ../loop.scala ../spi.scala ../stats.scala
    scala-cli run "$@" \
                  -q -O -nowarn -S 3.7.4-RC1 \
                  --dep org.scalanlp::breeze:2.1.0 \
                  --dep com.github.blemale::scaffeine:5.3.0 \
                  --dep org.typelevel::cats-effect:3.7.0-RC1 \
                  --dep eu.timepit::refined:0.11.3 \
                  -Dcats.effect.warnOnNonMainThreadDetected=false \
                  ${args#?} \
                  2>&1
#                  -Dpisc.stochastic.replications.exitcode.ignore=false \
#                  -Dpisc.stochastic.communications.parallelism.level=9 \
}

function spi_() {
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
    set ${srcs#?} ../dump_.scala ../loop_.scala ../spi_.scala ../stats_.scala
    scala-cli run "$@" \
                  -q -O -nowarn -S 3.7.4-RC1 \
                  --dep org.scalanlp::breeze:2.1.0 \
                  --dep com.github.blemale::scaffeine:5.3.0 \
                  --dep org.typelevel::cats-effect:3.7.0-RC1 \
                  --dep eu.timepit::refined:0.11.3 \
                  -Dcats.effect.warnOnNonMainThreadDetected=false \
                  ${args#?} \
                  2>&1
#                  -Dpisc.stochastic.replications.exitcode.ignore=false \
#                  -Dpisc.stochastic.communications.parallelism.level=9 \
}

function spio() {
    while [ $# -gt 0 ]
    do
        { cat ../main.scala.in; cat in/"$1".scala.in | sed -e 's/^/  /'; } >| out/"$1".scala.out
        cat out/"$1".scala.out |
        scalafmt --quiet --non-interactive --stdin >| "$1".scala || cp out/"$1".scala.out "$1".scala
        shift
    done
}

export -f spio spi spi_
