#!/bin/bash

function pi() {
    [ $# -gt 0 ] || return
    local srcs args emit=ce
    case "$1" in
        -c[ea])
            local emit="${1#?}"
            shift
            ;;
        *)
            ;;
    esac
    case "$emit" in
        ca)
            local deps='--repo https://jitpack.io
                        --dep com.github.suprnation.cats-actors::cats-actors:2.0.1
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            ;;
        ce)
            local deps='--dep org.typelevel::cats-effect:3.7.0-RC1
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            ;;
    esac
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
    set ../${emit}/pi.scala ${srcs#?}
    scala-cli run "$@" $deps \
                  -q -O -nowarn -S 3.7.4-RC3 \
                  --dep eu.timepit::refined:0.11.3 \
                  ${args#?} \
                  2>&1
}

function pi_() {
    [ $# -gt 0 ] || return
    local srcs args emit=ce
    case "$1" in
        -c[ea])
            local emit="${1#?}"
            shift
            ;;
        *)
            ;;
    esac
    case "$emit" in
        ca)
            local deps='--repo https://jitpack.io
                        --dep com.github.suprnation.cats-actors::cats-actors:2.0.1
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            ;;
        ce)
            local deps='--dep org.typelevel::cats-effect:3.7.0-RC1
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            ;;
    esac
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
    set ../${emit}/pi_.scala ${srcs#?}
    scala-cli run "$@" $deps \
                  -q -O -nowarn -S 3.7.4-RC3 \
                  --dep eu.timepit::refined:0.11.3 \
                  ${args#?} \
                  2>&1
}

function pio() {
    [ $# -gt 0 ] || return
    local emit=ce
    case "$1" in
        -c[ea])
            local emit="${1#?}"
            shift
            ;;
        *)
            ;;
    esac
    while [ $# -gt 0 ]
    do
        { cat ../${emit}/main.scala.in; cat in/"$1".scala.in | sed -e 's/^/  /'; } >| out/"$1".scala.out
        cat out/"$1".scala.out |
        scalafmt --quiet --non-interactive --stdin >| "$1".scala || cp out/"$1".scala.out "$1".scala
        shift
    done
}

export -f pio pi pi_
