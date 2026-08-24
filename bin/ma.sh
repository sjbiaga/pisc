#!/bin/bash

function ma() {
    [ $# -gt 0 ] || return
    local srcs args emit=ce
    case "$1" in
        -ce|-zio)
            local emit="${1#?}"
            shift
            ;;
        -*)
            return
            ;;
        *)
            ;;
    esac
    case "$emit" in
        ce)
            local deps='--dep org.typelevel::cats-effect:3.7.0
                        --dep io.github.timwspence::cats-stm:0.13.5
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            ;;
        zio)
            local deps='--dep dev.zio::zio:2.1.26
                        --dep dev.zio::zio-interop-cats:23.1.0.13'
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
    set ${srcs#?} ../${emit}/ma.scala
    scala-cli run "$@" $deps \
                  -q -O -nowarn -S 3.9.0-RC6 \
                  ${args#?} \
                  2>&1
}

function maio() {
    [ $# -gt 0 ] || return
    local emit=ce
    case "$1" in
        -ce|-zio)
            local emit="${1#?}"
            shift
            ;;
        -*)
            return
            ;;
        *)
            ;;
    esac
    case "$emit" in
        *)
            local F=
            ;;
    esac
    while [ $# -gt 0 ]
    do
        { cat ../${emit}/${F}main.scala.in; cat in/"$1".scala.in | sed -e 's/^/  /'; } >| out/"$1".scala.out
        cat out/"$1".scala.out |
        scalafmt --quiet --non-interactive --stdin >| "$1".scala || cp out/"$1".scala.out "$1".scala
        shift
    done
}

export -f maio ma
