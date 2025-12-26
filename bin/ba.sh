#!/bin/bash

function ba() {
    [ $# -gt 0 ] || return
    local srcs args emit=ce
    case "$1" in
        -ce|-cef|-fs2)
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
        ce|cef)
            local deps='--dep org.typelevel::cats-effect:3.7.0-RC1
                        --dep io.github.timwspence::cats-stm:0.13.5
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            ;;
        fs2)
            local deps='--dep co.fs2::fs2-core:3.13.0-M7
                        --dep io.github.timwspence::cats-stm:0.13.5
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
    set ${srcs#?} ../${emit}/ba.scala ../${emit}/dump.scala ../${emit}/loop.scala ../${emit}/stats.scala
    scala-cli run "$@" $deps \
                  -q -O -nowarn -S 3.8.0-RC4 \
                  --dep org.scalanlp::breeze:2.1.0 \
                  --dep com.github.blemale::scaffeine:5.3.0 \
                  --dep eu.timepit::refined:0.11.3 \
                  ${args#?} \
                  2>&1
#                  -Dpisc.bioambients.replications.exitcode.ignore=false \
#                  -Dpisc.bioambients.communications.parallelism.level=9 \
}

function ba_() {
    [ $# -gt 0 ] || return
    local srcs args emit=ce
    case "$1" in
        -ce|-cef|-fs2)
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
        ce|cef)
            local deps='--dep org.typelevel::cats-effect:3.7.0-RC1
                        --dep io.github.timwspence::cats-stm:0.13.5
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            ;;
        fs2)
            local deps='--dep co.fs2::fs2-core:3.13.0-M7
                        --dep io.github.timwspence::cats-stm:0.13.5
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
    set ${srcs#?} ../${emit}/ba_.scala ../${emit}/dump_.scala ../${emit}/loop_.scala ../${emit}/stats_.scala
    scala-cli run "$@" $deps \
                  -q -O -nowarn -S 3.8.0-RC4 \
                  --dep org.scalanlp::breeze:2.1.0 \
                  --dep com.github.blemale::scaffeine:5.3.0 \
                  --dep eu.timepit::refined:0.11.3 \
                  ${args#?} \
                  2>&1
#                  -Dpisc.bioambients.replications.exitcode.ignore=false \
#                  -Dpisc.bioambients.communications.parallelism.level=9 \
#                  -Dpisc.bioambients.ambients.hierarchy.snapshot=false \
}

function baio() {
    [ $# -gt 0 ] || return
    local emit=ce
    case "$1" in
        -ce|-cef|-fs2)
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
        fs2)
            local F=`grep 'type.F.=.' in/"$1".scala.in`
            local F=${F##*.}.
            ;;
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

export -f baio ba ba_
