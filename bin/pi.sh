#!/bin/bash

function pi() {
    [ $# -gt 0 ] || return
    local srcs args emit=ce
    case "$1" in
        -ce|-akka|-pekko|-fs2|-monix|-zs)
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
            local deps='--dep org.typelevel::cats-effect:3.7.0-RC1
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            ;;
        akka)
            local deps='--repo https://repo.akka.io/cAzJkaebGFNkNrv2ILttVDQWmf3u4ThOcE_EbfzM0-N8lDhx/secure
                        --dep com.typesafe.akka::akka-actor-typed:2.10.12'
            ;;
        pekko)
            local deps='--dep org.apache.pekko::pekko-actor-typed:1.3.0'
            ;;
        fs2)
            local deps='--dep co.fs2::fs2-core:3.13.0-M7
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            ;;
        monix)
            local deps='--dep io.monix::monix:3.4.1
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            ;;
        zs)
            local deps='--dep dev.zio::zio-streams:2.1.23'
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
    set ${srcs#?} ../${emit}/pi.scala
    scala-cli run "$@" $deps \
                  -q -O -nowarn -S 3.8.0-RC4 \
                  --dep eu.timepit::refined:0.11.3 \
                  ${args#?} \
                  2>&1
}

function pi_() {
    [ $# -gt 0 ] || return
    local srcs args emit=ce
    case "$1" in
        -ce|-akka|-pekko|-fs2|-monix|-zs)
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
            local deps='--dep org.typelevel::cats-effect:3.7.0-RC1
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            ;;
        akka)
            local deps='--repo https://repo.akka.io/cAzJkaebGFNkNrv2ILttVDQWmf3u4ThOcE_EbfzM0-N8lDhx/secure
                        --dep com.typesafe.akka::akka-actor-typed:2.10.12'
            ;;
        pekko)
            local deps='--dep org.apache.pekko::pekko-actor-typed:1.3.0'
            ;;
        fs2)
            local deps='--dep co.fs2::fs2-core:3.13.0-M7
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            ;;
        monix)
            local deps='--dep io.monix::monix:3.4.1
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            ;;
        zs)
            local deps='--dep dev.zio::zio-streams:2.1.23'
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
    set ${srcs#?} ../${emit}/pi_.scala
    scala-cli run "$@" $deps \
                  -q -O -nowarn -S 3.8.0-RC4 \
                  --dep eu.timepit::refined:0.11.3 \
                  ${args#?} \
                  2>&1
}

function pio() {
    [ $# -gt 0 ] || return
    local emit=ce
    case "$1" in
        -ce|-akka|-pekko|-fs2|-monix|-zs)
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
        fs2|monix)
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

export -f pio pi pi_
