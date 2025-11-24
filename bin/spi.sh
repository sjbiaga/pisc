#!/bin/bash

function spi() {
    [ $# -gt 0 ] || return
    local srcs args emit=ce
    case "$1" in
        -ce|-cef|-akka|-pekko)
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
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            ;;
        akka)
            local deps='--repo https://repo.akka.io/cAzJkaebGFNkNrv2ILttVDQWmf3u4ThOcE_EbfzM0-N8lDhx/secure
                        --dep com.typesafe.akka::akka-actor-typed:2.10.12'
            ;;
        pekko)
            local deps='--dep org.apache.pekko::pekko-actor-typed:1.3.0'
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
    set ${srcs#?} ../${emit}/spi.scala  ../${emit}/dump.scala ../${emit}/loop.scala ../${emit}/stats.scala
    scala-cli run "$@" $deps \
                  -q -O -nowarn -S 3.7.4 \
                  --dep org.scalanlp::breeze:2.1.0 \
                  --dep com.github.blemale::scaffeine:5.3.0 \
                  --dep eu.timepit::refined:0.11.3 \
                  ${args#?} \
                  2>&1
#                  -Dpisc.stochastic.replications.exitcode.ignore=false \
#                  -Dpisc.stochastic.communications.parallelism.level=9 \
}

function spi_() {
    [ $# -gt 0 ] || return
    local srcs args emit=ce
    case "$1" in
        -ce|-cef|-akka|-pekko)
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
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            ;;
        akka)
            local deps='--repo https://repo.akka.io/cAzJkaebGFNkNrv2ILttVDQWmf3u4ThOcE_EbfzM0-N8lDhx/secure
                        --dep com.typesafe.akka::akka-actor-typed:2.10.12'
            ;;
        pekko)
            local deps='--dep org.apache.pekko::pekko-actor-typed:1.3.0'
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
    set ${srcs#?} ../${emit}/spi_.scala  ../${emit}/dump_.scala ../${emit}/loop_.scala ../${emit}/stats_.scala
    scala-cli run "$@" $deps \
                  -q -O -nowarn -S 3.7.4 \
                  --dep org.scalanlp::breeze:2.1.0 \
                  --dep com.github.blemale::scaffeine:5.3.0 \
                  --dep eu.timepit::refined:0.11.3 \
                  ${args#?} \
                  2>&1
#                  -Dpisc.stochastic.replications.exitcode.ignore=false \
#                  -Dpisc.stochastic.communications.parallelism.level=9 \
}

function spio() {
    [ $# -gt 0 ] || return
    local emit=ce
    case "$1" in
        -ce|-cef|-akka|-pekko)
            local emit="${1#?}"
            shift
            ;;
        -*)
            return
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

export -f spio spi spi_
