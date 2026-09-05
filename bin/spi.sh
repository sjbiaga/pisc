#!/bin/bash

function spi() {
    [ $# -gt 0 ] || return
    local srcs args emit=ce
    case "$1" in
        -ce|-cef|-zio|-ziof|-akka|-pekko|-fs2|-zs)
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
            local deps='--dep org.typelevel::cats-effect:3.7.1
                        --dep org.http4s::http4s-ember-server:0.23.36
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            local srcs=\ ../${emit}/spim.scala\ ../${emit}/http4s.scala
            ;;
        zio|ziof)
            local deps='--dep dev.zio::zio-concurrent:2.1.26
                        --dep dev.zio::zio-http:3.11.4
                        --dep dev.zio::zio-interop-cats:23.1.0.13'
            local srcs=\ ../${emit}/spim.scala\ ../${emit}/http.scala
            ;;
        akka)
            local deps='--repo https://repo.akka.io/cAzJkaebGFNkNrv2ILttVDQWmf3u4ThOcE_EbfzM0-N8lDhx/secure
                        --dep com.typesafe.akka::akka-actor-typed:2.10.21'
            ;;
        pekko)
            local deps='--dep org.apache.pekko::pekko-actor-typed:1.7.0'
            ;;
        fs2)
            local deps='--dep co.fs2::fs2-core:3.13.0
                        --dep dev.zio::zio-interop-cats:23.1.0.13
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            ;;
        zs)
            local deps='--dep dev.zio::zio-concurrent:2.1.26
                        --dep dev.zio::zio-streams:2.1.26
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
    set ${srcs#?} ../${emit}/spi.scala  ../${emit}/dump.scala ../${emit}/loop.scala ../${emit}/stats.scala ../${emit}/traces.scala
    scala-cli run "$@" $deps \
                  -q -O -nowarn -S 3.10.0-RC1 \
                  --dep org.scalanlp::breeze:2.1.0 \
                  --dep com.github.blemale::scaffeine:5.3.0 \
                  --dep eu.timepit::refined:0.11.4 \
                  ${args#?} \
                  2>&1
#                  -Dpisc.stochastic.replications.exitcode.ignore=false \
#                  -Dpisc.stochastic.communications.parallelism.level=2147483647 \
#                  -Dpisc.stochastic.communications.batch.threshold=0 \
#                  -Dpisc.stochastic.communications.timeout.microseconds=123456 \
}

function spi_() {
    [ $# -gt 0 ] || return
    local srcs args emit=ce
    case "$1" in
        -ce|-cef|-zio|-ziof|-akka|-pekko|-fs2|-zs)
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
            local deps='--dep org.typelevel::cats-effect:3.7.1
                        --dep io.circe::circe-generic:0.14.16
                        --dep org.http4s::http4s-circe:0.23.36
                        --dep org.http4s::http4s-dsl:0.23.36
                        --dep org.http4s::http4s-ember-client:0.23.36
                        --dep org.http4s::http4s-ember-server:0.23.36
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            local srcs=\ ../${emit}/spim_.scala\ ../${emit}/http4s_.scala
            ;;
        zio|ziof)
            local deps='--dep dev.zio::zio-concurrent:2.1.26
                        --dep dev.zio::zio-http:3.11.4
                        --dep dev.zio::zio-interop-cats:23.1.0.13'
            local srcs=\ ../${emit}/spim_.scala\ ../${emit}/http_.scala
            ;;
        akka)
            local deps='--repo https://repo.akka.io/cAzJkaebGFNkNrv2ILttVDQWmf3u4ThOcE_EbfzM0-N8lDhx/secure
                        --dep com.typesafe.akka::akka-actor-typed:2.10.21'
            ;;
        pekko)
            local deps='--dep org.apache.pekko::pekko-actor-typed:1.7.0'
            ;;
        fs2)
            local deps='--dep co.fs2::fs2-core:3.13.0
                        --dep dev.zio::zio-interop-cats:23.1.0.13
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            ;;
        zs)
            local deps='--dep dev.zio::zio-concurrent:2.1.26
                        --dep dev.zio::zio-streams:2.1.26
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
    set ${srcs#?} ../${emit}/spi_.scala  ../${emit}/dump_.scala ../${emit}/loop_.scala ../${emit}/stats_.scala ../${emit}/traces_.scala
    scala-cli run "$@" $deps \
                  -q -O -nowarn -S 3.10.0-RC1 \
                  --dep org.scalanlp::breeze:2.1.0 \
                  --dep com.github.blemale::scaffeine:5.3.0 \
                  --dep eu.timepit::refined:0.11.4 \
                  --repo https://packages.confluent.io/maven \
                  --dep org.apache.kafka:kafka-clients:4.3.1 \
                  --dep org.apache.avro:avro:1.12.2 \
                  --dep io.confluent:kafka-avro-serializer:8.3.1,exclude=org.apache.kafka%kafka-clients \
                  --dep com.rabbitmq:amqp-client:5.35.0 \
                  --dep software.amazon.awssdk:sqs:2.54.12 \
                  ${args#?} \
                  2>&1
#                  -Dpisc.stochastic.replications.exitcode.ignore=false \
#                  -Dpisc.stochastic.communications.bind.address=localhost \
#                  -Dpisc.stochastic.communications.parallelism.level=2147483647 \
#                  -Dpisc.stochastic.communications.batch.threshold=0 \
#                  -Dpisc.stochastic.communications.timeout.microseconds=123456 \
#                  -Dpisc.stochastic.communications.exit.passthrough=true \
}

function spio() {
    [ $# -gt 0 ] || return
    local emit=ce
    case "$1" in
        -ce|-cef|-zio|-ziof|-akka|-pekko|-fs2|-zs)
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

export -f spio spi spi_
