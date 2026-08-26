#!/bin/bash

function ba() {
    [ $# -gt 0 ] || return
    local srcs args emit=ce
    case "$1" in
        -ce|-cef|-zio|-ziof|-fs2|-zs)
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
                        --dep io.github.timwspence::cats-stm:0.13.5
                        --dep org.http4s::http4s-ember-client:0.23.36
                        --dep org.http4s::http4s-ember-server:0.23.36
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            local srcs=\ ../${emit}/bam.scala\ ../${emit}/http4s.scala
            ;;
        zio|ziof)
            local deps='--dep dev.zio::zio-concurrent:2.1.26
                        --dep dev.zio::zio-http:3.11.4
                        --dep dev.zio::zio-interop-cats:23.1.0.13'
            local srcs=\ ../${emit}/http.scala
            ;;
        fs2)
            local deps='--dep co.fs2::fs2-core:3.13.0
                        --dep dev.zio::zio-interop-cats:23.1.0.13
                        --dep io.github.timwspence::cats-stm:0.13.5
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
    set ${srcs#?} ../${emit}/ba.scala ../${emit}/dump.scala ../${emit}/traces.scala ../${emit}/loop.scala ../${emit}/stats.scala
    scala-cli run "$@" $deps \
                  -q -O -nowarn -S 3.9.0-RC6 \
                  --dep org.scalanlp::breeze:2.1.0 \
                  --dep com.github.blemale::scaffeine:5.3.0 \
                  --dep eu.timepit::refined:0.11.4 \
                  ${args#?} \
                  2>&1
#                  -Dpisc.bioambients.replications.exitcode.ignore=false \
#                  -Dpisc.bioambients.communications.parallelism.level=2147483647 \
#                  -Dpisc.bioambients.communications.batch.threshold=0 \
#                  -Dpisc.bioambients.communications.timeout.microseconds=123456 \
#                  -Dpisc.bioambients.ambients.hierarchy.snapshot=false \
}

function ba_() {
    [ $# -gt 0 ] || return
    local srcs args emit=ce
    case "$1" in
        -ce|-cef|-zio|-ziof|-fs2|-zs)
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
                        --dep io.github.timwspence::cats-stm:0.13.5
                        --dep io.circe::circe-generic:0.14.16
                        --dep org.http4s::http4s-circe:0.23.36
                        --dep org.http4s::http4s-dsl:0.23.36
                        --dep org.http4s::http4s-ember-client:0.23.36
                        --dep org.http4s::http4s-ember-server:0.23.36
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            local srcs=\ ../${emit}/bam_.scala\ ../${emit}/http4s_.scala
            ;;
        zio|ziof)
            local deps='--dep dev.zio::zio-concurrent:2.1.26
                        --dep dev.zio::zio-http:3.11.4
                        --dep dev.zio::zio-interop-cats:23.1.0.13'
            local srcs=\ ../${emit}/http_.scala
            ;;
        fs2)
            local deps='--dep co.fs2::fs2-core:3.13.0
                        --dep dev.zio::zio-interop-cats:23.1.0.13
                        --dep io.github.timwspence::cats-stm:0.13.5
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
    set ${srcs#?} ../${emit}/ba_.scala ../${emit}/dump_.scala ../${emit}/traces_.scala ../${emit}/loop_.scala ../${emit}/stats_.scala
    scala-cli run "$@" $deps \
                  -q -O -nowarn -S 3.9.0-RC6 \
                  --dep org.scalanlp::breeze:2.1.0 \
                  --dep com.github.blemale::scaffeine:5.3.0 \
                  --dep eu.timepit::refined:0.11.4 \
                  --repo https://packages.confluent.io/maven \
                  --dep org.apache.kafka:kafka-clients:4.3.1 \
                  --dep org.apache.avro:avro:1.12.2 \
                  --dep io.confluent:kafka-avro-serializer:8.3.1 \
                  --dep com.rabbitmq:amqp-client:5.35.0 \
                  --dep software.amazon.awssdk:sqs:2.54.4 \
                  ${args#?} \
                  2>&1
#                  -Dpisc.bioambients.replications.exitcode.ignore=false \
#                  -Dpisc.bioambients.communications.bind.address=localhost \
#                  -Dpisc.bioambients.communications.parallelism.level=2147483647 \
#                  -Dpisc.bioambients.communications.batch.threshold=0 \
#                  -Dpisc.bioambients.communications.timeout.microseconds=123456 \
#                  -Dpisc.bioambients.communications.exit.passthrough=true \
#                  -Dpisc.bioambients.ambients.hierarchy.snapshot=false \
}

function baio() {
    [ $# -gt 0 ] || return
    local emit=ce
    case "$1" in
        -ce|-cef|-zio|-ziof|-fs2|-zs)
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
