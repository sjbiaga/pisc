#!/bin/bash

function spi() {
    [ $# -gt 0 ] || return
    local srcs args emit=ce type=scala
    case "$1" in
        -ce|-cef|-cot|-zio|-ziof|-akka|-pekko|-fs2|-zs)
            local emit="${1#?}"
            shift
            ;;
        -go)
            local emit="${1#?}"
            local type=gala
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
                        --dep org.http4s::http4s-ember-server:0.23.38
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            local srcs=\ ../${emit}/spim.scala\ ../${emit}/http4s.scala
            ;;
        cot)
            local deps='--dep org.typelevel::cats-effect:3.7.1
                        --dep org.http4s::http4s-ember-server:0.23.38
                        --dep dev.zio::zio-interop-cats:23.1.0.14
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            local srcs=\ ../${emit}/spim.scala\ ../${emit}/http4s.scala
            ;;
        zio|ziof)
            local deps='--dep dev.zio::zio-concurrent:2.1.27
                        --dep dev.zio::zio-http:3.11.6
                        --dep dev.zio::zio-interop-cats:23.1.0.14'
            local srcs=\ ../${emit}/spim.scala\ ../${emit}/http.scala
            ;;
        akka)
            local deps='--repo https://repo.akka.io/cAzJkaebGFNkNrv2ILttVDQWmf3u4ThOcE_EbfzM0-N8lDhx/secure
                        --dep com.typesafe.akka::akka-actor-typed:2.10.24'
            ;;
        pekko)
            local deps='--dep org.apache.pekko::pekko-actor-typed:1.7.1'
            ;;
        fs2)
            local deps='--dep co.fs2::fs2-core:3.14.0
                        --dep dev.zio::zio-interop-cats:23.1.0.14
                        --dep org.http4s::http4s-ember-server:0.23.38
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            local srcs=\ ../${emit}/http4s.scala
            ;;
        zs)
            local deps='--dep dev.zio::zio-concurrent:2.1.27
                        --dep dev.zio::zio-http:3.11.6
                        --dep dev.zio::zio-streams:2.1.27
                        --dep dev.zio::zio-interop-cats:23.1.0.14'
            local srcs=\ ../${emit}/http.scala
            ;;
        go)
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
    case ${type} in
        scala)
            set ${srcs#?} ../${emit}/spi.scala  ../${emit}/dump.scala ../${emit}/loop.scala ../${emit}/stats.scala ../${emit}/traces.scala
            scala-cli run "$@" $deps \
                          -q -O -nowarn -S 3.10.1-RC2 \
                          --dep org.scalanlp::breeze:2.1.0 \
                          --dep com.github.blemale::scaffeine:5.3.0 \
                          --dep eu.timepit::refined:0.11.4 \
                          ${args#?} \
                          2>&1
#                          -Dpisc.stochastic.replications.exitcode.ignore=false \
#                          -Dpisc.stochastic.communications.parallelism.level=2147483647 \
#                          -Dpisc.stochastic.communications.batch.threshold=0 \
#                          -Dpisc.stochastic.communications.timeout.microseconds=123456 \
            ;;
        gala)
            set ${srcs#?}
            [ $# -eq 1 -a -d "${1%.${type}}" ] || return
            trap 'popd &>/dev/null' SIGINT
            pushd "${1%.${type}}" &>/dev/null
            gala run "$1" ${args#?} 2>&1
            popd &>/dev/null
            ;;
    esac
}

function spi_() {
    [ $# -gt 0 ] || return
    local srcs args emit=ce type=scala
    case "$1" in
        -ce|-cef|-cot|-zio|-ziof|-akka|-pekko|-fs2|-zs)
            local emit="${1#?}"
            shift
            ;;
        -go)
            local emit="${1#?}"
            local type=gala
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
                        --dep io.circe::circe-generic:0.14.17
                        --dep org.gnieh::fs2-data-json-circe:1.14.1
                        --dep org.http4s::http4s-circe:0.23.38
                        --dep org.http4s::http4s-dsl:0.23.38
                        --dep org.http4s::http4s-ember-client:0.23.38
                        --dep org.http4s::http4s-ember-server:0.23.38
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            local srcs=\ ../${emit}/spim_.scala\ ../${emit}/http4s_.scala
            ;;
        cot)
            local deps='--dep org.typelevel::cats-effect:3.7.1
                        --dep io.circe::circe-generic:0.14.17
                        --dep org.gnieh::fs2-data-json-circe:1.14.1
                        --dep org.http4s::http4s-circe:0.23.38
                        --dep org.http4s::http4s-dsl:0.23.38
                        --dep org.http4s::http4s-ember-client:0.23.38
                        --dep org.http4s::http4s-ember-server:0.23.38
                        --dep dev.zio::zio-interop-cats:23.1.0.14
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            local srcs=\ ../${emit}/spim_.scala\ ../${emit}/http4s_.scala
            ;;
        zio|ziof)
            local deps='--dep dev.zio::zio-concurrent:2.1.27
                        --dep dev.zio::zio-http:3.11.6
                        --dep io.circe::circe-generic:0.14.17
                        --dep dev.zio::zio-interop-cats:23.1.0.14'
            local srcs=\ ../${emit}/spim_.scala\ ../${emit}/http_.scala
            ;;
        akka)
            local deps='--repo https://repo.akka.io/cAzJkaebGFNkNrv2ILttVDQWmf3u4ThOcE_EbfzM0-N8lDhx/secure
                        --dep com.typesafe.akka::akka-actor-typed:2.10.24
                        --dep io.circe::circe-generic:0.14.17'
            ;;
        pekko)
            local deps='--dep org.apache.pekko::pekko-actor-typed:1.7.1
                        --dep io.circe::circe-generic:0.14.17'
            ;;
        fs2)
            local deps='--dep co.fs2::fs2-core:3.14.0
                        --dep dev.zio::zio-interop-cats:23.1.0.14
                        --dep io.circe::circe-generic:0.14.17
                        --dep org.gnieh::fs2-data-json-circe:1.14.1
                        --dep org.http4s::http4s-circe:0.23.38
                        --dep org.http4s::http4s-dsl:0.23.38
                        --dep org.http4s::http4s-ember-client:0.23.38
                        --dep org.http4s::http4s-ember-server:0.23.38
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            local srcs=\ ../${emit}/http4s_.scala
            ;;
        zs)
            local deps='--dep dev.zio::zio-concurrent:2.1.27
                        --dep dev.zio::zio-http:3.11.6
                        --dep dev.zio::zio-streams:2.1.27
                        --dep io.circe::circe-generic:0.14.17
                        --dep dev.zio::zio-interop-cats:23.1.0.14'
            local srcs=\ ../${emit}/http_.scala
            ;;
        go)
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
    case ${type} in
        scala)
            set ${srcs#?} ../${emit}/spi_.scala  ../${emit}/dump_.scala ../${emit}/loop_.scala ../${emit}/stats_.scala ../${emit}/traces_.scala
            scala-cli run "$@" $deps \
                          -q -O -nowarn -S 3.10.1-RC2 \
                          --dep org.scalanlp::breeze:2.1.0 \
                          --dep com.github.blemale::scaffeine:5.3.0 \
                          --dep eu.timepit::refined:0.11.4 \
                          --repo https://packages.confluent.io/maven \
                          --dep org.apache.kafka:kafka-clients:4.3.1 \
                          --dep org.apache.avro:avro:1.12.2 \
                          --dep io.confluent:kafka-avro-serializer:8.3.2,exclude=org.apache.kafka%kafka-clients \
                          --dep com.rabbitmq:amqp-client:5.37.0 \
                          --dep software.amazon.awssdk:sqs:2.56.0 \
                          ${args#?} \
                          2>&1
#                          -Dpisc.stochastic.replications.exitcode.ignore=false \
#                          -Dpisc.stochastic.communications.bind.address=localhost \
#                          -Dpisc.stochastic.communications.parallelism.level=2147483647 \
#                          -Dpisc.stochastic.communications.batch.threshold=0 \
#                          -Dpisc.stochastic.communications.timeout.microseconds=123456 \
#                          -Dpisc.stochastic.communications.exit.passthrough=true \
            ;;
        gala)
            set ${srcs#?}
            [ $# -eq 1 -a -d "${1%.${type}}_" ] || return
            trap 'popd &>/dev/null' SIGINT
            pushd "${1%.${type}}_" &>/dev/null
            gala run "$1" ${args#?} 2>&1
            popd &>/dev/null
            ;;
    esac
}

function spio() {
    [ $# -gt 0 ] || return
    local emit=ce type=scala
    case "$1" in
        -ce|-cef|-cot|-zio|-ziof|-akka|-pekko|-fs2|-zs)
            local emit="${1#?}"
            shift
            ;;
        -go)
            local emit="${1#?}"
            local type=gala
            shift
            ;;
        -*)
            return
            ;;
        *)
            ;;
    esac
    case "$emit" in
        cot|fs2)
            local F=`grep 'type.F.=.' in/"$1".scala.in`
            local F=${F##*.}.
            ;;
        *)
            local F=
            ;;
    esac
    while [ $# -gt 0 ]
    do
        case ${type} in
            scala)
                { cat ../${emit}/${F}main.${type}.in; cat in/"$1".${type}.in | sed -e 's/^/  /'; } |
                awk -v RS='\\\\u[0-9a-fA-F]{4}' '{ORS=""; print $0; if (RT) printf "%c",strtonum("0x"substr(RT,3)) } END {print ""}' >| out/"$1".${type}.out
                cat out/"$1".${type}.out |
                scalafmt --quiet --non-interactive --stdin >| "$1".${type} || cp out/"$1".${type}.out "$1".${type}
                ;;
            gala)
                { head ../${emit}/import.${type}.in -n -1;
                  cat in/"$1".${type}.in |
                  grep '^[ ]*import' |
                  sed -e 's/^[ ]*import[ ]*[.][ ]*/    . /' -e 's/^[ ]*import[ ]*/      /';
                  echo ')';
                  cat ../${emit}/main.${type}.in;
                  cat in/"$1".${type}.in | sed -e 's/υ/u/g' | sed -e '/^import/d'; } |
                  awk -v RS='\\\\u[0-9a-fA-F]{4}' '{ORS=""; print $0; if (RT) printf "%c",strtonum("0x"substr(RT,3)) } END {print ""}' >| out/"$1".${type}.out
                trap 'popd &>/dev/null' SIGINT
                pushd . &>/dev/null
                cp out/"$1".${type}.out "$1".${type}
                rm -fr "$1"
                mkdir "$1"
                mv "$1".${type} "$1"
                cd "$1"
                mkdir spi
                cd spi
                ln -s ../../../${emit}/spi.${type}
                ln -s ../../../${emit}/spim.${type}
                ln -s ../../../${emit}/loop.${type}
                ln -s ../../../${emit}/stats.${type}
                ln -s ../../../${emit}/lbqueue.${type}
                cp ../../../${emit}/sleep.go .
                ln -s ../../../${emit}/trampoline.${type}
                ln -s ../../../${emit}/util.${type}
                cd ..
                gala mod init main &>/dev/null
                cd ..
                cp out/"$1".${type}.out "$1".${type}
                rm -fr "$1_"
                mkdir "$1_"
                mv "$1".${type} "$1_"
                cd "$1_"
                mkdir spi
                cd spi
                ln -s ../../../${emit}/spi.${type}
                ln -s ../../../${emit}/spim.${type}
                ln -s ../../../${emit}/loop.${type}
                ln -s ../../../${emit}/stats.${type}
                ln -s ../../../${emit}/lbqueue.${type}
                cp ../../../${emit}/sleep.go .
                ln -s ../../../${emit}/trampoline.${type}
                ln -s ../../../${emit}/util.${type}
                cd ..
                gala mod init main &>/dev/null
                popd &>/dev/null
                ;;
        esac
        shift
    done
}

export -f spio spi spi_
