#!/bin/bash

function pi() {
    [ $# -gt 0 ] || return
    local srcs args emit=ce type=scala
    case "$1" in
        -ce|-zio|-akka|-pekko|-fs2|-monix|-zs)
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
        ce)
            local deps='--dep org.typelevel::cats-effect:3.7.1
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            local srcs=\ ../${emit}/pim.scala
            ;;
        zio)
            local deps='--dep dev.zio::zio-concurrent:2.1.27
                        --dep dev.zio::zio-interop-cats:23.1.0.14'
            local srcs=\ ../${emit}/pim.scala
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
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            ;;
        monix)
            local deps='--dep io.monix::monix:3.5.2
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            ;;
        zs)
            local deps='--dep dev.zio::zio-concurrent:2.1.27
                        --dep dev.zio::zio-streams:2.1.27
                        --dep dev.zio::zio-interop-cats:23.1.0.14'
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
            set ${srcs#?} ../${emit}/pi.scala
            scala-cli run "$@" $deps \
                          -q -O -nowarn -S 3.10.0 \
                          --dep eu.timepit::refined:0.11.4 \
                          ${args#?} \
                          2>&1
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

function pi_() {
    [ $# -gt 0 ] || return
    local srcs args emit=ce type=scala
    case "$1" in
        -ce|-zio|-akka|-pekko|-fs2|-monix|-zs)
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
        ce)
            local deps='--dep org.typelevel::cats-effect:3.7.1
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            local srcs=\ ../${emit}/pim_.scala
            ;;
        zio)
            local deps='--dep dev.zio::zio-concurrent:2.1.27
                        --dep dev.zio::zio-interop-cats:23.1.0.14'
            local srcs=\ ../${emit}/pim_.scala
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
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            ;;
        monix)
            local deps='--dep io.monix::monix:3.5.2
                        -Dcats.effect.warnOnNonMainThreadDetected=false'
            ;;
        zs)
            local deps='--dep dev.zio::zio-concurrent:2.1.27
                        --dep dev.zio::zio-streams:2.1.27
                        --dep dev.zio::zio-interop-cats:23.1.0.14'
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
            set ${srcs#?} ../${emit}/pi_.scala
            scala-cli run "$@" $deps \
                          -q -O -nowarn -S 3.10.0 \
                          --dep eu.timepit::refined:0.11.4 \
                          ${args#?} \
                          2>&1
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

function pio() {
    [ $# -gt 0 ] || return
    local emit=ce type=scala
    case "$1" in
        -ce|-zio|-akka|-pekko|-fs2|-monix|-zs)
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
        case ${type} in
            scala)
                { cat ../${emit}/${F}main.${type}.in; cat in/"$1".${type}.in | sed -e 's/^/  /'; } >| out/"$1".${type}.out
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
                  cat in/"$1".${type}.in | sed -e 's/υ/u/g' | sed -e '/^import/d'; } >| out/"$1".${type}.out
                trap 'popd &>/dev/null' SIGINT
                pushd . &>/dev/null
                cp out/"$1".${type}.out "$1".${type}
                rm -fr "$1"
                mkdir "$1"
                mv "$1".${type} "$1"
                cd "$1"
                mkdir pi
                cd pi
                ln -s ../../../${emit}/pi.${type}
                ln -s ../../../${emit}/pim.${type}
                ln -s ../../../${emit}/util.${type}
                cd ..
                gala mod init main &>/dev/null
                cd ..
                cp out/"$1".${type}.out "$1".${type}
                rm -fr "$1_"
                mkdir "$1_"
                mv "$1".${type} "$1_"
                cd "$1_"
                mkdir pi
                cd pi
                ln -s ../../../${emit}/pi_.${type}
                ln -s ../../../${emit}/pim_.${type}
                ln -s ../../../${emit}/util.${type}
                cd ..
                gala mod init main &>/dev/null
                popd &>/dev/null
                ;;
        esac
        shift
    done
}

export -f pio pi pi_
