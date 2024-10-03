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
    set ../loop.scala ../spi.scala ../stats.scala ${srcs#?}
    scala-cli run "$@" \
                  -q -O -nowarn -S 3.5.2-RC1 \
                  --dep org.scalanlp::breeze:2.1.0 \
                  --dep com.github.blemale::scaffeine:5.3.0 \
                  --dep org.typelevel::cats-effect:3.6-0142603 \
                  ${args#?} \
                  2>&1
}

function spio() {
    while [ $# -gt 0 ]
    do
        { cat ../main.scala.in; cat in/"$1".scala.in | sed -e 's/^/  /'; } >| out/"$1".scala.out
        cat out/"$1".scala.out |
        scalafmt --non-interactive --stdin |
        sed 's|[(]implicit[ ]^[ ][:][ ]String[,][ ][%][ ][:][ ][%][,][ ][/][ ][:][ ][/][)]|(using ^ : String)(using % : %, / : /)|' >| "$1".scala
        shift
    done
}

export -f spio spi