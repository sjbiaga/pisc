#!/bin/sh

function pi() {
        set "$@" ../pi.scala
        ~/sjb/coursier/bin/scala-cli "$@" \
                                     -S 3.4.0-RC1 \
                                     --dependency org.typelevel::cats-effect:3.5.2
}

function spi() {
        set "$@" ../loop.scala ../stats.scala ../spi.scala
        ~/sjb/coursier/bin/scala-cli "$@" \
                                     -S 3.4.0-RC1 \
                                     --dependency org.scalanlp::breeze:2.1.0 \
                                     --dependency com.github.blemale::scaffeine:5.2.1 \
                                     --dependency org.typelevel::cats-effect:3.5.2
}

function pio() {
        while [ $# -gt 0 ]
        do
                rm out/"$1".scala.out; { cat ../main.scala.in; cat in/"$1".scala.in | sed -e 's/^/  /'; } > out/"$1".scala.out
                rm "$1".scala; cat out/"$1".scala.out |
                scalafmt --stdin --stdout |
                sed 's/for[ ][(][_][ ][<][-][ ]IO[.]unit[)][ ]yield[ ][(][)]/`𝟎`/g' |
                sed 's|[(]implicit[ ]^[ ][:][ ]String[,][ ][%][ ][:][ ][%][,][ ][\\][ ][:][ ][\\][,][ ][/][ ][:][ ][/][,][ ][*][ ][:][ ][*][,][ ][+][ ][:][ ][+][,][ ][-][ ][:][ ][-][)]|(using ^ : String)(using % : %, \\ : \\, / : /, * : *, + : +, - : -)|' > "$1".scala
                shift
        done
}

export pi pio spi
