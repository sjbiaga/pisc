#!/bin/sh

find \( -iname target -o -iname .metals -o -iname metals.sbt -o -iname .bloop -o -iname .bsp -o -iname .scala-build -o -iname '*~' -o -iname .stack-work \) -exec rm -fr '{}' ';'
