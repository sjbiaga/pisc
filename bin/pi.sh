#!/bin/bash

function pi() {
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
    set ../pi.scala ${srcs#?}
    ~/.local/share/coursier/bin/scala-cli run "$@" \
                                              -q -O -nowarn -S 3.4.2-RC1 \
                                              --dep org.typelevel::cats-effect:3.6-0142603 \
                                              ${args#?} \
                                              2>&1
}

function pi_() {
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
    set ../pi_.scala ${srcs#?}
    ~/.local/share/coursier/bin/scala-cli run "$@" \
                                              -q -O -nowarn -S 3.4.2-RC1 \
                                              --dep org.typelevel::cats-effect:3.6-0142603 \
                                              ${args#?} \
                                              2>&1
}

function pio() {
    while [ $# -gt 0 ]
    do
        { cat ../main.scala.in; cat in/"$1".scala.in | sed -e 's/^/  /'; } >| out/"$1".scala.out
        cat out/"$1".scala.out |
        scalafmt --non-interactive --stdin >| "$1".scala
        shift
    done
}

function dotarrowCirce() {
    [ $# -gt 0 ] || return
    local x=dotarrow
    local app="../$x/circe/app.scala.in"
    while [ $# -gt 0 ]
    do
        local n=`cat "$app" | grep -n '^..private.val.app' | awk -F: '{ print $1 }'`
        cat "$app" | head -n $n >| "$x/tmp/$1.tmp"
        cat "$x/src/$1.src" |
        sed -e 's/Init.Type.Select.Term.Name."IOApp".,.Type.Name."Simple"..,.Name.Anonymous.., Nil./Init(Type.Select(Term.Name("IOApp"), Type.Name("Simple")), Name.Anonymous(), Seq())/' \
            -e 's/Init.Type.Name.\(["][^"]\+["]\).,.Name.Anonymous..,.Nil./Init(Type.Name(\1), Name.Anonymous(), Seq())/g' \
            -e 's/^/    /' >> "$x/tmp/$1.tmp"
        cat "$app" | tail -n +`expr $n + 1` |
        sed -e '/[io]tmp.XXXXXXXXXX/s/\([io]\)tmp.XXXXXXXXXX/\1'"$1/g" \
            -e "/\"tmp.XXXXXXXXXX\"/s|[\"]tmp.XXXXXXXXXX[\"]|\"$x/tmp/$1.json\"|g" >> "$x/tmp/$1.tmp"
        tac "$x/tmp/$1.txt" |
        while read name_tpe
        do
            local name="${name_tpe%% *}"
            local tpe="${name_tpe#* }"
            sed -i "$x/tmp/$1.tmp" \
                -e "/val.ls:.List.Stat./aDefn.Var(Nil,List(Pat.Var(Term.Name(\"$name\"))),None,Term.ApplyType(Term.Select(Lit.Null(),Term.Name(\"asInstanceOf\")),Type.ArgClause(List(Type.Name(\"$tpe\")))))," \
                -e "/Defn.Class.*[io]$1/aTerm.Param(Nil,Term.Name(\"$name\"),Some(Type.Name(\"$tpe\")),None)," \
                -e "/Defn.Val.*i$1/aTerm.Assign(Term.Name(\"$name\"),Term.Select(Term.Name(\"json\"),Term.Name(\"$name\")))," \
                -e "/Defn.Val.*o$1/aTerm.Name(\"$name\"),"
        done
        shift
    done
}

function dotarrowCirce2() {
    [ $# -gt 0 ] || return
    local x=dotarrow
    local app2="../$x/circe/app2.scala.in"
    while [ $# -gt 0 ]
    do
        local n=`cat "$app2" | grep -n '^..private.val.app' | awk -F: '{ print $1 }'`
        local name_tpe="`tac \"$x/tmp/$1.txt\" | head -n 1`"
        local name="${name_tpe%% *}"
        local tpe="${name_tpe#* }"
        cat "$app2" | head -n $n >| "$x/tmp/$1.tmp"
        cat "$x/src/$1.src" |
        sed -e 's/Init.Type.Select.Term.Name."IOApp".,.Type.Name."Simple"..,.Name.Anonymous.., Nil./Init(Type.Select(Term.Name("IOApp"), Type.Name("Simple")), Name.Anonymous(), Seq())/' \
            -e 's/Init.Type.Name.\(["][^"]\+["]\).,.Name.Anonymous..,.Nil./Init(Type.Name(\1), Name.Anonymous(), Seq())/g' \
            -e 's/^/    /' >> "$x/tmp/$1.tmp"
        cat "$app2" | tail -n +`expr $n + 1` |
        sed -e "/name[ ]*:[ ]*String/s/\$/\"$name\"/" \
            -e "/tpe[ ]*:[ ]*String/s/\$/\"$tpe\"/" >> "$x/tmp/$1.tmp"
        shift
    done
}

function dotarrowStream() {
    [ $# -gt 0 ] || return
    local x=dotarrow
    local app="../$x/stream/app.scala.in"
    while [ $# -gt 0 ]
    do
        local n=`cat "$app" | grep -n '^..private.val.app' | awk -F: '{ print $1 }'`
        cat "$app" | head -n $n >| "$x/tmp/$1.tmp"
        cat "$x/src/$1.src" |
        sed -e 's/Init.Type.Select.Term.Name."IOApp".,.Type.Name."Simple"..,.Name.Anonymous.., Nil./Init(Type.Select(Term.Name("IOApp"), Type.Name("Simple")), Name.Anonymous(), Seq())/' \
            -e 's/Init.Type.Name.\(["][^"]\+["]\).,.Name.Anonymous..,.Nil./Init(Type.Name(\1), Name.Anonymous(), Seq())/g' \
            -e 's/^/    /' >> "$x/tmp/$1.tmp"
        cat "$app" | tail -n +`expr $n + 1` |
        sed -e "/tmp.XXXXXXXXXX/s|tmp.XXXXXXXXXX|$x/tmp/$1.bin|g" >> "$x/tmp/$1.tmp"
        tac "$x/tmp/$1.txt" |
        while read name_tpe
        do
            local name="${name_tpe%% *}"
            local tpe="${name_tpe#* }"
            sed -i "$x/tmp/$1.tmp" \
                -e "/val.ls:.List.Stat./aDefn.Var(Nil,List(Pat.Var(Term.Name(\"$name\"))),None,Term.ApplyType(Term.Select(Lit.Null(),Term.Name(\"asInstanceOf\")),Type.ArgClause(List(Type.Name(\"$tpe\")))))," \
                -e "/readBoolean/aTerm.Assign(Term.Name(\"$name\"),Term.ApplyType(Term.Select(Term.Apply(Term.Select(Term.Name(\"ois\"),Term.Name(\"readObject\")),Term.ArgClause(Nil,None)),Term.Name(\"asInstanceOf\")),Type.ArgClause(List(Type.Name(\"$tpe\")))))," \
                -e "/writeBoolean/aTerm.Apply(Term.Select(Term.Name(\"oos\"),Term.Name(\"writeObject\")),Term.ArgClause(List(Term.Name(\"$name\")),None)),"
        done
        shift
    done
}

function dotarrowStream2() {
    [ $# -gt 0 ] || return
    local x=dotarrow
    local app2="../$x/stream/app2.scala.in"
    while [ $# -gt 0 ]
    do
        local n=`cat "$app2" | grep -n '^..private.val.app' | awk -F: '{ print $1 }'`
        local name_tpe="`tac \"$x/tmp/$1.txt\" | head -n 1`"
        local name="${name_tpe%% *}"
        local tpe="${name_tpe#* }"
        cat "$app2" | head -n $n >| "$x/tmp/$1.tmp"
        cat "$x/src/$1.src" |
        sed -e 's/Init.Type.Select.Term.Name."IOApp".,.Type.Name."Simple"..,.Name.Anonymous.., Nil./Init(Type.Select(Term.Name("IOApp"), Type.Name("Simple")), Name.Anonymous(), Seq())/' \
            -e 's/Init.Type.Name.\(["][^"]\+["]\).,.Name.Anonymous..,.Nil./Init(Type.Name(\1), Name.Anonymous(), Seq())/g' \
            -e 's/^/    /' >> "$x/tmp/$1.tmp"
        cat "$app2" | tail -n +`expr $n + 1` |
        sed -e "/name[ ]*:[ ]*String/s/\$/\"$name\"/" \
            -e "/tpe[ ]*:[ ]*String/s/\$/\"$tpe\"/" >> "$x/tmp/$1.tmp"
        shift
    done
}

export -f dotarrowCirce dotarrowCirce2 dotarrowStream dotarrowStream2 pio pi pi_
