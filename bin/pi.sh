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
    scala-cli run "$@" \
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
    scala-cli run "$@" \
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

function dotarrowScalaToHaskell0() {
    local x=dotarrow
    local p="`readlink -m \"$x/tmp/$1\"`"
    local _24="`printf '%24s'`"
    local _24="${_24// /\\ }"
    local n=`cat "$p.txt" | wc -l`
    let n=n-1
    let m=n-1
    [ $m -ge 0 ] || let m=0
    sed -i "$p/app/Inp_"*.hs -e "10,+${m}d"
    sed -i "$p/app/Out_"*.hs -e "11,+$((m+1))d"
    cat "$p.txt" | head -n -1 |
    while read name_tpe
    do
        local name="${name_tpe%% *}"
        local tpe="${name_tpe#* }"
        sed -i "$p/app/Inp_"*.hs -e "/[}]/i${_24}${c:-\\ }\ $name\ ::\ $tpe"
        sed -i "$p/app/Out_"*.hs -e "/[}]/i${_24}${c:-\\ }\ $name\ ::\ $tpe"
        local c=,
    done
    cat "$p.txt" | tail -n 1 |
    if read name_tpe
    then
        [ $n -eq 0 ] || local c=,
        local name="${name_tpe%% *}"
        local tpe="${name_tpe#* }"
        sed -i "$p/app/Out_"*.hs -e "/[}]/i${_24}${c:-\\ }\ $name\ ::\ $tpe"
    fi
    [ $n -eq 0 ] || local c=,
    sed -i "$p/app/Inp_"*.hs -e "/[}]/i${_24}${c:-\\ }\ dummy_gUgVwYdD8r\ ::\ Maybe\ String"
    sed -i "$p/app/Out_"*.hs -e "/[}]/i${_24},\ dummy_gUgVwYdD8r\ ::\ Maybe\ String"
}

function dotarrowScalaToHaskell() {
    [ $# -eq 2 ] || return
    local x=dotarrow
    local app="../$x/sc2hs"
    local p="`readlink -m \"$x/tmp/$1\"`"
    local q="`readlink -m \"$x/tmp/$2\"`"
    scala-cli run -S 3.5.0-RC1 "../$x/source.scala" -- "$x/$2.scala"
    mkdir -p "$p/tmp"; rm -fr "$p/tmp/sc2hs" &>/dev/null; cp -r "$app" "$p/tmp"
    local hs="`cat \"$x/src/$2.scala.src\" | \"$p\"/tmp/sc2hs/bin/Scalameta2Haskell.pl`"
    pushd "$p/tmp/sc2hs" &>/dev/null
    mv app/Main.hs{.in,}
    sed -i app/Main.hs \
        -e '/^\s*source\s*=\s*$/a\ \ \ \ '"$hs"
    stack build &>/dev/null
    stack run >| "$p.tmp" || return 1
    popd &>/dev/null
    cat "$p.tmp" |
    sed -e '/^main.::/i\
module\ Main\ where\
\
import GHC.Base\
import GHC.Num\
import\ System.Exit\ (exitFailure)\
import\ System.IO\ (hPutStrLn,\ stderr)\
import\ Inp_gUgVwYdD8r\
import\ Out_gUgVwYdD8r\
' \
        -e '/^main.::/amain\ =\ do' \
        -e '2~1s/^/    /' >| "$q/app/Main.hs"
    dotarrowScalaToHaskell0 "$2"
    sed -i "$q.json" -e '/^[}]/i,"dummy_gUgVwYdD8r":null'
    return 0
}

function dotarrowHaskellToScala() {
    [ $# -eq 2 ] || return
    local x=dotarrow
    local app="../$x/hs2sc"
    local p="`readlink -m \"$x/tmp/$1\"`"
    local q="`readlink -m \"$x/tmp/$2\"`"
    local r="`readlink -m \"$x/src/$2\"`"
    mkdir -p "$p/tmp"; rm -fr "$p/tmp/hs2sc" &>/dev/null; cp -r "$app" "$p/tmp"
    pushd "$p/tmp/hs2sc/prj" &>/dev/null
    local n=`cat "app/Main.hs.in" | grep -n '^\s*source\s*=' | awk -F: '{ print $1 }'`
    cat "app/Main.hs.in" | head -n $n >| "$p.tmp"
    popd &>/dev/null
    pushd "$p/tmp/hs2sc" &>/dev/null
    stack build &>/dev/null
    stack run -- "$q/app/Main.hs" >> "$p.tmp"
    popd &>/dev/null
    pushd "$p/tmp/hs2sc/prj" &>/dev/null
    cat "app/Main.hs.in" | tail -n +`expr $n + 1` >> "$p.tmp"
    mv "$p.tmp" app/Main.hs
    sed -i app/Main.hs -e '/^SmSource/s/^/    /'
    stack build &>/dev/null
    stack run >| "$r.scala.src"
    popd &>/dev/null
    sed -e 's/Init.Type.Select.Term.Name."IOApp".,.Type.Name."Simple"..,.Name.Anonymous.., Nil./Init(Type.Select(Term.Name("IOApp"), Type.Name("Simple")), Name.Anonymous(), Seq())/' \
        -e 's/Init.Type.Name.\(["][^"]\+["]\).,.Name.Anonymous..,.Nil./Init(Type.Name(\1), Name.Anonymous(), Seq())/g' \
        -i "$r.scala.src"
    amm -c 'import $ivy.`org.scalameta:scalameta_2.13:4.9.5`;
            import scala.meta._; import dialects.Scala3;
            print('"`cat \"$r.scala.src\"`"')' 2>/dev/null >| "$x/$2.scala"
    sed -i "$q.json" -e '/dummy_gUgVwYdD8r/d'
    return 0
}

function dotarrowAeson0() {
    local x=dotarrow
    local p="`readlink -m \"$x/tmp/$1\"`"
    local _24="`printf '%24s'`"
    local _24="${_24// /\\ }"
    local n=`cat "$p.txt" | wc -l`
    let m=n-1
    [ $m -ge 0 ] || let m=0
    sed -i "$p/app/Inp_"*.hs -e "10,+${m}d"
    sed -i "$p/app/Out_"*.hs -e "11,+$((m+1))d"
    cat "$p.txt" |
    while read name_tpe
    do
        local name="${name_tpe%% *}"
        local tpe="${name_tpe#* }"
        sed -i "$p/app/Inp_"*.hs -e "/[}]/i${_24}${c:-\\ }\ $name\ ::\ $tpe"
        sed -i "$p/app/Out_"*.hs -e "/[}]/i${_24}${c:-\\ }\ $name\ ::\ $tpe"
        local c=,
    done
    cat "$p.tmp" | tail -n 1 |
    if read name_tpe
    then
        [ $n -eq 0 ] || local c=,
        local name="${name_tpe%% *}"
        local tpe="${name_tpe#* }"
        sed -i "$p/app/Out_"*.hs -e "/[}]/i${_24}${c:-\\ }\ $name\ ::\ $tpe"
    fi
    [ $n -eq 0 ] || local c=,
    sed -i "$p/app/Inp_"*.hs -e "/[}]/i${_24}${c:-\\ }\ dummy_gUgVwYdD8r\ ::\ Maybe\ String"
    sed -i "$p/app/Out_"*.hs -e "/[}]/i${_24},\ dummy_gUgVwYdD8r\ ::\ Maybe\ String"
}

function dotarrowAeson() {
    [ $# -eq 1 ] || return
    local x=dotarrow
    local app="../$x/aeson"
    local p="`readlink -m \"$x/tmp/$1\"`"
    local n=`cat "$p/app/Main.hs" | grep -n '^\\s*import' | awk -F: '{ print $1 }' | sort -n | tail -n 1`
    cat "$p/app/Main.hs" | head -n $n >| "$p.tmp"
    mkdir -p "$p/tmp"; rm -fr "$p/tmp/aeson" &>/dev/null; cp -r "$app" "$p/tmp"
    pushd "$p/tmp/aeson/app" &>/dev/null
    mv Main.hs{.in,}
    local _81="`printf '%81s'`"
    local _81="${_81// /\\ }"
    local _39="`printf '%39s'`"
    local _39="${_39// /\\ }"
    cat "$p.txt" |
    while read name_tpe
    do
        local name="${name_tpe%% *}"
        local tpe="${name_tpe#* }"
        sed -i Main.hs \
            -e "/WildP/i${_81}${c:-\\ }VarP\ (Name\ (OccName\ \"$name\")\ NameS)" \
            -e "/ConE..Name..OccName..JsonO/i${_39}(AppE" \
            -e "/VarE.name'/i${_39}(VarE\ (Name\ (OccName\ \"$name\")\ NameS)))"
        local c=,
    done
    if [ -s "$p.txt" ]
    then
        sed -i Main.hs -e "s/.WildP/,WildP/"
    fi
    stack build &>/dev/null
    stack run -- "$p/app/Main.hs" >> "$p.tmp" || return 1
    popd &>/dev/null
    sed -i "$p.tmp" \
        -e "/itmp.XXXXXXXXXX/s|itmp.XXXXXXXXXX|$p.json|" \
        -e "/otmp.XXXXXXXXXX/s|otmp.XXXXXXXXXX|$p.json.out|"
    dotarrowAeson0 "$1"
    cat "$p.tmp" | head -n -1 >| "$p/app/Main.hs"
    rm "$p.tmp" &>/dev/null
    pushd "$p/app" &>/dev/null
    stack build &>/dev/null
    stack run 3>&1 1>&2- 2>&3- | sed -e 's/[ ]/\\ /g' >> "$p.txt"
    popd &>/dev/null
    mv "$p.json"{.out,}
    return 0
}

function dotarrowAeson2() {
    [ $# -eq 1 ] || return
    local x=dotarrow
    local app="../$x/aeson2"
    local p="`readlink -m \"$x/tmp/$1\"`"
    local n=`cat "$p/app/Main.hs" | grep -n '^\\s*import' | awk -F: '{ print $1 }' | sort -n | tail -n 1`
    cat "$p/app/Main.hs" | head -n $n >| "$p.tmp"
    mkdir -p "$p/tmp"; rm -fr "$p/tmp/aeson2" &>/dev/null; cp -r "$app" "$p/tmp"
    pushd "$p/tmp/aeson2/app" &>/dev/null
    mv Main.hs{.in,}
    local name_tpe="`cat \"$p.txt\" | tail -n 1`"
    local _71="`printf '%71s'`"
    local _71="${_71// /\\ }"
    sed -i Main.hs -e "/stmt'.[(]\$/a${_71}\"$name_tpe\""
    stack build &>/dev/null
    stack run -- "$p/app/Main.hs" >> "$p.tmp" || return 1
    popd &>/dev/null
    mv "$p.tmp" "$p/app/Main.hs"
    rm "$p.tmp" &>/dev/null
    return 0
}

function dotarrowCirce() {
    [ $# -eq 1 ] || return
    local x=dotarrow
    local app="../$x/circe/app.scala.in"
    local n=`cat "$app" | grep -n '^..private.val.app' | awk -F: '{ print $1 }'`
    cat "$app" | head -n $n >| "$x/tmp/$1.tmp"
    cat "$x/src/$1.scala.src" |
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
    mv "$x/tmp/$1.tmp" "$x/$1.scala"
    rm "$x/src/$1.scala.src" &>/dev/null
}

function dotarrowCirce2() {
    [ $# -eq 1 ] || return
    local x=dotarrow
    local app2="../$x/circe/app2.scala.in"
    local n=`cat "$app2" | grep -n '^..private.val.app' | awk -F: '{ print $1 }'`
    local name_tpe="`tac \"$x/tmp/$1.txt\" | head -n 1`"
    local name="${name_tpe%% *}"
    local tpe="${name_tpe#* }"
    cat "$app2" | head -n $n >| "$x/tmp/$1.tmp"
    cat "$x/src/$1.scala.src" |
    sed -e 's/Init.Type.Select.Term.Name."IOApp".,.Type.Name."Simple"..,.Name.Anonymous.., Nil./Init(Type.Select(Term.Name("IOApp"), Type.Name("Simple")), Name.Anonymous(), Seq())/' \
        -e 's/Init.Type.Name.\(["][^"]\+["]\).,.Name.Anonymous..,.Nil./Init(Type.Name(\1), Name.Anonymous(), Seq())/g' \
        -e 's/^/    /' >> "$x/tmp/$1.tmp"
    cat "$app2" | tail -n +`expr $n + 1` |
    sed -e "/name[ ]*:[ ]*String/s/\$/\"$name\"/" \
        -e "/tpe[ ]*:[ ]*String/s/\$/\"$tpe\"/" >> "$x/tmp/$1.tmp"
    mv "$x/tmp/$1.tmp" "$x/$1.scala"
    rm "$x/src/$1.scala.src" &>/dev/null
}

function dotarrowStream() {
    [ $# -eq 1 ] || return
    local x=dotarrow
    local app="../$x/stream/app.scala.in"
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
}

function dotarrowStream2() {
    [ $# -eq 1 ] || return
    local x=dotarrow
    local app2="../$x/stream/app2.scala.in"
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
}

export -f dotarrowScalaToHaskell0 dotarrowScalaToHaskell dotarrowHaskellToScala
export -f dotarrowAeson0 dotarrowAeson dotarrowAeson2
export -f dotarrowCirce dotarrowCirce2 dotarrowStream dotarrowStream2
export -f pio pi pi_
