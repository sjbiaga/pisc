/*
 * Copyright (c) 2023-2025 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * [Except as contained in this notice, the name of Sebastian I. Gliţa-Catina
 * shall not be used in advertising or otherwise to promote the sale, use
 * or other dealings in this Software without prior written authorization
 * from Sebastian I. Gliţa-Catina.]
 */

package pisc
package fs2

import java.io.{ FileWriter, BufferedWriter }
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Paths

import scala.io.Source

import scala.meta.*
import dialects.Scala3

import parser.StochasticPi
import parser.Calculus.{ `(*)`, λ }
import emitter.fs2.Program


object Main extends helper.Main:

  val examples = "examples"

  def main(args: Array[String]): Unit =
    var F = "cats.effect.IO"

    def pin(arg: String) =
      val in = if arg.endsWith(".pisc") then arg else arg + ".pisc"
      val out = Paths.get(s"$examples/in/", in.stripSuffix("pisc") + "scala.in").toString
      var source: Source = null
      var fwr: FileWriter = null
      var bwr: BufferedWriter = null

      val spi = StochasticPi.Main(StochasticPi.Emitter.fs2, in)

      try
        val root = if arg.startsWith("test") then "test" else "pisc"
        source = Source.fromFile(s"$examples/$root/$in")
        fwr = FileWriter(out, UTF_8)
        bwr = BufferedWriter(fwr)

        val bs = spi(source)

        val bind = bs.zipWithIndex
        val prog_ = bind.filter(_._1.isRight).map(_.right.get -> _)

        val (prog, (discarded, excluded, enabled)) = spi(prog_.map(_._1))

        val ps =
          ( F.split('.').reverse match
              case Array(tpe, _path*) =>
                val path = _path.reverse.map(Term.Name(_)).foldLeft(Term.Name("_root_"): Term)(Term.Select(_, _))
                Defn.Type(Nil, Type.Name("F"), Type.ParamClause(Nil),
                          Type.Select(path.asInstanceOf[Term.Select], Type.Name(tpe)),
                          Type.Bounds(None, None, Nil, Nil))
          )
          ::
          ( prog.tail.head match
              case (`(*)`(_, λ(parallelism: Lit.Int)), _) =>
                Defn.Val(Nil, Pat.Var(Term.Name("π-parallelism")) :: Nil, None, parallelism)
          )
          :: Program.Main()(prog)

        val is = prog_.drop(2).map(_._2).zipWithIndex.map(_.swap).toMap

        val ls = bind.drop(2).filter(_._1.isLeft).map(_.left.get -> _)

        val tc: String => Type =
          _.split('.').reverse match
            case Array(it) => Type.Name(it)
            case Array(hd, tl*) => Type.Select(tl.reverse.map(Term.Name(_)).foldLeft(Term.Name("_root_"): Term)(Term.Select(_, _)).asInstanceOf[Term.Select], Type.Name(hd))

        val tcs =
          prog.head match
            case (`(*)`(_, λ(_: Lit.Null)), _) => Set("Async")
            case (`(*)`(_, λ(typeclasses: Term.Tuple)), _) =>
              typeclasses.args.map { case Term.Name(it) => it }.toSet

        val code = (ps.drop(2).zipWithIndex.map(_ -> is(_)) ++ ls.map(_.parse[Stat].get -> _))
          .sortBy(_._2)
          .map(_._1)

        val main = Defn.Class(Mod.Final() :: Nil,
                              Type.Name("πmain"),
                              Type.ParamClause(
                                Type.Param(Nil,
                                           Type.Name("F"),
                                           Type.ParamClause(Type.Param(Nil, Name.Placeholder(),
                                                                       Type.ParamClause(Nil), Type.Bounds(None, None, Nil, Nil)) :: Nil),
                                           Type.Bounds(None, None, tcs.map(tc).toList, Nil)) :: Nil),
                              Ctor.Primary(Nil, Name.Anonymous(), Seq.empty),
                              Template(None, Nil, Template.Body(None, code), Nil))

        val codeʹ = ps.take(2).mkString("\n\n") + "\n\n" + main.toString

        val trick = `trick-or-treat`("π-trick", discarded).toString
        val spell = `spell, magic spell`("π-spell", enabled).toString
        val wand = `magic wand`.toString

        val magic = trick + "\n\n" + spell + "\n\n" + wand + "\n\n"
        val elvis = `if-then-else`("π-elvis", excluded).toString + "\n\n"

        val init = Π(
          prog
            .find {
              case (`(*)`("Main"), _) => true
              case _ => false
            }
            .get
            ._2
            .enabled
        ).toString + "\n\n"

        bwr.write(magic + elvis + init + codeʹ, 0, magic.length + elvis.length + init.length + codeʹ.length)
      catch t =>
        Console.err.println(s"Error in file `$in' ${spi.ln}! " + t.getMessage + ".")
        throw t
      finally
        if bwr ne null then bwr.close()
        if fwr ne null then fwr.close()
        if source ne null then source.close()

    args.foreach {
      case "-F" => F = "cats.effect.IO"
      case it if it.startsWith("-F") => F = it.substring(2)
      case it => pin(it)
    }
