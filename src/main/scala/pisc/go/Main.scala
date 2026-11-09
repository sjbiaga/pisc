/*
 * Copyright (c) 2023-2026 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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
package go

import java.io.{ FileWriter, BufferedWriter }
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Paths

import scala.collection.Map

import scala.io.Source

import scala.meta.{ Lit, Term }

import parser.StochasticPi
import parser.StochasticPi.Actions
import parser.Calculus.{ `(*)`, λ }
import emitter.go.Program
import emitter.go.Gala.decl


object Main:

  val examples = "examples"

  def main(args: Array[String]): Unit =
    var P = Int.MaxValue
    var H = 0
    var T = 123456

    def spin(arg: String) =
      val in = if arg.endsWith(".pisc") then arg else arg + ".pisc"
      val out = Paths.get(s"$examples/in/", in.stripSuffix("pisc") + "gala.in").toString
      var source: Source = null
      var fwr: FileWriter = null
      var bwr: BufferedWriter = null

      val spi = StochasticPi.Main(StochasticPi.Emitter.go, in, "localhost", P, H, T)

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
          ( prog.tail.tail.head match
              case (`(*)`(_, λ(parameters: Term)), _) =>
                decl("go_spi_parameters", parameters.toString)
          ) ::
          Program.Main()(prog.drop(1+3))

        val is = prog_.drop(1+3).map(_._2).zipWithIndex.map(_.swap).toMap

        val ls = bind.drop(1+3).filter(_._1.isLeft).map(_.left.get -> _)

        val code = ps.take(1).mkString("\n\n") + "\n\n"
                 + (ps.drop(1).zipWithIndex.map(_ -> is(_)) ++ ls)
                   .sortBy(_._2)
                   .map(_._1)
                   .mkString("\n\n")

        val trick = `trick-or-treat`(discarded)
        val spell = `spell, magic spell`(enabled)
        val wand = `magic wand`

        val magic = trick + "\n\n" + spell + "\n\n" + wand + "\n\n"
        val elvis = `if-then-else`(excluded).toString + "\n\n"

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

        bwr.write(magic + elvis + init + code, 0, magic.length + elvis.length + init.length + code.length)
      catch t =>
        spi.last match
          case Some(((it, isMacroExpansion), (line, col))) =>
            Console.err.println(s"Error in file `$in' ${spi.ln}! " + t.getMessage
                              + s""".\nLast known parse${if isMacroExpansion then " (from macro expansion) " else " "}at line #$line, column #$col.\n$it""")
          case _ =>
            Console.err.println(s"Error in file `$in' ${spi.ln}! " + t.getMessage + ".")
        throw t
      finally
        if bwr ne null then bwr.close()
        if fwr ne null then fwr.close()
        if source ne null then source.close()

    args.foreach {
      case "-P" => P = Int.MaxValue
      case "-H" => H = 0
      case "-T" => T = 123456
      case it if it.startsWith("-P") => P = it.substring(2).toInt
      case it if it.startsWith("-H") => H = it.substring(2).toInt
      case it if it.startsWith("-T") => T = it.substring(2).toInt
      case it => spin(it)
    }

  def `magic wand`: String =
    s"val go_spi_wand Tuple[$HashMap, $HashMap] = (go_spi_trick, go_spi_spell)"

  def `trick-or-treat`(discarded: Map[String, Actions]): String =
    Π("go_spi_trick", discarded)

  def `spell, magic spell`(enabled: Map[String, Actions]): String =
    Π("go_spi_spell", enabled)

  def `if-then-else`(excluded: Map[String, Actions]): String =
    Π("go_spi_elvis", excluded)

  private object Π:

    def apply(name: String, self: Map[String, Actions]): String =
      { for
          (id, it) <- self
        yield
          "    (" + Lit.String(id).toString + ", " + it.map(Lit.String(_).toString).mkString(s"$HashSetOf(", ", ", ")") + ")"
      }.mkString(s"val $name $HashMap = $HashMapOf(\n", ",\n", "\n)")

    def apply(it: Actions): String =
      it.map("\"" + _ + "\"").mkString(s"val go_spi_main $HashSet = $HashSetOf(", ", ", ")")

  val HashSet = "HashSet[string]"

  val HashMap = s"HashMap[string, $HashSet]"

  val HashSetOf = "HashSetOf[string]"

  val HashMapOf = s"HashMapOf[string, $HashSet]"
