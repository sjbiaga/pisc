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
package kk

import java.io.{ FileWriter, BufferedWriter }
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Paths

import scala.io.Source

import parser.StochasticPi
import parser.Calculus.`(*)`
import emitter.kk.Program


object Main extends helper.Main:

  val examples = "examples"

  def main(args: Array[String]): Unit =
    var opt = 2

    def pin(arg: String) =
      val in = if arg.endsWith(".pisc") then arg else arg + ".pisc"
      val out = Paths.get(s"$examples/in/", in.stripSuffix("pisc") + "scala.in").toString
      var source: Source = null
      var fwr: FileWriter = null
      var bwr: BufferedWriter = null

      val spi = StochasticPi.Main(StochasticPi.Emitter.kk, in)

      try
        val root = if arg.startsWith("test") then "test" else "pisc"
        source = Source.fromFile(s"$examples/$root/$in")
        fwr = FileWriter(out, UTF_8)
        bwr = BufferedWriter(fwr)

        val bs = spi(source)

        val bind = bs.zipWithIndex
        val prog_ = bind.filter(_._1.isRight).map(_.right.get -> _)

        val (prog, (discarded, excluded, enabled)) = spi(prog_.map(_._1))

        val ps = Program.Main(opt)(prog)
        val is = prog_.map(_._2).zipWithIndex.map(_.swap).toMap

        val ls = bind.filter(_._1.isLeft).map(_.left.get -> _)

        val code = (ps.zipWithIndex.map { _ -> is(_) } ++ ls)
          .sortBy(_._2)
          .map(_._1)
          .mkString("\n\n")

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

        bwr.write(magic + elvis + init + code, 0, magic.length + elvis.length + init.length + code.length)
      catch t =>
        Console.err.println(s"Error in file `$in' ${spi.ln}! " + t.getMessage + ".")
        throw t
      finally
        if bwr ne null then bwr.close()
        if fwr ne null then fwr.close()
        if source ne null then source.close()

    args.foreach {
      case "-O" => opt = 2
      case it if it.startsWith("-O") =>
        val arg = it.substring(2)
        try opt = arg.toInt min 2 max 0
        catch _ => { opt = 2; pin(arg) }
      case it => pin(it)
    }
