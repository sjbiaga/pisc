/*
 * Copyright (c) 2023-2024 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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

import java.io.{ FileWriter, BufferedWriter }
import java.nio.charset.Charset
import java.nio.file.Paths

import scala.io.Source

import parser.PolyadicPi


object Main:

  val examples = "examples"

  def main(args: Array[String]): Unit =
    args.foreach { arg =>
      val in = if arg.endsWith(".pisc") then arg else arg + ".pisc"
      val out = Paths.get(s"$examples/in/", in.stripSuffix("pisc") + "scala.in").toString
      var source: Source = null
      var fwr: FileWriter = null
      var bwr: BufferedWriter = null

      try
        source = Source.fromFile(s"$examples/pisc/$in")
        fwr = FileWriter(out, Charset.forName("UTF-8"))
        bwr = BufferedWriter(fwr)

        val bind = PolyadicPi(source).zipWithIndex
        val prog = bind.filter(_._1.isRight).map { it => it._1.right.get -> it._2 }

        val ps = Program(prog.map(_._1))
        val is = prog.map(_._2).zipWithIndex.map(_.swap).toMap

        val ls = bind.filter(_._1.isLeft).map { it => it._1.left.get -> it._2 }

        val code = (ps.zipWithIndex.map { _ -> is(_) } ++ ls)
          .sortBy(_._2)
          .map(_._1)
          .mkString("\n\n")

        bwr.write(code, 0, code.length)
      finally
        if bwr ne null then bwr.close()
        if fwr ne null then fwr.close()
        if source ne null then source.close()
    }
