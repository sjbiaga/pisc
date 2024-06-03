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

//> using dep org.scalameta:scalameta_2.13:4.9.5

package dotarrow

import scala.meta.{ Source => File, _ }
import dialects.Scala3

import java.io.{ FileWriter, BufferedWriter }
import java.nio.charset.Charset
import java.nio.file.Paths

import scala.io.Source


object source:

  val dotarrow = "dotarrow"

  def main(args: Array[String]): Unit =
    args.foreach { arg =>
      var in = arg
      if arg.startsWith(s"$dotarrow/") then in = in.replaceAll(s"$dotarrow/", "")
      if !arg.endsWith(".scala") then in = in + ".scala"
      val src = Paths.get(s"$dotarrow/src/", in + ".src").toString
      var source: Source = null
      var fwr: FileWriter = null
      var bwr: BufferedWriter = null

      try
        source = Source.fromFile(s"$dotarrow/$in")
        fwr = FileWriter(src, Charset.forName("UTF-8"))
        bwr = BufferedWriter(fwr)

        val top = source.getLines.mkString("\n").parse[File].get.structure

        bwr.write(top, 0, top.length)
      finally
        if bwr ne null then bwr.close()
        if fwr ne null then fwr.close()
        if source ne null then source.close()
    }
