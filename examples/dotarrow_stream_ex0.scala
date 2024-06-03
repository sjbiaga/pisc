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

//package main.scala.in

import _root_.cats.effect.{IO, IOApp, ExitCode}

object App extends IOApp:

  override def run(args: List[String]): IO[ExitCode] =
    π.Main(args*).as(ExitCode.Success)

object π:

  import _root_.cats.effect.syntax.all._
  import _root_.cats.syntax.all._
  import _root_.cats.effect.std.Semaphore

  import Π._

  import scala.sys.process._

  given Conversion[`()`, Option[String]] = _.as[Option[String]]

  val bsh: String => String = _.replaceAll("([`\"\\\\$])", "\\\\$1")

  def cli(src: String)(args: String*) =
    s"""scala-cli compile "$src" &>/dev/null;                                            scala-cli run "$src" -q -O -nowarn -S 3.5.0-RC1 -- """ + args
      .mkString(" ")

  val tmp: String =
    "sh -c 'mktemp -up dotarrow'".!!.stripTrailing.stripPrefix("dotarrow/")

  var ex: Option[String] = None

  val run: Option[String] => IO[Option[String]] = { it =>
    IO.pure(it.flatMap { src =>
      if 0 == s"""sh -c 'echo -n "${bsh(
            src
          )}" >| "dotarrow/${tmp}_ex0.scala"'""".! && 0 == s"""sh -c '${cli(
            "../dotarrow/source.scala"
          )(
            s"dotarrow/${tmp}_ex0.scala"
          )}'""".! && 0 == s"""sh -c 'dotarrowStream "${tmp}_ex0.scala"'""".! && 0 == s"""sh -c 'mv dotarrow/tmp/"${tmp}_ex0.scala.tmp" dotarrow/"${tmp}_ex0.scala"'""".! && 0 == s"""sh -c 'rm dotarrow/src/"${tmp}_ex0.scala.src" &>/dev/null'""".!
      then {
        val out =
          s"""sh -c '${cli(s"dotarrow/${tmp}_ex0.scala")()}'""".lazyLines_!;
        if out.nonEmpty then {
          val src =
            "//> using dep org.typelevel::cats-effect:3.6-0142603\n" + out
              .mkString("\n");
          if 0 == s"""sh -c 'echo -n "${bsh(
                src
              )}" >| "dotarrow/${tmp}_ex0.scala"'""".! && 0 == s"""sh -c '${cli(
                s"dotarrow/${tmp}_ex0.scala"
              )()} 3>&1 1>&2- 2>&3- | sed -e "s/[ ]/\\\\\\\\ /g"                                                                  >> "dotarrow/tmp/${tmp}_ex0.scala.txt"'""".! && 0 == s"""sh -c '${cli(
                "../dotarrow/source.scala"
              )(
                s"dotarrow/${tmp}_ex0.scala"
              )}'""".! && 0 == s"""sh -c 'dotarrowStream2 "${tmp}_ex0.scala"'""".! && 0 == s"""sh -c 'mv dotarrow/tmp/"${tmp}_ex0.scala.tmp" dotarrow/"${tmp}_ex0.scala"'""".! && 0 == s"""sh -c 'rm dotarrow/src/"${tmp}_ex0.scala.src"' &>/dev/null""".!
          then {
            Some(s"""sh -c '${cli(s"dotarrow/${tmp}_ex0.scala")()}'""".!!);
          } else None
        } else None
      } else None
    })
  }

  def Main(args: String*): IO[Unit] = for {
    _  <- τ
    _  <- IO {
      ex = Some(s"""sh -c 'cat "dotarrow/${args(0)}.scala"'""".!!)
    }
    _  <- τ
    _  <- IO {
      s"""sh -c 'cp ../dotarrow/stream/bin.in dotarrow/tmp/"${tmp}_ex0.scala.bin"'""".!
    }
    _  <- τ
    _  <- IO {
      s"""sh -c 'touch dotarrow/tmp/"${tmp}_ex0.scala.txt"'""".!
    }
    ch <- ν
    _  <- (
      IO.unit,
      for {
        _8500f319_594e_4de7_9612_210ab10e78d7 <- IO {
          def _8500f319_594e_4de7_9612_210ab10e78d7(code: `()`): IO[Unit] =
            if (!code) IO.cede
            else (
              if (code.nonEmpty ==== true) for {
                _ <- τ
                _ <- IO {
                  println(code.get)
                }
                _ <- ch(code)
              } yield ()
              else for (_ <- ch(`()`(null))) yield (),
              for {
                code <- ch()(run)
                _    <- _8500f319_594e_4de7_9612_210ab10e78d7(code)
              } yield ()
            ).parMapN { (_, _) => }
          _8500f319_594e_4de7_9612_210ab10e78d7
        }
        code                                  <- ch()(run)
        _ <- _8500f319_594e_4de7_9612_210ab10e78d7(code)
      } yield (),
      for {
        _ <- τ
        _ <- IO {
          println(ex.get)
        }
        _ <- ch(ex)
      } yield ()
    ).parMapN { (_, _, _) => }
  } yield ()
