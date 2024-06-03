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

  val tmp: String =
    "sh -c 'mktemp -up dotarrow'".!!.stripTrailing.stripPrefix("dotarrow/")

  val cwd: String =
    s"""sh -c 'readlink -m dotarrow/tmp/$tmp'""".!!.stripTrailing

  var ex: Option[String] = None

  val run: Option[String] => IO[Option[String]] = { it =>
    IO.pure(it.flatMap { src =>
      if 0 == s"""sh -c 'echo -n "${bsh(
            src
          )}" >| "$cwd"/app/Main.hs'""".! && 0 == s"""sh -c 'dotarrowAeson $tmp'""".! && 0 == s"""sh -c 'dotarrowAeson2 $tmp'""".!
      then Some(s"""sh -c 'cat "$cwd"/app/Main.hs'""".!!)
      else None
    })
  }

  def Init(src: `()`, ch: `()`): IO[Unit] = for {
    _ <- τ
    _ <- IO {
      println(src)
    }
    _ <- ch(Some(src.as[String]))
  } yield ()

  def Main(args: String*): IO[Unit] = for {
    _  <- τ
    _  <- IO {
      s"""sh -c 'cp -r dotarrow/"${args(0)}" "$cwd"'""".!
    }
    _  <- τ
    _  <- IO {
      s"""sh -c 'cp ../dotarrow/aeson/json.in "$cwd".json'""".!
    }
    _  <- τ
    _  <- IO {
      s"""sh -c 'touch "$cwd".txt'""".!
    }
    ch <- ν
    _  <- (
      IO.unit,
      for {
        _59073c0a_2fad_4dd9_9812_8eba341480a2 <- IO {
          def _59073c0a_2fad_4dd9_9812_8eba341480a2(code: `()`): IO[Unit] =
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
                _    <- _59073c0a_2fad_4dd9_9812_8eba341480a2(code)
              } yield ()
            ).parMapN { (_, _) => }
          _59073c0a_2fad_4dd9_9812_8eba341480a2
        }
        code                                  <- ch()(run)
        _ <- _59073c0a_2fad_4dd9_9812_8eba341480a2(code)
      } yield (),
      Init(s"""sh -c 'cat "$cwd"/app/Main.hs'""".!!, ch)
    ).parMapN { (_, _, _) => }
  } yield ()
