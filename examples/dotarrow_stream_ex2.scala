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

  val _ =
    s"""sh -c 'cp ../dotarrow/stream/bin.in dotarrow/tmp/"${tmp}_ex2.scala.bin"'""".!

  val _ = s"""sh -c 'touch dotarrow/tmp/"${tmp}_ex2.scala.txt"'""".!

  val ex2: Option[String] = Some("""sh -c 'cat "dotarrow/ex2.scala"'""".!!)

  val run: Option[String] => IO[Option[String]] = { it =>
    IO.pure(it.flatMap { src =>
      if 0 == s"""sh -c 'echo -n "${bsh(
            src
          )}" >| "dotarrow/${tmp}_ex2.scala"'""".! && 0 == s"""sh -c '${cli(
            "../dotarrow/source.scala"
          )(
            s"dotarrow/${tmp}_ex2.scala"
          )}'""".! && 0 == s"""sh -c 'dotarrowStream "${tmp}_ex2.scala"'""".! && 0 == s"""sh -c 'mv dotarrow/tmp/"${tmp}_ex2.scala.tmp" dotarrow/"${tmp}_ex2.scala"'""".! && 0 == s"""sh -c 'rm dotarrow/src/"${tmp}_ex2.scala.src" &>/dev/null'""".!
      then {
        val out =
          s"""sh -c '${cli(s"dotarrow/${tmp}_ex2.scala")()}'""".lazyLines_!;
        if out.nonEmpty then {
          val src =
            "//> using dep org.typelevel::cats-effect:3.6-0142603\n" + out
              .mkString("\n");
          if 0 == s"""sh -c 'echo -n "${bsh(
                src
              )}" >| "dotarrow/${tmp}_ex2.scala"'""".! && 0 == s"""sh -c '${cli(
                s"dotarrow/${tmp}_ex2.scala"
              )()} 3>&1 1>&2- 2>&3- | sed -e "s/[ ]/\\\\\\\\ /g"                                                                  >> "dotarrow/tmp/${tmp}_ex2.scala.txt"'""".! && 0 == s"""sh -c '${cli(
                "../dotarrow/source.scala"
              )(
                s"dotarrow/${tmp}_ex2.scala"
              )}'""".! && 0 == s"""sh -c 'dotarrowStream2 "${tmp}_ex2.scala"'""".! && 0 == s"""sh -c 'mv dotarrow/tmp/"${tmp}_ex2.scala.tmp" dotarrow/"${tmp}_ex2.scala"'""".! && 0 == s"""sh -c 'rm dotarrow/src/"${tmp}_ex2.scala.src"' &>/dev/null""".!
          then {
            Some(s"""sh -c '${cli(s"dotarrow/${tmp}_ex2.scala")()}'""".!!);
          } else None
        } else None
      } else None
    })
  }

  def Main(args: String*): IO[Unit] = for {
    ch  <- ν
    lnk <- ν
    _   <- (Node1(ch), Node2(ch, lnk)).parMapN { (_, _) => }
  } yield ()

  def Node1(ch: `()`): IO[Unit] = for {
    _ <- τ
    _ <- IO {
      println(s"Node1:\n${ex2.get}")
    }
    _ <- ch(ex2)
  } yield ()

  def Node2(ch: `()`, lnk: `()`): IO[Unit] = for {
    code <- ch()(run)
    _    <-
      if (code.nonEmpty ==== true) for {
        _ <- τ
        _ <- IO {
          println(s"Node2:\n${code.get}")
        }
        _ <- (
          for {
            _ <- lnk(ch)
            _ <- ch(code)
          } yield (),
          Node3(lnk)
        ).parMapN { (_, _) => }
      } yield ()
      else IO.unit
  } yield ()

  def Node3(lnk: `()`): IO[Unit] = for {
    ch   <- lnk()
    code <- ch()(run)
    _    <-
      if (code.nonEmpty ==== true) for {
        _    <- τ
        _    <- IO {
          println(s"Node3:\n${code.get}")
        }
        ch2  <- ν
        lnk2 <- ν
        _    <-
          (for (_ <- ch2(code)) yield (), Node2(ch2, lnk2)).parMapN { (_, _) =>
          }
      } yield ()
      else IO.unit
  } yield ()
