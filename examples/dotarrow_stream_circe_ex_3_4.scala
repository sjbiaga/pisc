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
    s"""sh -c 'cp ../dotarrow/stream/bin.in dotarrow/tmp/"${tmp}_ex3.scala.bin"'""".!

  val _ = s"""sh -c 'touch dotarrow/tmp/"${tmp}_ex3.scala.txt"'""".!

  val ex3: Option[String] = Some("""sh -c 'cat "dotarrow/ex3.scala"'""".!!)

  val run_ex3: Option[String] => IO[Option[String]] = { it =>
    IO.pure(it.flatMap { src =>
      if 0 == s"""sh -c 'echo -n "${bsh(
            src
          )}" >| "dotarrow/${tmp}_ex3.scala"'""".! && 0 == s"""sh -c '${cli(
            "../dotarrow/source.scala"
          )(
            s"dotarrow/${tmp}_ex3.scala"
          )}'""".! && 0 == s"""sh -c 'dotarrowStream "${tmp}_ex3.scala"'""".! && 0 == s"""sh -c 'mv dotarrow/tmp/"${tmp}_ex3.scala.tmp" dotarrow/"${tmp}_ex3.scala"'""".! && 0 == s"""sh -c 'rm dotarrow/src/"${tmp}_ex3.scala.src" &>/dev/null'""".!
      then {
        val out =
          s"""sh -c '${cli(s"dotarrow/${tmp}_ex3.scala")()}'""".lazyLines_!;
        if out.nonEmpty then {
          val src =
            "//> using dep org.typelevel::cats-effect:3.6-0142603\n" + out
              .mkString("\n");
          if 0 == s"""sh -c 'echo -n "${bsh(
                src
              )}" >| "dotarrow/${tmp}_ex3.scala"'""".! && 0 == s"""sh -c '${cli(
                s"dotarrow/${tmp}_ex3.scala"
              )()} 3>&1 1>&2- 2>&3- | sed -e "s/[ ]/\\\\\\\\ /g"                                                                  >> "dotarrow/tmp/${tmp}_ex3.scala.txt"'""".! && 0 == s"""sh -c '${cli(
                "../dotarrow/source.scala"
              )(
                s"dotarrow/${tmp}_ex3.scala"
              )}'""".! && 0 == s"""sh -c 'dotarrowStream2 "${tmp}_ex3.scala"'""".! && 0 == s"""sh -c 'mv dotarrow/tmp/"${tmp}_ex3.scala.tmp" dotarrow/"${tmp}_ex3.scala"'""".! && 0 == s"""sh -c 'rm dotarrow/src/"${tmp}_ex3.scala.src"' &>/dev/null""".!
          then {
            Some(s"""sh -c '${cli(s"dotarrow/${tmp}_ex3.scala")()}'""".!!);
          } else None
        } else None
      } else None
    })
  }

  def Node1(): IO[Unit] = for {
    ch <- ν
    _  <- (
      IO.unit,
      for {
        _5e460c70_ed7d_47f8_b7ee_a205c2f4aadd <- IO {
          def _5e460c70_ed7d_47f8_b7ee_a205c2f4aadd(code: `()`): IO[Unit] =
            if (!code) IO.cede
            else (
              if (code.nonEmpty ==== true) for {
                _ <- τ
                _ <- IO {
                  println(s"Node1:\n${code.get}")
                }
                _ <- ch(code)
              } yield ()
              else for (_ <- ch(`()`(null))) yield (),
              for {
                code <- ch()(run_ex3)
                _    <- _5e460c70_ed7d_47f8_b7ee_a205c2f4aadd(code)
              } yield ()
            ).parMapN { (_, _) => }
          _5e460c70_ed7d_47f8_b7ee_a205c2f4aadd
        }
        code                                  <- ch()(run_ex3)
        _ <- _5e460c70_ed7d_47f8_b7ee_a205c2f4aadd(code)
      } yield (),
      for {
        _ <- τ
        _ <- IO {
          println(s"Node1:\n${ex3.get}")
        }
        _ <- ch(ex3)
      } yield ()
    ).parMapN { (_, _, _) => }
  } yield ()

  val _ = s"""sh -c 'echo "{}" >| dotarrow/tmp/"$tmp.json"'""".!

  val _ = s"""sh -c 'touch dotarrow/tmp/"$tmp.txt"'""".!

  val ex4: Option[String] = Some("""sh -c 'cat "dotarrow/ex4.scala"'""".!!)

  val run_ex4: Option[String] => IO[Option[String]] = { it =>
    IO.pure(it.flatMap { src =>
      if 0 == s"""sh -c 'echo -n "${bsh(
            src
          )}" >| "dotarrow/$tmp.scala"'""".! && 0 == s"""sh -c '${cli(
            "../dotarrow/source.scala"
          )(
            s"dotarrow/$tmp.scala"
          )}'""".! && 0 == s"""sh -c 'dotarrowCirce "$tmp"'""".!
      then {
        val out = s"""sh -c '${cli(s"dotarrow/$tmp.scala")()}'""".lazyLines_!;
        if out.nonEmpty then {
          val src =
            "//> using dep org.typelevel::cats-effect:3.6-0142603\n" + "//> using dep io.circe::circe-generic:0.15.0-M1\n" + "//> using dep io.circe::circe-parser:0.15.0-M1\n" + out
              .mkString("\n");
          if 0 == s"""sh -c 'echo -n "${bsh(
                src
              )}" >| "dotarrow/$tmp.scala"'""".! && 0 == s"""sh -c '${cli(
                s"dotarrow/$tmp.scala"
              )()} 3>&1 1>&2- 2>&3- | sed -e "s/[ ]/\\\\\\\\ /g"                                                            >> "dotarrow/tmp/$tmp.txt"'""".! && 0 == s"""sh -c '${cli(
                "../dotarrow/source.scala"
              )(
                s"dotarrow/$tmp.scala"
              )}'""".! && 0 == s"""sh -c 'dotarrowCirce2 "$tmp"'""".!
          then { Some(s"""sh -c '${cli(s"dotarrow/$tmp.scala")()}'""".!!); }
          else None
        } else None
      } else None
    })
  }

  def Node2(): IO[Unit] = for {
    ch  <- ν
    lnk <- ν
    _   <- (Node3(ch), Node4(ch, lnk)).parMapN { (_, _) => }
  } yield ()

  def Node3(ch: `()`): IO[Unit] = for {
    _ <- τ
    _ <- IO {
      println(s"Node3:\n${ex4.get}")
    }
    _ <- ch(ex4)
  } yield ()

  def Node4(ch: `()`, lnk: `()`): IO[Unit] = for {
    code <- ch()(run_ex4)
    _    <-
      if (code.nonEmpty ==== true) for {
        _ <- τ
        _ <- IO {
          println(s"Node4:\n${code.get}")
        }
        _ <- (
          for {
            _ <- lnk(ch)
            _ <- ch(code)
          } yield (),
          Node5(lnk)
        ).parMapN { (_, _) => }
      } yield ()
      else IO.unit
  } yield ()

  def Node5(lnk: `()`): IO[Unit] = for {
    ch   <- lnk()
    code <- ch()(run_ex4)
    _    <-
      if (code.nonEmpty ==== true) for {
        _    <- τ
        _    <- IO {
          println(s"Node5:\n${code.get}")
        }
        ch2  <- ν
        lnk2 <- ν
        _    <-
          (for (_ <- ch2(code)) yield (), Node4(ch2, lnk2)).parMapN { (_, _) =>
          }
      } yield ()
      else IO.unit
  } yield ()

  def Main(args: String*): IO[Unit] = (Node1(), Node2()).parMapN { (_, _) => }
