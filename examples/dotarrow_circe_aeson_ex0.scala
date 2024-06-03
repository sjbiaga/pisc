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

  given Conversion[`()`, String] = _.as[String]

  val bsh: String => String = _.replaceAll("([`\"\\\\$])", "\\\\$1")

  def cli(src: String)(args: String*) =
    s"""scala-cli compile "$src" &>/dev/null;                                            scala-cli run "$src" -q -O -nowarn -S 3.5.0-RC1 -- """ + args
      .mkString(" ")

  val tmp: String =
    "sh -c 'mktemp -up dotarrow'".!!.stripTrailing.stripPrefix("dotarrow/")

  val tmp2: String =
    "sh -c 'mktemp -up dotarrow'".!!.stripTrailing.stripPrefix("dotarrow/")

  val cwd: String =
    s"""sh -c 'readlink -m dotarrow/tmp/$tmp'""".!!.stripTrailing

  def circe(src: String): Option[String] = {
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
            )()} 3>&1 1>&2- 2>&3- | sed -e "s/[ ]/\\\\\\\\ /g"                                                                            >> "$cwd".txt'""".! && 0 == s"""sh -c '${cli(
              "../dotarrow/source.scala"
            )(
              s"dotarrow/$tmp.scala"
            )}'""".! && 0 == s"""sh -c 'dotarrowCirce2 "$tmp"'""".!
        then { Some(s"""sh -c '${cli(s"dotarrow/$tmp.scala")()}'""".!!); }
        else None
      } else None
    } else None
  }

  val s2s: Option[String] => IO[Option[String]] = { it =>
    IO.pure(it.flatMap(circe(_)))
  }

  val h2h: Option[String] => IO[Option[String]] = { it =>
    IO.pure(it.flatMap { src =>
      if 0 == s"""sh -c 'echo -n "${bsh(
            src
          )}" >| "$cwd"/app/Main.hs'""".! && 0 == s"""sh -c 'dotarrowAeson $tmp'""".! && 0 == s"""sh -c 'dotarrowAeson2 $tmp'""".!
      then Some(s"""sh -c 'cat "$cwd"/app/Main.hs'""".!!)
      else None
    })
  }

  val s2h: String => IO[String] = { src =>
    IO.pure {
      0 == s"""sh -c 'echo -n "${bsh(src)}" >| "dotarrow/$tmp.scala"'""".!;
      0 == s"""sh -c 'dotarrowScalaToHaskell $tmp2 $tmp'""".!;
      s"""sh -c 'cat "$cwd/app/Main.hs"'""".!!
    }
  }

  val h2s: String => IO[String] = { src =>
    IO.pure {
      0 == s"""sh -c 'echo -n "${bsh(src)}" >| "$cwd"/app/Main.hs'""".!;
      0 == s"""sh -c 'dotarrowHaskellToScala $tmp2 $tmp'""".!;
      s"""sh -c 'cat "dotarrow/$tmp.scala"'""".!!
    }
  }

  def Sc2Hs(s2: `()`, hs: `()`): IO[Unit] = for {
    _b531ce8f_f2c7_456a_a192_0c5d8dbf76ac <- IO {
      def _b531ce8f_f2c7_456a_a192_0c5d8dbf76ac(src: `()`): IO[Unit] =
        if (!src) IO.cede
        else (
          for {
            _ <- τ
            _ <- IO {
              println(src)
            }
            _ <- hs(Some(src.as[String]))
          } yield (),
          for {
            src <- s2()(s2h)
            _   <- _b531ce8f_f2c7_456a_a192_0c5d8dbf76ac(src)
          } yield ()
        ).parMapN { (_, _) => }
      _b531ce8f_f2c7_456a_a192_0c5d8dbf76ac
    }
    src                                   <- s2()(s2h)
    _ <- _b531ce8f_f2c7_456a_a192_0c5d8dbf76ac(src)
  } yield ()

  def Hs2Sc(h2: `()`, sc: `()`): IO[Unit] = for {
    _fc90432c_d3d0_419c_9a5e_397d56c517b6 <- IO {
      def _fc90432c_d3d0_419c_9a5e_397d56c517b6(src: `()`): IO[Unit] =
        if (!src) IO.cede
        else (
          for {
            _ <- τ
            _ <- IO {
              println(src)
            }
            _ <- sc(Some(src.as[String]))
          } yield (),
          for {
            src <- h2()(h2s)
            _   <- _fc90432c_d3d0_419c_9a5e_397d56c517b6(src)
          } yield ()
        ).parMapN { (_, _) => }
      _fc90432c_d3d0_419c_9a5e_397d56c517b6
    }
    src                                   <- h2()(h2s)
    _ <- _fc90432c_d3d0_419c_9a5e_397d56c517b6(src)
  } yield ()

  def Aeson(hs: `()`, h2: `()`, ch: `()`): IO[Unit] = for {
    _f10b6d30_8fe2_42bd_b728_57bb59679da1 <- IO {
      def _f10b6d30_8fe2_42bd_b728_57bb59679da1(code: `()`): IO[Unit] =
        if (!code) IO.cede
        else (
          if (code.nonEmpty ==== true) for {
            _ <- τ
            _ <- IO {
              println(code.get)
            }
            _ <- h2(code.get)
          } yield ()
          else for (_ <- ch(ch)) yield (),
          for {
            code <- hs()(h2h)
            _    <- _f10b6d30_8fe2_42bd_b728_57bb59679da1(code)
          } yield ()
        ).parMapN { (_, _) => }
      _f10b6d30_8fe2_42bd_b728_57bb59679da1
    }
    code                                  <- hs()(h2h)
    _ <- _f10b6d30_8fe2_42bd_b728_57bb59679da1(code)
  } yield ()

  def Circe(sc: `()`, s2: `()`, ch: `()`): IO[Unit] = for {
    _a44355df_dd17_4946_b0c0_ffa6cf5fc5d5 <- IO {
      def _a44355df_dd17_4946_b0c0_ffa6cf5fc5d5(code: `()`): IO[Unit] =
        if (!code) IO.cede
        else (
          if (code.nonEmpty ==== true) for {
            _ <- τ
            _ <- IO {
              println(code.get)
            }
            _ <- s2(code.get)
          } yield ()
          else for (_ <- ch(ch)) yield (),
          for {
            code <- sc()(s2s)
            _    <- _a44355df_dd17_4946_b0c0_ffa6cf5fc5d5(code)
          } yield ()
        ).parMapN { (_, _) => }
      _a44355df_dd17_4946_b0c0_ffa6cf5fc5d5
    }
    code                                  <- sc()(s2s)
    _ <- _a44355df_dd17_4946_b0c0_ffa6cf5fc5d5(code)
  } yield ()

  def Start(src: `()`, s2: `()`, ch: `()`): IO[Unit] = for {
    _    <- τ
    _    <- IO {
      println(src)
    }
    _    <- τ
    code <- IO {
      circe(src)
    }
    _    <-
      if (code.nonEmpty ==== true) for {
        _ <- τ
        _ <- IO {
          println(code.get)
        }
        _ <- s2(code.get)
      } yield ()
      else for (_ <- ch(ch)) yield ()
  } yield ()

  def Main(args: String*): IO[Unit] = for {
    _  <- τ
    _  <- IO {
      s"""sh -c 'echo -n "{}" >| "$cwd".json'""".!
    }
    _  <- τ
    _  <- IO {
      s"""sh -c 'echo -n >| "$cwd".txt'""".!
    }
    _  <- τ
    _  <- IO {
      s"""sh -c 'cp -r ../dotarrow/sc2hs/prj "$cwd"'""".!
    }
    sc <- ν
    hs <- ν
    s2 <- ν
    h2 <- ν
    ch <- ν
    _  <- (
      Start(s"""sh -c 'cat "dotarrow/${args(0)}.scala"'""".!!, s2, ch),
      Circe(sc, s2, ch),
      Sc2Hs(s2, hs),
      Aeson(hs, h2, ch),
      Hs2Sc(h2, sc),
      for {
        ch <- ch()
        _  <- sc(`()`(null))
        _  <- hs(`()`(null))
        _  <- s2(`()`(null))
        _  <- h2(`()`(null))
      } yield ()
    ).parMapN { (_, _, _, _, _, _) => }
  } yield ()
