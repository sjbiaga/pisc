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

  given Conversion[`()`, Int] = _.as[Int]

  val ! : String => IO[String] = { _ => IO.pure(null) }

  def Main(args: String*): IO[Unit] = for {
    a <- ν
    b <- ν
    c <- ν
    _ <- (
      for {
        _11ff61e6_20b8_43de_b268_ecf657c75e46 <- IO {
          lazy val _11ff61e6_20b8_43de_b268_ecf657c75e46: IO[Unit] = (
            IO.unit,
            for {
              _88cf54c7_0cfd_42eb_99b8_1cbff90a6131 <- a("a")
              _                                     <-
                if (_88cf54c7_0cfd_42eb_99b8_1cbff90a6131 eq None) IO.cede
                else _11ff61e6_20b8_43de_b268_ecf657c75e46
            } yield ()
          ).parMapN { (_, _) => }
          _11ff61e6_20b8_43de_b268_ecf657c75e46
        }
        _88cf54c7_0cfd_42eb_99b8_1cbff90a6131 <- a("a")
        _                                     <-
          if (_88cf54c7_0cfd_42eb_99b8_1cbff90a6131 eq None) IO.cede
          else _11ff61e6_20b8_43de_b268_ecf657c75e46
      } yield (),
      for {
        _630c9379_1957_46cb_82ec_312f6f8ed1b0 <- IO {
          lazy val _630c9379_1957_46cb_82ec_312f6f8ed1b0: IO[Unit] = (
            IO.unit,
            for {
              _0af7768f_8496_4b05_bbb9_1cddb6806b25 <- b("b")
              _                                     <-
                if (_0af7768f_8496_4b05_bbb9_1cddb6806b25 eq None) IO.cede
                else _630c9379_1957_46cb_82ec_312f6f8ed1b0
            } yield ()
          ).parMapN { (_, _) => }
          _630c9379_1957_46cb_82ec_312f6f8ed1b0
        }
        _0af7768f_8496_4b05_bbb9_1cddb6806b25 <- b("b")
        _                                     <-
          if (_0af7768f_8496_4b05_bbb9_1cddb6806b25 eq None) IO.cede
          else _630c9379_1957_46cb_82ec_312f6f8ed1b0
      } yield (),
      for {
        _73e302c9_0024_4853_92b1_81014df55b2c <- IO {
          lazy val _73e302c9_0024_4853_92b1_81014df55b2c: IO[Unit] = (
            IO.unit,
            for {
              _8cbb9f62_a569_43f1_a5d5_04fa9cc93f69 <- c("c")
              _                                     <-
                if (_8cbb9f62_a569_43f1_a5d5_04fa9cc93f69 eq None) IO.cede
                else _73e302c9_0024_4853_92b1_81014df55b2c
            } yield ()
          ).parMapN { (_, _) => }
          _73e302c9_0024_4853_92b1_81014df55b2c
        }
        _8cbb9f62_a569_43f1_a5d5_04fa9cc93f69 <- c("c")
        _                                     <-
          if (_8cbb9f62_a569_43f1_a5d5_04fa9cc93f69 eq None) IO.cede
          else _73e302c9_0024_4853_92b1_81014df55b2c
      } yield (),
      Hanoi(a, b, c, -1)
    ).parMapN { (_, _, _, _) => }
  } yield ()

  def Hanoi(a: `()`, b: `()`, c: `()`, n: `()`): IO[Unit] = for {
    w <- ν
    _ <- (
      for {
        _ <- τ
        _ <- IO.print("n = ")
        n <- IO.blocking {
          try scala.io.StdIn.readLine.toInt
          catch _ => -1
        }
        _ <- Hanoi(a, b, c, n, w)
      } yield (),
      for {
        w <- w()
        a <- a()(!)
        b <- b()(!)
        c <- c()(!)
        _ <- τ
        _ <- IO {
          println(s"#${w} moves")
        }
      } yield ()
    ).parMapN { (_, _) => }
  } yield ()

  def Move(from: `()`, to: `()`, z: `()`): IO[Unit] = for {
    x <- from()
    y <- to()
    _ <- τ
    _ <- IO {
      println(s"${x} -> ${y}")
    }
    _ <- z(1)
  } yield ()

  def Hanoi(a: `()`, b: `()`, c: `()`, n: `()`, w: `()`): IO[Unit] =
    if (n <= 1 ==== true) for {
      z <- ν
      _ <- (
        Move(a, c, z),
        for {
          z <- z()
          _ <- w(z)
        } yield ()
      ).parMapN { (_, _) => }
    } yield ()
    else for {
      x <- ν
      _ <- (
        Hanoi(a, c, b, n - 1, x),
        for {
          x <- x()
          y <- ν
          _ <- (
            Move(a, c, y),
            for {
              y <- y()
              z <- ν
              _ <- (
                Hanoi(b, a, c, n - 1, z),
                for {
                  z <- z()
                  _ <- w(x + y + z)
                } yield ()
              ).parMapN { (_, _) => }
            } yield ()
          ).parMapN { (_, _) => }
        } yield ()
      ).parMapN { (_, _) => }
    } yield ()
