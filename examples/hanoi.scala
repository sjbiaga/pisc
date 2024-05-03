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

  val ! : Seq[Any] => IO[Seq[Any]] = { _ => IO.pure(Seq(null)) }

  def Main(args: String*): IO[Unit] = for {
    a <- ν
    b <- ν
    c <- ν
    _ <- (
      IO.unit,
      for {
        _d2f1937e_4c88_4304_aeb3_b80a60fa9c58 <- IO {
          lazy val _d2f1937e_4c88_4304_aeb3_b80a60fa9c58: IO[Unit] = (
            IO.unit,
            for {
              _6905a3ea_8dc8_44f5_b7ec_af3bd77cd22d <- a("a")
              _                                     <-
                if (_6905a3ea_8dc8_44f5_b7ec_af3bd77cd22d eq None) IO.cede
                else _d2f1937e_4c88_4304_aeb3_b80a60fa9c58
            } yield ()
          ).parMapN { (_, _) => }
          _d2f1937e_4c88_4304_aeb3_b80a60fa9c58
        }
        _6905a3ea_8dc8_44f5_b7ec_af3bd77cd22d <- a("a")
        _                                     <-
          if (_6905a3ea_8dc8_44f5_b7ec_af3bd77cd22d eq None) IO.cede
          else _d2f1937e_4c88_4304_aeb3_b80a60fa9c58
      } yield (),
      for {
        _89aa1651_e8be_457c_a7df_e23809c16f10 <- IO {
          lazy val _89aa1651_e8be_457c_a7df_e23809c16f10: IO[Unit] = (
            IO.unit,
            for {
              _4e192a6d_361a_4b16_b099_fd7b0e574468 <- b("b")
              _                                     <-
                if (_4e192a6d_361a_4b16_b099_fd7b0e574468 eq None) IO.cede
                else _89aa1651_e8be_457c_a7df_e23809c16f10
            } yield ()
          ).parMapN { (_, _) => }
          _89aa1651_e8be_457c_a7df_e23809c16f10
        }
        _4e192a6d_361a_4b16_b099_fd7b0e574468 <- b("b")
        _                                     <-
          if (_4e192a6d_361a_4b16_b099_fd7b0e574468 eq None) IO.cede
          else _89aa1651_e8be_457c_a7df_e23809c16f10
      } yield (),
      for {
        _36d9667f_e4a6_4093_90ee_fd505efc7e43 <- IO {
          lazy val _36d9667f_e4a6_4093_90ee_fd505efc7e43: IO[Unit] = (
            IO.unit,
            for {
              _a7475f55_e900_450a_b3ae_90acf8e68c79 <- c("c")
              _                                     <-
                if (_a7475f55_e900_450a_b3ae_90acf8e68c79 eq None) IO.cede
                else _36d9667f_e4a6_4093_90ee_fd505efc7e43
            } yield ()
          ).parMapN { (_, _) => }
          _36d9667f_e4a6_4093_90ee_fd505efc7e43
        }
        _a7475f55_e900_450a_b3ae_90acf8e68c79 <- c("c")
        _                                     <-
          if (_a7475f55_e900_450a_b3ae_90acf8e68c79 eq None) IO.cede
          else _36d9667f_e4a6_4093_90ee_fd505efc7e43
      } yield (),
      Hanoi(a, b, c, -1)
    ).parMapN { (_, _, _, _, _) => }
  } yield ()

  def Hanoi(a: `()`, b: `()`, c: `()`, n: `()`): IO[Unit] = for {
    w <- ν
    _ <- (
      IO.unit,
      for {
        _ <- τ
        _ <- IO.print("n = ")
        n <- IO.blocking {
          try scala.io.StdIn.readLine.toInt
          catch _ => -1
        }
        _ <-
          if (n > 0 ==== true) (
            Hanoi(a, b, c, n, w),
            for {
              Seq(w) <- w()
              Seq(a) <- a()(!)
              Seq(b) <- b()(!)
              Seq(c) <- c()(!)
              _      <- τ
              _      <- IO {
                println(s"#${w} moves")
              }
            } yield ()
          ).parMapN { (_, _) => }
          else IO.unit
      } yield ()
    ).parMapN { (_, _) => }
  } yield ()

  def Move(from: `()`, to: `()`, z: `()`): IO[Unit] = for {
    Seq(x) <- from()
    Seq(y) <- to()
    _      <- τ
    _      <- IO {
      println(s"${x} -> ${y}")
    }
    _      <- z(1)
  } yield ()

  def Hanoi(a: `()`, b: `()`, c: `()`, n: `()`, w: `()`): IO[Unit] =
    if (n ==== 1) for {
      z <- ν
      _ <- (
        Move(a, c, z),
        for {
          Seq(z) <- z()
          _      <- w(z)
        } yield ()
      ).parMapN { (_, _) => }
    } yield ()
    else for {
      x <- ν
      _ <- (
        IO.unit,
        Hanoi(a, c, b, n - 1, x),
        for {
          Seq(x) <- x()
          y      <- ν
          _      <- (
            Move(a, c, y),
            for {
              Seq(y) <- y()
              z      <- ν
              _      <- (
                Hanoi(b, a, c, n - 1, z),
                for {
                  Seq(z) <- z()
                  _      <- w(x + y + z)
                } yield ()
              ).parMapN { (_, _) => }
            } yield ()
          ).parMapN { (_, _) => }
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
