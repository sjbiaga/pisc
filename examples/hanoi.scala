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
      for {
        _971d27d0_961a_4e66_b5b8_f12a3fd3e181 <- IO {
          lazy val _971d27d0_961a_4e66_b5b8_f12a3fd3e181: IO[Unit] = (
            IO.unit,
            for {
              _8247017a_1cb2_4d16_9345_e094e2a7c31f <- a("a")
              _                                     <-
                if (_8247017a_1cb2_4d16_9345_e094e2a7c31f eq None) IO.cede
                else _971d27d0_961a_4e66_b5b8_f12a3fd3e181
            } yield ()
          ).parMapN { (_, _) => }
          _971d27d0_961a_4e66_b5b8_f12a3fd3e181
        }
        _8247017a_1cb2_4d16_9345_e094e2a7c31f <- a("a")
        _                                     <-
          if (_8247017a_1cb2_4d16_9345_e094e2a7c31f eq None) IO.cede
          else _971d27d0_961a_4e66_b5b8_f12a3fd3e181
      } yield (),
      for {
        _6fa47f56_3dd3_44e3_a1ec_18ffc55026ab <- IO {
          lazy val _6fa47f56_3dd3_44e3_a1ec_18ffc55026ab: IO[Unit] = (
            IO.unit,
            for {
              _788658a5_e0dc_45b4_966f_bfcc3c2fc38a <- b("b")
              _                                     <-
                if (_788658a5_e0dc_45b4_966f_bfcc3c2fc38a eq None) IO.cede
                else _6fa47f56_3dd3_44e3_a1ec_18ffc55026ab
            } yield ()
          ).parMapN { (_, _) => }
          _6fa47f56_3dd3_44e3_a1ec_18ffc55026ab
        }
        _788658a5_e0dc_45b4_966f_bfcc3c2fc38a <- b("b")
        _                                     <-
          if (_788658a5_e0dc_45b4_966f_bfcc3c2fc38a eq None) IO.cede
          else _6fa47f56_3dd3_44e3_a1ec_18ffc55026ab
      } yield (),
      for {
        _1b3cb6e6_4bad_4958_aaa5_9387a6b15b0e <- IO {
          lazy val _1b3cb6e6_4bad_4958_aaa5_9387a6b15b0e: IO[Unit] = (
            IO.unit,
            for {
              _d6a50cdc_2de0_4461_bab3_0655301fb53b <- c("c")
              _                                     <-
                if (_d6a50cdc_2de0_4461_bab3_0655301fb53b eq None) IO.cede
                else _1b3cb6e6_4bad_4958_aaa5_9387a6b15b0e
            } yield ()
          ).parMapN { (_, _) => }
          _1b3cb6e6_4bad_4958_aaa5_9387a6b15b0e
        }
        _d6a50cdc_2de0_4461_bab3_0655301fb53b <- c("c")
        _                                     <-
          if (_d6a50cdc_2de0_4461_bab3_0655301fb53b eq None) IO.cede
          else _1b3cb6e6_4bad_4958_aaa5_9387a6b15b0e
      } yield (),
      Hanoi(a, b, c, -1)
    ).parMapN { (_, _, _, _) => }
  } yield ()

  def Hanoi(a: `()`, b: `()`, c: `()`, n: `()`): IO[Unit] = for {
    w <- ν
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
      ).parMapN { (_, _) => }
    } yield ()
