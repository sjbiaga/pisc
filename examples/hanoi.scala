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
        _964089ce_12f2_4dce_9560_3000ec4db1df <- IO {
          lazy val _964089ce_12f2_4dce_9560_3000ec4db1df: IO[Unit] = (
            IO.unit,
            for {
              _ce59bb83_5832_472a_ae73_2319e7238f21 <- a("a")
              _                                     <-
                if (_ce59bb83_5832_472a_ae73_2319e7238f21 eq None) IO.cede
                else _964089ce_12f2_4dce_9560_3000ec4db1df
            } yield ()
          ).parMapN { (_, _) => }
          _964089ce_12f2_4dce_9560_3000ec4db1df
        }
        _ce59bb83_5832_472a_ae73_2319e7238f21 <- a("a")
        _                                     <-
          if (_ce59bb83_5832_472a_ae73_2319e7238f21 eq None) IO.cede
          else _964089ce_12f2_4dce_9560_3000ec4db1df
      } yield (),
      for {
        _6604abf9_5340_4adc_80e9_e1f0bac86bdb <- IO {
          lazy val _6604abf9_5340_4adc_80e9_e1f0bac86bdb: IO[Unit] = (
            IO.unit,
            for {
              _8e9edee7_108e_44d6_88ed_a92432e2e345 <- b("b")
              _                                     <-
                if (_8e9edee7_108e_44d6_88ed_a92432e2e345 eq None) IO.cede
                else _6604abf9_5340_4adc_80e9_e1f0bac86bdb
            } yield ()
          ).parMapN { (_, _) => }
          _6604abf9_5340_4adc_80e9_e1f0bac86bdb
        }
        _8e9edee7_108e_44d6_88ed_a92432e2e345 <- b("b")
        _                                     <-
          if (_8e9edee7_108e_44d6_88ed_a92432e2e345 eq None) IO.cede
          else _6604abf9_5340_4adc_80e9_e1f0bac86bdb
      } yield (),
      for {
        _057684fb_a85b_4f69_8e01_539e05ea31b6 <- IO {
          lazy val _057684fb_a85b_4f69_8e01_539e05ea31b6: IO[Unit] = (
            IO.unit,
            for {
              _56b3e0e6_69d8_4b4f_add5_8809cb22c510 <- c("c")
              _                                     <-
                if (_56b3e0e6_69d8_4b4f_add5_8809cb22c510 eq None) IO.cede
                else _057684fb_a85b_4f69_8e01_539e05ea31b6
            } yield ()
          ).parMapN { (_, _) => }
          _057684fb_a85b_4f69_8e01_539e05ea31b6
        }
        _56b3e0e6_69d8_4b4f_add5_8809cb22c510 <- c("c")
        _                                     <-
          if (_56b3e0e6_69d8_4b4f_add5_8809cb22c510 eq None) IO.cede
          else _057684fb_a85b_4f69_8e01_539e05ea31b6
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
