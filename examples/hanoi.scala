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
        _1516d515_c857_4516_9b01_0bca819e6fe3 <- IO {
          lazy val _1516d515_c857_4516_9b01_0bca819e6fe3: IO[Unit] = (
            IO.unit,
            for {
              _d35d2fcd_7e4f_4d36_8702_1bd3d3330b50 <- a("a")
              _                                     <-
                if (_d35d2fcd_7e4f_4d36_8702_1bd3d3330b50 eq None) IO.cede
                else _1516d515_c857_4516_9b01_0bca819e6fe3
            } yield ()
          ).parMapN { (_, _) => }
          _1516d515_c857_4516_9b01_0bca819e6fe3
        }
        _d35d2fcd_7e4f_4d36_8702_1bd3d3330b50 <- a("a")
        _                                     <-
          if (_d35d2fcd_7e4f_4d36_8702_1bd3d3330b50 eq None) IO.cede
          else _1516d515_c857_4516_9b01_0bca819e6fe3
      } yield (),
      for {
        _e4fc91b9_6dcc_4263_ace7_13a97b17b3d8 <- IO {
          lazy val _e4fc91b9_6dcc_4263_ace7_13a97b17b3d8: IO[Unit] = (
            IO.unit,
            for {
              _fea81203_f93b_4b2b_9eb6_ee50df893e17 <- b("b")
              _                                     <-
                if (_fea81203_f93b_4b2b_9eb6_ee50df893e17 eq None) IO.cede
                else _e4fc91b9_6dcc_4263_ace7_13a97b17b3d8
            } yield ()
          ).parMapN { (_, _) => }
          _e4fc91b9_6dcc_4263_ace7_13a97b17b3d8
        }
        _fea81203_f93b_4b2b_9eb6_ee50df893e17 <- b("b")
        _                                     <-
          if (_fea81203_f93b_4b2b_9eb6_ee50df893e17 eq None) IO.cede
          else _e4fc91b9_6dcc_4263_ace7_13a97b17b3d8
      } yield (),
      for {
        _65c37d13_fe4f_4e9c_84dd_8679e1e72ec2 <- IO {
          lazy val _65c37d13_fe4f_4e9c_84dd_8679e1e72ec2: IO[Unit] = (
            IO.unit,
            for {
              _f55dc9e8_8a8b_4257_bdf1_00441de6c770 <- c("c")
              _                                     <-
                if (_f55dc9e8_8a8b_4257_bdf1_00441de6c770 eq None) IO.cede
                else _65c37d13_fe4f_4e9c_84dd_8679e1e72ec2
            } yield ()
          ).parMapN { (_, _) => }
          _65c37d13_fe4f_4e9c_84dd_8679e1e72ec2
        }
        _f55dc9e8_8a8b_4257_bdf1_00441de6c770 <- c("c")
        _                                     <-
          if (_f55dc9e8_8a8b_4257_bdf1_00441de6c770 eq None) IO.cede
          else _65c37d13_fe4f_4e9c_84dd_8679e1e72ec2
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
