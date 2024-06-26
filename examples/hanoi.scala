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
        _37946e94_33dc_46b8_b57f_88c6ae1ebac1 <- IO {
          lazy val _37946e94_33dc_46b8_b57f_88c6ae1ebac1: IO[Unit] = (
            IO.unit,
            for {
              _379c1ea3_6461_4755_b5dc_5eb7f9a7080d <- a("a")
              _                                     <-
                if (_379c1ea3_6461_4755_b5dc_5eb7f9a7080d eq None) IO.cede
                else _37946e94_33dc_46b8_b57f_88c6ae1ebac1
            } yield ()
          ).parMapN { (_, _) => }
          _37946e94_33dc_46b8_b57f_88c6ae1ebac1
        }
        _379c1ea3_6461_4755_b5dc_5eb7f9a7080d <- a("a")
        _                                     <-
          if (_379c1ea3_6461_4755_b5dc_5eb7f9a7080d eq None) IO.cede
          else _37946e94_33dc_46b8_b57f_88c6ae1ebac1
      } yield (),
      for {
        _bf7eb467_79a5_44cb_a20e_ed9f0e81792b <- IO {
          lazy val _bf7eb467_79a5_44cb_a20e_ed9f0e81792b: IO[Unit] = (
            IO.unit,
            for {
              _aeac6506_4659_47a3_9277_f45a854c46a3 <- b("b")
              _                                     <-
                if (_aeac6506_4659_47a3_9277_f45a854c46a3 eq None) IO.cede
                else _bf7eb467_79a5_44cb_a20e_ed9f0e81792b
            } yield ()
          ).parMapN { (_, _) => }
          _bf7eb467_79a5_44cb_a20e_ed9f0e81792b
        }
        _aeac6506_4659_47a3_9277_f45a854c46a3 <- b("b")
        _                                     <-
          if (_aeac6506_4659_47a3_9277_f45a854c46a3 eq None) IO.cede
          else _bf7eb467_79a5_44cb_a20e_ed9f0e81792b
      } yield (),
      for {
        _b5a84271_ed61_4c57_a174_d984854dd12d <- IO {
          lazy val _b5a84271_ed61_4c57_a174_d984854dd12d: IO[Unit] = (
            IO.unit,
            for {
              _81902077_e41e_44a2_8b46_77b1f8f90f33 <- c("c")
              _                                     <-
                if (_81902077_e41e_44a2_8b46_77b1f8f90f33 eq None) IO.cede
                else _b5a84271_ed61_4c57_a174_d984854dd12d
            } yield ()
          ).parMapN { (_, _) => }
          _b5a84271_ed61_4c57_a174_d984854dd12d
        }
        _81902077_e41e_44a2_8b46_77b1f8f90f33 <- c("c")
        _                                     <-
          if (_81902077_e41e_44a2_8b46_77b1f8f90f33 eq None) IO.cede
          else _b5a84271_ed61_4c57_a174_d984854dd12d
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
