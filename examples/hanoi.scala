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
        _8815836f_31cf_4e0d_b0d5_d24454e81578 <- IO {
          lazy val _8815836f_31cf_4e0d_b0d5_d24454e81578: IO[Unit] = (
            IO.unit,
            for {
              _30613a35_597c_44ef_9aeb_398db0b5d200 <- a("a")
              _                                     <-
                if (_30613a35_597c_44ef_9aeb_398db0b5d200 eq None) IO.cede
                else _8815836f_31cf_4e0d_b0d5_d24454e81578
            } yield ()
          ).parMapN { (_, _) => }
          _8815836f_31cf_4e0d_b0d5_d24454e81578
        }
        _30613a35_597c_44ef_9aeb_398db0b5d200 <- a("a")
        _                                     <-
          if (_30613a35_597c_44ef_9aeb_398db0b5d200 eq None) IO.cede
          else _8815836f_31cf_4e0d_b0d5_d24454e81578
      } yield (),
      for {
        _c7c6946a_80b5_454b_8ea6_97535d3318ca <- IO {
          lazy val _c7c6946a_80b5_454b_8ea6_97535d3318ca: IO[Unit] = (
            IO.unit,
            for {
              _87078da2_7ff8_494b_a7b8_42fff275f405 <- b("b")
              _                                     <-
                if (_87078da2_7ff8_494b_a7b8_42fff275f405 eq None) IO.cede
                else _c7c6946a_80b5_454b_8ea6_97535d3318ca
            } yield ()
          ).parMapN { (_, _) => }
          _c7c6946a_80b5_454b_8ea6_97535d3318ca
        }
        _87078da2_7ff8_494b_a7b8_42fff275f405 <- b("b")
        _                                     <-
          if (_87078da2_7ff8_494b_a7b8_42fff275f405 eq None) IO.cede
          else _c7c6946a_80b5_454b_8ea6_97535d3318ca
      } yield (),
      for {
        _af77d15e_75c1_4917_b81c_84c663bf023b <- IO {
          lazy val _af77d15e_75c1_4917_b81c_84c663bf023b: IO[Unit] = (
            IO.unit,
            for {
              _4f336906_363b_4856_be88_abab0b09b2da <- c("c")
              _                                     <-
                if (_4f336906_363b_4856_be88_abab0b09b2da eq None) IO.cede
                else _af77d15e_75c1_4917_b81c_84c663bf023b
            } yield ()
          ).parMapN { (_, _) => }
          _af77d15e_75c1_4917_b81c_84c663bf023b
        }
        _4f336906_363b_4856_be88_abab0b09b2da <- c("c")
        _                                     <-
          if (_4f336906_363b_4856_be88_abab0b09b2da eq None) IO.cede
          else _af77d15e_75c1_4917_b81c_84c663bf023b
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
