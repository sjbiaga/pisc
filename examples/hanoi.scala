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
      IO.unit,
      for {
        _4e5369cb_5cd4_48dd_940d_201154ead6ee <- IO {
          lazy val _4e5369cb_5cd4_48dd_940d_201154ead6ee: IO[Unit] = (
            IO.unit,
            for {
              _2d811aba_db22_4f6f_9270_224a5f7f0b5a <- a("a")
              _                                     <-
                if (_2d811aba_db22_4f6f_9270_224a5f7f0b5a eq None) IO.cede
                else _4e5369cb_5cd4_48dd_940d_201154ead6ee
            } yield ()
          ).parMapN { (_, _) => }
          _4e5369cb_5cd4_48dd_940d_201154ead6ee
        }
        _2d811aba_db22_4f6f_9270_224a5f7f0b5a <- a("a")
        _                                     <-
          if (_2d811aba_db22_4f6f_9270_224a5f7f0b5a eq None) IO.cede
          else _4e5369cb_5cd4_48dd_940d_201154ead6ee
      } yield (),
      for {
        _dbbda351_24a9_4340_bb7e_326a48a3bd02 <- IO {
          lazy val _dbbda351_24a9_4340_bb7e_326a48a3bd02: IO[Unit] = (
            IO.unit,
            for {
              _4df28a9a_39a2_454c_85da_4533e65c175d <- b("b")
              _                                     <-
                if (_4df28a9a_39a2_454c_85da_4533e65c175d eq None) IO.cede
                else _dbbda351_24a9_4340_bb7e_326a48a3bd02
            } yield ()
          ).parMapN { (_, _) => }
          _dbbda351_24a9_4340_bb7e_326a48a3bd02
        }
        _4df28a9a_39a2_454c_85da_4533e65c175d <- b("b")
        _                                     <-
          if (_4df28a9a_39a2_454c_85da_4533e65c175d eq None) IO.cede
          else _dbbda351_24a9_4340_bb7e_326a48a3bd02
      } yield (),
      for {
        _d8f0d62a_cfd3_482b_a2b9_7b3ca5649a46 <- IO {
          lazy val _d8f0d62a_cfd3_482b_a2b9_7b3ca5649a46: IO[Unit] = (
            IO.unit,
            for {
              _249853ee_15c6_4b05_ad55_6648bbc97c37 <- c("c")
              _                                     <-
                if (_249853ee_15c6_4b05_ad55_6648bbc97c37 eq None) IO.cede
                else _d8f0d62a_cfd3_482b_a2b9_7b3ca5649a46
            } yield ()
          ).parMapN { (_, _) => }
          _d8f0d62a_cfd3_482b_a2b9_7b3ca5649a46
        }
        _249853ee_15c6_4b05_ad55_6648bbc97c37 <- c("c")
        _                                     <-
          if (_249853ee_15c6_4b05_ad55_6648bbc97c37 eq None) IO.cede
          else _d8f0d62a_cfd3_482b_a2b9_7b3ca5649a46
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
    ).parMapN { (_, _, _) => }
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
        IO.unit,
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
      ).parMapN { (_, _, _) => }
    } yield ()
