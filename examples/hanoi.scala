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
        _7c293acb_e0fe_4d25_8f9a_8319d0058a42 <- IO {
          lazy val _7c293acb_e0fe_4d25_8f9a_8319d0058a42: IO[Unit] = (
            IO.unit,
            for {
              _45d71548_5b64_4231_b8d7_6a2a7b85cbc5 <- a("a")
              _                                     <-
                if (_45d71548_5b64_4231_b8d7_6a2a7b85cbc5 eq None) IO.cede
                else _7c293acb_e0fe_4d25_8f9a_8319d0058a42
            } yield ()
          ).parMapN { (_, _) => }
          _7c293acb_e0fe_4d25_8f9a_8319d0058a42
        }
        _45d71548_5b64_4231_b8d7_6a2a7b85cbc5 <- a("a")
        _                                     <-
          if (_45d71548_5b64_4231_b8d7_6a2a7b85cbc5 eq None) IO.cede
          else _7c293acb_e0fe_4d25_8f9a_8319d0058a42
      } yield (),
      for {
        _fe5add9e_b188_44f4_ae7e_a57846d176e1 <- IO {
          lazy val _fe5add9e_b188_44f4_ae7e_a57846d176e1: IO[Unit] = (
            IO.unit,
            for {
              _1eca7e2e_9c9f_428d_9bae_a2fd5b12ddaa <- b("b")
              _                                     <-
                if (_1eca7e2e_9c9f_428d_9bae_a2fd5b12ddaa eq None) IO.cede
                else _fe5add9e_b188_44f4_ae7e_a57846d176e1
            } yield ()
          ).parMapN { (_, _) => }
          _fe5add9e_b188_44f4_ae7e_a57846d176e1
        }
        _1eca7e2e_9c9f_428d_9bae_a2fd5b12ddaa <- b("b")
        _                                     <-
          if (_1eca7e2e_9c9f_428d_9bae_a2fd5b12ddaa eq None) IO.cede
          else _fe5add9e_b188_44f4_ae7e_a57846d176e1
      } yield (),
      for {
        _e8a63ec7_ea3e_4093_932d_79713db8bf20 <- IO {
          lazy val _e8a63ec7_ea3e_4093_932d_79713db8bf20: IO[Unit] = (
            IO.unit,
            for {
              _de13d0d8_6961_4bad_bf09_9147c1257290 <- c("c")
              _                                     <-
                if (_de13d0d8_6961_4bad_bf09_9147c1257290 eq None) IO.cede
                else _e8a63ec7_ea3e_4093_932d_79713db8bf20
            } yield ()
          ).parMapN { (_, _) => }
          _e8a63ec7_ea3e_4093_932d_79713db8bf20
        }
        _de13d0d8_6961_4bad_bf09_9147c1257290 <- c("c")
        _                                     <-
          if (_de13d0d8_6961_4bad_bf09_9147c1257290 eq None) IO.cede
          else _e8a63ec7_ea3e_4093_932d_79713db8bf20
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
