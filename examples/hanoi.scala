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

import cats.effect.{IO, IOApp}

object App extends IOApp.Simple:

  override def run: IO[Unit] = π.Main()

object π:

  import cats.effect.syntax.all._
  import cats.syntax.all._
  import cats.effect.std.Semaphore

  import Π._

  val `𝟎` = IO.unit

  given Conversion[`()`, Int] = _.name.asInstanceOf[Int]

  def Main(): IO[Unit] = for {
    a <- ν
    b <- ν
    c <- ν
    _ <- (
      `𝟎`,
      for {
        _f8082954_72dc_42e9_9e2d_3eaaeee2889f <- IO {
          lazy val _f8082954_72dc_42e9_9e2d_3eaaeee2889f: IO[Unit] = {
            for (
              _ <- (
                `𝟎`,
                for {
                  _ <- a("a")
                  _ <- _f8082954_72dc_42e9_9e2d_3eaaeee2889f
                } yield ()
              ).parMapN { (_, _) => }
            ) yield ()
          }
          _f8082954_72dc_42e9_9e2d_3eaaeee2889f
        }
        _ <- a("a")
        _ <- _f8082954_72dc_42e9_9e2d_3eaaeee2889f
      } yield (),
      for {
        _d0da7844_5959_48eb_95b8_cc0c7c40e0f9 <- IO {
          lazy val _d0da7844_5959_48eb_95b8_cc0c7c40e0f9: IO[Unit] = {
            for (
              _ <- (
                `𝟎`,
                for {
                  _ <- b("b")
                  _ <- _d0da7844_5959_48eb_95b8_cc0c7c40e0f9
                } yield ()
              ).parMapN { (_, _) => }
            ) yield ()
          }
          _d0da7844_5959_48eb_95b8_cc0c7c40e0f9
        }
        _ <- b("b")
        _ <- _d0da7844_5959_48eb_95b8_cc0c7c40e0f9
      } yield (),
      for {
        _4d461c3d_a670_4ec2_b6f9_f273cf2de925 <- IO {
          lazy val _4d461c3d_a670_4ec2_b6f9_f273cf2de925: IO[Unit] = {
            for (
              _ <- (
                `𝟎`,
                for {
                  _ <- c("c")
                  _ <- _4d461c3d_a670_4ec2_b6f9_f273cf2de925
                } yield ()
              ).parMapN { (_, _) => }
            ) yield ()
          }
          _4d461c3d_a670_4ec2_b6f9_f273cf2de925
        }
        _ <- c("c")
        _ <- _4d461c3d_a670_4ec2_b6f9_f273cf2de925
      } yield (),
      for (_ <- Hanoi(a, b, c, -1)) yield ()
    ).parMapN { (_, _, _, _, _) => }
  } yield ()

  def Hanoi(a: `()`, b: `()`, c: `()`, n: `()`): IO[Unit] = for {
    z <- ν
    _ <- (
      `𝟎`,
      for {
        _ <- τ
        _ <- IO.print("n = ")
        n <- IO.blocking {
          scala.io.StdIn.readLine.toInt
        }
        _ <- Hanoi(a, b, c, n, z)
      } yield (),
      for {
        z <- z()
        _ <- `𝟎`
      } yield ()
    ).parMapN { (_, _, _) => }
  } yield ()

  def Move(from: `()`, to: `()`, z: `()`): IO[Unit] = for {
    x <- from()
    y <- to()
    _ <- τ
    _ <- IO {
      println(s"$x -> $y")
    }
    _ <- z(z)
    _ <- `𝟎`
  } yield ()

  def Hanoi(a: `()`, b: `()`, c: `()`, n: `()`, z: `()`): IO[Unit] = for (
    _ <-
      if (n ==== 1) for {
        x <- ν
        _ <- (
          for (_ <- Move(a, c, x)) yield (),
          for {
            x <- x()
            _ <- z(x)
            _ <- `𝟎`
          } yield ()
        ).parMapN { (_, _) => }
      } yield ()
      else {
        for {
          y <- ν
          _ <- (
            `𝟎`,
            for (_ <- Hanoi(a, c, b, n - 1, y)) yield (),
            for {
              y <- y()
              x <- ν
              _ <- (
                for (_ <- Move(a, c, x)) yield (),
                for {
                  x <- x()
                  w <- ν
                  _ <- (
                    for (_ <- Hanoi(b, a, c, n - 1, w)) yield (),
                    for {
                      w <- w()
                      _ <- z(w)
                      _ <- `𝟎`
                    } yield ()
                  ).parMapN { (_, _) => }
                } yield ()
              ).parMapN { (_, _) => }
            } yield ()
          ).parMapN { (_, _, _) => }
        } yield ()
      }) yield ()
