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

  given Conversion[`()`, Long] = _.name.asInstanceOf[Long]

  val max = 15

  val gen = new scala.util.Random

  def random = math.abs(gen.nextLong) % max

  def Main(): IO[Unit] = for {
    num <- ν
    _ <- (
      `𝟎`,
      for {
        _edbd1482_f309_42fa_a2b5_8099d3235013 <- IO {
          lazy val _edbd1482_f309_42fa_a2b5_8099d3235013: IO[Unit] = {
            for (
              _ <- (
                for {
                  n   <- num()
                  out <- ν
                  _ <- (
                    `𝟎`,
                    for (_ <- Fib(n, out)) yield (),
                    for {
                      fib <- out()
                      _   <- τ
                      _ <- IO {
                        println(s"fib($n) = $fib")
                      }
                      _ <- num(random)
                    } yield ()
                  ).parMapN { (_, _, _) => }
                } yield (),
                for {
                  _ <- IO.unit
                  _ <- _edbd1482_f309_42fa_a2b5_8099d3235013
                } yield ()
              ).parMapN { (_, _) => }
            ) yield ()
          }
          _edbd1482_f309_42fa_a2b5_8099d3235013
        }
        _ <- _edbd1482_f309_42fa_a2b5_8099d3235013
      } yield (),
      for (_ <- num(random)) yield ()
    ).parMapN { (_, _, _) => }
  } yield ()

  def Fib(n: `()`, out: `()`): IO[Unit] = for {
    _ <- τ
    _ <- IO {
      println(s"n=$n")
    }
    f <- ν
    _ <- (
      `𝟎`,
      for (_ <- Fibonacci(f, n)) yield (),
      for {
        res <- f()
        _   <- out(res)
      } yield ()
    ).parMapN { (_, _, _) => }
  } yield ()

  def Fibonacci(f: `()`, n: `()`): IO[Unit] = for (
    _ <-
      if (n < 2 === true) for (_ <- f(1L)) yield ()
      else {
        for {
          g <- ν
          h <- ν
          _ <- (
            `𝟎`,
            for (_ <- Fibonacci(g, n - 1)) yield (),
            for (_ <- Fibonacci(h, n - 2)) yield (),
            for {
              p <- g()
              r <- h()
              _ <- f(p + r)
            } yield ()
          ).parMapN { (_, _, _, _) => }
        } yield ()
      }) yield ()
