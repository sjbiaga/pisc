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

  given Conversion[`()`, Long] = _.as[Long]

  val max = 13

  val gen = new scala.util.Random

  def random = 3 + gen.nextLong(max)

  def Main(args: String*): IO[Unit] = for {
    num <- ν
    _   <- (
      for {
        _5eba5565_9909_414d_baca_b69177b8c3c8 <- IO {
          def _5eba5565_9909_414d_baca_b69177b8c3c8(n: `()`): IO[Unit] =
            if (!n) IO.cede
            else (
              for {
                out <- ν
                _   <- (
                  IO.unit,
                  Fib(n, out),
                  for {
                    fib <- out()
                    _   <- τ
                    _   <- IO {
                      println(s"fib(${n}) = ${fib}")
                    }
                    _   <- num(random)
                  } yield ()
                ).parMapN { (_, _, _) => }
              } yield (),
              for {
                n <- num()
                _ <- _5eba5565_9909_414d_baca_b69177b8c3c8(n)
              } yield ()
            ).parMapN { (_, _) => }
          _5eba5565_9909_414d_baca_b69177b8c3c8
        }
        n                                     <- num()
        _ <- _5eba5565_9909_414d_baca_b69177b8c3c8(n)
      } yield (),
      for (_ <- num(random)) yield ()
    ).parMapN { (_, _) => }
  } yield ()

  def Fib(n: `()`, out: `()`): IO[Unit] = for {
    _ <- τ
    _ <- IO {
      println(s"n=${n}")
    }
    f <- ν
    _ <- (
      Fibonacci(f, n),
      for {
        res <- f()
        _   <- out(res)
      } yield ()
    ).parMapN { (_, _) => }
  } yield ()

  def Fibonacci(f: `()`, n: `()`): IO[Unit] =
    if (n < 2 ==== true) for (_ <- f(n)) yield ()
    else for {
      g <- ν
      h <- ν
      _ <- (
        Fibonacci(g, n - 1),
        Fibonacci(h, n - 2),
        for {
          p <- g()
          r <- h()
          _ <- f(p + r)
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
