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

  import scala.util.control.TailCalls.{done, tailcall, TailRec}

  given Conversion[`()`, Long] = _.name.asInstanceOf[Long]

  def Main(): IO[Unit] = for (_ <- Fib(-1)) yield ()

  def Fib(n: `()`): IO[Unit] = for {
    x <- ν
    _ <- (
      for {
        _ <- τ
        _ <- IO {
          print("n = ")
        }
        _ <- τ
        n <- IO {
          scala.io.StdIn.readLine.toLong
        }
        _ <- x(n)
        _ <- x {
          def fibonacci(k: Long): TailRec[Long] = if (k < 2) done(k * k)
          else {
            for {
              m <- tailcall {
                fibonacci(k - 2)
              }
              n <- tailcall {
                fibonacci(k - 1)
              }
            } yield m + n
          }
          if (n < 0) println("Enter a non-negative number")
          else fibonacci(n).result
        }
        _ <- `𝟎`
      } yield (),
      for {
        n <- x()
        f <- x()
        _ <- τ
        _ <- IO {
          println(s"fib($n) = $f")
        }
        _ <- Fib(-1)
      } yield ()
    ).parMapN { (_, _) => }
  } yield ()
