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

  import scala.concurrent.duration._

  val gen = new scala.util.Random

  def ms = (10 + math.abs(gen.nextInt % 10)).milliseconds

  def Main(args: String*): IO[Any] = for {
    c <- IO.ref(0)
    x <- ν(2147483647)
    _ <- (
      for {
        _69999ae9_6f8d_47a8_add4_e0d5140fd033 <- IO {
          lazy val _69999ae9_6f8d_47a8_add4_e0d5140fd033: IO[Unit] = (
            for {
              i <- c.get
              _ <- IO.sleep(ms)
              _ <- x(i)
              _ <- τ
              _ <- IO {
                println(s"out $i")
              }
            } yield (),
            for {
              _ <- IO.unit
              _ <- c.update(_ + 1 min 50)
              i <- c.get
              _ <- if i == 50 then IO.never else IO.cede
              _ <- _69999ae9_6f8d_47a8_add4_e0d5140fd033
            } yield ()
          ).parMapN { (_, _) => }
          _69999ae9_6f8d_47a8_add4_e0d5140fd033
        }
        _ <- _69999ae9_6f8d_47a8_add4_e0d5140fd033
      } yield (),
      for {
        _5f724e49_38e2_410b_9c70_b545f3a59b83 <- IO {
          lazy val _5f724e49_38e2_410b_9c70_b545f3a59b83: IO[Unit] = (
            for {
              _ <- IO.sleep(ms)
              y <- x()
              _ <- τ
              _ <- IO {
                println(s"in $y")
              }
            } yield (),
            for {
              _ <- IO.unit
              i <- c.get
              _ <- if i == 50 then IO.never else IO.cede
              _ <- _5f724e49_38e2_410b_9c70_b545f3a59b83
            } yield ()
          ).parMapN { (_, _) => }
          _5f724e49_38e2_410b_9c70_b545f3a59b83
        }
        _ <- _5f724e49_38e2_410b_9c70_b545f3a59b83
      } yield ()
    ).parMapN { (_, _) => }
  } yield ()
