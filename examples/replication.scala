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

  def Main(args: String*): IO[Unit] = for {
    c <- IO.ref(0)
    x <- ν
    _ <- (
      for {
        _bf52d60b_3541_411f_92eb_99a556d9bf14 <- IO {
          lazy val _bf52d60b_3541_411f_92eb_99a556d9bf14: IO[Unit] = (
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
              _ <- _bf52d60b_3541_411f_92eb_99a556d9bf14
            } yield ()
          ).parMapN { (_, _) => }
          _bf52d60b_3541_411f_92eb_99a556d9bf14
        }
        _ <- _bf52d60b_3541_411f_92eb_99a556d9bf14
      } yield (),
      for {
        _691c8465_ab6e_44c2_b1f8_2e6135f78b08 <- IO {
          lazy val _691c8465_ab6e_44c2_b1f8_2e6135f78b08: IO[Unit] = (
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
              _ <- _691c8465_ab6e_44c2_b1f8_2e6135f78b08
            } yield ()
          ).parMapN { (_, _) => }
          _691c8465_ab6e_44c2_b1f8_2e6135f78b08
        }
        _ <- _691c8465_ab6e_44c2_b1f8_2e6135f78b08
      } yield ()
    ).parMapN { (_, _) => }
  } yield ()
