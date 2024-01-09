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

  import scala.concurrent.duration._

  val gen = new scala.util.Random

  def ms = (10 + math.abs(gen.nextInt % 10)).milliseconds

  def Main(): IO[Unit] = for {
    c <- IO.ref(0)
    x <- ν
    _ <- (
      for {
        pi <- IO {
          lazy val `3521e7af-5d14-4f84-a8e6-3d02391fe404`: IO[Unit] = {
            for (
              _ <- (
                for {
                  i <- c.get
                  _ <- IO.sleep(ms)
                  _ <- x(i)
                  _ <- IO.println(s"out $i")
                  _ <- `𝟎`
                } yield (),
                for {
                  _ <- IO.unit
                  _ <- c.update(_ + 1 min 50)
                  i <- c.get
                  _ <- if i == 50 then IO.never else IO.cede
                  _ <- `3521e7af-5d14-4f84-a8e6-3d02391fe404`
                } yield ()
              ).parMapN { (_, _) => }
            ) yield ()
          }
          `3521e7af-5d14-4f84-a8e6-3d02391fe404`
        }
        _ <- pi
        _ <- `𝟎`
      } yield (),
      for {
        pi <- IO {
          lazy val `3356ae0e-ce31-44cf-ba2b-14779c86318f`: IO[Unit] = {
            for (
              _ <- (
                for {
                  _ <- IO.sleep(ms)
                  y <- x()
                  _ <- IO.println(s"in $y")
                  _ <- `𝟎`
                } yield (),
                for {
                  _ <- IO.unit
                  i <- c.get
                  _ <- if i == 50 then IO.never else IO.cede
                  _ <- `3356ae0e-ce31-44cf-ba2b-14779c86318f`
                } yield ()
              ).parMapN { (_, _) => }
            ) yield ()
          }
          `3356ae0e-ce31-44cf-ba2b-14779c86318f`
        }
        _ <- pi
        _ <- `𝟎`
      } yield ()
    ).parMapN { (_, _) => }
  } yield ()
