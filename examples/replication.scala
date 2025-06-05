/*
 * Copyright (c) 2023-2025 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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

  import _root_.scala.collection.immutable.{List => πLs}
  import _root_.cats.syntax.all.*
  import _root_.cats.effect.syntax.all.*
  import _root_.cats.effect.std.Semaphore

  extension (self: πLs[IO[?]])
    private[π] inline def πparSequence: IO[Unit] =
      _root_.cats.data.NonEmptyList.fromListUnsafe(self).parSequence.void

  import Π.*

  import scala.concurrent.duration._

  val gen = new scala.util.Random

  def ms = (10 + math.abs(gen.nextInt % 10)).milliseconds

  def Main(args: String*): IO[Any] = for {
    c <- IO.ref(0)
    x <- ν
    _ <- πLs(
      for {
        _υ2υ <- IO {
          lazy val _υ2υ: IO[Any] = πLs(
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
              _ <- _υ2υ
            } yield ()
          ).πparSequence
          _υ2υ
        }
        _    <- _υ2υ
      } yield (),
      for {
        _υ1υ <- IO {
          lazy val _υ1υ: IO[Any] = πLs(
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
              _ <- _υ1υ
            } yield ()
          ).πparSequence
          _υ1υ
        }
        _    <- _υ1υ
      } yield ()
    ).πparSequence
  } yield ()
