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

  given Conversion[`()`, List[Int]] = _.name.asInstanceOf[List[Int]]

  given Conversion[`()`, Int] = _.name.asInstanceOf[Int]

  def Main(): IO[Unit] = for {
    sumlist <- ν
    print   <- ν
    rch     <- ν
    _ <- (
      `𝟎`,
      for (_ <- Client(sumlist, print, rch, Nil)) yield (),
      for (_ <- Server(sumlist)) yield (),
      for {
        Seq(sum) <- print()
        _        <- τ
        _ <- IO {
          println(s"sum=$sum")
        }
        _ <- `𝟎`
      } yield ()
    ).parMapN { (_, _, _, _) => }
  } yield ()

  def Client(sumlist: `()`, print: `()`, rch: `()`, l: `()`): IO[Unit] = for {
    _ <- τ
    l <- IO {
      List(1, 2, 3, 4, 5, 6)
    }
    _        <- sumlist(l, rch)
    Seq(sum) <- rch()
    _        <- print(sum)
    _        <- `𝟎`
  } yield ()

  def Server(sumlist: `()`): IO[Unit] = for {
    _fdc1cfce_1d76_4340_b924_4dab3f97d462 <- IO {
      def _fdc1cfce_1d76_4340_b924_4dab3f97d462(list: `()`, rch: `()`)
        : IO[Unit] = for (
        _ <- (
          for (
            _ <-
              if (list.isEmpty ==== true) for {
                _ <- rch(0)
                _ <- `𝟎`
              } yield ()
              else {
                for {
                  rch_recursive_call <- ν
                  _ <- for {
                    _            <- sumlist(list.tail, rch_recursive_call)
                    Seq(tailsum) <- rch_recursive_call()
                    _            <- rch(list.head + tailsum)
                    _            <- `𝟎`
                  } yield ()
                } yield ()
              }) yield (),
          for {
            Seq(list, rch) <- sumlist()
            _              <- _fdc1cfce_1d76_4340_b924_4dab3f97d462(list, rch)
          } yield ()
        ).parMapN { (_, _) => }
      ) yield ()
      _fdc1cfce_1d76_4340_b924_4dab3f97d462
    }
    Seq(list, rch) <- sumlist()
    _              <- _fdc1cfce_1d76_4340_b924_4dab3f97d462(list, rch)
  } yield ()
