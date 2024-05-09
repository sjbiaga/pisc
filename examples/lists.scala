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

  given Conversion[`()`, List[Int]] = _.as[List[Int]]

  given Conversion[`()`, Int] = _.as[Int]

  def Main(args: String*): IO[Unit] = for {
    sumlist <- ν
    print   <- ν
    rch     <- ν
    _       <- (
      Client(sumlist, print, rch, args.map(_.toInt)),
      Server(sumlist),
      for {
        Seq(sum) <- print()
        _        <- τ
        _        <- IO {
          println(s"sum=${sum}")
        }
        _        <- sumlist(`()`(null), `()`(null))
      } yield ()
    ).parMapN { (_, _, _) => }
  } yield ()

  def Client(sumlist: `()`, print: `()`, rch: `()`, l: `()`): IO[Unit] = for {
    _        <- sumlist(l, rch)
    Seq(sum) <- rch()
    _        <- print(sum)
  } yield ()

  def Server(sumlist: `()`): IO[Unit] = for {
    _db95f6f1_6d91_40dd_839f_cec0d06a1d33 <- IO {
      def _db95f6f1_6d91_40dd_839f_cec0d06a1d33(list: `()`, rch: `()`)
        : IO[Unit] =
        if (!list) IO.cede
        else (
          if (list.isEmpty ==== true) for (_ <- rch(0)) yield ()
          else for {
            rch_recursive_call <- ν
            _                  <- sumlist(list.tail, rch_recursive_call)
            Seq(tailsum)       <- rch_recursive_call()
            _                  <- rch(list.head + tailsum)
          } yield (),
          for {
            Seq(list, rch) <- sumlist()
            _              <- _db95f6f1_6d91_40dd_839f_cec0d06a1d33(list, rch)
          } yield ()
        ).parMapN { (_, _) => }
      _db95f6f1_6d91_40dd_839f_cec0d06a1d33
    }
    Seq(list, rch)                        <- sumlist()
    _ <- _db95f6f1_6d91_40dd_839f_cec0d06a1d33(list, rch)
  } yield ()
