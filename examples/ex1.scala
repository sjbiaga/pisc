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
import _root_.cats.effect.std.{Queue, Semaphore, Supervisor}

import `Π-loop`._
import `Π-stats`.{Rate, ∞, `ℝ⁺`, ⊤}

object App extends IOApp:

  private def run(args: List[String])(% : %, / : /, * : *): IO[Unit] = (for
    _ <- loop(π.`π-trick`)(using %, *).background
    _ <- poll(using %, /, *).background
  yield ()).use { _ => π.Main(args*)(using π.`π-uuid`)(using %, /) }

  override def run(args: List[String]): IO[ExitCode] =
    for
      % <- IO.ref(Map[String, Int | +]())
      / <- Queue.unbounded[IO, ((String, String), +)]
      * <- Semaphore[IO](1)
      _ <- run(args)(%, /, *)
    yield ExitCode.Success

object π:

  import _root_.java.util.UUID

  import _root_.cats.effect.syntax.all._
  import _root_.cats.syntax.all._

  import sΠ._

  def `π-uuid` = UUID.randomUUID.toString

  def `π-supervised`(io: => IO[Unit]): IO[Unit] =
    for _ <- Supervisor[IO](await = true).use(_.supervise(io).void)
    yield ()

  val `π-trick`: `Π-Map`[String, `Π-Set`[String]] =
    _root_.scala.collection.immutable.Map()

  val `π-spell`: `Π-Map`[String, `Π-Set`[String]] =
    _root_.scala.collection.immutable.Map(
      "f1892907-4e45-470e-a526-66dfa7684541" -> _root_.scala.collection.immutable
        .Set(
          "f1892907-4e45-470e-a526-66dfa7684541",
          "918efa2e-51da-4345-8dbc-d3c0e6b32788"
        ),
      "160750e4-9362-4a2c-b80a-bfee3d856676" -> _root_.scala.collection.immutable
        .Set("160bbfc3-7bbf-4e5e-befe-454a82382c10"),
      "ff40798e-cb0c-45b8-a8ae-ebeb6220714d" -> _root_.scala.collection.immutable
        .Set(),
      "160bbfc3-7bbf-4e5e-befe-454a82382c10" -> _root_.scala.collection.immutable
        .Set(),
      "918efa2e-51da-4345-8dbc-d3c0e6b32788" -> _root_.scala.collection.immutable
        .Set("160750e4-9362-4a2c-b80a-bfee3d856676")
    )

  implicit val `π-wand`
    : (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) =
    `π-trick` -> `π-spell`

  def Main(args: String*)(using ^ : String)(using % : %, / : /): IO[Unit] = for {
    _ <- IO.unit
    _6ecfb1f7_1a6f_4e76_9679_886106f157df =
      _root_.scala.collection.immutable.Set(
        "f1892907-4e45-470e-a526-66dfa7684541",
        "ff40798e-cb0c-45b8-a8ae-ebeb6220714d"
      )
    _ <- `π-enable`(_6ecfb1f7_1a6f_4e76_9679_886106f157df)
    x <- ν
    _ <- (
      IO.unit,
      `π-supervised`(for {
        _25efb736_765c_47ca_b2b8_ca368e4da477 <- IO {
          lazy val _25efb736_765c_47ca_b2b8_ca368e4da477: String => IO[Unit] = {
            implicit ^ =>
              (
                `π-supervised`(for {
                  _      <- τ(⊤(1L))("918efa2e-51da-4345-8dbc-d3c0e6b32788")
                  _      <- IO {
                    println("out 5")
                  }
                  (z, _) <- x(∞(1L))("160750e4-9362-4a2c-b80a-bfee3d856676")
                  _      <- τ(⊤(1L))("160bbfc3-7bbf-4e5e-befe-454a82382c10")
                  _      <- IO {
                    println(s"in ${z}")
                  }
                } yield ()),
                `π-supervised`(for {
                  _6e8c78c0_441d_4544_9c44_3fbb25430d5c <-
                    x(∞(1L), 5)("f1892907-4e45-470e-a526-66dfa7684541")
                  _                                     <-
                    if (_6e8c78c0_441d_4544_9c44_3fbb25430d5c == null) IO.cede
                    else _25efb736_765c_47ca_b2b8_ca368e4da477(`π-uuid`)
                } yield ())
              ).parMapN { (_, _) => }
          }
          _25efb736_765c_47ca_b2b8_ca368e4da477
        }
        _6e8c78c0_441d_4544_9c44_3fbb25430d5c <-
          x(∞(1L), 5)("f1892907-4e45-470e-a526-66dfa7684541")
        _                                     <-
          if (_6e8c78c0_441d_4544_9c44_3fbb25430d5c == null) IO.cede
          else _25efb736_765c_47ca_b2b8_ca368e4da477(`π-uuid`)
      } yield ()),
      `π-supervised`(
        for ((z, _) <- x(∞(1L))("ff40798e-cb0c-45b8-a8ae-ebeb6220714d"))
          yield ()
      )
    ).parMapN { (_, _, _) => }
  } yield ()
