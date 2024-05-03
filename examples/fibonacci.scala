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
      "19966db4-0350-4e4d-91d1-78c229472524" -> _root_.scala.collection.immutable
        .Set("8f0dd2e1-87fe-4707-bd90-e2745184d8fa"),
      "8f0dd2e1-87fe-4707-bd90-e2745184d8fa" -> _root_.scala.collection.immutable
        .Set("aa5c5817-7c55-4b4c-b75b-bda14a27e92c"),
      "815011a8-0dbb-401f-a5f4-63a1745791c8" -> _root_.scala.collection.immutable
        .Set(),
      "ba6d20c8-3f4a-403f-a368-3817217542ac" -> _root_.scala.collection.immutable
        .Set("66e0ccb6-9a3e-4a53-a27f-94a9803ee982"),
      "66e0ccb6-9a3e-4a53-a27f-94a9803ee982" -> _root_.scala.collection.immutable
        .Set("815011a8-0dbb-401f-a5f4-63a1745791c8")
    )

  implicit val `π-wand`
    : (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) =
    `π-trick` -> `π-spell`

  import scala.util.control.TailCalls.{done, tailcall, TailRec}

  given Conversion[`()`, Long] = _.as[Long]

  def Main(args: String*)(using ^ : String)(using % : %, / : /): IO[Unit] =
    Fib(-1)(using `π-uuid`)

  def Fib(n: `()`)(using ^ : String)(using % : %, / : /): IO[Unit] = for {
    _ <- IO.unit
    _bfa34f36_678f_4c68_b8f7_5041158653c7 =
      _root_.scala.collection.immutable.Set(
        "ba6d20c8-3f4a-403f-a368-3817217542ac",
        "19966db4-0350-4e4d-91d1-78c229472524"
      )
    _ <- `π-enable`(_bfa34f36_678f_4c68_b8f7_5041158653c7)
    x <- ν
    _ <- (
      IO.unit,
      `π-supervised`(for {
        _ <- τ(⊤(1L))("ba6d20c8-3f4a-403f-a368-3817217542ac")
        _ <- IO {
          print("n = ")
        }
        n <- IO.blocking {
          try scala.io.StdIn.readLine.toLong
          catch _ => -1L
        }
        _ <- x(⊤(1L), n)("66e0ccb6-9a3e-4a53-a27f-94a9803ee982")
        _ <- x(
          ⊤(1L), {
            def fibonacci(k: Long): TailRec[Long] =
              if (k < 2) done(k * k)
              else for {
                m <- tailcall {
                  fibonacci(k - 2)
                }
                n <- tailcall {
                  fibonacci(k - 1)
                }
              } yield m + n
            if (n < 0) println("Enter a non-negative number.")
            else fibonacci(n).result
          }
        )("815011a8-0dbb-401f-a5f4-63a1745791c8")
      } yield ()),
      `π-supervised`(for {
        (n, _) <- x(⊤(1L))("19966db4-0350-4e4d-91d1-78c229472524")
        (f, _) <- x(⊤(1L))("8f0dd2e1-87fe-4707-bd90-e2745184d8fa")
        _      <- τ(⊤(1L))("aa5c5817-7c55-4b4c-b75b-bda14a27e92c")
        _      <- IO {
          if (n >= 0) println(s"fib(${n}) = ${f}")
        }
        _      <- if (n < 0 ==== true) Fib(-1)(using `π-uuid`) else IO.unit
      } yield ())
    ).parMapN { (_, _, _) => }
  } yield ()
