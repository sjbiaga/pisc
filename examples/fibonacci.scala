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
      "d87e0879-d677-44b8-bb94-c3547432434b" -> _root_.scala.collection.immutable
        .Set(),
      "2950fe1b-863a-42cf-bde4-9d56d975ba4d" -> _root_.scala.collection.immutable
        .Set("96df382d-fe0c-4b10-9d7f-d737ad29b3fd"),
      "a408137d-af90-43e8-aea7-bb3ebb3cf9e3" -> _root_.scala.collection.immutable
        .Set("d87e0879-d677-44b8-bb94-c3547432434b"),
      "c0222c36-a8d2-4301-9b82-2b3aa19b9b38" -> _root_.scala.collection.immutable
        .Set("2950fe1b-863a-42cf-bde4-9d56d975ba4d"),
      "3221f0ea-f091-4d6b-86ac-b30d87e2ae33" -> _root_.scala.collection.immutable
        .Set("a408137d-af90-43e8-aea7-bb3ebb3cf9e3")
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
    _1ac8fa2d_8f8e_48ff_9cdf_0db57c2f46be =
      _root_.scala.collection.immutable.Set(
        "3221f0ea-f091-4d6b-86ac-b30d87e2ae33",
        "c0222c36-a8d2-4301-9b82-2b3aa19b9b38"
      )
    _ <- `π-enable`(_1ac8fa2d_8f8e_48ff_9cdf_0db57c2f46be)
    x <- ν
    _ <- (
      `π-supervised`(for {
        _ <- τ(⊤(1L))("3221f0ea-f091-4d6b-86ac-b30d87e2ae33")
        _ <- IO {
          print("n = ")
        }
        n <- IO.blocking {
          try scala.io.StdIn.readLine.toLong
          catch _ => -1L
        }
        _ <- x(⊤(1L), n)("a408137d-af90-43e8-aea7-bb3ebb3cf9e3")
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
        )("d87e0879-d677-44b8-bb94-c3547432434b")
      } yield ()),
      `π-supervised`(for {
        (n, _) <- x(⊤(1L))("c0222c36-a8d2-4301-9b82-2b3aa19b9b38")
        (f, _) <- x(⊤(1L))("2950fe1b-863a-42cf-bde4-9d56d975ba4d")
        _      <- τ(⊤(1L))("96df382d-fe0c-4b10-9d7f-d737ad29b3fd")
        _      <- IO {
          if (n >= 0) println(s"fib(${n}) = ${f}")
        }
        _      <- if (n < 0 ==== true) Fib(-1)(using `π-uuid`) else IO.unit
      } yield ())
    ).parMapN { (_, _) => }
  } yield ()
