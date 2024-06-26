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
      "3d5cb94a-1929-42dd-b47b-9eb671b91b39" -> _root_.scala.collection.immutable
        .Set("40469bba-dc2e-44da-bdea-bcb2af2ca463"),
      "40469bba-dc2e-44da-bdea-bcb2af2ca463" -> _root_.scala.collection.immutable
        .Set(),
      "a8887893-591d-469e-bdc3-2190bc9d5ab0" -> _root_.scala.collection.immutable
        .Set("c39e5b1e-7cef-45b2-b7ef-5d27ae212ec1"),
      "b090c685-3054-4cc2-bdfb-47b98170f3f1" -> _root_.scala.collection.immutable
        .Set("3d5cb94a-1929-42dd-b47b-9eb671b91b39"),
      "47dbe767-fd91-43ce-9e22-d79a0cb0820c" -> _root_.scala.collection.immutable
        .Set("a8887893-591d-469e-bdc3-2190bc9d5ab0")
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
    _a85370eb_e4c9_4a03_82c8_ff3f68c670ba =
      _root_.scala.collection.immutable.Set(
        "b090c685-3054-4cc2-bdfb-47b98170f3f1",
        "47dbe767-fd91-43ce-9e22-d79a0cb0820c"
      )
    _ <- `π-enable`(_a85370eb_e4c9_4a03_82c8_ff3f68c670ba)
    x <- ν
    _ <- (
      `π-supervised`(for {
        _ <- τ(⊤(1L))("b090c685-3054-4cc2-bdfb-47b98170f3f1")
        _ <- IO {
          print("n = ")
        }
        n <- IO.blocking {
          try scala.io.StdIn.readLine.toLong
          catch _ => -1L
        }
        _ <- x(⊤(1L), n)("3d5cb94a-1929-42dd-b47b-9eb671b91b39")
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
        )("40469bba-dc2e-44da-bdea-bcb2af2ca463")
      } yield ()),
      `π-supervised`(for {
        (n, _) <- x(⊤(1L))("47dbe767-fd91-43ce-9e22-d79a0cb0820c")
        (f, _) <- x(⊤(1L))("a8887893-591d-469e-bdc3-2190bc9d5ab0")
        _      <- τ(⊤(1L))("c39e5b1e-7cef-45b2-b7ef-5d27ae212ec1")
        _      <- IO {
          if (n >= 0) println(s"fib(${n}) = ${f}")
        }
        _      <- if (n < 0 ==== true) Fib(-1)(using `π-uuid`) else IO.unit
      } yield ())
    ).parMapN { (_, _) => }
  } yield ()
