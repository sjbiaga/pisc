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

package pisc.fibonacci

import _root_.cats.effect.{IO, IOApp, Deferred}
import _root_.cats.effect.std.{Queue, Semaphore}

import `Π-loop`._
import `Π-stats`.{Rate, ∞, `@`}

object App extends IOApp.Simple:

  private def run(% : %, \ : \, / : /, * : (*, *)): IO[Unit] = (for
    _ <- loop(using %, \, *).background
    _ <- poll(using %, /, *._1).background
  yield ()).use { _ =>
    for _ <- π.Main()(using "")(using %, \, /, *._2)
    yield ()
  }

  override def run: IO[Unit] =
    for
      % <- IO.ref(Map[String, (Deferred[IO, BigDecimal], Option[Rate])]())
      \ <- IO.ref(Set[String]())
      / <- Queue.unbounded[IO, (String, Rate)]
      * <- Semaphore[IO](1)
      - <- Semaphore[IO](1)
      _ <- -.acquire
      _ <- run(%, \, /, (*, -))
    yield ()

object π:

  import _root_.java.util.UUID

  import _root_.cats.effect.syntax.all._
  import _root_.cats.syntax.all._

  import sΠ._

  private val `𝟎` = IO.unit

  private def `π-uuid` = UUID.randomUUID.toString

  val `π-trick`: `Π-Map`[String, `Π-Set`[String]] = _root_.scala.collection.immutable.Map()

  val `π-spell`: `Π-Map`[String, `Π-Set`[String]] = _root_.scala.collection.immutable.Map(
    "c882244b-b858-43c9-844b-f9a11a2c8ac4" -> _root_.scala.collection.immutable.Set("5e9999dd-a7fe-4074-9bf9-dea29b02f7c2"),
    "5e9999dd-a7fe-4074-9bf9-dea29b02f7c2" -> _root_.scala.collection.immutable.Set(),
    "3b7d5c3c-d144-4cac-a212-68142a42fb10" -> _root_.scala.collection.immutable.Set("2e8647b4-b027-4586-b186-90544660f98c"),
    "2b80c881-f340-4055-b676-d17cd0eed0d0" -> _root_.scala.collection.immutable.Set("7771f511-5ed1-438a-873b-9a3bfa41d125"),
    "2e8647b4-b027-4586-b186-90544660f98c" -> _root_.scala.collection.immutable.Set("2c880d00-6c50-4e59-8a4c-a65951f729d6"),
    "7771f511-5ed1-438a-873b-9a3bfa41d125" -> _root_.scala.collection.immutable.Set("c882244b-b858-43c9-844b-f9a11a2c8ac4")
  )

  implicit val `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) = `π-trick` -> `π-spell`

  import scala.util.control.TailCalls.{done, tailcall, TailRec}

  given Conversion[`()`, Long] = _.name.asInstanceOf[Long]

  def Main()(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for (_ <- Fib(-1)(using `π-uuid`)) yield ()

  def Fib(n: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _f8499c0a_1c14_4696_98b0_2cdedadc00ae = _root_.scala.collection.immutable.Set("2b80c881-f340-4055-b676-d17cd0eed0d0", "3b7d5c3c-d144-4cac-a212-68142a42fb10")
    _ <- `π-none`(_f8499c0a_1c14_4696_98b0_2cdedadc00ae)
    _ <- for {
      x <- ν
      _ <- (
        `𝟎`,
        for {
          _ <- τ(∞)("2b80c881-f340-4055-b676-d17cd0eed0d0")
          _ <- IO {
            print("n = ")
          }
          _ <- τ(∞)("7771f511-5ed1-438a-873b-9a3bfa41d125")
          n <- IO {
            scala.io.StdIn.readLine.toLong
          }
          _ <- x(null, n)("c882244b-b858-43c9-844b-f9a11a2c8ac4")
          _ <- x(
            null, {
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
              if (n < 0) println("Enter a non-negative number.") else fibonacci(n).result
            }
          )("5e9999dd-a7fe-4074-9bf9-dea29b02f7c2")
        } yield (),
        for {
          (n, _) <- x(null)("3b7d5c3c-d144-4cac-a212-68142a42fb10")
          (f, _) <- x(null)("2e8647b4-b027-4586-b186-90544660f98c")
          _      <- τ(∞)("2c880d00-6c50-4e59-8a4c-a65951f729d6")
          _      <- IO {
            if (n >= 0) println(s"fib($n) = $f")
          }
          _      <- if (n < 0 ==== true) for (_ <- Fib(-1)(using `π-uuid`)) yield () else for (_ <- pisc.greeter.π.Main()(using `π-uuid`)) yield ()
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()
