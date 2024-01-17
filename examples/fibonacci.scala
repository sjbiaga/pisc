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
    "9b692f65-9cf3-4f6e-8987-b99a05bdbc38" -> _root_.scala.collection.immutable.Set("5174c31d-b070-4c21-8f7a-7f59bc74366a"),
    "36429e6e-8605-49cd-b8f4-90f311b22061" -> _root_.scala.collection.immutable.Set("044dcbfb-26aa-486b-96bb-7ac432e5fb73"),
    "5174c31d-b070-4c21-8f7a-7f59bc74366a" -> _root_.scala.collection.immutable.Set("182de67b-174f-483e-b833-0e98a9c2d52b"),
    "182de67b-174f-483e-b833-0e98a9c2d52b" -> _root_.scala.collection.immutable.Set(),
    "1163a119-92ca-4452-bf59-f7a9ef949496" -> _root_.scala.collection.immutable.Set("36429e6e-8605-49cd-b8f4-90f311b22061")
  )

  implicit val `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) = `π-trick` -> `π-spell`

  import scala.util.control.TailCalls.{done, tailcall, TailRec}

  given Conversion[`()`, Long] = _.name.asInstanceOf[Long]

  def Main()(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for (_ <- Fib(-1)(using `π-uuid`)) yield ()

  def Fib(n: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _18ad1d4a_114e_44bd_87ea_803b20cdede3 = _root_.scala.collection.immutable.Set("9b692f65-9cf3-4f6e-8987-b99a05bdbc38", "1163a119-92ca-4452-bf59-f7a9ef949496")
    _ <- `π-none`(_18ad1d4a_114e_44bd_87ea_803b20cdede3)
    _ <- for {
      x <- ν
      _ <- (
        `𝟎`,
        for {
          _ <- τ(∞)("9b692f65-9cf3-4f6e-8987-b99a05bdbc38")
          _ <- IO {
            print("n = ")
          }
          n <- IO.blocking {
            scala.io.StdIn.readLine.toLong
          }
          _ <- x(null, n)("5174c31d-b070-4c21-8f7a-7f59bc74366a")
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
          )("182de67b-174f-483e-b833-0e98a9c2d52b")
        } yield (),
        for {
          (n, _) <- x(null)("1163a119-92ca-4452-bf59-f7a9ef949496")
          (f, _) <- x(null)("36429e6e-8605-49cd-b8f4-90f311b22061")
          _      <- τ(∞)("044dcbfb-26aa-486b-96bb-7ac432e5fb73")
          _      <- IO {
            if (n >= 0) println(s"fib($n) = $f")
          }
          _      <- if (n < 0 ==== true) for (_ <- Fib(-1)(using `π-uuid`)) yield () else for (_ <- pisc.greeter.π.Main()(using `π-uuid`)) yield ()
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()
