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

package examples

import _root_.cats.effect.{IO, IOApp}
import _root_.cats.effect.std.{CyclicBarrier, Queue, Semaphore}

import `Π-loop`._
import `Π-stats`.{Rate, ∞, `@`}

object App extends IOApp.Simple:

  private def run(% : %, / : /, * : *, - : -): IO[Unit] = (for
    _ <- loop(π.`π-trick`)(using %, *, -).background
    _ <- poll(using %, /, *).background
  yield ()).use { _ =>
    for _ <- π.Main()(using π.`π-uuid`)(using %, /, -)
    yield ()
  }

  override def run: IO[Unit] =
    for
      % <- IO.ref(Map[String, Int | +]())
      / <- Queue.unbounded[IO, ((String, String), +)]
      * <- Semaphore[IO](1)
      - <- CyclicBarrier[IO](3)
      _ <- run(%, /, *, -)
    yield ()

object π:

  import _root_.java.util.UUID

  import _root_.cats.effect.syntax.all._
  import _root_.cats.syntax.all._

  import sΠ._

  private val `𝟎` = IO.unit

  def `π-uuid` = UUID.randomUUID.toString

  val `π-trick`: `Π-Map`[String, `Π-Set`[String]] = _root_.scala.collection.immutable.Map()

  val `π-spell`: `Π-Map`[String, `Π-Set`[String]] = _root_.scala.collection.immutable.Map(
    "5584555c-82e6-4f71-b99c-8b17cf0c4125" -> _root_.scala.collection.immutable.Set("759ea715-920d-4e2a-a805-4cb5a415e428"),
    "8888c4e0-4d9e-42fc-a593-d35e76e91f38" -> _root_.scala.collection.immutable.Set("011538a2-9fd8-4aaa-b7d4-923335bf6732"),
    "24be3d59-54d0-4eac-822c-2e33180bd23e" -> _root_.scala.collection.immutable.Set(),
    "011538a2-9fd8-4aaa-b7d4-923335bf6732" -> _root_.scala.collection.immutable.Set("df94ee8b-ce40-4fed-b9ff-d9e31e46113a"),
    "759ea715-920d-4e2a-a805-4cb5a415e428" -> _root_.scala.collection.immutable.Set("24be3d59-54d0-4eac-822c-2e33180bd23e")
  )

  implicit val `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) = `π-trick` -> `π-spell`

  import scala.util.control.TailCalls.{done, tailcall, TailRec}

  given Conversion[`()`, Long] = _.name.asInstanceOf[Long]

  def Main()(using ^ : String)(using % : %, / : /, - : -): IO[Unit] = for (_ <- Fib(-1)(using `π-uuid`)) yield ()

  def Fib(n: `()`)(using ^ : String)(using % : %, / : /, - : -): IO[Unit] = for {
    _ <- IO.unit
    _ff039dcf_28c2_42f2_81d8_b1c038861283 = _root_.scala.collection.immutable.Set("5584555c-82e6-4f71-b99c-8b17cf0c4125", "8888c4e0-4d9e-42fc-a593-d35e76e91f38")
    _ <- `π-incr`(_ff039dcf_28c2_42f2_81d8_b1c038861283)
    _ <- for {
      x <- ν
      _ <- (
        `𝟎`,
        for {
          _ <- τ(∞)("5584555c-82e6-4f71-b99c-8b17cf0c4125")
          _ <- IO {
            print("n = ")
          }
          n <- IO.blocking {
            scala.io.StdIn.readLine.toLong
          }
          _ <- x(null, n)("759ea715-920d-4e2a-a805-4cb5a415e428")
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
          )("24be3d59-54d0-4eac-822c-2e33180bd23e")
          _ <- `𝟎`
        } yield (),
        for {
          (n, _) <- x(null)("8888c4e0-4d9e-42fc-a593-d35e76e91f38")
          (f, _) <- x(null)("011538a2-9fd8-4aaa-b7d4-923335bf6732")
          _      <- τ(∞)("df94ee8b-ce40-4fed-b9ff-d9e31e46113a")
          _      <- IO {
            if (n >= 0) println(s"fib($n) = $f")
          }
          _      <- if (n < 0 ==== true) for (_ <- Fib(-1)(using `π-uuid`)) yield () else `𝟎`
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()
