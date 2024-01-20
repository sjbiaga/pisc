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

import _root_.cats.effect.{IO, IOApp}
import _root_.cats.effect.std.{Queue, Semaphore}

import `Π-loop`._
import `Π-stats`.{Rate, ∞, `@`}

object App extends IOApp.Simple:

  private def run(% : %, / : /, * : (*, *)): IO[Unit] = (for
    _ <- loop(using %, *).background
    _ <- poll(using %, /, *._1).background
  yield ()).use { _ =>
    for _ <- π.Main()(using π.`π-uuid`)(using %, /, *._2)
    yield ()
  }

  override def run: IO[Unit] =
    for
      % <- IO.ref(Map[String, Int | +]())
      / <- Queue.unbounded[IO, ((String, String), +)]
      * <- Semaphore[IO](1)
      - <- Semaphore[IO](1)
      _ <- -.acquire
      _ <- run(%, /, (*, -))
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
    "b58be151-536d-42b3-b902-b2a9ab73b29c" -> _root_.scala.collection.immutable.Set("db20fac4-33e5-489c-ac20-f46a3cae3f3c"),
    "db20fac4-33e5-489c-ac20-f46a3cae3f3c" -> _root_.scala.collection.immutable.Set("9ed0c78c-be9a-4692-aab5-5af3a9cb56ee"),
    "ddd2314d-66f4-40a7-9831-8aeaf45bcdfb" -> _root_.scala.collection.immutable.Set("bbf5fc5e-3128-4675-a08e-22cf1e45471b"),
    "bbf5fc5e-3128-4675-a08e-22cf1e45471b" -> _root_.scala.collection.immutable.Set("b586da4c-c31e-4542-a6a1-162c56603e0d"),
    "b586da4c-c31e-4542-a6a1-162c56603e0d" -> _root_.scala.collection.immutable.Set()
  )

  implicit val `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) = `π-trick` -> `π-spell`

  import scala.util.control.TailCalls.{done, tailcall, TailRec}

  given Conversion[`()`, Long] = _.name.asInstanceOf[Long]

  def Main()(using ^ : String)(using % : %, / : /, * : *): IO[Unit] = for (_ <- Fib(-1)(using `π-uuid`)) yield ()

  def Fib(n: `()`)(using ^ : String)(using % : %, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _195e5fc9_72c2_4f05_9660_235dbbca1e8a = _root_.scala.collection.immutable.Set("ddd2314d-66f4-40a7-9831-8aeaf45bcdfb", "b58be151-536d-42b3-b902-b2a9ab73b29c")
    _ <- `π-incr`(_195e5fc9_72c2_4f05_9660_235dbbca1e8a)
    _ <- for {
      x <- ν
      _ <- (
        `𝟎`,
        for {
          _ <- τ(∞)("ddd2314d-66f4-40a7-9831-8aeaf45bcdfb")
          _ <- IO {
            print("n = ")
          }
          n <- IO.blocking {
            scala.io.StdIn.readLine.toLong
          }
          _ <- x(null, n)("bbf5fc5e-3128-4675-a08e-22cf1e45471b")
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
          )("b586da4c-c31e-4542-a6a1-162c56603e0d")
          _ <- `𝟎`
        } yield (),
        for {
          (n, _) <- x(null)("b58be151-536d-42b3-b902-b2a9ab73b29c")
          (f, _) <- x(null)("db20fac4-33e5-489c-ac20-f46a3cae3f3c")
          _      <- τ(∞)("9ed0c78c-be9a-4692-aab5-5af3a9cb56ee")
          _      <- IO {
            if (n >= 0) println(s"fib($n) = $f")
          }
          _      <- if (n < 0 ==== true) for (_ <- Fib(-1)(using `π-uuid`)) yield () else for (_ <- pisc.greeter.π.Main()(using `π-uuid`)) yield ()
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()
