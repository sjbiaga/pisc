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

  implicit val `π-wand`: `Π-Map`[String, `Π-Set`[String]] = _root_.scala.collection.immutable.Map(
    "0f136d58-8903-48e2-bd66-278567112dc7" -> _root_.scala.collection.immutable.Set("a0179c37-7983-47bc-9863-e10720a26529"),
    "83e6a533-65a2-41d0-b473-941c15677912" -> _root_.scala.collection.immutable.Set("d2fdc9d0-d23c-4483-9868-a34a0008772c"),
    "422b9822-98b9-46c3-8d80-7adb250f0e4f" -> _root_.scala.collection.immutable.Set("83e6a533-65a2-41d0-b473-941c15677912"),
    "a0179c37-7983-47bc-9863-e10720a26529" -> _root_.scala.collection.immutable.Set(),
    "5ab44274-84f6-4796-9440-4b756818b8ce" -> _root_.scala.collection.immutable.Set("0f136d58-8903-48e2-bd66-278567112dc7")
  )

  import scala.util.control.TailCalls.{done, tailcall, TailRec}

  given Conversion[`()`, Long] = _.name.asInstanceOf[Long]

  def Main()(using ^ : String)(using % : %, / : /, * : *): IO[Unit] = for (_ <- Fib(-1)(using `π-uuid`)) yield ()

  def Fib(n: `()`)(using ^ : String)(using % : %, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _df2f17cf_0684_4bc3_8522_c0610292c049 = _root_.scala.collection.immutable.Set("5ab44274-84f6-4796-9440-4b756818b8ce", "422b9822-98b9-46c3-8d80-7adb250f0e4f")
    _ <- `π-incr`(_df2f17cf_0684_4bc3_8522_c0610292c049)
    _ <- for {
      x <- ν
      _ <- (
        `𝟎`,
        for {
          _ <- τ(∞)("5ab44274-84f6-4796-9440-4b756818b8ce")
          _ <- IO {
            print("n = ")
          }
          n <- IO.blocking {
            scala.io.StdIn.readLine.toLong
          }
          _ <- x(null, n)("0f136d58-8903-48e2-bd66-278567112dc7")
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
          )("a0179c37-7983-47bc-9863-e10720a26529")
          _ <- `𝟎`
        } yield (),
        for {
          (n, _) <- x(null)("422b9822-98b9-46c3-8d80-7adb250f0e4f")
          (f, _) <- x(null)("83e6a533-65a2-41d0-b473-941c15677912")
          _      <- τ(∞)("d2fdc9d0-d23c-4483-9868-a34a0008772c")
          _      <- IO {
            if (n >= 0) println(s"fib($n) = $f")
          }
          _      <- if (n < 0 ==== true) for (_ <- Fib(-1)(using `π-uuid`)) yield () else for (_ <- pisc.greeter.π.Main()(using `π-uuid`)) yield ()
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()
