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
import _root_.cats.effect.std.{CyclicBarrier, Queue, Semaphore}

import `Π-loop`._
import `Π-stats`.{Rate, ∞, `@`}

object App extends IOApp.Simple:

  private def run(% : %, \ : \, / : /, * : (*, *), + : +, - : -): IO[Unit] = (for
    _ <- loop(using %, \, *, +, -).background
    _ <- poll(using %, /, *._1).background
  yield ()).use { _ =>
    for _ <- π.Main()(using "")(using %, \, /, *._2, +, -)
    yield ()
  }

  override def run: IO[Unit] =
    for
      %    <- IO.ref(Map[String, Option[Rate]]())
      \    <- IO.ref(Set[String]())
      /    <- Queue.unbounded[IO, (String, Rate)]
      *    <- Semaphore[IO](1)
      ^    <- Semaphore[IO](1)
      _    <- ^.acquire
      +    <- CyclicBarrier[IO](2)
      turn <- Deferred[IO, (String, BigDecimal)]
      -    <- IO.ref(turn)
      _    <- run(%, \, /, (*, ^), +, -)
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
    "5ab5453d-bbf8-42c3-8c2e-97318e3d00f1" -> _root_.scala.collection.immutable.Set("c45e57ed-e465-4e88-bcf9-c19fc50b30b7"),
    "fd7b6a88-8892-4f4e-b66a-665e35da79be" -> _root_.scala.collection.immutable.Set("428ee9c3-1ce2-497a-baed-50894f6a5b33"),
    "3993ecbb-8a64-4905-886c-368fefdb641c" -> _root_.scala.collection.immutable.Set("ce89bfbc-7268-4428-9d27-efa76ed0014b"),
    "c75964d4-b811-497b-9a6f-eae8fbb90913" -> _root_.scala.collection.immutable.Set(),
    "c45e57ed-e465-4e88-bcf9-c19fc50b30b7" -> _root_.scala.collection.immutable.Set("221dfc7c-8402-4f08-8630-14b32b07d5e1"),
    "428ee9c3-1ce2-497a-baed-50894f6a5b33" -> _root_.scala.collection.immutable.Set("3993ecbb-8a64-4905-886c-368fefdb641c"),
    "221dfc7c-8402-4f08-8630-14b32b07d5e1" -> _root_.scala.collection.immutable.Set("be068163-ccb0-4537-9ec3-37e6e156e509"),
    "ce89bfbc-7268-4428-9d27-efa76ed0014b" -> _root_.scala.collection.immutable.Set("c75964d4-b811-497b-9a6f-eae8fbb90913")
  )

  implicit val `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) = `π-trick` -> `π-spell`

  import scala.math.{signum => sgn}

  import scala.util.control.TailCalls.{done, tailcall, TailRec}

  given Conversion[`()`, Long] = _.name.asInstanceOf[Long]

  def Main()(using ^ : String)(using % : %, \ : \, / : /, * : *, + : +, - : -): IO[Unit] = for (_ <- Fib(-1)(using `π-uuid`)) yield ()

  def Fib(n: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *, + : +, - : -): IO[Unit] = for {
    _ <- IO.unit
    _278ed2e9_33de_45c6_bf20_e5fd7757c92a = _root_.scala.collection.immutable.Set("fd7b6a88-8892-4f4e-b66a-665e35da79be", "5ab5453d-bbf8-42c3-8c2e-97318e3d00f1")
    _ <- %.update(_278ed2e9_33de_45c6_bf20_e5fd7757c92a.foldLeft(_)(_ + _))
    _ <- for {
      x <- ν
      _ <- (
        for {
          _ <- τ(∞)("fd7b6a88-8892-4f4e-b66a-665e35da79be")
          _ <- IO {
            print("n = ")
          }
          _ <- τ(∞)("428ee9c3-1ce2-497a-baed-50894f6a5b33")
          n <- IO {
            scala.io.StdIn.readLine.toLong
          }
          _ <- x(null, n)("3993ecbb-8a64-4905-886c-368fefdb641c")
          _ <- x(null, sgn(n).toLong)("ce89bfbc-7268-4428-9d27-efa76ed0014b")
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
          )("c75964d4-b811-497b-9a6f-eae8fbb90913")
        } yield (),
        for {
          (n, _) <- x(null)("5ab5453d-bbf8-42c3-8c2e-97318e3d00f1")
          (s, _) <- x(null)("c45e57ed-e465-4e88-bcf9-c19fc50b30b7")
          (f, _) <- x(null)("221dfc7c-8402-4f08-8630-14b32b07d5e1")
          _      <- τ(∞)("be068163-ccb0-4537-9ec3-37e6e156e509")
          _      <- IO {
            if (s >= 0) println(s"fib($n) = $f")
          }
          _      <- if (s === -1) for (_ <- Fib(-1)(using `π-uuid`)) yield () else for (_ <- pisc.greeter.π.Main()(using `π-uuid`)) yield ()
        } yield ()
      ).parMapN { (_, _) => }
    } yield ()
  } yield ()
