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
    "883d1404-2753-4ebb-ac3c-6fe704817d75" -> _root_.scala.collection.immutable.Set("e45244ee-84b7-41e0-a9fe-cfc75d1b0289"),
    "112ede79-2776-4032-99b8-38a69a3eefd2" -> _root_.scala.collection.immutable.Set("eb600e64-f4a3-47ad-ad14-75ef01086fdb"),
    "d752321f-98c3-4523-a8f9-c71561fdc2b4" -> _root_.scala.collection.immutable.Set("d752321f-98c3-4523-a8f9-c71561fdc2b4", "63cb1482-e529-4cde-856c-b1393136fbc6"),
    "bfbc74cb-a053-4ac8-bbbe-67efb59bfc2c" -> _root_.scala.collection.immutable.Set("0f4a939f-8be4-4d69-bf1e-1879de5bac8d"),
    "0f4a939f-8be4-4d69-bf1e-1879de5bac8d" -> _root_.scala.collection.immutable.Set(),
    "40e19fce-ebf1-40c5-a219-9f9e2a961dba" -> _root_.scala.collection.immutable.Set("bfbc74cb-a053-4ac8-bbbe-67efb59bfc2c"),
    "49248ab2-1db2-40a6-b853-b140bd757afc" -> _root_.scala.collection.immutable.Set("112ede79-2776-4032-99b8-38a69a3eefd2"),
    "e45244ee-84b7-41e0-a9fe-cfc75d1b0289" -> _root_.scala.collection.immutable.Set(),
    "63cb1482-e529-4cde-856c-b1393136fbc6" -> _root_.scala.collection.immutable.Set("883d1404-2753-4ebb-ac3c-6fe704817d75"),
    "13238855-73ed-4346-9eed-d166f473d4c5" -> _root_.scala.collection.immutable.Set(),
    "eb600e64-f4a3-47ad-ad14-75ef01086fdb" -> _root_.scala.collection.immutable.Set(),
    "ab4613a0-ecb1-4eff-a127-84a225e2e382" -> _root_.scala.collection.immutable.Set()
  )

  implicit val `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) = `π-trick` -> `π-spell`

  given Conversion[`()`, Long] = _.name.asInstanceOf[Long]

  val max = 15

  val gen = new scala.util.Random

  def random = math.abs(gen.nextLong) % max

  def Main()(using ^ : String)(using % : %, / : /, - : -): IO[Unit] = for {
    _ <- IO.unit
    _53142b56_9a97_4c28_9961_73f77843c31c = _root_.scala.collection.immutable.Set("d752321f-98c3-4523-a8f9-c71561fdc2b4", "ab4613a0-ecb1-4eff-a127-84a225e2e382")
    _ <- `π-incr`(_53142b56_9a97_4c28_9961_73f77843c31c)
    _ <- for {
      num <- ν
      _   <- (
        `𝟎`,
        for {
          _7cb964fa_03b7_4e9f_8a82_5a487744e951 <- IO {
            def _7cb964fa_03b7_4e9f_8a82_5a487744e951(n: `()`): String => IO[Unit] = { implicit ^ =>
              for (
                _ <- (
                  for {
                    in <- ν
                    _  <- (
                      `𝟎`,
                      for (_ <- Fib(n, in)(using `π-uuid`)) yield (),
                      for {
                        (fib, _) <- in(null)("63cb1482-e529-4cde-856c-b1393136fbc6")
                        _        <- τ(∞)("883d1404-2753-4ebb-ac3c-6fe704817d75")
                        _        <- IO {
                          println(s"fib($n) = $fib")
                        }
                        _        <- num(null, random)("e45244ee-84b7-41e0-a9fe-cfc75d1b0289")
                        _        <- `𝟎`
                      } yield ()
                    ).parMapN { (_, _, _) => }
                  } yield (),
                  for {
                    (n, _) <- num(null)("d752321f-98c3-4523-a8f9-c71561fdc2b4")
                    _      <- _7cb964fa_03b7_4e9f_8a82_5a487744e951(n)(`π-uuid`)
                  } yield ()
                ).parMapN { (_, _) => }
              ) yield ()
            }
            _7cb964fa_03b7_4e9f_8a82_5a487744e951
          }
          (n, _)                                <- num(null)("d752321f-98c3-4523-a8f9-c71561fdc2b4")
          _                                     <- _7cb964fa_03b7_4e9f_8a82_5a487744e951(n)(`π-uuid`)
        } yield (),
        for {
          _ <- num(null, random)("ab4613a0-ecb1-4eff-a127-84a225e2e382")
          _ <- `𝟎`
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()

  def Fib(n: `()`, out: `()`)(using ^ : String)(using % : %, / : /, - : -): IO[Unit] = for {
    _ <- IO.unit
    _15f22b51_3bda_4a7b_a4d6_9e6d725ae13f = _root_.scala.collection.immutable.Set("40e19fce-ebf1-40c5-a219-9f9e2a961dba")
    _ <- `π-incr`(_15f22b51_3bda_4a7b_a4d6_9e6d725ae13f)
    _ <- for {
      _ <- τ(∞)("40e19fce-ebf1-40c5-a219-9f9e2a961dba")
      _ <- IO {
        println(s"n=$n")
      }
      f <- ν
      _ <- (
        `𝟎`,
        for (_ <- Fibonacci(f, n)(using `π-uuid`)) yield (),
        for {
          (res, _) <- f(null)("bfbc74cb-a053-4ac8-bbbe-67efb59bfc2c")
          _        <- out(null, res)("0f4a939f-8be4-4d69-bf1e-1879de5bac8d")
          _        <- `𝟎`
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()

  def Fibonacci(f: `()`, n: `()`)(using ^ : String)(using % : %, / : /, - : -): IO[Unit] = for (
    _ <-
      if (n < 2 ==== true) for {
        _ <- IO.unit
        _046e041b_4832_4959_9adb_17913794a9b6 = _root_.scala.collection.immutable.Set("13238855-73ed-4346-9eed-d166f473d4c5")
        _ <- `π-incr`(_046e041b_4832_4959_9adb_17913794a9b6)
        _ <- for {
          _ <- f(null, 1L)("13238855-73ed-4346-9eed-d166f473d4c5")
          _ <- `𝟎`
        } yield ()
      } yield ()
      else for {
        _ <- IO.unit
        _7c64d146_8a8b_4936_9bbc_b54891de4316 = _root_.scala.collection.immutable.Set("49248ab2-1db2-40a6-b853-b140bd757afc")
        _ <- `π-incr`(_7c64d146_8a8b_4936_9bbc_b54891de4316)
        _ <- for {
          g <- ν
          h <- ν
          _ <- (
            `𝟎`,
            for (_ <- Fibonacci(g, n - 1)(using `π-uuid`)) yield (),
            for (_ <- Fibonacci(h, n - 2)(using `π-uuid`)) yield (),
            for {
              (p, _) <- g(null)("49248ab2-1db2-40a6-b853-b140bd757afc")
              (r, _) <- h(null)("112ede79-2776-4032-99b8-38a69a3eefd2")
              _      <- f(null, p + r)("eb600e64-f4a3-47ad-ad14-75ef01086fdb")
              _      <- `𝟎`
            } yield ()
          ).parMapN { (_, _, _, _) => }
        } yield ()
      } yield ()
  ) yield ()
