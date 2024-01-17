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
    "3d37f0c5-4ad0-493f-bc1d-9ca99bd1f511" -> _root_.scala.collection.immutable.Set(),
    "69224f6a-ae02-45a0-af7e-0a3b7baf8e9e" -> _root_.scala.collection.immutable.Set("f7e710f9-33e7-4320-ac1e-b46dafff32a5"),
    "f7e710f9-33e7-4320-ac1e-b46dafff32a5" -> _root_.scala.collection.immutable.Set("c15cfe57-0d42-4a36-8dff-8e32fb2f1555"),
    "c15cfe57-0d42-4a36-8dff-8e32fb2f1555" -> _root_.scala.collection.immutable.Set(),
    "dcfc3bc9-7124-44cc-8e48-9e5850e4422c" -> _root_.scala.collection.immutable.Set("7bb8f1a0-142c-49c2-9325-c63ff870c5d1"),
    "7bb8f1a0-142c-49c2-9325-c63ff870c5d1" -> _root_.scala.collection.immutable.Set(),
    "47906ba1-af69-446b-9912-32ad70ad7c96" -> _root_.scala.collection.immutable.Set(),
    "98372dd0-ae90-4e6d-8b67-e2fcb651629b" -> _root_.scala.collection.immutable.Set("dcfc3bc9-7124-44cc-8e48-9e5850e4422c"),
    "3d4d03d7-d512-477f-80cf-af3bf1f7f169" -> _root_.scala.collection.immutable.Set("be7a924e-36e4-4f68-a72f-1b421a0a8913"),
    "8a2dcd80-84a1-465c-8e5c-25d74d06f41a" -> _root_.scala.collection.immutable.Set(),
    "be7a924e-36e4-4f68-a72f-1b421a0a8913" -> _root_.scala.collection.immutable.Set("3d37f0c5-4ad0-493f-bc1d-9ca99bd1f511")
  )

  implicit val `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) = `π-trick` -> `π-spell`

  given Conversion[`()`, Long] = _.name.asInstanceOf[Long]

  val max = 15

  val gen = new scala.util.Random

  def random = math.abs(gen.nextLong) % max

  def Main()(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _c8cc89f8_87f8_4214_b349_19e0826e84a3 = _root_.scala.collection.immutable.Set("b4b0243e-a3b7-404f-8b2f-2a19a41dcbef", "8a2dcd80-84a1-465c-8e5c-25d74d06f41a")
    _ <- `π-none`(_c8cc89f8_87f8_4214_b349_19e0826e84a3)
    _ <- for {
      num <- ν
      _   <- (
        `𝟎`,
        for {
          _6834d331_1d7b_45f8_9dac_2aac5c335c5c <- IO {
            def _6834d331_1d7b_45f8_9dac_2aac5c335c5c(n: `()`): String => IO[Unit] = { implicit ^ =>
              for {
                _ <- IO.unit
                _f9e05d32_9ccc_4e0a_85f4_0a0d4f5e585b = _root_.scala.collection.immutable.Set("b4b0243e-a3b7-404f-8b2f-2a19a41dcbef", "3d4d03d7-d512-477f-80cf-af3bf1f7f169")
                _ <- `π-none`(_f9e05d32_9ccc_4e0a_85f4_0a0d4f5e585b)
                _ <- (
                  for {
                    in <- ν
                    _  <- (
                      `𝟎`,
                      for (_ <- Fib(n, in)(using `π-uuid`)) yield (),
                      for {
                        (fib, _) <- in(null)("3d4d03d7-d512-477f-80cf-af3bf1f7f169")
                        _        <- τ(∞)("be7a924e-36e4-4f68-a72f-1b421a0a8913")
                        _        <- IO {
                          println(s"fib($n) = $fib")
                        }
                        _        <- num(null, random)("3d37f0c5-4ad0-493f-bc1d-9ca99bd1f511")
                      } yield ()
                    ).parMapN { (_, _, _) => }
                  } yield (),
                  for {
                    (n, _) <- num(null)("b4b0243e-a3b7-404f-8b2f-2a19a41dcbef")
                    _      <- _6834d331_1d7b_45f8_9dac_2aac5c335c5c(n)(`π-uuid`)
                  } yield ()
                ).parMapN { (_, _) => }
              } yield ()
            }
            _6834d331_1d7b_45f8_9dac_2aac5c335c5c
          }
          (n, _)                                <- num(null)("b4b0243e-a3b7-404f-8b2f-2a19a41dcbef")
          _                                     <- _6834d331_1d7b_45f8_9dac_2aac5c335c5c(n)(`π-uuid`)
        } yield (),
        for (_ <- num(null, random)("8a2dcd80-84a1-465c-8e5c-25d74d06f41a")) yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()

  def Fib(n: `()`, out: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _90423b19_507f_42b8_9d43_8771befa8af9 = _root_.scala.collection.immutable.Set("69224f6a-ae02-45a0-af7e-0a3b7baf8e9e")
    _ <- `π-none`(_90423b19_507f_42b8_9d43_8771befa8af9)
    _ <- for {
      _ <- τ(∞)("69224f6a-ae02-45a0-af7e-0a3b7baf8e9e")
      _ <- IO {
        println(s"n=$n")
      }
      f <- ν
      _ <- (
        `𝟎`,
        for (_ <- Fibonacci(f, n)(using `π-uuid`)) yield (),
        for {
          (res, _) <- f(null)("f7e710f9-33e7-4320-ac1e-b46dafff32a5")
          _        <- out(null, res)("c15cfe57-0d42-4a36-8dff-8e32fb2f1555")
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()

  def Fibonacci(f: `()`, n: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for (
    _ <-
      if (n < 2 ==== true) for {
        _ <- IO.unit
        _1ed0b2fc_2705_4f8d_8ff3_1f2b43b8263a = _root_.scala.collection.immutable.Set("47906ba1-af69-446b-9912-32ad70ad7c96")
        _ <- `π-none`(_1ed0b2fc_2705_4f8d_8ff3_1f2b43b8263a)
        _ <- for (_ <- f(null, 1L)("47906ba1-af69-446b-9912-32ad70ad7c96")) yield ()
      } yield ()
      else for {
        _ <- IO.unit
        _b981e8de_1212_44f3_b1a5_3326d85f087a = _root_.scala.collection.immutable.Set("98372dd0-ae90-4e6d-8b67-e2fcb651629b")
        _ <- `π-none`(_b981e8de_1212_44f3_b1a5_3326d85f087a)
        _ <- for {
          g <- ν
          h <- ν
          _ <- (
            `𝟎`,
            for (_ <- Fibonacci(g, n - 1)(using `π-uuid`)) yield (),
            for (_ <- Fibonacci(h, n - 2)(using `π-uuid`)) yield (),
            for {
              (p, _) <- g(null)("98372dd0-ae90-4e6d-8b67-e2fcb651629b")
              (r, _) <- h(null)("dcfc3bc9-7124-44cc-8e48-9e5850e4422c")
              _      <- f(null, p + r)("7bb8f1a0-142c-49c2-9325-c63ff870c5d1")
            } yield ()
          ).parMapN { (_, _, _, _) => }
        } yield ()
      } yield ()
  ) yield ()
