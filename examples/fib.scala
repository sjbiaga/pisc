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
    "7d4358de-31b8-43bd-acd4-4b6ee9c788d0" -> _root_.scala.collection.immutable.Set("c5d7e510-50d2-43d9-a8d1-43da4362be50"),
    "57b81dcc-693a-449d-b261-d3de80dfb2d5" -> _root_.scala.collection.immutable.Set(),
    "9f7ce171-3560-4a83-bc4f-d5cab92e663c" -> _root_.scala.collection.immutable.Set(),
    "7e095ff4-356d-4bb9-99a9-11b73da5a092" -> _root_.scala.collection.immutable.Set("7a0abc84-9b25-4967-ac0f-21d2ee906021"),
    "c5d7e510-50d2-43d9-a8d1-43da4362be50" -> _root_.scala.collection.immutable.Set(),
    "7a0abc84-9b25-4967-ac0f-21d2ee906021" -> _root_.scala.collection.immutable.Set(),
    "0550cf58-ebdf-4de0-b01a-b966fc9be982" -> _root_.scala.collection.immutable.Set("f920111b-9d13-4b26-a1d4-849ce3f03577"),
    "7734a920-9f2d-42d4-913e-d90b52675a86" -> _root_.scala.collection.immutable.Set("7e095ff4-356d-4bb9-99a9-11b73da5a092"),
    "d68d0d68-6633-4ba8-afc2-d8d356ae824c" -> _root_.scala.collection.immutable.Set("429a9c67-f272-4331-bc74-bb3064de9411"),
    "a4d77900-fcaf-498e-8858-d12799e0eee9" -> _root_.scala.collection.immutable.Set("96e27670-3e3f-4943-8a65-2e5cbf8c838b"),
    "3626dd82-0782-444f-b5cb-58437d05f5f2" -> _root_.scala.collection.immutable.Set(),
    "96e27670-3e3f-4943-8a65-2e5cbf8c838b" -> _root_.scala.collection.immutable.Set("7734a920-9f2d-42d4-913e-d90b52675a86"),
    "429a9c67-f272-4331-bc74-bb3064de9411" -> _root_.scala.collection.immutable.Set("3626dd82-0782-444f-b5cb-58437d05f5f2"),
    "f920111b-9d13-4b26-a1d4-849ce3f03577" -> _root_.scala.collection.immutable.Set("57b81dcc-693a-449d-b261-d3de80dfb2d5")
  )

  implicit val `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) = `π-trick` -> `π-spell`

  given Conversion[`()`, Long] = _.name.asInstanceOf[Long]

  val max = 15

  val gen = new scala.util.Random

  def random = math.abs(gen.nextLong) % max

  def Main()(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _9ccdc7ce_b250_49e3_95ba_976477a5c1f1 = _root_.scala.collection.immutable.Set("9e4ce0c6-5f00-4fa3-8379-b9763fadc1f5", "7d4358de-31b8-43bd-acd4-4b6ee9c788d0")
    _ <- `π-none`(_9ccdc7ce_b250_49e3_95ba_976477a5c1f1)
    _ <- for {
      num1 <- ν
      num2 <- ν
      _    <- (
        `𝟎`,
        for {
          _b1644840_3438_4a9d_acdb_a68841735c52 <- IO {
            lazy val _b1644840_3438_4a9d_acdb_a68841735c52: String => IO[Unit] = { implicit ^ =>
              for {
                _ <- IO.unit
                _d739087e_4ed7_4232_9d99_cbe492b7b785 = _root_.scala.collection.immutable.Set("9e4ce0c6-5f00-4fa3-8379-b9763fadc1f5", "a4d77900-fcaf-498e-8858-d12799e0eee9")
                _ <- `π-none`(_d739087e_4ed7_4232_9d99_cbe492b7b785)
                _ <- (
                  for {
                    (n, _) <- num2(null)("a4d77900-fcaf-498e-8858-d12799e0eee9")
                    in     <- ν
                    _      <- (
                      `𝟎`,
                      for (_ <- Fib(n, in)(using `π-uuid`)) yield (),
                      for {
                        (fib, _) <- in(null)("96e27670-3e3f-4943-8a65-2e5cbf8c838b")
                        _        <- τ(∞)("7734a920-9f2d-42d4-913e-d90b52675a86")
                        _        <- IO {
                          println(s"fib($n) = $fib")
                        }
                        (n, _)   <- num1(null)("7e095ff4-356d-4bb9-99a9-11b73da5a092")
                        _        <- num2(null, n)("7a0abc84-9b25-4967-ac0f-21d2ee906021")
                      } yield ()
                    ).parMapN { (_, _, _) => }
                  } yield (),
                  for {
                    _ <- num1(null, random)("9e4ce0c6-5f00-4fa3-8379-b9763fadc1f5")
                    _ <- _b1644840_3438_4a9d_acdb_a68841735c52(`π-uuid`)
                  } yield ()
                ).parMapN { (_, _) => }
              } yield ()
            }
            _b1644840_3438_4a9d_acdb_a68841735c52
          }
          _                                     <- num1(null, random)("9e4ce0c6-5f00-4fa3-8379-b9763fadc1f5")
          _                                     <- _b1644840_3438_4a9d_acdb_a68841735c52(`π-uuid`)
        } yield (),
        for {
          (n, _) <- num1(null)("7d4358de-31b8-43bd-acd4-4b6ee9c788d0")
          _      <- num2(null, n)("c5d7e510-50d2-43d9-a8d1-43da4362be50")
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()

  def Fib(n: `()`, out: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _7c39db0a_28dc_4322_9814_b381ede45960 = _root_.scala.collection.immutable.Set("0550cf58-ebdf-4de0-b01a-b966fc9be982")
    _ <- `π-none`(_7c39db0a_28dc_4322_9814_b381ede45960)
    _ <- for {
      _ <- τ(∞)("0550cf58-ebdf-4de0-b01a-b966fc9be982")
      _ <- IO {
        println(s"n=$n")
      }
      f <- ν
      _ <- (
        `𝟎`,
        for (_ <- Fibonacci(f, n)(using `π-uuid`)) yield (),
        for {
          (res, _) <- f(null)("f920111b-9d13-4b26-a1d4-849ce3f03577")
          _        <- out(null, res)("57b81dcc-693a-449d-b261-d3de80dfb2d5")
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()

  def Fibonacci(f: `()`, n: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for (
    _ <-
      if (n < 2 ==== true) for {
        _ <- IO.unit
        _726c0522_96b9_4805_a2df_f566f0f604ba = _root_.scala.collection.immutable.Set("9f7ce171-3560-4a83-bc4f-d5cab92e663c")
        _ <- `π-none`(_726c0522_96b9_4805_a2df_f566f0f604ba)
        _ <- for (_ <- f(null, 1L)("9f7ce171-3560-4a83-bc4f-d5cab92e663c")) yield ()
      } yield ()
      else for {
        _ <- IO.unit
        _b14f8023_5efa_4b05_bdb1_33d9d21efc4d = _root_.scala.collection.immutable.Set("d68d0d68-6633-4ba8-afc2-d8d356ae824c")
        _ <- `π-none`(_b14f8023_5efa_4b05_bdb1_33d9d21efc4d)
        _ <- for {
          g <- ν
          h <- ν
          _ <- (
            `𝟎`,
            for (_ <- Fibonacci(g, n - 1)(using `π-uuid`)) yield (),
            for (_ <- Fibonacci(h, n - 2)(using `π-uuid`)) yield (),
            for {
              (p, _) <- g(null)("d68d0d68-6633-4ba8-afc2-d8d356ae824c")
              (r, _) <- h(null)("429a9c67-f272-4331-bc74-bb3064de9411")
              _      <- f(null, p + r)("3626dd82-0782-444f-b5cb-58437d05f5f2")
            } yield ()
          ).parMapN { (_, _, _, _) => }
        } yield ()
      } yield ()
  ) yield ()
