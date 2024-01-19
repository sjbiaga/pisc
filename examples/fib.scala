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
    "7f393a61-43d7-4e85-b026-e41d0c27a569" -> _root_.scala.collection.immutable.Set(),
    "26733836-6ff6-49aa-9fe8-10ea3e0c3d73" -> _root_.scala.collection.immutable.Set(),
    "068e6e8a-b603-43c9-9bcf-6f6b0e86c09b" -> _root_.scala.collection.immutable.Set("ae55e8d2-6f7f-49a4-90ce-8649eeb9c6d7"),
    "b98a168c-b5a2-4297-acd6-4d3284afa2ac" -> _root_.scala.collection.immutable.Set(),
    "f1f80969-6a1c-48a7-889a-5584446ade70" -> _root_.scala.collection.immutable.Set(),
    "606a752d-5871-4710-8d65-36cb30020ede" -> _root_.scala.collection.immutable.Set("e63909a9-bff6-42b4-b688-2d71c5067ca3"),
    "09889210-14fb-49d5-b92d-476134ab0407" -> _root_.scala.collection.immutable.Set(),
    "245d40e7-647c-4dba-a3db-3d7ff8ff3e74" -> _root_.scala.collection.immutable.Set("cd64f06f-8150-4090-9552-047fa953f8b8"),
    "ae55e8d2-6f7f-49a4-90ce-8649eeb9c6d7" -> _root_.scala.collection.immutable.Set("26733836-6ff6-49aa-9fe8-10ea3e0c3d73"),
    "0eba90e8-0819-420f-ab17-2edee7dac878" -> _root_.scala.collection.immutable.Set("0eba90e8-0819-420f-ab17-2edee7dac878", "068e6e8a-b603-43c9-9bcf-6f6b0e86c09b"),
    "e63909a9-bff6-42b4-b688-2d71c5067ca3" -> _root_.scala.collection.immutable.Set("f1f80969-6a1c-48a7-889a-5584446ade70"),
    "cd64f06f-8150-4090-9552-047fa953f8b8" -> _root_.scala.collection.immutable.Set("b98a168c-b5a2-4297-acd6-4d3284afa2ac")
  )

  given Conversion[`()`, Long] = _.name.asInstanceOf[Long]

  val max = 15

  val gen = new scala.util.Random

  def random = math.abs(gen.nextLong) % max

  def Main()(using ^ : String)(using % : %, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _4c9abc63_2255_473e_a7d8_9e50cb649217 = _root_.scala.collection.immutable.Set("0eba90e8-0819-420f-ab17-2edee7dac878", "068e6e8a-b603-43c9-9bcf-6f6b0e86c09b", "7f393a61-43d7-4e85-b026-e41d0c27a569")
    _ <- `π-incr`(_4c9abc63_2255_473e_a7d8_9e50cb649217)
    _ <- for {
      num <- ν
      _   <- (
        `𝟎`,
        for {
          _f0a9c786_37da_46d9_9a52_4667379d738e <- IO {
            def _f0a9c786_37da_46d9_9a52_4667379d738e(n: `()`): String => IO[Unit] = { implicit ^ =>
              for (
                _ <- (
                  for {
                    in <- ν
                    _  <- (
                      `𝟎`,
                      for (_ <- Fib(n, in)(using `π-uuid`)) yield (),
                      for {
                        (fib, _) <- in(null)("068e6e8a-b603-43c9-9bcf-6f6b0e86c09b")
                        _        <- τ(∞)("ae55e8d2-6f7f-49a4-90ce-8649eeb9c6d7")
                        _        <- IO {
                          println(s"fib($n) = $fib")
                        }
                        _        <- num(null, random)("26733836-6ff6-49aa-9fe8-10ea3e0c3d73")
                        _        <- `𝟎`
                      } yield ()
                    ).parMapN { (_, _, _) => }
                  } yield (),
                  for {
                    (n, _) <- num(null)("0eba90e8-0819-420f-ab17-2edee7dac878")
                    _      <- _f0a9c786_37da_46d9_9a52_4667379d738e(n)(`π-uuid`)
                  } yield ()
                ).parMapN { (_, _) => }
              ) yield ()
            }
            _f0a9c786_37da_46d9_9a52_4667379d738e
          }
          (n, _)                                <- num(null)("0eba90e8-0819-420f-ab17-2edee7dac878")
          _                                     <- _f0a9c786_37da_46d9_9a52_4667379d738e(n)(`π-uuid`)
        } yield (),
        for {
          _ <- num(null, random)("7f393a61-43d7-4e85-b026-e41d0c27a569")
          _ <- `𝟎`
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()

  def Fib(n: `()`, out: `()`)(using ^ : String)(using % : %, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _6e4a15a5_247e_4f4e_83f3_583c713bdf45 = _root_.scala.collection.immutable.Set("606a752d-5871-4710-8d65-36cb30020ede")
    _ <- `π-incr`(_6e4a15a5_247e_4f4e_83f3_583c713bdf45)
    _ <- for {
      _ <- τ(∞)("606a752d-5871-4710-8d65-36cb30020ede")
      _ <- IO {
        println(s"n=$n")
      }
      f <- ν
      _ <- (
        `𝟎`,
        for (_ <- Fibonacci(f, n)(using `π-uuid`)) yield (),
        for {
          (res, _) <- f(null)("e63909a9-bff6-42b4-b688-2d71c5067ca3")
          _        <- out(null, res)("f1f80969-6a1c-48a7-889a-5584446ade70")
          _        <- `𝟎`
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()

  def Fibonacci(f: `()`, n: `()`)(using ^ : String)(using % : %, / : /, * : *): IO[Unit] = for (
    _ <-
      if (n < 2 ==== true) for {
        _ <- IO.unit
        _21c1b1db_f625_4915_894a_270ac5ea9c1a = _root_.scala.collection.immutable.Set("09889210-14fb-49d5-b92d-476134ab0407")
        _ <- `π-incr`(_21c1b1db_f625_4915_894a_270ac5ea9c1a)
        _ <- for {
          _ <- f(null, 1L)("09889210-14fb-49d5-b92d-476134ab0407")
          _ <- `𝟎`
        } yield ()
      } yield ()
      else for {
        _ <- IO.unit
        _0ab67e5d_19d3_45f6_8808_a1981bf6408d = _root_.scala.collection.immutable.Set("245d40e7-647c-4dba-a3db-3d7ff8ff3e74")
        _ <- `π-incr`(_0ab67e5d_19d3_45f6_8808_a1981bf6408d)
        _ <- for {
          g <- ν
          h <- ν
          _ <- (
            `𝟎`,
            for (_ <- Fibonacci(g, n - 1)(using `π-uuid`)) yield (),
            for (_ <- Fibonacci(h, n - 2)(using `π-uuid`)) yield (),
            for {
              (p, _) <- g(null)("245d40e7-647c-4dba-a3db-3d7ff8ff3e74")
              (r, _) <- h(null)("cd64f06f-8150-4090-9552-047fa953f8b8")
              _      <- f(null, p + r)("b98a168c-b5a2-4297-acd6-4d3284afa2ac")
              _      <- `𝟎`
            } yield ()
          ).parMapN { (_, _, _, _) => }
        } yield ()
      } yield ()
  ) yield ()
