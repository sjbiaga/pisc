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
    "65f6018d-e7bc-493a-95a7-b960c420c710" -> _root_.scala.collection.immutable.Set(),
    "8da09c23-efc7-42df-bf06-83de8bb2b17f" -> _root_.scala.collection.immutable.Set(),
    "3039f8c1-baaf-426b-9e09-24297ce96fb6" -> _root_.scala.collection.immutable.Set("193484f9-f1c4-4d81-b13d-28ea56b48847"),
    "d3f81450-9fa3-435d-bcbf-6fe61ee7e5d1" -> _root_.scala.collection.immutable.Set("8da09c23-efc7-42df-bf06-83de8bb2b17f"),
    "8058a303-93c8-4b07-a39e-ddd7daf70f19" -> _root_.scala.collection.immutable.Set(),
    "538a6f71-f211-45e9-9214-c0fbf1ecba1e" -> _root_.scala.collection.immutable.Set(),
    "9ca170db-62b2-4b7c-bb4f-f43c10f95369" -> _root_.scala.collection.immutable.Set(),
    "193484f9-f1c4-4d81-b13d-28ea56b48847" -> _root_.scala.collection.immutable.Set("65f6018d-e7bc-493a-95a7-b960c420c710"),
    "e23ecbb8-fa00-48bb-91e7-def202626d30" -> _root_.scala.collection.immutable.Set("ac907092-2a7b-4f89-bded-d4c15b69dc21"),
    "b1154a74-72db-4b2f-8d7e-edd76d94d5be" -> _root_.scala.collection.immutable.Set("d3f81450-9fa3-435d-bcbf-6fe61ee7e5d1"),
    "ac907092-2a7b-4f89-bded-d4c15b69dc21" -> _root_.scala.collection.immutable.Set("538a6f71-f211-45e9-9214-c0fbf1ecba1e")
  )

  implicit val `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) = `π-trick` -> `π-spell`

  given Conversion[`()`, Long] = _.name.asInstanceOf[Long]

  val max = 15

  val gen = new scala.util.Random

  def random = math.abs(gen.nextLong) % max

  def Main()(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _aab04747_ef3c_46a2_8150_e34042803277 = _root_.scala.collection.immutable.Set("ebd8eb66-3bd2-431f-8b9c-62593cd6e552", "8058a303-93c8-4b07-a39e-ddd7daf70f19")
    _ <- `π-none`(_aab04747_ef3c_46a2_8150_e34042803277)
    _ <- for {
      num <- ν
      _   <- (
        `𝟎`,
        for {
          _205197f5_6c04_4a93_ab8f_8d9d1e8ed06e <- IO {
            def _205197f5_6c04_4a93_ab8f_8d9d1e8ed06e(n: `()`): String => IO[Unit] = { implicit ^ =>
              for {
                _ <- IO.unit
                _b16af8e8_be3f_4d49_b7f8_cb6d44c2eefb = _root_.scala.collection.immutable.Set("ebd8eb66-3bd2-431f-8b9c-62593cd6e552", "e23ecbb8-fa00-48bb-91e7-def202626d30")
                _ <- `π-none`(_b16af8e8_be3f_4d49_b7f8_cb6d44c2eefb)
                _ <- (
                  for {
                    in <- ν
                    _  <- (
                      `𝟎`,
                      for (_ <- Fib(n, in)(using `π-uuid`)) yield (),
                      for {
                        (fib, _) <- in(null)("e23ecbb8-fa00-48bb-91e7-def202626d30")
                        _        <- τ(∞)("ac907092-2a7b-4f89-bded-d4c15b69dc21")
                        _        <- IO {
                          println(s"fib($n) = $fib")
                        }
                        _        <- num(null, random)("538a6f71-f211-45e9-9214-c0fbf1ecba1e")
                      } yield ()
                    ).parMapN { (_, _, _) => }
                  } yield (),
                  for {
                    (n, _) <- num(null)("ebd8eb66-3bd2-431f-8b9c-62593cd6e552")
                    _      <- _205197f5_6c04_4a93_ab8f_8d9d1e8ed06e(n)(`π-uuid`)
                  } yield ()
                ).parMapN { (_, _) => }
              } yield ()
            }
            _205197f5_6c04_4a93_ab8f_8d9d1e8ed06e
          }
          (n, _)                                <- num(null)("ebd8eb66-3bd2-431f-8b9c-62593cd6e552")
          _                                     <- _205197f5_6c04_4a93_ab8f_8d9d1e8ed06e(n)(`π-uuid`)
        } yield (),
        for (_ <- num(null, random)("8058a303-93c8-4b07-a39e-ddd7daf70f19")) yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()

  def Fib(n: `()`, out: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _816ac34d_4d5d_4eca_bcae_f75dd2d5bd4b = _root_.scala.collection.immutable.Set("3039f8c1-baaf-426b-9e09-24297ce96fb6")
    _ <- `π-none`(_816ac34d_4d5d_4eca_bcae_f75dd2d5bd4b)
    _ <- for {
      _ <- τ(∞)("3039f8c1-baaf-426b-9e09-24297ce96fb6")
      _ <- IO {
        println(s"n=$n")
      }
      f <- ν
      _ <- (
        `𝟎`,
        for (_ <- Fibonacci(f, n)(using `π-uuid`)) yield (),
        for {
          (res, _) <- f(null)("193484f9-f1c4-4d81-b13d-28ea56b48847")
          _        <- out(null, res)("65f6018d-e7bc-493a-95a7-b960c420c710")
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()

  def Fibonacci(f: `()`, n: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for (
    _ <-
      if (n < 2 ==== true) for {
        _ <- IO.unit
        _9bff51d7_36e2_4d47_8a3a_a353fc6524bc = _root_.scala.collection.immutable.Set("9ca170db-62b2-4b7c-bb4f-f43c10f95369")
        _ <- `π-none`(_9bff51d7_36e2_4d47_8a3a_a353fc6524bc)
        _ <- for (_ <- f(null, 1L)("9ca170db-62b2-4b7c-bb4f-f43c10f95369")) yield ()
      } yield ()
      else for {
        _ <- IO.unit
        _45431b4e_6098_414e_b0a8_08c8765d1a75 = _root_.scala.collection.immutable.Set("b1154a74-72db-4b2f-8d7e-edd76d94d5be")
        _ <- `π-none`(_45431b4e_6098_414e_b0a8_08c8765d1a75)
        _ <- for {
          g <- ν
          h <- ν
          _ <- (
            `𝟎`,
            for (_ <- Fibonacci(g, n - 1)(using `π-uuid`)) yield (),
            for (_ <- Fibonacci(h, n - 2)(using `π-uuid`)) yield (),
            for {
              (p, _) <- g(null)("b1154a74-72db-4b2f-8d7e-edd76d94d5be")
              (r, _) <- h(null)("d3f81450-9fa3-435d-bcbf-6fe61ee7e5d1")
              _      <- f(null, p + r)("8da09c23-efc7-42df-bf06-83de8bb2b17f")
            } yield ()
          ).parMapN { (_, _, _, _) => }
        } yield ()
      } yield ()
  ) yield ()
