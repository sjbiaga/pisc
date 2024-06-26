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

import _root_.cats.effect.{IO, IOApp, ExitCode}
import _root_.cats.effect.std.{Queue, Semaphore, Supervisor}

import `Π-loop`._
import `Π-stats`.{Rate, ∞, `ℝ⁺`, ⊤}

object App extends IOApp:

  private def run(args: List[String])(% : %, / : /, * : *): IO[Unit] = (for
    _ <- loop(π.`π-trick`)(using %, *).background
    _ <- poll(using %, /, *).background
  yield ()).use { _ => π.Main(args*)(using π.`π-uuid`)(using %, /) }

  override def run(args: List[String]): IO[ExitCode] =
    for
      % <- IO.ref(Map[String, Int | +]())
      / <- Queue.unbounded[IO, ((String, String), +)]
      * <- Semaphore[IO](1)
      _ <- run(args)(%, /, *)
    yield ExitCode.Success

object π:

  import _root_.java.util.UUID

  import _root_.cats.effect.syntax.all._
  import _root_.cats.syntax.all._

  import sΠ._

  def `π-uuid` = UUID.randomUUID.toString

  def `π-supervised`(io: => IO[Unit]): IO[Unit] =
    for _ <- Supervisor[IO](await = true).use(_.supervise(io).void)
    yield ()

  val `π-trick`: `Π-Map`[String, `Π-Set`[String]] =
    _root_.scala.collection.immutable.Map()

  val `π-spell`: `Π-Map`[String, `Π-Set`[String]] =
    _root_.scala.collection.immutable.Map(
      "15794edb-7a9d-42a0-b9c1-c45c743d29cb" -> _root_.scala.collection.immutable
        .Set(),
      "86a11619-baf6-48fd-b66e-ce496e6b3662" -> _root_.scala.collection.immutable
        .Set(),
      "041c92dc-3e69-4d8b-9347-dffd1d2fdbb2" -> _root_.scala.collection.immutable
        .Set("33c2da7f-dfb5-4513-ab39-58d0a01fb71e"),
      "1b085ca6-b733-4c4d-83cc-0f6c77b47cf1" -> _root_.scala.collection.immutable
        .Set("61c4c061-1847-47fc-99a2-085c43c6573a"),
      "ec9a5513-d213-45cf-b250-0365430f322b" -> _root_.scala.collection.immutable
        .Set("86a11619-baf6-48fd-b66e-ce496e6b3662"),
      "7cd91bf2-b1d0-425d-b9dd-265139816547" -> _root_.scala.collection.immutable
        .Set("ec9a5513-d213-45cf-b250-0365430f322b"),
      "d3ff2d7a-e89b-4567-8573-906cc795059c" -> _root_.scala.collection.immutable
        .Set(),
      "33c2da7f-dfb5-4513-ab39-58d0a01fb71e" -> _root_.scala.collection.immutable
        .Set("15794edb-7a9d-42a0-b9c1-c45c743d29cb"),
      "52bc53f4-6f7f-403b-a75a-1ca1d84f196a" -> _root_.scala.collection.immutable
        .Set(),
      "05d72d97-5968-405d-a7bf-ef6a99bc9b32" -> _root_.scala.collection.immutable
        .Set(
          "05d72d97-5968-405d-a7bf-ef6a99bc9b32",
          "7cd91bf2-b1d0-425d-b9dd-265139816547"
        ),
      "61c4c061-1847-47fc-99a2-085c43c6573a" -> _root_.scala.collection.immutable
        .Set("d3ff2d7a-e89b-4567-8573-906cc795059c"),
      "c4482750-946a-4b41-9506-887482639e36" -> _root_.scala.collection.immutable
        .Set()
    )

  implicit val `π-wand`
    : (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) =
    `π-trick` -> `π-spell`

  given Conversion[`()`, Long] = _.as[Long]

  val max = 11

  val gen = new scala.util.Random

  def random = 3 + gen.nextLong(max - 2)

  def Main(args: String*)(using ^ : String)(using % : %, / : /): IO[Unit] = for {
    _ <- IO.unit
    _46d31378_3a3d_40f2_a949_5c2dc1779ce1 =
      _root_.scala.collection.immutable.Set(
        "05d72d97-5968-405d-a7bf-ef6a99bc9b32",
        "52bc53f4-6f7f-403b-a75a-1ca1d84f196a"
      )
    _   <- `π-enable`(_46d31378_3a3d_40f2_a949_5c2dc1779ce1)
    num <- ν
    _   <- (
      `π-supervised`(for {
        _72e7923b_27bc_4004_834f_935ecb23b6c1 <- IO {
          def _72e7923b_27bc_4004_834f_935ecb23b6c1(n: `()`)
            : String => IO[Unit] = { implicit ^ =>
            if (!n) IO.unit
            else (
              `π-supervised`(for {
                in <- ν
                _  <- (
                  `π-supervised`(Fib(n, in)(using `π-uuid`)),
                  `π-supervised`(for {
                    (fib, _) <-
                      in(⊤(1L))("7cd91bf2-b1d0-425d-b9dd-265139816547")
                    _        <- τ(⊤(1L))("ec9a5513-d213-45cf-b250-0365430f322b")
                    _        <- IO {
                      println(s"fib(${n}) = ${fib}")
                    }
                    _        <-
                      num(⊤(1L), random)("86a11619-baf6-48fd-b66e-ce496e6b3662")
                  } yield ())
                ).parMapN { (_, _) => }
              } yield ()),
              `π-supervised`(for {
                (n, _) <- num(⊤(1L))("05d72d97-5968-405d-a7bf-ef6a99bc9b32")
                _      <- _72e7923b_27bc_4004_834f_935ecb23b6c1(n)(`π-uuid`)
              } yield ())
            ).parMapN { (_, _) => }
          }
          _72e7923b_27bc_4004_834f_935ecb23b6c1
        }
        (n, _) <- num(⊤(1L))("05d72d97-5968-405d-a7bf-ef6a99bc9b32")
        _      <- _72e7923b_27bc_4004_834f_935ecb23b6c1(n)(`π-uuid`)
      } yield ()),
      `π-supervised`(
        for (_ <- num(⊤(1L), random)("52bc53f4-6f7f-403b-a75a-1ca1d84f196a"))
          yield ()
      )
    ).parMapN { (_, _) => }
  } yield ()

  def Fib(n: `()`, out: `()`)(using ^ : String)(using % : %, / : /): IO[Unit] =
    for {
      _ <- IO.unit
      _0d4184d0_83aa_44bd_a9e5_bd07b4fac8c8 = _root_.scala.collection.immutable
        .Set("041c92dc-3e69-4d8b-9347-dffd1d2fdbb2")
      _ <- `π-enable`(_0d4184d0_83aa_44bd_a9e5_bd07b4fac8c8)
      _ <- τ(⊤(1L))("041c92dc-3e69-4d8b-9347-dffd1d2fdbb2")
      _ <- IO {
        println(s"n=${n}")
      }
      f <- ν
      _ <- (
        `π-supervised`(Fibonacci(f, n)(using `π-uuid`)),
        `π-supervised`(for {
          (res, _) <- f(∞(1L))("33c2da7f-dfb5-4513-ab39-58d0a01fb71e")
          _        <- out(⊤(1L), res)("15794edb-7a9d-42a0-b9c1-c45c743d29cb")
        } yield ())
      ).parMapN { (_, _) => }
    } yield ()

  def Fibonacci(f: `()`, n: `()`)(using ^ : String)(using % : %, / : /): IO[Unit] =
    if (n < 2 ==== true) for {
      _ <- IO.unit
      _cdbdfb41_f46f_4520_821a_e73830aa1193 = _root_.scala.collection.immutable
        .Set("c4482750-946a-4b41-9506-887482639e36")
      _ <- `π-enable`(_cdbdfb41_f46f_4520_821a_e73830aa1193)
      _ <- f(∞(1L), n)("c4482750-946a-4b41-9506-887482639e36")
    } yield ()
    else for {
      _ <- IO.unit
      _2c93ef4d_7181_4aa8_9ac3_f950d256e208 = _root_.scala.collection.immutable
        .Set("1b085ca6-b733-4c4d-83cc-0f6c77b47cf1")
      _ <- `π-enable`(_2c93ef4d_7181_4aa8_9ac3_f950d256e208)
      g <- ν
      h <- ν
      _ <- (
        `π-supervised`(Fibonacci(g, n - 1)(using `π-uuid`)),
        `π-supervised`(Fibonacci(h, n - 2)(using `π-uuid`)),
        `π-supervised`(for {
          (p, _) <- g(∞(1L))("1b085ca6-b733-4c4d-83cc-0f6c77b47cf1")
          (r, _) <- h(∞(1L))("61c4c061-1847-47fc-99a2-085c43c6573a")
          _      <- f(∞(1L), p + r)("d3ff2d7a-e89b-4567-8573-906cc795059c")
        } yield ())
      ).parMapN { (_, _, _) => }
    } yield ()
