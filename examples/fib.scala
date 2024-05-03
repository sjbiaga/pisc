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
      "09a8cae1-1d41-462a-9089-cec97523b50d" -> _root_.scala.collection.immutable
        .Set("cf2224d0-f314-4456-a33f-6e24b98ced43"),
      "551d0157-9bde-42a2-878f-31b5fbf5aeca" -> _root_.scala.collection.immutable
        .Set(),
      "10f22a60-a8f5-481d-bc4d-da9c244d5f8c" -> _root_.scala.collection.immutable
        .Set(
          "10f22a60-a8f5-481d-bc4d-da9c244d5f8c",
          "911f2878-1b3c-4d31-9d1d-03cdbf7355ef"
        ),
      "a0fe0985-210d-48fa-ad9d-20b5804a7bba" -> _root_.scala.collection.immutable
        .Set(),
      "c658758d-cf88-4574-8c61-e7fc760d9597" -> _root_.scala.collection.immutable
        .Set("a0fe0985-210d-48fa-ad9d-20b5804a7bba"),
      "cf2224d0-f314-4456-a33f-6e24b98ced43" -> _root_.scala.collection.immutable
        .Set(),
      "911f2878-1b3c-4d31-9d1d-03cdbf7355ef" -> _root_.scala.collection.immutable
        .Set("e8f9859f-4e93-41b1-84f8-e3d891d2fb1c"),
      "d369a835-ae6a-475d-8588-c1fd822d5399" -> _root_.scala.collection.immutable
        .Set(),
      "6ba8c99b-c39c-4adb-afaf-8178156f41dd" -> _root_.scala.collection.immutable
        .Set("09a8cae1-1d41-462a-9089-cec97523b50d"),
      "e8f9859f-4e93-41b1-84f8-e3d891d2fb1c" -> _root_.scala.collection.immutable
        .Set("d369a835-ae6a-475d-8588-c1fd822d5399"),
      "cebaeea3-a7cf-44af-bce7-18d52e2a6804" -> _root_.scala.collection.immutable
        .Set(),
      "0b6090bc-00b4-4d90-9bd0-643bcb97afcb" -> _root_.scala.collection.immutable
        .Set("c658758d-cf88-4574-8c61-e7fc760d9597")
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
    _0b106d9e_f74f_495f_8ca5_3bf6b172492c =
      _root_.scala.collection.immutable.Set(
        "10f22a60-a8f5-481d-bc4d-da9c244d5f8c",
        "551d0157-9bde-42a2-878f-31b5fbf5aeca"
      )
    _   <- `π-enable`(_0b106d9e_f74f_495f_8ca5_3bf6b172492c)
    num <- ν
    _   <- (
      IO.unit,
      `π-supervised`(for {
        _69908c19_bb53_4924_8dde_b67dd8a13e51 <- IO {
          def _69908c19_bb53_4924_8dde_b67dd8a13e51(n: `()`)
            : String => IO[Unit] = { implicit ^ =>
            if (!n) IO.unit
            else (
              `π-supervised`(for {
                in <- ν
                _  <- (
                  IO.unit,
                  `π-supervised`(Fib(n, in)(using `π-uuid`)),
                  `π-supervised`(for {
                    (fib, _) <-
                      in(⊤(1L))("911f2878-1b3c-4d31-9d1d-03cdbf7355ef")
                    _        <- τ(⊤(1L))("e8f9859f-4e93-41b1-84f8-e3d891d2fb1c")
                    _        <- IO {
                      println(s"fib(${n}) = ${fib}")
                    }
                    _        <-
                      num(⊤(1L), random)("d369a835-ae6a-475d-8588-c1fd822d5399")
                  } yield ())
                ).parMapN { (_, _, _) => }
              } yield ()),
              `π-supervised`(for {
                (n, _) <- num(⊤(1L))("10f22a60-a8f5-481d-bc4d-da9c244d5f8c")
                _      <- _69908c19_bb53_4924_8dde_b67dd8a13e51(n)(`π-uuid`)
              } yield ())
            ).parMapN { (_, _) => }
          }
          _69908c19_bb53_4924_8dde_b67dd8a13e51
        }
        (n, _) <- num(⊤(1L))("10f22a60-a8f5-481d-bc4d-da9c244d5f8c")
        _      <- _69908c19_bb53_4924_8dde_b67dd8a13e51(n)(`π-uuid`)
      } yield ()),
      `π-supervised`(
        for (_ <- num(⊤(1L), random)("551d0157-9bde-42a2-878f-31b5fbf5aeca"))
          yield ()
      )
    ).parMapN { (_, _, _) => }
  } yield ()

  def Fib(n: `()`, out: `()`)(using ^ : String)(using % : %, / : /): IO[Unit] =
    for {
      _ <- IO.unit
      _e6f3ddf4_ba78_4d36_b354_4c82d7c3c911 = _root_.scala.collection.immutable
        .Set("6ba8c99b-c39c-4adb-afaf-8178156f41dd")
      _ <- `π-enable`(_e6f3ddf4_ba78_4d36_b354_4c82d7c3c911)
      _ <- τ(⊤(1L))("6ba8c99b-c39c-4adb-afaf-8178156f41dd")
      _ <- IO {
        println(s"n=${n}")
      }
      f <- ν
      _ <- (
        IO.unit,
        `π-supervised`(Fibonacci(f, n)(using `π-uuid`)),
        `π-supervised`(for {
          (res, _) <- f(∞(1L))("09a8cae1-1d41-462a-9089-cec97523b50d")
          _        <- out(⊤(1L), res)("cf2224d0-f314-4456-a33f-6e24b98ced43")
        } yield ())
      ).parMapN { (_, _, _) => }
    } yield ()

  def Fibonacci(f: `()`, n: `()`)(using ^ : String)(using % : %, / : /): IO[Unit] =
    if (n < 2 ==== true) for {
      _ <- IO.unit
      _53a82c95_ee72_45c7_9fc2_11c9bcabb1b1 = _root_.scala.collection.immutable
        .Set("cebaeea3-a7cf-44af-bce7-18d52e2a6804")
      _ <- `π-enable`(_53a82c95_ee72_45c7_9fc2_11c9bcabb1b1)
      _ <- f(∞(1L), n)("cebaeea3-a7cf-44af-bce7-18d52e2a6804")
    } yield ()
    else for {
      _ <- IO.unit
      _cf73f722_7b53_4d4e_b35c_0f80f7e6755f = _root_.scala.collection.immutable
        .Set("0b6090bc-00b4-4d90-9bd0-643bcb97afcb")
      _ <- `π-enable`(_cf73f722_7b53_4d4e_b35c_0f80f7e6755f)
      g <- ν
      h <- ν
      _ <- (
        IO.unit,
        `π-supervised`(Fibonacci(g, n - 1)(using `π-uuid`)),
        `π-supervised`(Fibonacci(h, n - 2)(using `π-uuid`)),
        `π-supervised`(for {
          (p, _) <- g(∞(1L))("0b6090bc-00b4-4d90-9bd0-643bcb97afcb")
          (r, _) <- h(∞(1L))("c658758d-cf88-4574-8c61-e7fc760d9597")
          _      <- f(∞(1L), p + r)("a0fe0985-210d-48fa-ad9d-20b5804a7bba")
        } yield ())
      ).parMapN { (_, _, _, _) => }
    } yield ()
