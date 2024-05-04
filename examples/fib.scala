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
      "3c14c50c-2f25-46b5-bc88-b678658ea716" -> _root_.scala.collection.immutable
        .Set(),
      "4ff275ea-6d03-4383-a086-01aa21e6e9ba" -> _root_.scala.collection.immutable
        .Set(),
      "3a1ed11b-f5b4-4f5b-a901-f43a670667f8" -> _root_.scala.collection.immutable
        .Set("4ff275ea-6d03-4383-a086-01aa21e6e9ba"),
      "7bbf7151-ac4e-4c0a-88e0-29e80793d733" -> _root_.scala.collection.immutable
        .Set("84bf1dc5-c472-447c-b60f-197729cbca59"),
      "19808100-d00b-4dcf-9b6f-b984a146336a" -> _root_.scala.collection.immutable
        .Set("6e7e53de-d0ae-4ad4-886e-ee6a96b5f3a9"),
      "6e7e53de-d0ae-4ad4-886e-ee6a96b5f3a9" -> _root_.scala.collection.immutable
        .Set(),
      "57a1440a-951c-4d25-a270-b1fe23e617fa" -> _root_.scala.collection.immutable
        .Set("19808100-d00b-4dcf-9b6f-b984a146336a"),
      "84bf1dc5-c472-447c-b60f-197729cbca59" -> _root_.scala.collection.immutable
        .Set(),
      "a2ad6581-a40a-4f7b-a62b-8af802d50138" -> _root_.scala.collection.immutable
        .Set("7bbf7151-ac4e-4c0a-88e0-29e80793d733"),
      "4a055f12-bd57-4359-8c2b-cee214da99aa" -> _root_.scala.collection.immutable
        .Set(
          "4a055f12-bd57-4359-8c2b-cee214da99aa",
          "57a1440a-951c-4d25-a270-b1fe23e617fa"
        ),
      "3b49fb3b-3489-499a-b64d-800abdfdcaab" -> _root_.scala.collection.immutable
        .Set(),
      "81c77f4a-604e-4aef-aab9-fd80f243fe7c" -> _root_.scala.collection.immutable
        .Set("3a1ed11b-f5b4-4f5b-a901-f43a670667f8")
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
    _db2f9053_3bcb_4501_91f6_74f2d9395d5a =
      _root_.scala.collection.immutable.Set(
        "4a055f12-bd57-4359-8c2b-cee214da99aa",
        "3b49fb3b-3489-499a-b64d-800abdfdcaab"
      )
    _   <- `π-enable`(_db2f9053_3bcb_4501_91f6_74f2d9395d5a)
    num <- ν
    _   <- (
      `π-supervised`(for {
        _05b98b4b_5d46_4f77_9518_150d7f886ec3 <- IO {
          def _05b98b4b_5d46_4f77_9518_150d7f886ec3(n: `()`)
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
                      in(⊤(1L))("57a1440a-951c-4d25-a270-b1fe23e617fa")
                    _        <- τ(⊤(1L))("19808100-d00b-4dcf-9b6f-b984a146336a")
                    _        <- IO {
                      println(s"fib(${n}) = ${fib}")
                    }
                    _        <-
                      num(⊤(1L), random)("6e7e53de-d0ae-4ad4-886e-ee6a96b5f3a9")
                  } yield ())
                ).parMapN { (_, _, _) => }
              } yield ()),
              `π-supervised`(for {
                (n, _) <- num(⊤(1L))("4a055f12-bd57-4359-8c2b-cee214da99aa")
                _      <- _05b98b4b_5d46_4f77_9518_150d7f886ec3(n)(`π-uuid`)
              } yield ())
            ).parMapN { (_, _) => }
          }
          _05b98b4b_5d46_4f77_9518_150d7f886ec3
        }
        (n, _) <- num(⊤(1L))("4a055f12-bd57-4359-8c2b-cee214da99aa")
        _      <- _05b98b4b_5d46_4f77_9518_150d7f886ec3(n)(`π-uuid`)
      } yield ()),
      `π-supervised`(
        for (_ <- num(⊤(1L), random)("3b49fb3b-3489-499a-b64d-800abdfdcaab"))
          yield ()
      )
    ).parMapN { (_, _) => }
  } yield ()

  def Fib(n: `()`, out: `()`)(using ^ : String)(using % : %, / : /): IO[Unit] =
    for {
      _ <- IO.unit
      _02a03c8d_3b17_49c0_84d7_85eb69e30019 = _root_.scala.collection.immutable
        .Set("81c77f4a-604e-4aef-aab9-fd80f243fe7c")
      _ <- `π-enable`(_02a03c8d_3b17_49c0_84d7_85eb69e30019)
      _ <- τ(⊤(1L))("81c77f4a-604e-4aef-aab9-fd80f243fe7c")
      _ <- IO {
        println(s"n=${n}")
      }
      f <- ν
      _ <- (
        `π-supervised`(Fibonacci(f, n)(using `π-uuid`)),
        `π-supervised`(for {
          (res, _) <- f(∞(1L))("3a1ed11b-f5b4-4f5b-a901-f43a670667f8")
          _        <- out(⊤(1L), res)("4ff275ea-6d03-4383-a086-01aa21e6e9ba")
        } yield ())
      ).parMapN { (_, _) => }
    } yield ()

  def Fibonacci(f: `()`, n: `()`)(using ^ : String)(using % : %, / : /): IO[Unit] =
    if (n < 2 ==== true) for {
      _ <- IO.unit
      _f926e751_5b54_48d2_b9fc_8824e810da55 = _root_.scala.collection.immutable
        .Set("3c14c50c-2f25-46b5-bc88-b678658ea716")
      _ <- `π-enable`(_f926e751_5b54_48d2_b9fc_8824e810da55)
      _ <- f(∞(1L), n)("3c14c50c-2f25-46b5-bc88-b678658ea716")
    } yield ()
    else for {
      _ <- IO.unit
      _f5ca5f40_b74e_41f8_8a80_e24d0852ae6b = _root_.scala.collection.immutable
        .Set("a2ad6581-a40a-4f7b-a62b-8af802d50138")
      _ <- `π-enable`(_f5ca5f40_b74e_41f8_8a80_e24d0852ae6b)
      g <- ν
      h <- ν
      _ <- (
        `π-supervised`(Fibonacci(g, n - 1)(using `π-uuid`)),
        `π-supervised`(Fibonacci(h, n - 2)(using `π-uuid`)),
        `π-supervised`(for {
          (p, _) <- g(∞(1L))("a2ad6581-a40a-4f7b-a62b-8af802d50138")
          (r, _) <- h(∞(1L))("7bbf7151-ac4e-4c0a-88e0-29e80793d733")
          _      <- f(∞(1L), p + r)("84bf1dc5-c472-447c-b60f-197729cbca59")
        } yield ())
      ).parMapN { (_, _, _) => }
    } yield ()
