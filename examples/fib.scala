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
    "42fbea3e-bd80-4af3-8042-22247249634a" -> _root_.scala.collection.immutable.Set("5d852ce8-f039-48b1-b28a-8520a3bd5e7b"),
    "cc7abd05-b582-4deb-a5f1-27fd4dcd109a" -> _root_.scala.collection.immutable.Set("2efd2cdb-bbe0-4e5e-81ad-b0794600ecc7"),
    "3cd4b134-2a1b-4dd5-89e6-6bd666b601bc" -> _root_.scala.collection.immutable.Set(),
    "9f018a51-eeaa-4fe4-b719-4a2698e56abb" -> _root_.scala.collection.immutable.Set(),
    "931a50ca-17c6-4574-883e-2830f84d8daa" -> _root_.scala.collection.immutable.Set("42fbea3e-bd80-4af3-8042-22247249634a"),
    "456b3ba1-107d-4509-88c2-56a772d73956" -> _root_.scala.collection.immutable.Set(),
    "5d852ce8-f039-48b1-b28a-8520a3bd5e7b" -> _root_.scala.collection.immutable.Set(),
    "c38883dc-19d3-4deb-bc3c-f5a8edadfdf4" -> _root_.scala.collection.immutable.Set("c38883dc-19d3-4deb-bc3c-f5a8edadfdf4", "cc7abd05-b582-4deb-a5f1-27fd4dcd109a"),
    "e090a46b-7db2-4e28-bb7e-153dd1686977" -> _root_.scala.collection.immutable.Set(),
    "2efd2cdb-bbe0-4e5e-81ad-b0794600ecc7" -> _root_.scala.collection.immutable.Set("9f018a51-eeaa-4fe4-b719-4a2698e56abb"),
    "ecdbbd72-54cc-4d15-8a23-86155a800734" -> _root_.scala.collection.immutable.Set("456b3ba1-107d-4509-88c2-56a772d73956"),
    "d6cdd8bd-8caa-4a91-aff0-3eed08d7184b" -> _root_.scala.collection.immutable.Set("ecdbbd72-54cc-4d15-8a23-86155a800734")
  )

  implicit val `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) = `π-trick` -> `π-spell`

  given Conversion[`()`, Long] = _.name.asInstanceOf[Long]

  val max = 11

  val gen = new scala.util.Random

  def random = 3 + gen.nextLong(max - 2)

  def Main()(using ^ : String)(using % : %, / : /, - : -): IO[Unit] = for {
    _ <- IO.unit
    _84ebbe8d_8146_4b35_8e55_7ea8613f4ebe = _root_.scala.collection.immutable.Set("c38883dc-19d3-4deb-bc3c-f5a8edadfdf4", "e090a46b-7db2-4e28-bb7e-153dd1686977")
    _ <- `π-incr`(_84ebbe8d_8146_4b35_8e55_7ea8613f4ebe)
    _ <- for {
      num <- ν
      _   <- (
        `𝟎`,
        for {
          _f7dd350c_1e9c_48b4_96ac_398e2caf70d0 <- IO {
            def _f7dd350c_1e9c_48b4_96ac_398e2caf70d0(n: `()`): String => IO[Unit] = { implicit ^ =>
              for (
                _ <- (
                  for {
                    in <- ν
                    _  <- (
                      `𝟎`,
                      for (_ <- Fib(n, in)(using `π-uuid`)) yield (),
                      for {
                        (fib, _) <- in(null)("cc7abd05-b582-4deb-a5f1-27fd4dcd109a")
                        _        <- τ(∞)("2efd2cdb-bbe0-4e5e-81ad-b0794600ecc7")
                        _        <- IO {
                          println(s"fib($n) = $fib")
                        }
                        _        <- num(null, random)("9f018a51-eeaa-4fe4-b719-4a2698e56abb")
                        _        <- `𝟎`
                      } yield ()
                    ).parMapN { (_, _, _) => }
                  } yield (),
                  for {
                    (n, _) <- num(null)("c38883dc-19d3-4deb-bc3c-f5a8edadfdf4")
                    _      <- _f7dd350c_1e9c_48b4_96ac_398e2caf70d0(n)(`π-uuid`)
                  } yield ()
                ).parMapN { (_, _) => }
              ) yield ()
            }
            _f7dd350c_1e9c_48b4_96ac_398e2caf70d0
          }
          (n, _)                                <- num(null)("c38883dc-19d3-4deb-bc3c-f5a8edadfdf4")
          _                                     <- _f7dd350c_1e9c_48b4_96ac_398e2caf70d0(n)(`π-uuid`)
        } yield (),
        for {
          _ <- num(null, random)("e090a46b-7db2-4e28-bb7e-153dd1686977")
          _ <- `𝟎`
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()

  def Fib(n: `()`, out: `()`)(using ^ : String)(using % : %, / : /, - : -): IO[Unit] = for {
    _ <- IO.unit
    _045834a8_9f5e_42fb_b811_e5abd1cf4fc6 = _root_.scala.collection.immutable.Set("d6cdd8bd-8caa-4a91-aff0-3eed08d7184b")
    _ <- `π-incr`(_045834a8_9f5e_42fb_b811_e5abd1cf4fc6)
    _ <- for {
      _ <- τ(∞)("d6cdd8bd-8caa-4a91-aff0-3eed08d7184b")
      _ <- IO {
        println(s"n=$n")
      }
      f <- ν
      _ <- (
        `𝟎`,
        for (_ <- Fibonacci(f, n)(using `π-uuid`)) yield (),
        for {
          (res, _) <- f(null)("ecdbbd72-54cc-4d15-8a23-86155a800734")
          _        <- out(null, res)("456b3ba1-107d-4509-88c2-56a772d73956")
          _        <- `𝟎`
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()

  def Fibonacci(f: `()`, n: `()`)(using ^ : String)(using % : %, / : /, - : -): IO[Unit] = for (
    _ <-
      if (n < 2 ==== true) for {
        _ <- IO.unit
        _e527c111_4753_4c5e_9716_3dc8246a9f99 = _root_.scala.collection.immutable.Set("3cd4b134-2a1b-4dd5-89e6-6bd666b601bc")
        _ <- `π-incr`(_e527c111_4753_4c5e_9716_3dc8246a9f99)
        _ <- for {
          _ <- f(null, 1L)("3cd4b134-2a1b-4dd5-89e6-6bd666b601bc")
          _ <- `𝟎`
        } yield ()
      } yield ()
      else for {
        _ <- IO.unit
        _a744b470_ce32_4ef9_b153_555cb5d75ce4 = _root_.scala.collection.immutable.Set("931a50ca-17c6-4574-883e-2830f84d8daa")
        _ <- `π-incr`(_a744b470_ce32_4ef9_b153_555cb5d75ce4)
        _ <- for {
          g <- ν
          h <- ν
          _ <- (
            `𝟎`,
            for (_ <- Fibonacci(g, n - 1)(using `π-uuid`)) yield (),
            for (_ <- Fibonacci(h, n - 2)(using `π-uuid`)) yield (),
            for {
              (p, _) <- g(null)("931a50ca-17c6-4574-883e-2830f84d8daa")
              (r, _) <- h(null)("42fbea3e-bd80-4af3-8042-22247249634a")
              _      <- f(null, p + r)("5d852ce8-f039-48b1-b28a-8520a3bd5e7b")
              _      <- `𝟎`
            } yield ()
          ).parMapN { (_, _, _, _) => }
        } yield ()
      } yield ()
  ) yield ()
