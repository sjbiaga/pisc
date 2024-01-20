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

  val `π-trick`: `Π-Map`[String, `Π-Set`[String]] = _root_.scala.collection.immutable.Map()

  val `π-spell`: `Π-Map`[String, `Π-Set`[String]] = _root_.scala.collection.immutable.Map(
    "50794912-afa0-4f46-a10c-655cfe4210fc" -> _root_.scala.collection.immutable.Set("7a738c8e-dcf0-48a3-af9a-82e41cb5f643"),
    "a91bc02e-72a8-455b-988f-a4ec987a326d" -> _root_.scala.collection.immutable.Set(),
    "1b3e6c04-33d0-490d-993f-b317a1413a29" -> _root_.scala.collection.immutable.Set("1a8975b6-4f26-4d92-9f81-7c72c6e51354"),
    "fe83c092-bbca-4c30-a528-a095a8321df7" -> _root_.scala.collection.immutable.Set("a91bc02e-72a8-455b-988f-a4ec987a326d"),
    "9798b640-c4a7-4751-941a-1caf96738b2b" -> _root_.scala.collection.immutable.Set(),
    "405b3ead-8000-49ca-9b30-6531d452a658" -> _root_.scala.collection.immutable.Set(),
    "1a8975b6-4f26-4d92-9f81-7c72c6e51354" -> _root_.scala.collection.immutable.Set("767cf573-145b-4c2f-9bea-6603f3ec7814"),
    "767cf573-145b-4c2f-9bea-6603f3ec7814" -> _root_.scala.collection.immutable.Set(),
    "3558a178-3928-4303-b4aa-1ed27247d59c" -> _root_.scala.collection.immutable.Set("3558a178-3928-4303-b4aa-1ed27247d59c", "1b3e6c04-33d0-490d-993f-b317a1413a29"),
    "a35b0655-844f-4fcd-a430-ff9f111d6fd2" -> _root_.scala.collection.immutable.Set("fe83c092-bbca-4c30-a528-a095a8321df7"),
    "12ad98c5-bd6e-4e51-8fb5-276dacb19081" -> _root_.scala.collection.immutable.Set(),
    "7a738c8e-dcf0-48a3-af9a-82e41cb5f643" -> _root_.scala.collection.immutable.Set("9798b640-c4a7-4751-941a-1caf96738b2b")
  )

  implicit val `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) = `π-trick` -> `π-spell`

  given Conversion[`()`, Long] = _.name.asInstanceOf[Long]

  val max = 15

  val gen = new scala.util.Random

  def random = math.abs(gen.nextLong) % max

  def Main()(using ^ : String)(using % : %, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _38bd9d4a_efa3_4616_a2f0_aa8f251d5c71 = _root_.scala.collection.immutable.Set("3558a178-3928-4303-b4aa-1ed27247d59c", "12ad98c5-bd6e-4e51-8fb5-276dacb19081")
    _ <- `π-incr`(_38bd9d4a_efa3_4616_a2f0_aa8f251d5c71)
    _ <- for {
      num <- ν
      _   <- (
        `𝟎`,
        for {
          _ddf7e626_2702_420b_bb17_3a2d1df30d77 <- IO {
            def _ddf7e626_2702_420b_bb17_3a2d1df30d77(n: `()`): String => IO[Unit] = { implicit ^ =>
              for (
                _ <- (
                  for {
                    in <- ν
                    _  <- (
                      `𝟎`,
                      for (_ <- Fib(n, in)(using `π-uuid`)) yield (),
                      for {
                        (fib, _) <- in(null)("1b3e6c04-33d0-490d-993f-b317a1413a29")
                        _        <- τ(∞)("1a8975b6-4f26-4d92-9f81-7c72c6e51354")
                        _        <- IO {
                          println(s"fib($n) = $fib")
                        }
                        _        <- num(null, random)("767cf573-145b-4c2f-9bea-6603f3ec7814")
                        _        <- `𝟎`
                      } yield ()
                    ).parMapN { (_, _, _) => }
                  } yield (),
                  for {
                    (n, _) <- num(null)("3558a178-3928-4303-b4aa-1ed27247d59c")
                    _      <- _ddf7e626_2702_420b_bb17_3a2d1df30d77(n)(`π-uuid`)
                  } yield ()
                ).parMapN { (_, _) => }
              ) yield ()
            }
            _ddf7e626_2702_420b_bb17_3a2d1df30d77
          }
          (n, _)                                <- num(null)("3558a178-3928-4303-b4aa-1ed27247d59c")
          _                                     <- _ddf7e626_2702_420b_bb17_3a2d1df30d77(n)(`π-uuid`)
        } yield (),
        for {
          _ <- num(null, random)("12ad98c5-bd6e-4e51-8fb5-276dacb19081")
          _ <- `𝟎`
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()

  def Fib(n: `()`, out: `()`)(using ^ : String)(using % : %, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _ab68802d_c733_4190_bfe3_df3f7ce2b25b = _root_.scala.collection.immutable.Set("50794912-afa0-4f46-a10c-655cfe4210fc")
    _ <- `π-incr`(_ab68802d_c733_4190_bfe3_df3f7ce2b25b)
    _ <- for {
      _ <- τ(∞)("50794912-afa0-4f46-a10c-655cfe4210fc")
      _ <- IO {
        println(s"n=$n")
      }
      f <- ν
      _ <- (
        `𝟎`,
        for (_ <- Fibonacci(f, n)(using `π-uuid`)) yield (),
        for {
          (res, _) <- f(null)("7a738c8e-dcf0-48a3-af9a-82e41cb5f643")
          _        <- out(null, res)("9798b640-c4a7-4751-941a-1caf96738b2b")
          _        <- `𝟎`
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()

  def Fibonacci(f: `()`, n: `()`)(using ^ : String)(using % : %, / : /, * : *): IO[Unit] = for (
    _ <-
      if (n < 2 ==== true) for {
        _ <- IO.unit
        _60615efd_11f1_48be_bed7_ca4c992f0b90 = _root_.scala.collection.immutable.Set("405b3ead-8000-49ca-9b30-6531d452a658")
        _ <- `π-incr`(_60615efd_11f1_48be_bed7_ca4c992f0b90)
        _ <- for {
          _ <- f(null, 1L)("405b3ead-8000-49ca-9b30-6531d452a658")
          _ <- `𝟎`
        } yield ()
      } yield ()
      else for {
        _ <- IO.unit
        _e8a8ce79_f5a2_466a_bf08_3bc9f75c47a4 = _root_.scala.collection.immutable.Set("a35b0655-844f-4fcd-a430-ff9f111d6fd2")
        _ <- `π-incr`(_e8a8ce79_f5a2_466a_bf08_3bc9f75c47a4)
        _ <- for {
          g <- ν
          h <- ν
          _ <- (
            `𝟎`,
            for (_ <- Fibonacci(g, n - 1)(using `π-uuid`)) yield (),
            for (_ <- Fibonacci(h, n - 2)(using `π-uuid`)) yield (),
            for {
              (p, _) <- g(null)("a35b0655-844f-4fcd-a430-ff9f111d6fd2")
              (r, _) <- h(null)("fe83c092-bbca-4c30-a528-a095a8321df7")
              _      <- f(null, p + r)("a91bc02e-72a8-455b-988f-a4ec987a326d")
              _      <- `𝟎`
            } yield ()
          ).parMapN { (_, _, _, _) => }
        } yield ()
      } yield ()
  ) yield ()
