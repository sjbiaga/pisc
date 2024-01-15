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
import _root_.cats.effect.std.{CyclicBarrier, Queue, Semaphore}

import `Π-loop`._
import `Π-stats`.{Rate, ∞, `@`}

object App extends IOApp.Simple:

  private def run(% : %, \ : \, / : /, * : (*, *), + : +, - : -): IO[Unit] = (for
    _ <- loop(using %, \, *, +, -).background
    _ <- poll(using %, /, *._1).background
  yield ()).use { _ =>
    for _ <- π.Main()(using "")(using %, \, /, *._2, +, -)
    yield ()
  }

  override def run: IO[Unit] =
    for
      %    <- IO.ref(Map[String, Option[Rate]]())
      \    <- IO.ref(Set[String]())
      /    <- Queue.unbounded[IO, (String, Rate)]
      *    <- Semaphore[IO](1)
      ^    <- Semaphore[IO](1)
      _    <- ^.acquire
      +    <- CyclicBarrier[IO](2)
      turn <- Deferred[IO, (String, BigDecimal)]
      -    <- IO.ref(turn)
      _    <- run(%, \, /, (*, ^), +, -)
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
    "1a20773a-d972-4ae3-915f-860358f81fdd" -> _root_.scala.collection.immutable.Set("ca2e55f2-7a1b-4d75-92d2-b55fe1797edc"),
    "a41660f6-9e43-4957-ac25-517585af43bf" -> _root_.scala.collection.immutable.Set(),
    "9877deda-c488-432e-a0be-8f6e9725885f" -> _root_.scala.collection.immutable.Set(),
    "0d31402a-8811-44fd-82ee-83336831645c" -> _root_.scala.collection.immutable.Set("a41660f6-9e43-4957-ac25-517585af43bf"),
    "ca2e55f2-7a1b-4d75-92d2-b55fe1797edc" -> _root_.scala.collection.immutable.Set("9877deda-c488-432e-a0be-8f6e9725885f")
  )

  implicit val `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) = `π-trick` -> `π-spell`

  given Conversion[`()`, Long] = _.name.asInstanceOf[Long]

  val max = 15

  val gen = new scala.util.Random

  def random = math.abs(gen.nextLong) % max

  def Main()(using ^ : String)(using % : %, \ : \, / : /, * : *, + : +, - : -): IO[Unit] = for {
    _ <- IO.unit
    _f624ef57_e138_4ca1_84d8_0eb05f94683b = _root_.scala.collection.immutable.Set("78254596-a1f6-41e9-ba1b-89fbe4d35555", "0d31402a-8811-44fd-82ee-83336831645c")
    _ <- %.update(_f624ef57_e138_4ca1_84d8_0eb05f94683b.foldLeft(_)(_ + _))
    _ <- for {
      num1 <- ν
      num2 <- ν
      _    <- (
        `𝟎`,
        for {
          _ <- IO.unit
          _27a7f462_8ee0_4806_834c_a6fad8826ca4 = _root_.scala.collection.immutable.Set("78254596-a1f6-41e9-ba1b-89fbe4d35555")
          _                                     <- %.update(_27a7f462_8ee0_4806_834c_a6fad8826ca4.foldLeft(_)(_ + _))
          _acac37eb_7f9f_47df_8a0d_14c10df9e6d9 <- IO {
            lazy val _acac37eb_7f9f_47df_8a0d_14c10df9e6d9: String => IO[Unit] = { implicit ^ =>
              for {
                _ <- IO.unit
                _95585226_61aa_486f_b40a_76d6cf2956ae = _root_.scala.collection.immutable.Set("78254596-a1f6-41e9-ba1b-89fbe4d35555", "47b03625-81c4-4fe0-9236-2b9f9a2348b4")
                _ <- %.update(_95585226_61aa_486f_b40a_76d6cf2956ae.foldLeft(_)(_ + _))
                _ <- (
                  for {
                    (n, _) <- num2(null)("47b03625-81c4-4fe0-9236-2b9f9a2348b4")
                    in     <- ν
                    _      <- (
                      `𝟎`,
                      for (_ <- Fib(n, in)(using `π-uuid`)) yield (),
                      for {
                        (fib, _) <- in(null)("2689ade2-bd9e-4289-b85e-47c53e7ca723")
                        _        <- τ(∞)("be2d46d2-e584-475d-bd31-f449ab3ef2f8")
                        _        <- IO {
                          println(s"fib($n) = $fib")
                        }
                        (n, _)   <- num1(null)("efe30c4f-4a0b-428b-8486-db5afc9fa952")
                        _        <- num2(null, n)("8557360f-25d6-4609-9ff0-b02b2428dc8c")
                      } yield ()
                    ).parMapN { (_, _, _) => }
                  } yield (),
                  for {
                    _ <- num1(null, random)("78254596-a1f6-41e9-ba1b-89fbe4d35555")
                    _ <- _acac37eb_7f9f_47df_8a0d_14c10df9e6d9(`π-uuid`)
                  } yield ()
                ).parMapN { (_, _) => }
              } yield ()
            }
            _acac37eb_7f9f_47df_8a0d_14c10df9e6d9
          }
          _                                     <- num1(null, random)("78254596-a1f6-41e9-ba1b-89fbe4d35555")
          _                                     <- _acac37eb_7f9f_47df_8a0d_14c10df9e6d9(`π-uuid`)
        } yield (),
        for {
          (n, _) <- num1(null)("0d31402a-8811-44fd-82ee-83336831645c")
          _      <- num2(null, n)("a41660f6-9e43-4957-ac25-517585af43bf")
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()

  def Fib(n: `()`, out: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *, + : +, - : -): IO[Unit] = for {
    _ <- IO.unit
    _4dfc8130_4835_4588_931c_e2abf8e8a19f = _root_.scala.collection.immutable.Set("1a20773a-d972-4ae3-915f-860358f81fdd")
    _ <- %.update(_4dfc8130_4835_4588_931c_e2abf8e8a19f.foldLeft(_)(_ + _))
    _ <- for {
      _ <- τ(∞)("1a20773a-d972-4ae3-915f-860358f81fdd")
      _ <- IO {
        println(s"n=$n")
      }
      f <- ν
      _ <- (
        `𝟎`,
        for (_ <- Fibonacci(f, n)(using `π-uuid`)) yield (),
        for {
          (res, _) <- f(null)("ca2e55f2-7a1b-4d75-92d2-b55fe1797edc")
          _        <- out(null, res)("9877deda-c488-432e-a0be-8f6e9725885f")
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()

  def Fibonacci(f: `()`, n: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *, + : +, - : -): IO[Unit] = for (
    _ <-
      if (n < 2 === true) for {
        _ <- IO.unit
        _8ad64a46_e8e3_40d6_bf89_e5b36a7d11de = _root_.scala.collection.immutable.Set("128d2288-6a2a-428e-8cc9-364879d19180")
        _ <- %.update(_8ad64a46_e8e3_40d6_bf89_e5b36a7d11de.foldLeft(_)(_ + _))
        _ <- for (_ <- f(null, 1L)("128d2288-6a2a-428e-8cc9-364879d19180")) yield ()
      } yield ()
      else for {
        _ <- IO.unit
        _77443c85_3dfd_4746_bc51_3bcfeb470d9b = _root_.scala.collection.immutable.Set("1c07f3fb-f3f3-410f-9d4f-99c780177efb")
        _ <- %.update(_77443c85_3dfd_4746_bc51_3bcfeb470d9b.foldLeft(_)(_ + _))
        _ <- for {
          g <- ν
          h <- ν
          _ <- (
            `𝟎`,
            for (_ <- Fibonacci(g, n - 1)(using `π-uuid`)) yield (),
            for (_ <- Fibonacci(h, n - 2)(using `π-uuid`)) yield (),
            for {
              (p, _) <- g(null)("1c07f3fb-f3f3-410f-9d4f-99c780177efb")
              (r, _) <- h(null)("1681eff0-a356-461f-ac62-70953cb90440")
              _      <- f(null, p + r)("6e3519d3-d1e2-433a-bbb4-89facb7a15a0")
            } yield ()
          ).parMapN { (_, _, _, _) => }
        } yield ()
      } yield ()
  ) yield ()
