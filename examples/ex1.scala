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
    "9e7fbd55-32fe-47ea-92a1-b2d3e9b91ad0" -> _root_.scala.collection.immutable.Set(),
    "288f8da5-8b04-45dc-82a9-809c7eadacea" -> _root_.scala.collection.immutable.Set("9e7fbd55-32fe-47ea-92a1-b2d3e9b91ad0"),
    "35bde5f4-9d29-4d46-93eb-7b6cfbfca846" -> _root_.scala.collection.immutable.Set("35bde5f4-9d29-4d46-93eb-7b6cfbfca846", "54aed8ad-9193-490f-af89-3b62ed893c05"),
    "93f95623-9c98-4807-8536-42b4ff36b5c5" -> _root_.scala.collection.immutable.Set(),
    "54aed8ad-9193-490f-af89-3b62ed893c05" -> _root_.scala.collection.immutable.Set("288f8da5-8b04-45dc-82a9-809c7eadacea")
  )

  implicit val `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) = `π-trick` -> `π-spell`

  def Main()(using ^ : String)(using % : %, / : /, - : -): IO[Unit] = for {
    _ <- IO.unit
    _c8f813e9_a47f_43b9_950e_a6e4d38d197f = _root_.scala.collection.immutable.Set("35bde5f4-9d29-4d46-93eb-7b6cfbfca846", "93f95623-9c98-4807-8536-42b4ff36b5c5")
    _ <- `π-incr`(_c8f813e9_a47f_43b9_950e_a6e4d38d197f)
    _ <- for {
      x <- ν
      _ <- (
        `𝟎`,
        for {
          _9f59cfc2_3b32_48df_9487_81b8b9786e1f <- IO {
            lazy val _9f59cfc2_3b32_48df_9487_81b8b9786e1f: String => IO[Unit] = { implicit ^ =>
              for (
                _ <- (
                  for {
                    _      <- τ(∞)("54aed8ad-9193-490f-af89-3b62ed893c05")
                    _      <- IO {
                      println("out 5")
                    }
                    (z, _) <- x(∞)("288f8da5-8b04-45dc-82a9-809c7eadacea")
                    _      <- τ(∞)("9e7fbd55-32fe-47ea-92a1-b2d3e9b91ad0")
                    _      <- IO {
                      println(s"in $z")
                    }
                    _      <- `𝟎`
                  } yield (),
                  for {
                    _ <- x(∞, 5)("35bde5f4-9d29-4d46-93eb-7b6cfbfca846")
                    _ <- _9f59cfc2_3b32_48df_9487_81b8b9786e1f(`π-uuid`)
                  } yield ()
                ).parMapN { (_, _) => }
              ) yield ()
            }
            _9f59cfc2_3b32_48df_9487_81b8b9786e1f
          }
          _                                     <- x(∞, 5)("35bde5f4-9d29-4d46-93eb-7b6cfbfca846")
          _                                     <- _9f59cfc2_3b32_48df_9487_81b8b9786e1f(`π-uuid`)
        } yield (),
        for {
          (z, _) <- x(null)("93f95623-9c98-4807-8536-42b4ff36b5c5")
          _      <- `𝟎`
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()
