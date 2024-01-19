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
    "d25e7cda-4b24-49f5-97fb-b56246b58b95" -> _root_.scala.collection.immutable.Set(),
    "115ee795-b65b-4c08-8f5a-c6f06965ef7c" -> _root_.scala.collection.immutable.Set("115ee795-b65b-4c08-8f5a-c6f06965ef7c", "10246973-bf15-473d-80d8-3d34bac62ea1"),
    "95f4ad0e-b1a7-496f-aa92-d0782e4550f8" -> _root_.scala.collection.immutable.Set("7dac2404-c1bd-4553-9103-ec0aad100b81"),
    "7dac2404-c1bd-4553-9103-ec0aad100b81" -> _root_.scala.collection.immutable.Set(),
    "10246973-bf15-473d-80d8-3d34bac62ea1" -> _root_.scala.collection.immutable.Set("95f4ad0e-b1a7-496f-aa92-d0782e4550f8")
  )

  def Main()(using ^ : String)(using % : %, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _3e01c680_eddf_44ca_8f25_0db8a29dd68d = _root_.scala.collection.immutable.Set("115ee795-b65b-4c08-8f5a-c6f06965ef7c", "10246973-bf15-473d-80d8-3d34bac62ea1", "d25e7cda-4b24-49f5-97fb-b56246b58b95")
    _ <- `π-incr`(_3e01c680_eddf_44ca_8f25_0db8a29dd68d)
    _ <- for {
      x <- ν
      _ <- (
        `𝟎`,
        for {
          _82d67361_2ec7_49f2_a85d_d853c8a36070 <- IO {
            lazy val _82d67361_2ec7_49f2_a85d_d853c8a36070: String => IO[Unit] = { implicit ^ =>
              for (
                _ <- (
                  for {
                    _      <- τ(∞)("10246973-bf15-473d-80d8-3d34bac62ea1")
                    _      <- IO {
                      println("out 5")
                    }
                    (z, _) <- x(∞)("95f4ad0e-b1a7-496f-aa92-d0782e4550f8")
                    _      <- τ(∞)("7dac2404-c1bd-4553-9103-ec0aad100b81")
                    _      <- IO {
                      println(s"in $z")
                    }
                    _      <- `𝟎`
                  } yield (),
                  for {
                    _ <- x(∞, 5)("115ee795-b65b-4c08-8f5a-c6f06965ef7c")
                    _ <- _82d67361_2ec7_49f2_a85d_d853c8a36070(`π-uuid`)
                  } yield ()
                ).parMapN { (_, _) => }
              ) yield ()
            }
            _82d67361_2ec7_49f2_a85d_d853c8a36070
          }
          _                                     <- x(∞, 5)("115ee795-b65b-4c08-8f5a-c6f06965ef7c")
          _                                     <- _82d67361_2ec7_49f2_a85d_d853c8a36070(`π-uuid`)
        } yield (),
        for {
          (z, _) <- x(null)("d25e7cda-4b24-49f5-97fb-b56246b58b95")
          _      <- `𝟎`
        } yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()
