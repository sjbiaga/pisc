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
    "bcc5a5e5-03f6-4197-bf4d-933d909e8f5b" -> _root_.scala.collection.immutable.Set(),
    "51fa5e1d-5f3e-4f12-aee1-592fc6244b04" -> _root_.scala.collection.immutable.Set()
  )

  implicit val `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) = `π-trick` -> `π-spell`

  def Main()(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _9855aa2a_3f03_4f09_8e87_5ada85dd56c1 = _root_.scala.collection.immutable.Set("5c1425fa-85da-4369-a537-1237ddceb056", "bcc5a5e5-03f6-4197-bf4d-933d909e8f5b")
    _ <- `π-none`(_9855aa2a_3f03_4f09_8e87_5ada85dd56c1)
    _ <- for {
      x <- ν
      _ <- (
        `𝟎`,
        for {
          _b10f25e8_8570_46d1_9cfe_39ed8cb7402c <- IO {
            lazy val _b10f25e8_8570_46d1_9cfe_39ed8cb7402c: String => IO[Unit] = { implicit ^ =>
              for {
                _ <- IO.unit
                _c468940c_c8ac_432c_bbff_4096927209db = _root_.scala.collection.immutable.Set("5c1425fa-85da-4369-a537-1237ddceb056", "51fa5e1d-5f3e-4f12-aee1-592fc6244b04")
                _ <- `π-none`(_c468940c_c8ac_432c_bbff_4096927209db)
                _ <- (
                  for ((z, _) <- x(∞)("51fa5e1d-5f3e-4f12-aee1-592fc6244b04")) yield (),
                  for {
                    _ <- x(∞, 5)("5c1425fa-85da-4369-a537-1237ddceb056")
                    _ <- _b10f25e8_8570_46d1_9cfe_39ed8cb7402c(`π-uuid`)
                  } yield ()
                ).parMapN { (_, _) => }
              } yield ()
            }
            _b10f25e8_8570_46d1_9cfe_39ed8cb7402c
          }
          _                                     <- x(∞, 5)("5c1425fa-85da-4369-a537-1237ddceb056")
          _                                     <- _b10f25e8_8570_46d1_9cfe_39ed8cb7402c(`π-uuid`)
        } yield (),
        for ((z, _) <- x(null)("bcc5a5e5-03f6-4197-bf4d-933d909e8f5b")) yield ()
      ).parMapN { (_, _, _) => }
    } yield ()
  } yield ()
