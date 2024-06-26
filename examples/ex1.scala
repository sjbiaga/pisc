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
      "a4f52d03-8424-454e-9e78-67e6f7d394d7" -> _root_.scala.collection.immutable
        .Set("4479f3d7-7253-47b5-9368-64275b264b30"),
      "7362be3c-b706-4f0e-9a53-b2ce845764d6" -> _root_.scala.collection.immutable
        .Set("a4f52d03-8424-454e-9e78-67e6f7d394d7"),
      "b6097521-0529-40fe-ba67-f99caef236c8" -> _root_.scala.collection.immutable
        .Set(
          "b6097521-0529-40fe-ba67-f99caef236c8",
          "7362be3c-b706-4f0e-9a53-b2ce845764d6"
        ),
      "bc060912-fa3f-4c75-9b75-e87570146d63" -> _root_.scala.collection.immutable
        .Set(),
      "4479f3d7-7253-47b5-9368-64275b264b30" -> _root_.scala.collection.immutable
        .Set()
    )

  implicit val `π-wand`
    : (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) =
    `π-trick` -> `π-spell`

  def Main(args: String*)(using ^ : String)(using % : %, / : /): IO[Unit] = for {
    _ <- IO.unit
    _293a0c90_2925_4856_9279_52a0766dedfa =
      _root_.scala.collection.immutable.Set(
        "b6097521-0529-40fe-ba67-f99caef236c8",
        "bc060912-fa3f-4c75-9b75-e87570146d63"
      )
    _ <- `π-enable`(_293a0c90_2925_4856_9279_52a0766dedfa)
    x <- ν
    _ <- (
      `π-supervised`(for {
        _b23541bf_a403_4927_9707_84cf34efb2e9 <- IO {
          lazy val _b23541bf_a403_4927_9707_84cf34efb2e9: String => IO[Unit] = {
            implicit ^ =>
              (
                `π-supervised`(for {
                  _      <- τ(⊤(1L))("7362be3c-b706-4f0e-9a53-b2ce845764d6")
                  _      <- IO {
                    println("out 5")
                  }
                  (z, _) <- x(∞(1L))("a4f52d03-8424-454e-9e78-67e6f7d394d7")
                  _      <- τ(⊤(1L))("4479f3d7-7253-47b5-9368-64275b264b30")
                  _      <- IO {
                    println(s"in ${z}")
                  }
                } yield ()),
                `π-supervised`(for {
                  _a759501d_2bf1_4c5b_b9a4_89a696eca092 <-
                    x(∞(1L), 5)("b6097521-0529-40fe-ba67-f99caef236c8")
                  _                                     <-
                    if (_a759501d_2bf1_4c5b_b9a4_89a696eca092 == null) IO.cede
                    else _b23541bf_a403_4927_9707_84cf34efb2e9(`π-uuid`)
                } yield ())
              ).parMapN { (_, _) => }
          }
          _b23541bf_a403_4927_9707_84cf34efb2e9
        }
        _a759501d_2bf1_4c5b_b9a4_89a696eca092 <-
          x(∞(1L), 5)("b6097521-0529-40fe-ba67-f99caef236c8")
        _                                     <-
          if (_a759501d_2bf1_4c5b_b9a4_89a696eca092 == null) IO.cede
          else _b23541bf_a403_4927_9707_84cf34efb2e9(`π-uuid`)
      } yield ()),
      `π-supervised`(
        for ((z, _) <- x(∞(1L))("bc060912-fa3f-4c75-9b75-e87570146d63"))
          yield ()
      )
    ).parMapN { (_, _) => }
  } yield ()
