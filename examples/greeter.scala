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
    _root_.scala.collection.immutable.Map(
      "4c296d62-7c0d-4dc3-a1bc-a3a40fc8d030" -> _root_.scala.collection.immutable
        .Set(
          "ffda9242-570d-4387-9ee0-0de32c41ba3f",
          "1db5dbd3-ec90-4286-b888-4e484322c2ee"
        ),
      "1db5dbd3-ec90-4286-b888-4e484322c2ee" -> _root_.scala.collection.immutable
        .Set(
          "4c296d62-7c0d-4dc3-a1bc-a3a40fc8d030",
          "ffda9242-570d-4387-9ee0-0de32c41ba3f"
        ),
      "ffda9242-570d-4387-9ee0-0de32c41ba3f" -> _root_.scala.collection.immutable
        .Set(
          "4c296d62-7c0d-4dc3-a1bc-a3a40fc8d030",
          "1db5dbd3-ec90-4286-b888-4e484322c2ee"
        )
    )

  val `π-spell`: `Π-Map`[String, `Π-Set`[String]] =
    _root_.scala.collection.immutable.Map(
      "e3fc90a3-2b94-401a-abbc-3f9a4fb57f4a" -> _root_.scala.collection.immutable
        .Set("39d18550-0813-4e5c-88f0-90722f55762b"),
      "59de1e3c-6cf3-4263-90f9-972b7c335552" -> _root_.scala.collection.immutable
        .Set(),
      "dc550dd8-2261-4f02-ad15-27816c23c6c5" -> _root_.scala.collection.immutable
        .Set("e7a1bfbf-4eb1-40f0-9168-47c6d4a37eac"),
      "fc8f234a-0b3d-4c63-898d-01541c535378" -> _root_.scala.collection.immutable
        .Set(),
      "4f672993-5ed0-481b-8efb-e0f4cc12420e" -> _root_.scala.collection.immutable
        .Set(),
      "6f3c0c6c-a876-412f-8295-9d71b79c84a7" -> _root_.scala.collection.immutable
        .Set(),
      "846638fe-f88f-4979-abe2-1d10a7212cb6" -> _root_.scala.collection.immutable
        .Set(),
      "62db4165-2328-43c2-b583-23349e943bb8" -> _root_.scala.collection.immutable
        .Set(),
      "dda70efb-0f1a-4e1a-ba02-121b5346be12" -> _root_.scala.collection.immutable
        .Set(),
      "fbb0a22d-ab4b-4a0a-bba9-eb2b3f6c2d73" -> _root_.scala.collection.immutable
        .Set(),
      "39d18550-0813-4e5c-88f0-90722f55762b" -> _root_.scala.collection.immutable
        .Set("90dcfa3a-a90e-4eae-bfae-395a87a6a8cd"),
      "26a83ffe-351e-414d-83e5-a2f24964ec3b" -> _root_.scala.collection.immutable
        .Set(),
      "8b6391ce-aa55-4c44-8731-db7cca859009" -> _root_.scala.collection.immutable
        .Set("e3fc90a3-2b94-401a-abbc-3f9a4fb57f4a"),
      "996a3067-e063-49ad-8e89-a8bb671e636a" -> _root_.scala.collection.immutable
        .Set()
    )

  implicit val `π-wand`
    : (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) =
    `π-trick` -> `π-spell`

  given Conversion[`()`, String] = _.as[String]

  val nl: String => IO[String] = { it => IO { s"$it\n" } }

  val gen = new scala.util.Random

  def r = 1 + math.abs(gen.nextInt % 3)

  def Main(args: String*)(using ^ : String)(using % : %, / : /): IO[Unit] = for {
    stdin  <- ν
    stdout <- ν
    _      <- Greeter(stdin, stdout, "")(using `π-uuid`)
  } yield ()

  def Greeter(
    stdin: `()`,
    stdout: `()`,
    line: `()`
  )(implicit
    ^ : String,
    % : %,
    / : /
  ): IO[Unit] = for {
    _ <- IO.unit
    _b15784ee_3a5a_4bde_929b_e32351b9489d =
      _root_.scala.collection.immutable.Set(
        "dc550dd8-2261-4f02-ad15-27816c23c6c5",
        "8b6391ce-aa55-4c44-8731-db7cca859009"
      )
    _ <- `π-enable`(_b15784ee_3a5a_4bde_929b_e32351b9489d)
    _ <- (
      IO.unit,
      `π-supervised`(for {
        _         <- stdout(⊤(1L), "What's your name?")(
          "dc550dd8-2261-4f02-ad15-27816c23c6c5"
        )
        (name, _) <- stdin(⊤(1L))("e7a1bfbf-4eb1-40f0-9168-47c6d4a37eac")
        _         <-
          if (name.isBlank ==== true) for {
            _ <- IO.unit
            _725b483c_7fa0_4920_93a3_791d14167d49 =
              _root_.scala.collection.immutable
                .Set("bfb93a64-ef72-438a-889d-a3132878f57f")
            _ <- `π-enable`(_725b483c_7fa0_4920_93a3_791d14167d49)
            _ <-
              stdout(⊤(1L), `()`(null))("bfb93a64-ef72-438a-889d-a3132878f57f")
            _ <- Greeter(stdin, stdout, line)(using `π-uuid`)
          } yield ()
          else Chooser(stdout, name)(using `π-uuid`)
      } yield ()),
      `π-supervised`(for {
        (prompt, _) <- stdout(⊤(1L))("8b6391ce-aa55-4c44-8731-db7cca859009")(nl)
        _           <- τ(⊤(1L))("e3fc90a3-2b94-401a-abbc-3f9a4fb57f4a")
        _           <- IO {
          print(prompt)
        }
        line        <- IO.blocking {
          scala.io.StdIn.readLine
        }
        _          <- stdin(⊤(1L), line)("39d18550-0813-4e5c-88f0-90722f55762b")
        (greet, _) <- stdout(⊤(1L))("90dcfa3a-a90e-4eae-bfae-395a87a6a8cd")(nl)
        _          <-
          if (!greet ==== false) for {
            _ <- IO.unit
            _7ed54f27_6026_4469_b824_da70df183868 =
              _root_.scala.collection.immutable
                .Set("4f672993-5ed0-481b-8efb-e0f4cc12420e")
            _ <- `π-enable`(_7ed54f27_6026_4469_b824_da70df183868)
            _ <- τ(⊤(1L))("4f672993-5ed0-481b-8efb-e0f4cc12420e")
            _ <- IO {
              print(greet)
            }
          } yield ()
          else IO.unit
      } yield ())
    ).parMapN { (_, _, _) => }
  } yield ()

  def Chooser(stdout: `()`, name: `()`)(using ^ : String)(using % : %, / : /)
    : IO[Unit] = for {
    _ <- IO.unit
    _b44286f0_c78c_4682_a27e_d5d3dcef760b =
      _root_.scala.collection.immutable.Set(
        "4c296d62-7c0d-4dc3-a1bc-a3a40fc8d030",
        "ffda9242-570d-4387-9ee0-0de32c41ba3f",
        "1db5dbd3-ec90-4286-b888-4e484322c2ee"
      )
    _ <- `π-enable`(_b44286f0_c78c_4682_a27e_d5d3dcef760b)
    _ <- (
      `π-supervised`(for {
        _ <- τ(`ℝ⁺`(r))("4c296d62-7c0d-4dc3-a1bc-a3a40fc8d030")
        _ <- `Greeter'`(stdout, name)(using `π-uuid`)
      } yield ()),
      `π-supervised`(for {
        _ <- τ(`ℝ⁺`(r))("ffda9242-570d-4387-9ee0-0de32c41ba3f")
        _ <- `Greeter"`(stdout, name)(using `π-uuid`)
      } yield ()),
      `π-supervised`(for {
        _ <- τ(`ℝ⁺`(r))("1db5dbd3-ec90-4286-b888-4e484322c2ee")
        _ <- `Greeter"'`(stdout, name)(using `π-uuid`)
      } yield ())
    ).parMapN { (_, _, _) => }
  } yield ()

  def `Greeter'`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, / : /)
    : IO[Unit] = (
    IO.unit,
    `π-supervised`(
      if (name.substring(0, 1).toUpperCase ==== "Q") for {
        _ <- IO.unit
        _398d393b_ce53_4f9e_96ba_27fa7db0bde1 =
          _root_.scala.collection.immutable
            .Set("fc8f234a-0b3d-4c63-898d-01541c535378")
        _ <- `π-enable`(_398d393b_ce53_4f9e_96ba_27fa7db0bde1)
        _ <- stdout(⊤(1L), "That's an unusual name.")(
          "fc8f234a-0b3d-4c63-898d-01541c535378"
        )
      } yield ()
      else IO.unit
    ),
    `π-supervised`(
      if (name ==== "Voldemort") for {
        _ <- IO.unit
        _7e35c2e3_d5c0_4f21_a6a3_b7a2163cab32 =
          _root_.scala.collection.immutable
            .Set("dda70efb-0f1a-4e1a-ba02-121b5346be12")
        _ <- `π-enable`(_7e35c2e3_d5c0_4f21_a6a3_b7a2163cab32)
        _ <- stdout(⊤(1L), "WARNING! LORD VOLDEMORT IS HERE!")(
          "dda70efb-0f1a-4e1a-ba02-121b5346be12"
        )
      } yield ()
      else IO.unit
    ),
    `π-supervised`(
      if (name.substring(0, 1).toUpperCase ==== "Q") IO.unit
      else if (name ==== "Voldemort") IO.unit
      else for {
        _ <- IO.unit
        _124779e9_2b86_4071_a8c2_4ee0c721c60a =
          _root_.scala.collection.immutable
            .Set("62db4165-2328-43c2-b583-23349e943bb8")
        _ <- `π-enable`(_124779e9_2b86_4071_a8c2_4ee0c721c60a)
        _ <-
          stdout(⊤(1L), s"Hello $name!")("62db4165-2328-43c2-b583-23349e943bb8")
      } yield ()
    )
  ).parMapN { (_, _, _, _) => }

  def `Greeter"`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, / : /)
    : IO[Unit] =
    if (name.substring(0, 1).toUpperCase ==== "Q") for {
      _ <- IO.unit
      _ab8cc6b8_7113_41c7_acca_90fb608e9cf2 = _root_.scala.collection.immutable
        .Set("59de1e3c-6cf3-4263-90f9-972b7c335552")
      _ <- `π-enable`(_ab8cc6b8_7113_41c7_acca_90fb608e9cf2)
      _ <- stdout(⊤(1L), "That's an unusual name.")(
        "59de1e3c-6cf3-4263-90f9-972b7c335552"
      )
    } yield ()
    else if (name ==== "Voldemort") for {
      _ <- IO.unit
      _a7aabc64_ae73_4976_8370_bae4363c739c = _root_.scala.collection.immutable
        .Set("26a83ffe-351e-414d-83e5-a2f24964ec3b")
      _ <- `π-enable`(_a7aabc64_ae73_4976_8370_bae4363c739c)
      _ <- stdout(⊤(1L), "WARNING! LORD VOLDEMORT IS HERE!")(
        "26a83ffe-351e-414d-83e5-a2f24964ec3b"
      )
    } yield ()
    else for {
      _ <- IO.unit
      _fbe8e086_4b5e_4481_ba14_b88167f281f4 = _root_.scala.collection.immutable
        .Set("6f3c0c6c-a876-412f-8295-9d71b79c84a7")
      _ <- `π-enable`(_fbe8e086_4b5e_4481_ba14_b88167f281f4)
      _ <-
        stdout(⊤(1L), s"Hello $name!")("6f3c0c6c-a876-412f-8295-9d71b79c84a7")
    } yield ()

  def `Greeter"'`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, / : /)
    : IO[Unit] =
    if (name.substring(0, 1).toUpperCase ==== "Q") for {
      _ <- IO.unit
      _1e475eb2_3b72_44df_96f8_7a82afb829b2 = _root_.scala.collection.immutable
        .Set("fbb0a22d-ab4b-4a0a-bba9-eb2b3f6c2d73")
      _ <- `π-enable`(_1e475eb2_3b72_44df_96f8_7a82afb829b2)
      _ <- stdout(⊤(1L), "That's an unusual name.")(
        "fbb0a22d-ab4b-4a0a-bba9-eb2b3f6c2d73"
      )
    } yield ()
    else if (name ==== "Voldemort") for {
      _ <- IO.unit
      _160c5030_a5b2_4a04_8ada_ac48f424ca74 = _root_.scala.collection.immutable
        .Set("996a3067-e063-49ad-8e89-a8bb671e636a")
      _ <- `π-enable`(_160c5030_a5b2_4a04_8ada_ac48f424ca74)
      _ <- stdout(⊤(1L), "WARNING! LORD VOLDEMORT IS HERE!")(
        "996a3067-e063-49ad-8e89-a8bb671e636a"
      )
    } yield ()
    else for {
      _ <- IO.unit
      _62ed4d5d_dbc4_4d32_9258_d1939f74fc6f = _root_.scala.collection.immutable
        .Set("846638fe-f88f-4979-abe2-1d10a7212cb6")
      _ <- `π-enable`(_62ed4d5d_dbc4_4d32_9258_d1939f74fc6f)
      _ <-
        stdout(⊤(1L), s"Hello $name!")("846638fe-f88f-4979-abe2-1d10a7212cb6")
    } yield ()
