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
      "135a246d-2e00-41d6-898b-c31acba63ce5" -> _root_.scala.collection.immutable
        .Set(
          "e2ab6dff-9ff2-4db4-8cee-3475374cc398",
          "1567bcbf-bd10-460d-b0fe-626fbe6140e2"
        ),
      "e2ab6dff-9ff2-4db4-8cee-3475374cc398" -> _root_.scala.collection.immutable
        .Set(
          "1567bcbf-bd10-460d-b0fe-626fbe6140e2",
          "135a246d-2e00-41d6-898b-c31acba63ce5"
        ),
      "1567bcbf-bd10-460d-b0fe-626fbe6140e2" -> _root_.scala.collection.immutable
        .Set(
          "e2ab6dff-9ff2-4db4-8cee-3475374cc398",
          "135a246d-2e00-41d6-898b-c31acba63ce5"
        )
    )

  val `π-spell`: `Π-Map`[String, `Π-Set`[String]] =
    _root_.scala.collection.immutable.Map(
      "7e46afb9-a2eb-45a1-bba0-d69d72ff815a" -> _root_.scala.collection.immutable
        .Set(),
      "38043904-7c0d-4cb6-bcee-4c2c3c224a01" -> _root_.scala.collection.immutable
        .Set(),
      "6dc1dd1b-0a97-48d9-aa4c-1c00c8d68985" -> _root_.scala.collection.immutable
        .Set(),
      "e50e1849-e91f-4e69-9eb9-5137c366f3dd" -> _root_.scala.collection.immutable
        .Set(),
      "212a56fe-f3e6-456b-947c-837026f354c8" -> _root_.scala.collection.immutable
        .Set(),
      "35a09dd0-8813-4d35-92af-976ca68be41b" -> _root_.scala.collection.immutable
        .Set("09c74e4d-fd00-48ca-99de-3ed8bab4dd7d"),
      "1dbeb274-01c8-45c1-b7e3-8af706663a38" -> _root_.scala.collection.immutable
        .Set(),
      "55f56208-9727-4809-be51-7178a5e835f8" -> _root_.scala.collection.immutable
        .Set(),
      "4370b94e-742e-4771-a1ca-ebc93f8f0279" -> _root_.scala.collection.immutable
        .Set(),
      "a4f45fa3-5140-4989-8443-b13b12c54a68" -> _root_.scala.collection.immutable
        .Set(),
      "cb7def5d-35c3-48a1-937e-147b6fa1d39a" -> _root_.scala.collection.immutable
        .Set("35a09dd0-8813-4d35-92af-976ca68be41b"),
      "09c74e4d-fd00-48ca-99de-3ed8bab4dd7d" -> _root_.scala.collection.immutable
        .Set("9d4130c3-b1ad-45a4-a0ba-59275feda15e"),
      "dfe62023-76b7-462d-bad0-cc40553a071e" -> _root_.scala.collection.immutable
        .Set("23110d44-02fd-47d4-8f33-1c2fcc156ebf"),
      "29274e3a-b1f1-48d7-8cae-3ebf9dba2251" -> _root_.scala.collection.immutable
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
    _5507f7e9_1631_4dbb_b5ad_6e2dfdd81574 =
      _root_.scala.collection.immutable.Set(
        "dfe62023-76b7-462d-bad0-cc40553a071e",
        "cb7def5d-35c3-48a1-937e-147b6fa1d39a"
      )
    _ <- `π-enable`(_5507f7e9_1631_4dbb_b5ad_6e2dfdd81574)
    _ <- (
      `π-supervised`(for {
        _         <- stdout(⊤(1L), "What's your name?")(
          "dfe62023-76b7-462d-bad0-cc40553a071e"
        )
        (name, _) <- stdin(⊤(1L))("23110d44-02fd-47d4-8f33-1c2fcc156ebf")
        _         <-
          if (name.isBlank ==== true) for {
            _ <- IO.unit
            _6019c011_5d5c_4765_b165_dd41573fe30e =
              _root_.scala.collection.immutable
                .Set("953f934a-279a-4ca5-8406-423217d07026")
            _ <- `π-enable`(_6019c011_5d5c_4765_b165_dd41573fe30e)
            _ <-
              stdout(⊤(1L), `()`(null))("953f934a-279a-4ca5-8406-423217d07026")
            _ <- Greeter(stdin, stdout, line)(using `π-uuid`)
          } yield ()
          else Chooser(stdout, name)(using `π-uuid`)
      } yield ()),
      `π-supervised`(for {
        (prompt, _) <- stdout(⊤(1L))("cb7def5d-35c3-48a1-937e-147b6fa1d39a")(nl)
        _           <- τ(⊤(1L))("35a09dd0-8813-4d35-92af-976ca68be41b")
        _           <- IO {
          print(prompt)
        }
        line        <- IO.blocking {
          scala.io.StdIn.readLine
        }
        _          <- stdin(⊤(1L), line)("09c74e4d-fd00-48ca-99de-3ed8bab4dd7d")
        (greet, _) <- stdout(⊤(1L))("9d4130c3-b1ad-45a4-a0ba-59275feda15e")(nl)
        _          <-
          if (!greet ==== false) for {
            _ <- IO.unit
            _278cbad7_c4e6_44d5_95bf_a3046178aa3c =
              _root_.scala.collection.immutable
                .Set("212a56fe-f3e6-456b-947c-837026f354c8")
            _ <- `π-enable`(_278cbad7_c4e6_44d5_95bf_a3046178aa3c)
            _ <- τ(⊤(1L))("212a56fe-f3e6-456b-947c-837026f354c8")
            _ <- IO {
              print(greet)
            }
          } yield ()
          else IO.unit
      } yield ())
    ).parMapN { (_, _) => }
  } yield ()

  def Chooser(stdout: `()`, name: `()`)(using ^ : String)(using % : %, / : /)
    : IO[Unit] = for {
    _ <- IO.unit
    _33dc4677_dc7f_40db_8f4e_276130f6d61c =
      _root_.scala.collection.immutable.Set(
        "e2ab6dff-9ff2-4db4-8cee-3475374cc398",
        "1567bcbf-bd10-460d-b0fe-626fbe6140e2",
        "135a246d-2e00-41d6-898b-c31acba63ce5"
      )
    _ <- `π-enable`(_33dc4677_dc7f_40db_8f4e_276130f6d61c)
    _ <- (
      `π-supervised`(for {
        _ <- τ(`ℝ⁺`(r))("e2ab6dff-9ff2-4db4-8cee-3475374cc398")
        _ <- `Greeter'`(stdout, name)(using `π-uuid`)
      } yield ()),
      `π-supervised`(for {
        _ <- τ(`ℝ⁺`(r))("1567bcbf-bd10-460d-b0fe-626fbe6140e2")
        _ <- `Greeter"`(stdout, name)(using `π-uuid`)
      } yield ()),
      `π-supervised`(for {
        _ <- τ(`ℝ⁺`(r))("135a246d-2e00-41d6-898b-c31acba63ce5")
        _ <- `Greeter"'`(stdout, name)(using `π-uuid`)
      } yield ())
    ).parMapN { (_, _, _) => }
  } yield ()

  def `Greeter'`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, / : /)
    : IO[Unit] = (
    `π-supervised`(
      if (name.substring(0, 1).toUpperCase ==== "Q") for {
        _ <- IO.unit
        _ccef12d6_3f14_4274_bf94_bb2531f208a4 =
          _root_.scala.collection.immutable
            .Set("1dbeb274-01c8-45c1-b7e3-8af706663a38")
        _ <- `π-enable`(_ccef12d6_3f14_4274_bf94_bb2531f208a4)
        _ <- stdout(⊤(1L), "That's an unusual name.")(
          "1dbeb274-01c8-45c1-b7e3-8af706663a38"
        )
      } yield ()
      else IO.unit
    ),
    `π-supervised`(
      if (name ==== "Voldemort") for {
        _ <- IO.unit
        _b83e552a_fa80_4f55_9bab_edf085e9214a =
          _root_.scala.collection.immutable
            .Set("4370b94e-742e-4771-a1ca-ebc93f8f0279")
        _ <- `π-enable`(_b83e552a_fa80_4f55_9bab_edf085e9214a)
        _ <- stdout(⊤(1L), "WARNING! LORD VOLDEMORT IS HERE!")(
          "4370b94e-742e-4771-a1ca-ebc93f8f0279"
        )
      } yield ()
      else IO.unit
    ),
    `π-supervised`(
      if (name.substring(0, 1).toUpperCase ==== "Q") IO.unit
      else if (name ==== "Voldemort") IO.unit
      else for {
        _ <- IO.unit
        _f8e15bfc_940f_4fc0_bdd8_a1ed947990b6 =
          _root_.scala.collection.immutable
            .Set("7e46afb9-a2eb-45a1-bba0-d69d72ff815a")
        _ <- `π-enable`(_f8e15bfc_940f_4fc0_bdd8_a1ed947990b6)
        _ <-
          stdout(⊤(1L), s"Hello $name!")("7e46afb9-a2eb-45a1-bba0-d69d72ff815a")
      } yield ()
    )
  ).parMapN { (_, _, _) => }

  def `Greeter"`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, / : /)
    : IO[Unit] =
    if (name.substring(0, 1).toUpperCase ==== "Q") for {
      _ <- IO.unit
      _70a46e24_f919_4020_8b38_87fe77c2a41a = _root_.scala.collection.immutable
        .Set("e50e1849-e91f-4e69-9eb9-5137c366f3dd")
      _ <- `π-enable`(_70a46e24_f919_4020_8b38_87fe77c2a41a)
      _ <- stdout(⊤(1L), "That's an unusual name.")(
        "e50e1849-e91f-4e69-9eb9-5137c366f3dd"
      )
    } yield ()
    else if (name ==== "Voldemort") for {
      _ <- IO.unit
      _ed8da432_6642_419b_9495_fdece09054ab = _root_.scala.collection.immutable
        .Set("38043904-7c0d-4cb6-bcee-4c2c3c224a01")
      _ <- `π-enable`(_ed8da432_6642_419b_9495_fdece09054ab)
      _ <- stdout(⊤(1L), "WARNING! LORD VOLDEMORT IS HERE!")(
        "38043904-7c0d-4cb6-bcee-4c2c3c224a01"
      )
    } yield ()
    else for {
      _ <- IO.unit
      _d8a821c4_822f_4448_ba2e_81a3fbd824e8 = _root_.scala.collection.immutable
        .Set("6dc1dd1b-0a97-48d9-aa4c-1c00c8d68985")
      _ <- `π-enable`(_d8a821c4_822f_4448_ba2e_81a3fbd824e8)
      _ <-
        stdout(⊤(1L), s"Hello $name!")("6dc1dd1b-0a97-48d9-aa4c-1c00c8d68985")
    } yield ()

  def `Greeter"'`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, / : /)
    : IO[Unit] =
    if (name.substring(0, 1).toUpperCase ==== "Q") for {
      _ <- IO.unit
      _6624a93e_0b5c_4ad6_9289_995b66e0247c = _root_.scala.collection.immutable
        .Set("a4f45fa3-5140-4989-8443-b13b12c54a68")
      _ <- `π-enable`(_6624a93e_0b5c_4ad6_9289_995b66e0247c)
      _ <- stdout(⊤(1L), "That's an unusual name.")(
        "a4f45fa3-5140-4989-8443-b13b12c54a68"
      )
    } yield ()
    else if (name ==== "Voldemort") for {
      _ <- IO.unit
      _202f0f99_8c45_4bf3_9ca7_62c407b20d59 = _root_.scala.collection.immutable
        .Set("55f56208-9727-4809-be51-7178a5e835f8")
      _ <- `π-enable`(_202f0f99_8c45_4bf3_9ca7_62c407b20d59)
      _ <- stdout(⊤(1L), "WARNING! LORD VOLDEMORT IS HERE!")(
        "55f56208-9727-4809-be51-7178a5e835f8"
      )
    } yield ()
    else for {
      _ <- IO.unit
      _e821e8ac_3d3b_4e0b_a337_6e1162323d39 = _root_.scala.collection.immutable
        .Set("29274e3a-b1f1-48d7-8cae-3ebf9dba2251")
      _ <- `π-enable`(_e821e8ac_3d3b_4e0b_a337_6e1162323d39)
      _ <-
        stdout(⊤(1L), s"Hello $name!")("29274e3a-b1f1-48d7-8cae-3ebf9dba2251")
    } yield ()
