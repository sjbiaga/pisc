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
      "718c67fe-96b1-419b-b074-ab8e39ad604e" -> _root_.scala.collection.immutable
        .Set(
          "8f24a30c-c59f-4707-8398-397ea5bcadfb",
          "cb315806-0362-4636-9590-8ac133fe13ff"
        ),
      "8f24a30c-c59f-4707-8398-397ea5bcadfb" -> _root_.scala.collection.immutable
        .Set(
          "718c67fe-96b1-419b-b074-ab8e39ad604e",
          "cb315806-0362-4636-9590-8ac133fe13ff"
        ),
      "cb315806-0362-4636-9590-8ac133fe13ff" -> _root_.scala.collection.immutable
        .Set(
          "718c67fe-96b1-419b-b074-ab8e39ad604e",
          "8f24a30c-c59f-4707-8398-397ea5bcadfb"
        )
    )

  val `π-spell`: `Π-Map`[String, `Π-Set`[String]] =
    _root_.scala.collection.immutable.Map(
      "ed4eb2d5-6209-4a6e-9472-4a9f16e5c9b6" -> _root_.scala.collection.immutable
        .Set(),
      "692d53a5-aefe-4195-84e8-dd673a952838" -> _root_.scala.collection.immutable
        .Set(),
      "0416161b-9936-4607-920b-73176235e423" -> _root_.scala.collection.immutable
        .Set(),
      "946ffb9e-2386-491c-abb9-5ddd43f15868" -> _root_.scala.collection.immutable
        .Set(),
      "1bb6cf6a-5579-452b-888f-3b91332e64c4" -> _root_.scala.collection.immutable
        .Set("0809352c-8912-402d-8a69-0e4e7dab6667"),
      "fc6e1c5d-188b-4ddc-902f-a3beae95e112" -> _root_.scala.collection.immutable
        .Set(),
      "45489be6-4bb9-4dd0-9173-a32db0f735ca" -> _root_.scala.collection.immutable
        .Set(),
      "01fb1e0d-5083-40e2-963f-4ef967296328" -> _root_.scala.collection.immutable
        .Set("1bb6cf6a-5579-452b-888f-3b91332e64c4"),
      "60906cf9-6172-4b85-95e5-25c5f238fa02" -> _root_.scala.collection.immutable
        .Set(),
      "0809352c-8912-402d-8a69-0e4e7dab6667" -> _root_.scala.collection.immutable
        .Set("aec39ada-e2de-47c0-a09f-cf47ffead177"),
      "1d7a0c64-1aff-4ca4-a006-09977e1ed262" -> _root_.scala.collection.immutable
        .Set(),
      "8f7a0482-46f7-4dec-a038-8417ecd040e4" -> _root_.scala.collection.immutable
        .Set(),
      "4fbcf221-19cc-4ef6-bbf8-a0652543291b" -> _root_.scala.collection.immutable
        .Set("4f524875-1125-436f-b2c6-98833198d08b"),
      "8a15d63c-3373-450c-a8da-e44a80951094" -> _root_.scala.collection.immutable
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
    _b93fc677_d77c_4d7a_8a82_e72955c9cbc0 =
      _root_.scala.collection.immutable.Set(
        "4fbcf221-19cc-4ef6-bbf8-a0652543291b",
        "01fb1e0d-5083-40e2-963f-4ef967296328"
      )
    _ <- `π-enable`(_b93fc677_d77c_4d7a_8a82_e72955c9cbc0)
    _ <- (
      `π-supervised`(for {
        _         <- stdout(⊤(1L), "What's your name?")(
          "4fbcf221-19cc-4ef6-bbf8-a0652543291b"
        )
        (name, _) <- stdin(⊤(1L))("4f524875-1125-436f-b2c6-98833198d08b")
        _         <-
          if (name.isBlank ==== true) for {
            _ <- IO.unit
            _f531c583_2776_4bd5_8600_36d94eba43e3 =
              _root_.scala.collection.immutable
                .Set("b9ed770b-d628-4761-86a7-4463b6225710")
            _ <- `π-enable`(_f531c583_2776_4bd5_8600_36d94eba43e3)
            _ <-
              stdout(⊤(1L), `()`(null))("b9ed770b-d628-4761-86a7-4463b6225710")
            _ <- Greeter(stdin, stdout, line)(using `π-uuid`)
          } yield ()
          else Chooser(stdout, name)(using `π-uuid`)
      } yield ()),
      `π-supervised`(for {
        (prompt, _) <- stdout(⊤(1L))("01fb1e0d-5083-40e2-963f-4ef967296328")(nl)
        _           <- τ(⊤(1L))("1bb6cf6a-5579-452b-888f-3b91332e64c4")
        _           <- IO {
          print(prompt)
        }
        line        <- IO.blocking {
          scala.io.StdIn.readLine
        }
        _          <- stdin(⊤(1L), line)("0809352c-8912-402d-8a69-0e4e7dab6667")
        (greet, _) <- stdout(⊤(1L))("aec39ada-e2de-47c0-a09f-cf47ffead177")(nl)
        _          <-
          if (!greet ==== false) for {
            _ <- IO.unit
            _d6546187_fcec_4a55_a360_4c319ddacf69 =
              _root_.scala.collection.immutable
                .Set("45489be6-4bb9-4dd0-9173-a32db0f735ca")
            _ <- `π-enable`(_d6546187_fcec_4a55_a360_4c319ddacf69)
            _ <- τ(⊤(1L))("45489be6-4bb9-4dd0-9173-a32db0f735ca")
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
    _de094c2b_3808_41d5_ac1b_ca2b15f40acc =
      _root_.scala.collection.immutable.Set(
        "718c67fe-96b1-419b-b074-ab8e39ad604e",
        "8f24a30c-c59f-4707-8398-397ea5bcadfb",
        "cb315806-0362-4636-9590-8ac133fe13ff"
      )
    _ <- `π-enable`(_de094c2b_3808_41d5_ac1b_ca2b15f40acc)
    _ <- (
      `π-supervised`(for {
        _ <- τ(`ℝ⁺`(r))("718c67fe-96b1-419b-b074-ab8e39ad604e")
        _ <- `Greeter'`(stdout, name)(using `π-uuid`)
      } yield ()),
      `π-supervised`(for {
        _ <- τ(`ℝ⁺`(r))("8f24a30c-c59f-4707-8398-397ea5bcadfb")
        _ <- `Greeter"`(stdout, name)(using `π-uuid`)
      } yield ()),
      `π-supervised`(for {
        _ <- τ(`ℝ⁺`(r))("cb315806-0362-4636-9590-8ac133fe13ff")
        _ <- `Greeter"'`(stdout, name)(using `π-uuid`)
      } yield ())
    ).parMapN { (_, _, _) => }
  } yield ()

  def `Greeter'`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, / : /)
    : IO[Unit] = (
    `π-supervised`(
      if (name.substring(0, 1).toUpperCase ==== "Q") for {
        _ <- IO.unit
        _4573d50f_97c6_40aa_ae60_9e20b4ea8758 =
          _root_.scala.collection.immutable
            .Set("946ffb9e-2386-491c-abb9-5ddd43f15868")
        _ <- `π-enable`(_4573d50f_97c6_40aa_ae60_9e20b4ea8758)
        _ <- stdout(⊤(1L), "That's an unusual name.")(
          "946ffb9e-2386-491c-abb9-5ddd43f15868"
        )
      } yield ()
      else IO.unit
    ),
    `π-supervised`(
      if (name ==== "Voldemort") for {
        _ <- IO.unit
        _a6803b4a_c12e_497e_8d0d_14d8d42e799f =
          _root_.scala.collection.immutable
            .Set("8a15d63c-3373-450c-a8da-e44a80951094")
        _ <- `π-enable`(_a6803b4a_c12e_497e_8d0d_14d8d42e799f)
        _ <- stdout(⊤(1L), "WARNING! LORD VOLDEMORT IS HERE!")(
          "8a15d63c-3373-450c-a8da-e44a80951094"
        )
      } yield ()
      else IO.unit
    ),
    `π-supervised`(
      if (name.substring(0, 1).toUpperCase ==== "Q") IO.unit
      else if (name ==== "Voldemort") IO.unit
      else for {
        _ <- IO.unit
        _08599a71_7dc0_45ee_af2a_7747b9407f47 =
          _root_.scala.collection.immutable
            .Set("0416161b-9936-4607-920b-73176235e423")
        _ <- `π-enable`(_08599a71_7dc0_45ee_af2a_7747b9407f47)
        _ <-
          stdout(⊤(1L), s"Hello $name!")("0416161b-9936-4607-920b-73176235e423")
      } yield ()
    )
  ).parMapN { (_, _, _) => }

  def `Greeter"`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, / : /)
    : IO[Unit] =
    if (name.substring(0, 1).toUpperCase ==== "Q") for {
      _ <- IO.unit
      _b56752e3_6601_405e_ba15_f1601449b3e0 = _root_.scala.collection.immutable
        .Set("ed4eb2d5-6209-4a6e-9472-4a9f16e5c9b6")
      _ <- `π-enable`(_b56752e3_6601_405e_ba15_f1601449b3e0)
      _ <- stdout(⊤(1L), "That's an unusual name.")(
        "ed4eb2d5-6209-4a6e-9472-4a9f16e5c9b6"
      )
    } yield ()
    else if (name ==== "Voldemort") for {
      _ <- IO.unit
      _6b10cd0e_e4e8_41d5_9232_2c664418d4f4 = _root_.scala.collection.immutable
        .Set("fc6e1c5d-188b-4ddc-902f-a3beae95e112")
      _ <- `π-enable`(_6b10cd0e_e4e8_41d5_9232_2c664418d4f4)
      _ <- stdout(⊤(1L), "WARNING! LORD VOLDEMORT IS HERE!")(
        "fc6e1c5d-188b-4ddc-902f-a3beae95e112"
      )
    } yield ()
    else for {
      _ <- IO.unit
      _0df59750_4fae_4c1c_b3c5_1fc565ac3f04 = _root_.scala.collection.immutable
        .Set("1d7a0c64-1aff-4ca4-a006-09977e1ed262")
      _ <- `π-enable`(_0df59750_4fae_4c1c_b3c5_1fc565ac3f04)
      _ <-
        stdout(⊤(1L), s"Hello $name!")("1d7a0c64-1aff-4ca4-a006-09977e1ed262")
    } yield ()

  def `Greeter"'`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, / : /)
    : IO[Unit] =
    if (name.substring(0, 1).toUpperCase ==== "Q") for {
      _ <- IO.unit
      _13b695d5_db7c_4b2a_965a_a3144f00d4b1 = _root_.scala.collection.immutable
        .Set("60906cf9-6172-4b85-95e5-25c5f238fa02")
      _ <- `π-enable`(_13b695d5_db7c_4b2a_965a_a3144f00d4b1)
      _ <- stdout(⊤(1L), "That's an unusual name.")(
        "60906cf9-6172-4b85-95e5-25c5f238fa02"
      )
    } yield ()
    else if (name ==== "Voldemort") for {
      _ <- IO.unit
      _9df7fd84_47b9_4275_abcc_83d6368f58ab = _root_.scala.collection.immutable
        .Set("692d53a5-aefe-4195-84e8-dd673a952838")
      _ <- `π-enable`(_9df7fd84_47b9_4275_abcc_83d6368f58ab)
      _ <- stdout(⊤(1L), "WARNING! LORD VOLDEMORT IS HERE!")(
        "692d53a5-aefe-4195-84e8-dd673a952838"
      )
    } yield ()
    else for {
      _ <- IO.unit
      _18d2e7bf_d5ec_4677_a794_0da37ed92037 = _root_.scala.collection.immutable
        .Set("8f7a0482-46f7-4dec-a038-8417ecd040e4")
      _ <- `π-enable`(_18d2e7bf_d5ec_4677_a794_0da37ed92037)
      _ <-
        stdout(⊤(1L), s"Hello $name!")("8f7a0482-46f7-4dec-a038-8417ecd040e4")
    } yield ()
