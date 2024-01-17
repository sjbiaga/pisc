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

package pisc.greeter

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

  val `π-trick`: `Π-Map`[String, `Π-Set`[String]] = _root_.scala.collection.immutable.Map(
    "abeb8948-218e-4c79-ad7d-bc2be00efd8a" -> _root_.scala.collection.immutable.Set("da147ad9-58b6-47a5-94eb-a44c0a16d7aa", "a014560d-edc5-417c-9713-a2ea7680639c"),
    "a014560d-edc5-417c-9713-a2ea7680639c" -> _root_.scala.collection.immutable.Set("abeb8948-218e-4c79-ad7d-bc2be00efd8a", "da147ad9-58b6-47a5-94eb-a44c0a16d7aa"),
    "da147ad9-58b6-47a5-94eb-a44c0a16d7aa" -> _root_.scala.collection.immutable.Set("abeb8948-218e-4c79-ad7d-bc2be00efd8a", "a014560d-edc5-417c-9713-a2ea7680639c")
  )

  val `π-spell`: `Π-Map`[String, `Π-Set`[String]] = _root_.scala.collection.immutable.Map(
    "39066390-70b2-4392-83bf-558a12a6d02b" -> _root_.scala.collection.immutable.Set("8ef91037-77b5-44a8-a2c1-652e2a0eb5a4"),
    "3bafc188-2fa8-4a76-9592-fcfda181f4f5" -> _root_.scala.collection.immutable.Set(),
    "2f3b4a1e-eb59-4c53-bf2c-e8847007af80" -> _root_.scala.collection.immutable.Set(),
    "35c4c94b-bae8-47df-8f50-75196d52309f" -> _root_.scala.collection.immutable.Set("c7bb90a3-fd5f-4604-8671-4080e8181e6b"),
    "656c0784-5c07-47bd-aa67-a04ef132ae2d" -> _root_.scala.collection.immutable.Set(),
    "4c3d549f-72af-49de-9571-20cffb0dd396" -> _root_.scala.collection.immutable.Set(),
    "1de281f8-255c-4362-9acd-401b087e5cca" -> _root_.scala.collection.immutable.Set(),
    "8ef91037-77b5-44a8-a2c1-652e2a0eb5a4" -> _root_.scala.collection.immutable.Set("572be4eb-784d-460a-b792-4520782b29a5"),
    "7836a9d1-314a-4cc2-b182-c68e270c175a" -> _root_.scala.collection.immutable.Set("ea34b22a-c6a0-4c73-b132-43c9513f913a"),
    "a3498a7b-6b95-4a2c-99a7-c093cf075574" -> _root_.scala.collection.immutable.Set(),
    "e877d779-8be2-47b7-a6e2-8f9eac78b1e4" -> _root_.scala.collection.immutable.Set(),
    "6d0a8c6a-23ae-4115-92fe-31d9830c4475" -> _root_.scala.collection.immutable.Set(),
    "c7bb90a3-fd5f-4604-8671-4080e8181e6b" -> _root_.scala.collection.immutable.Set("39066390-70b2-4392-83bf-558a12a6d02b"),
    "38c4c717-cf0a-4860-9918-c7673fd82853" -> _root_.scala.collection.immutable.Set()
  )

  implicit val `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) = `π-trick` -> `π-spell`

  given Conversion[`()`, String] = _.name.asInstanceOf[String]

  val gen = new scala.util.Random

  def r = 1 + math.abs(gen.nextInt % 3)

  def Main()(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for {
    stdin  <- ν
    stdout <- ν
    _      <- Greeter(stdin, stdout, "")(using `π-uuid`)
  } yield ()

  def Greeter(stdin: `()`, stdout: `()`, line: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _6ae08f0d_625a_4e32_bb69_9fdb7445fc1b = _root_.scala.collection.immutable.Set("7836a9d1-314a-4cc2-b182-c68e270c175a", "35c4c94b-bae8-47df-8f50-75196d52309f")
    _ <- `π-none`(_6ae08f0d_625a_4e32_bb69_9fdb7445fc1b)
    _ <- (
      for {
        _         <- stdout(null, "What's your name?\n")("7836a9d1-314a-4cc2-b182-c68e270c175a")
        (name, _) <- stdin(null)("ea34b22a-c6a0-4c73-b132-43c9513f913a")
        _         <- Chooser(stdout, name)(using `π-uuid`)
      } yield (),
      for {
        (prompt, _) <- stdout(null)("35c4c94b-bae8-47df-8f50-75196d52309f")
        _           <- τ(∞)("c7bb90a3-fd5f-4604-8671-4080e8181e6b")
        _           <- IO {
          print(prompt)
        }
        line        <- IO.blocking {
          scala.io.StdIn.readLine
        }
        _           <- stdin(null, line)("39066390-70b2-4392-83bf-558a12a6d02b")
        (greet, _)  <- stdout(null)("8ef91037-77b5-44a8-a2c1-652e2a0eb5a4")
        _           <- τ(∞)("572be4eb-784d-460a-b792-4520782b29a5")
        _           <- IO {
          print(greet)
        }
        _           <- pisc.fibonacci.π.Main()(using `π-uuid`)
      } yield ()
    ).parMapN { (_, _) => }
  } yield ()

  def Chooser(stdout: `()`, name: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _2d9f3f81_bbbf_47e1_90e9_ae27b7b24a8b = _root_.scala.collection.immutable.Set("abeb8948-218e-4c79-ad7d-bc2be00efd8a", "da147ad9-58b6-47a5-94eb-a44c0a16d7aa", "a014560d-edc5-417c-9713-a2ea7680639c")
    _ <- `π-none`(_2d9f3f81_bbbf_47e1_90e9_ae27b7b24a8b)
    _ <- (
      for {
        _ <- τ(`@`(r))("abeb8948-218e-4c79-ad7d-bc2be00efd8a")
        _ <- `Greeter'`(stdout, name)(using `π-uuid`)
      } yield (),
      for {
        _ <- τ(`@`(r))("da147ad9-58b6-47a5-94eb-a44c0a16d7aa")
        _ <- `Greeter"`(stdout, name)(using `π-uuid`)
      } yield (),
      for {
        _ <- τ(`@`(r))("a014560d-edc5-417c-9713-a2ea7680639c")
        _ <- `Greeter"'`(stdout, name)(using `π-uuid`)
      } yield ()
    ).parMapN { (_, _, _) => }
  } yield ()

  def `Greeter'`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for (
    _ <- (
      `𝟎`,
      for (
        _ <-
          if (name.substring(0, 1).toUpperCase ==== "Q") for {
            _ <- IO.unit
            _6a8d60bf_ff87_4f6b_813d_48823d3735f6 = _root_.scala.collection.immutable.Set("e877d779-8be2-47b7-a6e2-8f9eac78b1e4")
            _ <- `π-none`(_6a8d60bf_ff87_4f6b_813d_48823d3735f6)
            _ <- for (_ <- stdout(null, "That's an unusual name.\n")("e877d779-8be2-47b7-a6e2-8f9eac78b1e4")) yield ()
          } yield ()
          else `𝟎`
      ) yield (),
      for (
        _ <-
          if (name ==== "Voldemort") for {
            _ <- IO.unit
            _5c3c723e_4442_4a01_b6a8_2eea74a5f2fa = _root_.scala.collection.immutable.Set("1de281f8-255c-4362-9acd-401b087e5cca")
            _ <- `π-none`(_5c3c723e_4442_4a01_b6a8_2eea74a5f2fa)
            _ <- for (_ <- stdout(null, "WARNING! LORD VOLDEMORT IS HERE!\n")("1de281f8-255c-4362-9acd-401b087e5cca")) yield ()
          } yield ()
          else `𝟎`
      ) yield (),
      for (
        _ <-
          if (name.substring(0, 1).toUpperCase ==== "Q") `𝟎`
          else for (
            _ <-
              if (name ==== "Voldemort") `𝟎`
              else for {
                _ <- IO.unit
                _06eadc2e_4e01_4abc_a918_a5dfd10665b9 = _root_.scala.collection.immutable.Set("6d0a8c6a-23ae-4115-92fe-31d9830c4475")
                _ <- `π-none`(_06eadc2e_4e01_4abc_a918_a5dfd10665b9)
                _ <- for (_ <- stdout(null, s"Hello $name!\n")("6d0a8c6a-23ae-4115-92fe-31d9830c4475")) yield ()
              } yield ()
          ) yield ()
      ) yield ()
    ).parMapN { (_, _, _, _) => }
  ) yield ()

  def `Greeter"`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for (
    _ <-
      if (name.substring(0, 1).toUpperCase ==== "Q") for {
        _ <- IO.unit
        _23f5805f_da45_4c64_aa2f_4912bcb0e4cf = _root_.scala.collection.immutable.Set("656c0784-5c07-47bd-aa67-a04ef132ae2d")
        _ <- `π-none`(_23f5805f_da45_4c64_aa2f_4912bcb0e4cf)
        _ <- for (_ <- stdout(null, "That's an unusual name.\n")("656c0784-5c07-47bd-aa67-a04ef132ae2d")) yield ()
      } yield ()
      else for (
        _ <-
          if (name ==== "Voldemort") for {
            _ <- IO.unit
            _6c510c7e_bf28_4f1d_a8e8_249f0998f93c = _root_.scala.collection.immutable.Set("2f3b4a1e-eb59-4c53-bf2c-e8847007af80")
            _ <- `π-none`(_6c510c7e_bf28_4f1d_a8e8_249f0998f93c)
            _ <- for (_ <- stdout(null, "WARNING! LORD VOLDEMORT IS HERE!\n")("2f3b4a1e-eb59-4c53-bf2c-e8847007af80")) yield ()
          } yield ()
          else for {
            _ <- IO.unit
            _bb9b7717_1be8_4316_b4e3_75709b509cfd = _root_.scala.collection.immutable.Set("a3498a7b-6b95-4a2c-99a7-c093cf075574")
            _ <- `π-none`(_bb9b7717_1be8_4316_b4e3_75709b509cfd)
            _ <- for (_ <- stdout(null, s"Hello $name!\n")("a3498a7b-6b95-4a2c-99a7-c093cf075574")) yield ()
          } yield ()
      ) yield ()
  ) yield ()

  def `Greeter"'`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for (
    _ <-
      if (name.substring(0, 1).toUpperCase ==== "Q") for {
        _ <- IO.unit
        _e2b26744_f111_40bb_870b_84d4274c4e51 = _root_.scala.collection.immutable.Set("3bafc188-2fa8-4a76-9592-fcfda181f4f5")
        _ <- `π-none`(_e2b26744_f111_40bb_870b_84d4274c4e51)
        _ <- for (_ <- stdout(null, "That's an unusual name.\n")("3bafc188-2fa8-4a76-9592-fcfda181f4f5")) yield ()
      } yield ()
      else for (
        _ <-
          if (name ==== "Voldemort") for {
            _ <- IO.unit
            _6f04baa3_1175_4583_a335_ac0c101e94de = _root_.scala.collection.immutable.Set("38c4c717-cf0a-4860-9918-c7673fd82853")
            _ <- `π-none`(_6f04baa3_1175_4583_a335_ac0c101e94de)
            _ <- for (_ <- stdout(null, "WARNING! LORD VOLDEMORT IS HERE!\n")("38c4c717-cf0a-4860-9918-c7673fd82853")) yield ()
          } yield ()
          else for {
            _ <- IO.unit
            _ee94018d_438e_4dba_a90a_82895ec498b8 = _root_.scala.collection.immutable.Set("4c3d549f-72af-49de-9571-20cffb0dd396")
            _ <- `π-none`(_ee94018d_438e_4dba_a90a_82895ec498b8)
            _ <- for (_ <- stdout(null, s"Hello $name!\n")("4c3d549f-72af-49de-9571-20cffb0dd396")) yield ()
          } yield ()
      ) yield ()
  ) yield ()
