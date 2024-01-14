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

  val `π-trick`: `Π-Map`[String, `Π-Set`[String]] = _root_.scala.collection.immutable.Map(
    "ade8f856-7dbf-45d3-b50a-e2851ceb625a" -> _root_.scala.collection.immutable.Set("7383fa39-10d5-439d-ab02-41dd4c2e5498", "f1162115-05c5-42ae-a501-fe2bc4e4f1f1"),
    "7383fa39-10d5-439d-ab02-41dd4c2e5498" -> _root_.scala.collection.immutable.Set("ade8f856-7dbf-45d3-b50a-e2851ceb625a", "f1162115-05c5-42ae-a501-fe2bc4e4f1f1"),
    "f1162115-05c5-42ae-a501-fe2bc4e4f1f1" -> _root_.scala.collection.immutable.Set("7383fa39-10d5-439d-ab02-41dd4c2e5498", "ade8f856-7dbf-45d3-b50a-e2851ceb625a")
  )

  val `π-spell`: `Π-Map`[String, `Π-Set`[String]] = _root_.scala.collection.immutable.Map(
    "fe54a8cb-34c6-4de9-857f-aabba9225964" -> _root_.scala.collection.immutable.Set("03098f2a-527d-4b0e-afa1-3d7e81438ae2"),
    "41de4bb1-61d1-4a48-a09a-3101e5cdb3d3" -> _root_.scala.collection.immutable.Set("ceac65cd-39e9-45e3-822a-331148e55743"),
    "03098f2a-527d-4b0e-afa1-3d7e81438ae2" -> _root_.scala.collection.immutable.Set("14cb917c-1377-4f81-837c-772b31b04138"),
    "14cb917c-1377-4f81-837c-772b31b04138" -> _root_.scala.collection.immutable.Set(),
    "ceac65cd-39e9-45e3-822a-331148e55743" -> _root_.scala.collection.immutable.Set("ab1f8702-79a0-4321-a78b-8d38b5028928"),
    "68c5c230-6fe5-4849-aee0-6202e2cc17c3" -> _root_.scala.collection.immutable.Set("6511aa96-589d-448a-a0a2-b76ca0c7010b"),
    "ab1f8702-79a0-4321-a78b-8d38b5028928" -> _root_.scala.collection.immutable.Set("fe54a8cb-34c6-4de9-857f-aabba9225964")
  )

  implicit val `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) = `π-trick` -> `π-spell`

  given Conversion[`()`, String] = _.name.asInstanceOf[String]

  val gen = new scala.util.Random

  def r = 1 + math.abs(gen.nextInt % 3)

  def Main()(using ^ : String)(using % : %, \ : \, / : /, * : *, + : +, - : -): IO[Unit] = for {
    stdin  <- ν
    stdout <- ν
    _      <- Greeter(stdin, stdout, "")(using `π-uuid`)
  } yield ()

  def Greeter(stdin: `()`, stdout: `()`, sc_name: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *, + : +, - : -): IO[Unit] = for {
    _ <- IO.unit
    _7ac5a042_8cc2_410e_a6dc_77a1b863ae40 = _root_.scala.collection.immutable.Set("68c5c230-6fe5-4849-aee0-6202e2cc17c3", "41de4bb1-61d1-4a48-a09a-3101e5cdb3d3")
    _ <- %.update(_7ac5a042_8cc2_410e_a6dc_77a1b863ae40.foldLeft(_)(_ + _))
    _ <- (
      for {
        _         <- stdout(null, "What's your name?\n")("68c5c230-6fe5-4849-aee0-6202e2cc17c3")
        (name, _) <- stdin(null)("6511aa96-589d-448a-a0a2-b76ca0c7010b")
        _         <- Chooser(stdout, name)(using `π-uuid`)
      } yield (),
      for {
        (prompt, _)   <- stdout(null)("41de4bb1-61d1-4a48-a09a-3101e5cdb3d3")
        _             <- τ(null)("ceac65cd-39e9-45e3-822a-331148e55743")
        _             <- IO {
          print(prompt)
        }
        _             <- τ(null)("ab1f8702-79a0-4321-a78b-8d38b5028928")
        sc_name       <- IO {
          scala.io.StdIn.readLine
        }
        _             <- stdin(null, sc_name)("fe54a8cb-34c6-4de9-857f-aabba9225964")
        (pi_greet, _) <- stdout(null)("03098f2a-527d-4b0e-afa1-3d7e81438ae2")
        _             <- τ(null)("14cb917c-1377-4f81-837c-772b31b04138")
        _             <- IO {
          print(pi_greet)
        }
        _             <- `𝟎`
      } yield ()
    ).parMapN { (_, _) => }
  } yield ()

  def Chooser(stdout: `()`, name: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *, + : +, - : -): IO[Unit] = for {
    _ <- IO.unit
    _6ac3e2b1_b8e7_4b41_80d6_d0b84098d55f = _root_.scala.collection.immutable.Set("7383fa39-10d5-439d-ab02-41dd4c2e5498", "ade8f856-7dbf-45d3-b50a-e2851ceb625a", "f1162115-05c5-42ae-a501-fe2bc4e4f1f1")
    _ <- %.update(_6ac3e2b1_b8e7_4b41_80d6_d0b84098d55f.foldLeft(_)(_ + _))
    _ <- (
      for {
        _ <- τ(`@`(r))("7383fa39-10d5-439d-ab02-41dd4c2e5498")
        _ <- `Greeter'`(stdout, name)(using `π-uuid`)
      } yield (),
      for {
        _ <- τ(`@`(r))("ade8f856-7dbf-45d3-b50a-e2851ceb625a")
        _ <- `Greeter"`(stdout, name)(using `π-uuid`)
      } yield (),
      for {
        _ <- τ(`@`(r))("f1162115-05c5-42ae-a501-fe2bc4e4f1f1")
        _ <- `Greeter"'`(stdout, name)(using `π-uuid`)
      } yield ()
    ).parMapN { (_, _, _) => }
  } yield ()

  def `Greeter'`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *, + : +, - : -): IO[Unit] = for (
    _ <- (
      for (
        _ <-
          if (name.substring(0, 1).toUpperCase === "Q") for {
            _ <- IO.unit
            _79e3f27c_74b2_48d4_88d9_d175852b3b30 = _root_.scala.collection.immutable.Set("d9b0110e-70d1-4f73-9de9-68ffa5a09213")
            _ <- %.update(_79e3f27c_74b2_48d4_88d9_d175852b3b30.foldLeft(_)(_ + _))
            _ <- for (_ <- stdout(null, "That's an unusual name.\n")("d9b0110e-70d1-4f73-9de9-68ffa5a09213")) yield ()
          } yield ()
          else `𝟎`
      ) yield (),
      for (
        _ <-
          if (name === "Voldemort") for {
            _ <- IO.unit
            _71c05e6b_fd0e_49b9_8cb5_e8ff414e6d63 = _root_.scala.collection.immutable.Set("71a31717-35a8-425c-89fa-3356f89c5dca")
            _ <- %.update(_71c05e6b_fd0e_49b9_8cb5_e8ff414e6d63.foldLeft(_)(_ + _))
            _ <- for (_ <- stdout(null, "WARNING! LORD VOLDEMORT IS HERE!\n")("71a31717-35a8-425c-89fa-3356f89c5dca")) yield ()
          } yield ()
          else `𝟎`
      ) yield (),
      for (
        _ <-
          if (name.substring(0, 1).toUpperCase === "Q") `𝟎`
          else for (
            _ <-
              if (name === "Voldemort") `𝟎`
              else for {
                _ <- IO.unit
                _3891f7a4_fd90_4de3_9df1_321e52845bb5 = _root_.scala.collection.immutable.Set("05c87631-3548-4be4-8116-679e96c41456")
                _ <- %.update(_3891f7a4_fd90_4de3_9df1_321e52845bb5.foldLeft(_)(_ + _))
                _ <- for {
                  _ <- stdout(null, s"Hello $name!\n")("05c87631-3548-4be4-8116-679e96c41456")
                  _ <- `𝟎`
                } yield ()
              } yield ()
          ) yield ()
      ) yield ()
    ).parMapN { (_, _, _) => }
  ) yield ()

  def `Greeter"`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *, + : +, - : -): IO[Unit] = for (
    _ <-
      if (name.substring(0, 1).toUpperCase === "Q") for {
        _ <- IO.unit
        _5916cea8_e3b5_43fe_acff_7af5d6ce8323 = _root_.scala.collection.immutable.Set("3f37a44c-ff1d-4f54-94e8-9447f79cb214")
        _ <- %.update(_5916cea8_e3b5_43fe_acff_7af5d6ce8323.foldLeft(_)(_ + _))
        _ <- for {
          _ <- stdout(null, "That's an unusual name.\n")("3f37a44c-ff1d-4f54-94e8-9447f79cb214")
          _ <- `𝟎`
        } yield ()
      } yield ()
      else for (
        _ <-
          if (name === "Voldemort") for {
            _ <- IO.unit
            _2687a540_4333_443d_aba9_fd104e3c7ffa = _root_.scala.collection.immutable.Set("482b92fc-1945-4b67-9a92-3a90db233547")
            _ <- %.update(_2687a540_4333_443d_aba9_fd104e3c7ffa.foldLeft(_)(_ + _))
            _ <- for {
              _ <- stdout(null, "WARNING! LORD VOLDEMORT IS HERE!\n")("482b92fc-1945-4b67-9a92-3a90db233547")
              _ <- `𝟎`
            } yield ()
          } yield ()
          else for {
            _ <- IO.unit
            _6449fd39_eaa3_419d_8551_8e14efa2b88a = _root_.scala.collection.immutable.Set("06b37d18-773b-4d9d-8bac-08661513decf")
            _ <- %.update(_6449fd39_eaa3_419d_8551_8e14efa2b88a.foldLeft(_)(_ + _))
            _ <- for {
              _ <- stdout(null, s"Hello $name!\n")("06b37d18-773b-4d9d-8bac-08661513decf")
              _ <- `𝟎`
            } yield ()
          } yield ()
      ) yield ()
  ) yield ()

  def `Greeter"'`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *, + : +, - : -): IO[Unit] = for (
    _ <-
      if (name.substring(0, 1).toUpperCase === "Q") for {
        _ <- IO.unit
        _5a7094aa_f3df_4821_ac69_7497f0544dd9 = _root_.scala.collection.immutable.Set("b6022844-699d-4484-b5a9-735ca143812f")
        _ <- %.update(_5a7094aa_f3df_4821_ac69_7497f0544dd9.foldLeft(_)(_ + _))
        _ <- for {
          _ <- stdout(null, "That's an unusual name.\n")("b6022844-699d-4484-b5a9-735ca143812f")
          _ <- `𝟎`
        } yield ()
      } yield ()
      else for (
        _ <-
          if (name === "Voldemort") for {
            _ <- IO.unit
            _e9fbee11_cc5e_4844_9996_be68f02ee970 = _root_.scala.collection.immutable.Set("5094d1ad-96d9-466c-9e0d-a475f02050f8")
            _ <- %.update(_e9fbee11_cc5e_4844_9996_be68f02ee970.foldLeft(_)(_ + _))
            _ <- for {
              _ <- stdout(null, "WARNING! LORD VOLDEMORT IS HERE!\n")("5094d1ad-96d9-466c-9e0d-a475f02050f8")
              _ <- `𝟎`
            } yield ()
          } yield ()
          else for {
            _ <- IO.unit
            _ae4969ca_7668_4ad4_92d8_b29ed9eab142 = _root_.scala.collection.immutable.Set("360418bd-8a07-4f43-b0fe-6f8acf9acec7")
            _ <- %.update(_ae4969ca_7668_4ad4_92d8_b29ed9eab142.foldLeft(_)(_ + _))
            _ <- for {
              _ <- stdout(null, s"Hello $name!\n")("360418bd-8a07-4f43-b0fe-6f8acf9acec7")
              _ <- `𝟎`
            } yield ()
          } yield ()
      ) yield ()
  ) yield ()
