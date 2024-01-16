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
    "ea75fd9c-17b0-46f0-a5ee-92330e6ce0fd" -> _root_.scala.collection.immutable.Set("f97d1f5b-3996-479a-9b7b-118681f7eef5", "3f0442c3-5e7b-4352-9e3b-5ece1bb1a602"),
    "f97d1f5b-3996-479a-9b7b-118681f7eef5" -> _root_.scala.collection.immutable.Set("ea75fd9c-17b0-46f0-a5ee-92330e6ce0fd", "3f0442c3-5e7b-4352-9e3b-5ece1bb1a602"),
    "3f0442c3-5e7b-4352-9e3b-5ece1bb1a602" -> _root_.scala.collection.immutable.Set("f97d1f5b-3996-479a-9b7b-118681f7eef5", "ea75fd9c-17b0-46f0-a5ee-92330e6ce0fd")
  )

  val `π-spell`: `Π-Map`[String, `Π-Set`[String]] = _root_.scala.collection.immutable.Map(
    "064e4eca-4e90-4691-a1a6-83f95f88a43d" -> _root_.scala.collection.immutable.Set(),
    "83231987-dadc-4153-8dc1-79eabf341bc3" -> _root_.scala.collection.immutable.Set(),
    "006e10fe-a2b3-4b47-be64-05900e3c22e5" -> _root_.scala.collection.immutable.Set("9058fca2-e535-4a38-8eed-fa915b2c901b"),
    "1f704f3a-4d81-422e-a0ec-f814243fdf10" -> _root_.scala.collection.immutable.Set(),
    "279c6e4c-7bec-441c-aaf2-ed21dae4de17" -> _root_.scala.collection.immutable.Set(),
    "9e8c58c3-9616-4a17-a965-b3d45aa8c9c6" -> _root_.scala.collection.immutable.Set(),
    "31faa3a9-ca31-477d-ae32-00b196b99c40" -> _root_.scala.collection.immutable.Set("acbec39e-36cf-4e1f-8b2d-1e353e96aae6"),
    "30ad95e2-ab78-4e79-b779-523f66483aa0" -> _root_.scala.collection.immutable.Set(),
    "f0c96ec1-36cb-4af8-9cbf-885f3e7c1544" -> _root_.scala.collection.immutable.Set("b6e1db04-95df-4dbc-9b99-b4075a860521"),
    "9058fca2-e535-4a38-8eed-fa915b2c901b" -> _root_.scala.collection.immutable.Set("f0c96ec1-36cb-4af8-9cbf-885f3e7c1544"),
    "a6dd01cc-98a5-4690-ac7f-8449075cfea1" -> _root_.scala.collection.immutable.Set(),
    "0a4a4a6d-03e3-407f-a03b-38d684d9dca8" -> _root_.scala.collection.immutable.Set("74ea4996-51c2-413f-8411-6f0e0e357c82"),
    "b6e1db04-95df-4dbc-9b99-b4075a860521" -> _root_.scala.collection.immutable.Set("31faa3a9-ca31-477d-ae32-00b196b99c40"),
    "ba43adf7-ea38-4b52-b382-56acd9a0583a" -> _root_.scala.collection.immutable.Set(),
    "9bc99c76-2572-4203-a862-a61a54209ed3" -> _root_.scala.collection.immutable.Set()
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

  def Greeter(stdin: `()`, stdout: `()`, sc_name: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _f83fc8b3_21a5_4edf_89aa_8f9dd33114e3 = _root_.scala.collection.immutable.Set("0a4a4a6d-03e3-407f-a03b-38d684d9dca8", "006e10fe-a2b3-4b47-be64-05900e3c22e5")
    _ <- `π-none`(_f83fc8b3_21a5_4edf_89aa_8f9dd33114e3)
    _ <- (
      for {
        _         <- stdout(null, "What's your name?\n")("0a4a4a6d-03e3-407f-a03b-38d684d9dca8")
        (name, _) <- stdin(null)("74ea4996-51c2-413f-8411-6f0e0e357c82")
        _         <- Chooser(stdout, name)(using `π-uuid`)
      } yield (),
      for {
        (prompt, _)   <- stdout(null)("006e10fe-a2b3-4b47-be64-05900e3c22e5")
        _             <- τ(∞)("9058fca2-e535-4a38-8eed-fa915b2c901b")
        _             <- IO {
          print(prompt)
        }
        _             <- τ(∞)("f0c96ec1-36cb-4af8-9cbf-885f3e7c1544")
        sc_name       <- IO {
          scala.io.StdIn.readLine
        }
        _             <- stdin(null, sc_name)("b6e1db04-95df-4dbc-9b99-b4075a860521")
        (pi_greet, _) <- stdout(null)("31faa3a9-ca31-477d-ae32-00b196b99c40")
        _             <- τ(∞)("acbec39e-36cf-4e1f-8b2d-1e353e96aae6")
        _             <- IO {
          print(pi_greet)
        }
        _             <- pisc.fibonacci.π.Main()(using `π-uuid`)
      } yield ()
    ).parMapN { (_, _) => }
  } yield ()

  def Chooser(stdout: `()`, name: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _5a4ca746_43eb_4480_a0d5_531fa972a5d5 = _root_.scala.collection.immutable.Set("f97d1f5b-3996-479a-9b7b-118681f7eef5", "ea75fd9c-17b0-46f0-a5ee-92330e6ce0fd", "3f0442c3-5e7b-4352-9e3b-5ece1bb1a602")
    _ <- `π-none`(_5a4ca746_43eb_4480_a0d5_531fa972a5d5)
    _ <- (
      for {
        _ <- τ(`@`(r))("f97d1f5b-3996-479a-9b7b-118681f7eef5")
        _ <- `Greeter'`(stdout, name)(using `π-uuid`)
      } yield (),
      for {
        _ <- τ(`@`(r))("ea75fd9c-17b0-46f0-a5ee-92330e6ce0fd")
        _ <- `Greeter"`(stdout, name)(using `π-uuid`)
      } yield (),
      for {
        _ <- τ(`@`(r))("3f0442c3-5e7b-4352-9e3b-5ece1bb1a602")
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
            _ab14dcbb_33b2_4d32_a8ba_6dc2730cca19 = _root_.scala.collection.immutable.Set("83231987-dadc-4153-8dc1-79eabf341bc3")
            _ <- `π-none`(_ab14dcbb_33b2_4d32_a8ba_6dc2730cca19)
            _ <- for (_ <- stdout(null, "That's an unusual name.\n")("83231987-dadc-4153-8dc1-79eabf341bc3")) yield ()
          } yield ()
          else `𝟎`
      ) yield (),
      for (
        _ <-
          if (name ==== "Voldemort") for {
            _ <- IO.unit
            _fb75f9b4_b886_4288_99ac_b30d44cd0ae8 = _root_.scala.collection.immutable.Set("064e4eca-4e90-4691-a1a6-83f95f88a43d")
            _ <- `π-none`(_fb75f9b4_b886_4288_99ac_b30d44cd0ae8)
            _ <- for (_ <- stdout(null, "WARNING! LORD VOLDEMORT IS HERE!\n")("064e4eca-4e90-4691-a1a6-83f95f88a43d")) yield ()
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
                _e0cd3645_f7de_4ee1_bc59_eb748d19df09 = _root_.scala.collection.immutable.Set("9bc99c76-2572-4203-a862-a61a54209ed3")
                _ <- `π-none`(_e0cd3645_f7de_4ee1_bc59_eb748d19df09)
                _ <- for (_ <- stdout(null, s"Hello $name!\n")("9bc99c76-2572-4203-a862-a61a54209ed3")) yield ()
              } yield ()
          ) yield ()
      ) yield ()
    ).parMapN { (_, _, _, _) => }
  ) yield ()

  def `Greeter"`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for (
    _ <-
      if (name.substring(0, 1).toUpperCase ==== "Q") for {
        _ <- IO.unit
        _867a6630_13d6_4f3a_addd_cae7280cc5b4 = _root_.scala.collection.immutable.Set("279c6e4c-7bec-441c-aaf2-ed21dae4de17")
        _ <- `π-none`(_867a6630_13d6_4f3a_addd_cae7280cc5b4)
        _ <- for (_ <- stdout(null, "That's an unusual name.\n")("279c6e4c-7bec-441c-aaf2-ed21dae4de17")) yield ()
      } yield ()
      else for (
        _ <-
          if (name ==== "Voldemort") for {
            _ <- IO.unit
            _a6e37e4e_0c3a_4655_806e_805bf1f84ea4 = _root_.scala.collection.immutable.Set("1f704f3a-4d81-422e-a0ec-f814243fdf10")
            _ <- `π-none`(_a6e37e4e_0c3a_4655_806e_805bf1f84ea4)
            _ <- for (_ <- stdout(null, "WARNING! LORD VOLDEMORT IS HERE!\n")("1f704f3a-4d81-422e-a0ec-f814243fdf10")) yield ()
          } yield ()
          else for {
            _ <- IO.unit
            _a480d774_3830_45f0_a17f_25e0ac68a13a = _root_.scala.collection.immutable.Set("ba43adf7-ea38-4b52-b382-56acd9a0583a")
            _ <- `π-none`(_a480d774_3830_45f0_a17f_25e0ac68a13a)
            _ <- for (_ <- stdout(null, s"Hello $name!\n")("ba43adf7-ea38-4b52-b382-56acd9a0583a")) yield ()
          } yield ()
      ) yield ()
  ) yield ()

  def `Greeter"'`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *): IO[Unit] = for (
    _ <-
      if (name.substring(0, 1).toUpperCase ==== "Q") for {
        _ <- IO.unit
        _801f5e5d_96de_4023_917f_5068024008c2 = _root_.scala.collection.immutable.Set("9e8c58c3-9616-4a17-a965-b3d45aa8c9c6")
        _ <- `π-none`(_801f5e5d_96de_4023_917f_5068024008c2)
        _ <- for (_ <- stdout(null, "That's an unusual name.\n")("9e8c58c3-9616-4a17-a965-b3d45aa8c9c6")) yield ()
      } yield ()
      else for (
        _ <-
          if (name ==== "Voldemort") for {
            _ <- IO.unit
            _2be209c8_cfeb_429a_8590_27d456808581 = _root_.scala.collection.immutable.Set("a6dd01cc-98a5-4690-ac7f-8449075cfea1")
            _ <- `π-none`(_2be209c8_cfeb_429a_8590_27d456808581)
            _ <- for (_ <- stdout(null, "WARNING! LORD VOLDEMORT IS HERE!\n")("a6dd01cc-98a5-4690-ac7f-8449075cfea1")) yield ()
          } yield ()
          else for {
            _ <- IO.unit
            _8973c9b0_f666_484f_a6ae_3f3d3a62b18b = _root_.scala.collection.immutable.Set("30ad95e2-ab78-4e79-b779-523f66483aa0")
            _ <- `π-none`(_8973c9b0_f666_484f_a6ae_3f3d3a62b18b)
            _ <- for (_ <- stdout(null, s"Hello $name!\n")("30ad95e2-ab78-4e79-b779-523f66483aa0")) yield ()
          } yield ()
      ) yield ()
  ) yield ()
