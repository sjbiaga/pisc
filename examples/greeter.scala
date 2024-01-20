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

  val `π-trick`: `Π-Map`[String, `Π-Set`[String]] = _root_.scala.collection.immutable.Map(
    "66846938-e433-4880-ae2d-e894827722b3" -> _root_.scala.collection.immutable.Set("d21db7b0-7755-4389-b569-064c6785dec4", "4c603b88-1a8a-491b-889e-efe270345689"),
    "d21db7b0-7755-4389-b569-064c6785dec4" -> _root_.scala.collection.immutable.Set("66846938-e433-4880-ae2d-e894827722b3", "4c603b88-1a8a-491b-889e-efe270345689"),
    "4c603b88-1a8a-491b-889e-efe270345689" -> _root_.scala.collection.immutable.Set("d21db7b0-7755-4389-b569-064c6785dec4", "66846938-e433-4880-ae2d-e894827722b3")
  )

  val `π-spell`: `Π-Map`[String, `Π-Set`[String]] = _root_.scala.collection.immutable.Map(
    "04fb19f3-da0d-4632-8b0b-73993c45df9a" -> _root_.scala.collection.immutable.Set("8c0b80b7-144b-48c8-aa45-41e0abbbc9b9"),
    "833bd049-447c-46fc-9612-186c6cc88d39" -> _root_.scala.collection.immutable.Set(),
    "6e2bad1f-19bb-4a40-b703-074a93ebba9d" -> _root_.scala.collection.immutable.Set(),
    "77caaf7c-0c21-4bf9-bedc-69fb1842688b" -> _root_.scala.collection.immutable.Set(),
    "8c0b80b7-144b-48c8-aa45-41e0abbbc9b9" -> _root_.scala.collection.immutable.Set("a3574b40-7571-4fb6-ab5b-d22bea554568"),
    "88e3f010-e0f0-4017-87af-afec69d740a1" -> _root_.scala.collection.immutable.Set(),
    "d5fad8f8-4489-4187-97cf-0e2c7f9565db" -> _root_.scala.collection.immutable.Set("4deff850-0da9-49e3-8d97-29d0a0d45200"),
    "c21c0022-e097-4dc8-9860-75f13052023e" -> _root_.scala.collection.immutable.Set(),
    "d5ea8fbb-3fd8-48b1-ac67-57a5d0c638d1" -> _root_.scala.collection.immutable.Set(),
    "a3574b40-7571-4fb6-ab5b-d22bea554568" -> _root_.scala.collection.immutable.Set("744742b0-884a-4cf3-81bf-12a4ca324788"),
    "d8ef5975-6315-40e7-8796-3fef599246af" -> _root_.scala.collection.immutable.Set("04fb19f3-da0d-4632-8b0b-73993c45df9a"),
    "0cf6d0b4-c3ce-425c-bfff-db0a34b1f572" -> _root_.scala.collection.immutable.Set(),
    "0bb2d658-8fbb-4d5e-8bd0-ec2e286ba102" -> _root_.scala.collection.immutable.Set(),
    "b7a8f1a1-fb34-41ac-b67c-d9f4b5497235" -> _root_.scala.collection.immutable.Set()
  )

  implicit val `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) = `π-trick` -> `π-spell`

  given Conversion[`()`, String] = _.name.asInstanceOf[String]

  val gen = new scala.util.Random

  def r = 1 + math.abs(gen.nextInt % 3)

  def Main()(using ^ : String)(using % : %, / : /, * : *): IO[Unit] = for {
    stdin  <- ν
    stdout <- ν
    _      <- Greeter(stdin, stdout, "")(using `π-uuid`)
  } yield ()

  def Greeter(stdin: `()`, stdout: `()`, line: `()`)(using ^ : String)(using % : %, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _d44ba8f6_459b_405c_9b2c_610e0ccd9ffd = _root_.scala.collection.immutable.Set("d5fad8f8-4489-4187-97cf-0e2c7f9565db", "d8ef5975-6315-40e7-8796-3fef599246af")
    _ <- `π-incr`(_d44ba8f6_459b_405c_9b2c_610e0ccd9ffd)
    _ <- (
      for {
        _         <- stdout(null, "What's your name?\n")("d5fad8f8-4489-4187-97cf-0e2c7f9565db")
        (name, _) <- stdin(null)("4deff850-0da9-49e3-8d97-29d0a0d45200")
        _         <- Chooser(stdout, name)(using `π-uuid`)
      } yield (),
      for {
        (prompt, _) <- stdout(null)("d8ef5975-6315-40e7-8796-3fef599246af")
        _           <- τ(∞)("04fb19f3-da0d-4632-8b0b-73993c45df9a")
        _           <- IO {
          print(prompt)
        }
        line        <- IO.blocking {
          scala.io.StdIn.readLine
        }
        _           <- stdin(null, line)("8c0b80b7-144b-48c8-aa45-41e0abbbc9b9")
        (greet, _)  <- stdout(null)("a3574b40-7571-4fb6-ab5b-d22bea554568")
        _           <- τ(∞)("744742b0-884a-4cf3-81bf-12a4ca324788")
        _           <- IO {
          print(greet)
        }
        _           <- pisc.fibonacci.π.Main()(using `π-uuid`)
      } yield ()
    ).parMapN { (_, _) => }
  } yield ()

  def Chooser(stdout: `()`, name: `()`)(using ^ : String)(using % : %, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _26e603d2_a960_48bd_aee5_e7b752a024e0 = _root_.scala.collection.immutable.Set("d21db7b0-7755-4389-b569-064c6785dec4", "66846938-e433-4880-ae2d-e894827722b3", "4c603b88-1a8a-491b-889e-efe270345689")
    _ <- `π-incr`(_26e603d2_a960_48bd_aee5_e7b752a024e0)
    _ <- (
      for {
        _ <- τ(`@`(r))("d21db7b0-7755-4389-b569-064c6785dec4")
        _ <- `Greeter'`(stdout, name)(using `π-uuid`)
      } yield (),
      for {
        _ <- τ(`@`(r))("66846938-e433-4880-ae2d-e894827722b3")
        _ <- `Greeter"`(stdout, name)(using `π-uuid`)
      } yield (),
      for {
        _ <- τ(`@`(r))("4c603b88-1a8a-491b-889e-efe270345689")
        _ <- `Greeter"'`(stdout, name)(using `π-uuid`)
      } yield ()
    ).parMapN { (_, _, _) => }
  } yield ()

  def `Greeter'`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, / : /, * : *): IO[Unit] = for (
    _ <- (
      `𝟎`,
      for (
        _ <-
          if (name.substring(0, 1).toUpperCase ==== "Q") for {
            _ <- IO.unit
            _1c652913_b487_4fbf_827f_5460bc031b1d = _root_.scala.collection.immutable.Set("0cf6d0b4-c3ce-425c-bfff-db0a34b1f572")
            _ <- `π-incr`(_1c652913_b487_4fbf_827f_5460bc031b1d)
            _ <- for {
              _ <- stdout(null, "That's an unusual name.\n")("0cf6d0b4-c3ce-425c-bfff-db0a34b1f572")
              _ <- `𝟎`
            } yield ()
          } yield ()
          else `𝟎`
      ) yield (),
      for (
        _ <-
          if (name ==== "Voldemort") for {
            _ <- IO.unit
            _7f58fc6c_c3a8_437e_ba0c_8e9a9078650e = _root_.scala.collection.immutable.Set("77caaf7c-0c21-4bf9-bedc-69fb1842688b")
            _ <- `π-incr`(_7f58fc6c_c3a8_437e_ba0c_8e9a9078650e)
            _ <- for {
              _ <- stdout(null, "WARNING! LORD VOLDEMORT IS HERE!\n")("77caaf7c-0c21-4bf9-bedc-69fb1842688b")
              _ <- `𝟎`
            } yield ()
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
                _8bb01e6d_8c49_4166_aac8_3f78d2c6daa7 = _root_.scala.collection.immutable.Set("88e3f010-e0f0-4017-87af-afec69d740a1")
                _ <- `π-incr`(_8bb01e6d_8c49_4166_aac8_3f78d2c6daa7)
                _ <- for {
                  _ <- stdout(null, s"Hello $name!\n")("88e3f010-e0f0-4017-87af-afec69d740a1")
                  _ <- `𝟎`
                } yield ()
              } yield ()
          ) yield ()
      ) yield ()
    ).parMapN { (_, _, _, _) => }
  ) yield ()

  def `Greeter"`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, / : /, * : *): IO[Unit] = for (
    _ <-
      if (name.substring(0, 1).toUpperCase ==== "Q") for {
        _ <- IO.unit
        _2c387e2f_e665_475c_8f9f_059e33e2259e = _root_.scala.collection.immutable.Set("d5ea8fbb-3fd8-48b1-ac67-57a5d0c638d1")
        _ <- `π-incr`(_2c387e2f_e665_475c_8f9f_059e33e2259e)
        _ <- for {
          _ <- stdout(null, "That's an unusual name.\n")("d5ea8fbb-3fd8-48b1-ac67-57a5d0c638d1")
          _ <- `𝟎`
        } yield ()
      } yield ()
      else for (
        _ <-
          if (name ==== "Voldemort") for {
            _ <- IO.unit
            _3136a0d2_e21c_43c0_a1c0_081706f6d73d = _root_.scala.collection.immutable.Set("833bd049-447c-46fc-9612-186c6cc88d39")
            _ <- `π-incr`(_3136a0d2_e21c_43c0_a1c0_081706f6d73d)
            _ <- for {
              _ <- stdout(null, "WARNING! LORD VOLDEMORT IS HERE!\n")("833bd049-447c-46fc-9612-186c6cc88d39")
              _ <- `𝟎`
            } yield ()
          } yield ()
          else for {
            _ <- IO.unit
            _30f6cd7d_1924_4dcc_bdef_cbee30838a2e = _root_.scala.collection.immutable.Set("6e2bad1f-19bb-4a40-b703-074a93ebba9d")
            _ <- `π-incr`(_30f6cd7d_1924_4dcc_bdef_cbee30838a2e)
            _ <- for {
              _ <- stdout(null, s"Hello $name!\n")("6e2bad1f-19bb-4a40-b703-074a93ebba9d")
              _ <- `𝟎`
            } yield ()
          } yield ()
      ) yield ()
  ) yield ()

  def `Greeter"'`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, / : /, * : *): IO[Unit] = for (
    _ <-
      if (name.substring(0, 1).toUpperCase ==== "Q") for {
        _ <- IO.unit
        _d0eff868_8354_4a88_9546_1da47f8cab5a = _root_.scala.collection.immutable.Set("b7a8f1a1-fb34-41ac-b67c-d9f4b5497235")
        _ <- `π-incr`(_d0eff868_8354_4a88_9546_1da47f8cab5a)
        _ <- for {
          _ <- stdout(null, "That's an unusual name.\n")("b7a8f1a1-fb34-41ac-b67c-d9f4b5497235")
          _ <- `𝟎`
        } yield ()
      } yield ()
      else for (
        _ <-
          if (name ==== "Voldemort") for {
            _ <- IO.unit
            _f747ca53_21b2_41d7_92d7_4afda39d0773 = _root_.scala.collection.immutable.Set("c21c0022-e097-4dc8-9860-75f13052023e")
            _ <- `π-incr`(_f747ca53_21b2_41d7_92d7_4afda39d0773)
            _ <- for {
              _ <- stdout(null, "WARNING! LORD VOLDEMORT IS HERE!\n")("c21c0022-e097-4dc8-9860-75f13052023e")
              _ <- `𝟎`
            } yield ()
          } yield ()
          else for {
            _ <- IO.unit
            _e487b10e_d438_4b75_924d_d7c7432d41e4 = _root_.scala.collection.immutable.Set("0bb2d658-8fbb-4d5e-8bd0-ec2e286ba102")
            _ <- `π-incr`(_e487b10e_d438_4b75_924d_d7c7432d41e4)
            _ <- for {
              _ <- stdout(null, s"Hello $name!\n")("0bb2d658-8fbb-4d5e-8bd0-ec2e286ba102")
              _ <- `𝟎`
            } yield ()
          } yield ()
      ) yield ()
  ) yield ()
