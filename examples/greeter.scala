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
import _root_.cats.effect.std.{CyclicBarrier, Queue, Semaphore}

import `Π-loop`._
import `Π-stats`.{Rate, ∞, `@`}

object App extends IOApp.Simple:

  private def run(% : %, / : /, * : *, - : -): IO[Unit] = (for
    _ <- loop(π.`π-trick`)(using %, *, -).background
    _ <- poll(using %, /, *).background
  yield ()).use { _ =>
    for _ <- π.Main()(using π.`π-uuid`)(using %, /, -)
    yield ()
  }

  override def run: IO[Unit] =
    for
      % <- IO.ref(Map[String, Int | +]())
      / <- Queue.unbounded[IO, ((String, String), +)]
      * <- Semaphore[IO](1)
      - <- CyclicBarrier[IO](3)
      _ <- run(%, /, *, -)
    yield ()

object π:

  import _root_.java.util.UUID

  import _root_.cats.effect.syntax.all._
  import _root_.cats.syntax.all._

  import sΠ._

  private val `𝟎` = IO.unit

  def `π-uuid` = UUID.randomUUID.toString

  val `π-trick`: `Π-Map`[String, `Π-Set`[String]] = _root_.scala.collection.immutable.Map(
    "7d2e820e-41f3-42c1-8025-937fca58596a" -> _root_.scala.collection.immutable.Set("68a45dd2-9135-4406-8cad-3d0a3f8cae4c", "60b1ecec-e0de-40e7-99a7-58437733c5b6"),
    "68a45dd2-9135-4406-8cad-3d0a3f8cae4c" -> _root_.scala.collection.immutable.Set("7d2e820e-41f3-42c1-8025-937fca58596a", "60b1ecec-e0de-40e7-99a7-58437733c5b6"),
    "60b1ecec-e0de-40e7-99a7-58437733c5b6" -> _root_.scala.collection.immutable.Set("68a45dd2-9135-4406-8cad-3d0a3f8cae4c", "7d2e820e-41f3-42c1-8025-937fca58596a")
  )

  val `π-spell`: `Π-Map`[String, `Π-Set`[String]] = _root_.scala.collection.immutable.Map(
    "8f44b089-b619-42fe-aeba-efc116a5378f" -> _root_.scala.collection.immutable.Set(),
    "c98a7d07-3a36-4123-bc77-d99f84406812" -> _root_.scala.collection.immutable.Set("1533a058-2778-460d-8b57-2e7fa86d7441"),
    "1533a058-2778-460d-8b57-2e7fa86d7441" -> _root_.scala.collection.immutable.Set("8f44b089-b619-42fe-aeba-efc116a5378f"),
    "a474bc2f-e315-430c-bcb7-d1d5ed42c883" -> _root_.scala.collection.immutable.Set(),
    "fccb150f-4123-4190-a2c7-18bf2de30bcd" -> _root_.scala.collection.immutable.Set(),
    "f4050095-0b44-47ec-a735-138d2f505b96" -> _root_.scala.collection.immutable.Set("c98a7d07-3a36-4123-bc77-d99f84406812"),
    "abc7be51-3585-494a-b250-c8557ab0985e" -> _root_.scala.collection.immutable.Set(),
    "0ee04354-ffa9-40ab-a578-00193a6cfcfe" -> _root_.scala.collection.immutable.Set("f4050095-0b44-47ec-a735-138d2f505b96"),
    "086c2e19-82a9-4746-a64e-02d3f2546cce" -> _root_.scala.collection.immutable.Set(),
    "5cfd7fc4-7394-4686-873f-61e81b481182" -> _root_.scala.collection.immutable.Set(),
    "a87b02e1-c1be-4cbb-90dc-6e8796439917" -> _root_.scala.collection.immutable.Set(),
    "a6bbe125-b126-4c26-9b2e-d7d675807ca9" -> _root_.scala.collection.immutable.Set("ec0155af-0d15-4b86-96d9-3b7516a830e2"),
    "9c3f86f5-689a-4718-a54d-1b8d95ff0315" -> _root_.scala.collection.immutable.Set(),
    "8fe60499-5bb4-485d-9cc3-c95a350bceeb" -> _root_.scala.collection.immutable.Set(),
    "3ecdc930-cb8a-40e6-af13-8736037dce6b" -> _root_.scala.collection.immutable.Set()
  )

  implicit val `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) = `π-trick` -> `π-spell`

  given Conversion[`()`, String] = _.name.asInstanceOf[String]

  val gen = new scala.util.Random

  def r = 1 + math.abs(gen.nextInt % 3)

  def Main()(using ^ : String)(using % : %, / : /, - : -): IO[Unit] = for {
    stdin  <- ν
    stdout <- ν
    _      <- Greeter(stdin, stdout, "")(using `π-uuid`)
  } yield ()

  def Greeter(stdin: `()`, stdout: `()`, line: `()`)(using ^ : String)(using % : %, / : /, - : -): IO[Unit] = for {
    _ <- IO.unit
    _ffd0eb50_c136_4e25_8701_6d536f001c01 = _root_.scala.collection.immutable.Set("a6bbe125-b126-4c26-9b2e-d7d675807ca9", "0ee04354-ffa9-40ab-a578-00193a6cfcfe")
    _ <- `π-incr`(_ffd0eb50_c136_4e25_8701_6d536f001c01)
    _ <- (
      for {
        _         <- stdout(null, "What's your name?\n")("a6bbe125-b126-4c26-9b2e-d7d675807ca9")
        (name, _) <- stdin(null)("ec0155af-0d15-4b86-96d9-3b7516a830e2")
        _         <- Chooser(stdout, name)(using `π-uuid`)
      } yield (),
      for {
        (prompt, _) <- stdout(null)("0ee04354-ffa9-40ab-a578-00193a6cfcfe")
        _           <- τ(∞)("f4050095-0b44-47ec-a735-138d2f505b96")
        _           <- IO {
          print(prompt)
        }
        line        <- IO.blocking {
          scala.io.StdIn.readLine
        }
        _           <- stdin(null, line)("c98a7d07-3a36-4123-bc77-d99f84406812")
        (greet, _)  <- stdout(null)("1533a058-2778-460d-8b57-2e7fa86d7441")
        _           <- τ(∞)("8f44b089-b619-42fe-aeba-efc116a5378f")
        _           <- IO {
          print(greet)
        }
        _           <- `𝟎`
      } yield ()
    ).parMapN { (_, _) => }
  } yield ()

  def Chooser(stdout: `()`, name: `()`)(using ^ : String)(using % : %, / : /, - : -): IO[Unit] = for {
    _ <- IO.unit
    _7625a0f4_cc40_4653_a892_dbcf3c8298cd = _root_.scala.collection.immutable.Set("68a45dd2-9135-4406-8cad-3d0a3f8cae4c", "7d2e820e-41f3-42c1-8025-937fca58596a", "60b1ecec-e0de-40e7-99a7-58437733c5b6")
    _ <- `π-incr`(_7625a0f4_cc40_4653_a892_dbcf3c8298cd)
    _ <- (
      for {
        _ <- τ(`@`(r))("68a45dd2-9135-4406-8cad-3d0a3f8cae4c")
        _ <- `Greeter'`(stdout, name)(using `π-uuid`)
      } yield (),
      for {
        _ <- τ(`@`(r))("7d2e820e-41f3-42c1-8025-937fca58596a")
        _ <- `Greeter"`(stdout, name)(using `π-uuid`)
      } yield (),
      for {
        _ <- τ(`@`(r))("60b1ecec-e0de-40e7-99a7-58437733c5b6")
        _ <- `Greeter"'`(stdout, name)(using `π-uuid`)
      } yield ()
    ).parMapN { (_, _, _) => }
  } yield ()

  def `Greeter'`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, / : /, - : -): IO[Unit] = for (
    _ <- (
      `𝟎`,
      for (
        _ <-
          if (name.substring(0, 1).toUpperCase ==== "Q") for {
            _ <- IO.unit
            _2bcc54cb_457c_4685_bdc3_9826ce65fe8e = _root_.scala.collection.immutable.Set("a474bc2f-e315-430c-bcb7-d1d5ed42c883")
            _ <- `π-incr`(_2bcc54cb_457c_4685_bdc3_9826ce65fe8e)
            _ <- for {
              _ <- stdout(null, "That's an unusual name.\n")("a474bc2f-e315-430c-bcb7-d1d5ed42c883")
              _ <- `𝟎`
            } yield ()
          } yield ()
          else `𝟎`
      ) yield (),
      for (
        _ <-
          if (name ==== "Voldemort") for {
            _ <- IO.unit
            _8beddfab_c038_4970_a6f3_d7a22cde9704 = _root_.scala.collection.immutable.Set("5cfd7fc4-7394-4686-873f-61e81b481182")
            _ <- `π-incr`(_8beddfab_c038_4970_a6f3_d7a22cde9704)
            _ <- for {
              _ <- stdout(null, "WARNING! LORD VOLDEMORT IS HERE!\n")("5cfd7fc4-7394-4686-873f-61e81b481182")
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
                _b2d0ec05_2446_46d6_8667_477808cb5894 = _root_.scala.collection.immutable.Set("fccb150f-4123-4190-a2c7-18bf2de30bcd")
                _ <- `π-incr`(_b2d0ec05_2446_46d6_8667_477808cb5894)
                _ <- for {
                  _ <- stdout(null, s"Hello $name!\n")("fccb150f-4123-4190-a2c7-18bf2de30bcd")
                  _ <- `𝟎`
                } yield ()
              } yield ()
          ) yield ()
      ) yield ()
    ).parMapN { (_, _, _, _) => }
  ) yield ()

  def `Greeter"`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, / : /, - : -): IO[Unit] = for (
    _ <-
      if (name.substring(0, 1).toUpperCase ==== "Q") for {
        _ <- IO.unit
        _474a2be5_7f88_443b_b42d_aedf8d21cff5 = _root_.scala.collection.immutable.Set("3ecdc930-cb8a-40e6-af13-8736037dce6b")
        _ <- `π-incr`(_474a2be5_7f88_443b_b42d_aedf8d21cff5)
        _ <- for {
          _ <- stdout(null, "That's an unusual name.\n")("3ecdc930-cb8a-40e6-af13-8736037dce6b")
          _ <- `𝟎`
        } yield ()
      } yield ()
      else for (
        _ <-
          if (name ==== "Voldemort") for {
            _ <- IO.unit
            _ca894578_d791_4a08_8c99_9efee1f48a4f = _root_.scala.collection.immutable.Set("8fe60499-5bb4-485d-9cc3-c95a350bceeb")
            _ <- `π-incr`(_ca894578_d791_4a08_8c99_9efee1f48a4f)
            _ <- for {
              _ <- stdout(null, "WARNING! LORD VOLDEMORT IS HERE!\n")("8fe60499-5bb4-485d-9cc3-c95a350bceeb")
              _ <- `𝟎`
            } yield ()
          } yield ()
          else for {
            _ <- IO.unit
            _2f97f3ae_af5a_4f4f_bc4e_5f331d93be04 = _root_.scala.collection.immutable.Set("a87b02e1-c1be-4cbb-90dc-6e8796439917")
            _ <- `π-incr`(_2f97f3ae_af5a_4f4f_bc4e_5f331d93be04)
            _ <- for {
              _ <- stdout(null, s"Hello $name!\n")("a87b02e1-c1be-4cbb-90dc-6e8796439917")
              _ <- `𝟎`
            } yield ()
          } yield ()
      ) yield ()
  ) yield ()

  def `Greeter"'`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, / : /, - : -): IO[Unit] = for (
    _ <-
      if (name.substring(0, 1).toUpperCase ==== "Q") for {
        _ <- IO.unit
        _0ff3b70e_83c7_4249_8fa7_03868cd4bdf6 = _root_.scala.collection.immutable.Set("abc7be51-3585-494a-b250-c8557ab0985e")
        _ <- `π-incr`(_0ff3b70e_83c7_4249_8fa7_03868cd4bdf6)
        _ <- for {
          _ <- stdout(null, "That's an unusual name.\n")("abc7be51-3585-494a-b250-c8557ab0985e")
          _ <- `𝟎`
        } yield ()
      } yield ()
      else for (
        _ <-
          if (name ==== "Voldemort") for {
            _ <- IO.unit
            _980034c5_0afd_4978_9c25_7a14ecc71f15 = _root_.scala.collection.immutable.Set("086c2e19-82a9-4746-a64e-02d3f2546cce")
            _ <- `π-incr`(_980034c5_0afd_4978_9c25_7a14ecc71f15)
            _ <- for {
              _ <- stdout(null, "WARNING! LORD VOLDEMORT IS HERE!\n")("086c2e19-82a9-4746-a64e-02d3f2546cce")
              _ <- `𝟎`
            } yield ()
          } yield ()
          else for {
            _ <- IO.unit
            _855536e9_685a_4584_af73_22f9aedcc7b3 = _root_.scala.collection.immutable.Set("9c3f86f5-689a-4718-a54d-1b8d95ff0315")
            _ <- `π-incr`(_855536e9_685a_4584_af73_22f9aedcc7b3)
            _ <- for {
              _ <- stdout(null, s"Hello $name!\n")("9c3f86f5-689a-4718-a54d-1b8d95ff0315")
              _ <- `𝟎`
            } yield ()
          } yield ()
      ) yield ()
  ) yield ()
