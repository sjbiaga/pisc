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

  implicit val `π-wand`: `Π-Map`[String, `Π-Set`[String]] = _root_.scala.collection.immutable.Map(
    "14099e60-5bfa-4dbe-852a-215035c028b8" -> _root_.scala.collection.immutable.Set(),
    "bb2efd7b-7f1a-4f16-9b26-3cb1cf4dfeb2" -> _root_.scala.collection.immutable.Set("619aabf0-472d-403b-8753-7c152b94a802"),
    "6edb058a-4a44-414c-95d5-e495e9b57a18" -> _root_.scala.collection.immutable.Set("b6e1f764-9614-490f-9ab7-e7fd5368086d"),
    "cd9217c5-aa2c-4ff6-abe9-268915ab1322" -> _root_.scala.collection.immutable.Set(),
    "619aabf0-472d-403b-8753-7c152b94a802" -> _root_.scala.collection.immutable.Set("e2485d01-9d3e-407c-9764-0d9b16a6b6d2"),
    "aafbb929-cae5-4be0-a4c4-1228ff96dad7" -> _root_.scala.collection.immutable.Set(),
    "6b96ae00-eb6b-4f97-b598-9d9ed1a8e527" -> _root_.scala.collection.immutable.Set(),
    "88dbfef7-fe95-444b-9e50-2e42b05755b9" -> _root_.scala.collection.immutable.Set(),
    "e2485d01-9d3e-407c-9764-0d9b16a6b6d2" -> _root_.scala.collection.immutable.Set("248cd835-69fb-49ed-8eab-51b50a35fa1f"),
    "248cd835-69fb-49ed-8eab-51b50a35fa1f" -> _root_.scala.collection.immutable.Set("af3778c2-3056-45e2-8ff3-f95b2938585a"),
    "4b9cc54e-1e73-45fe-a8c2-e37c86f8fefc" -> _root_.scala.collection.immutable.Set(),
    "3439b552-c0c7-4bf7-8d5e-b22ffaa425ba" -> _root_.scala.collection.immutable.Set(),
    "590dd8da-23fc-4872-923e-fccd38cefb20" -> _root_.scala.collection.immutable.Set(),
    "c3d090ed-5083-46d0-ad85-5cd0a934c0a7" -> _root_.scala.collection.immutable.Set()
  )

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
    _ea08e699_3463_4d4d_9d36_46e583e0972b = _root_.scala.collection.immutable.Set("6edb058a-4a44-414c-95d5-e495e9b57a18", "bb2efd7b-7f1a-4f16-9b26-3cb1cf4dfeb2")
    _ <- `π-incr`(_ea08e699_3463_4d4d_9d36_46e583e0972b)
    _ <- (
      for {
        _         <- stdout(null, "What's your name?\n")("6edb058a-4a44-414c-95d5-e495e9b57a18")
        (name, _) <- stdin(null)("b6e1f764-9614-490f-9ab7-e7fd5368086d")
        _         <- Chooser(stdout, name)(using `π-uuid`)
      } yield (),
      for {
        (prompt, _) <- stdout(null)("bb2efd7b-7f1a-4f16-9b26-3cb1cf4dfeb2")
        _           <- τ(∞)("619aabf0-472d-403b-8753-7c152b94a802")
        _           <- IO {
          print(prompt)
        }
        line        <- IO.blocking {
          scala.io.StdIn.readLine
        }
        _           <- stdin(null, line)("e2485d01-9d3e-407c-9764-0d9b16a6b6d2")
        (greet, _)  <- stdout(null)("248cd835-69fb-49ed-8eab-51b50a35fa1f")
        _           <- τ(∞)("af3778c2-3056-45e2-8ff3-f95b2938585a")
        _           <- IO {
          print(greet)
        }
        _           <- pisc.fibonacci.π.Main()(using `π-uuid`)
      } yield ()
    ).parMapN { (_, _) => }
  } yield ()

  def Chooser(stdout: `()`, name: `()`)(using ^ : String)(using % : %, / : /, * : *): IO[Unit] = for {
    _ <- IO.unit
    _f047bea6_97d1_48e3_aa98_0629948c6b30 = _root_.scala.collection.immutable.Set("8db971af-9f51-402a-a1f4-5d514beda7d9", "317aab6d-df6c-4763-84b6-a26fbb41e515", "d9941f48-49a7-4ed3-9f1b-a65abbd25574")
    _ <- `π-incr`(_f047bea6_97d1_48e3_aa98_0629948c6b30)
    _ <- (
      for {
        _ <- τ(`@`(r))("8db971af-9f51-402a-a1f4-5d514beda7d9")
        _ <- `Greeter'`(stdout, name)(using `π-uuid`)
      } yield (),
      for {
        _ <- τ(`@`(r))("317aab6d-df6c-4763-84b6-a26fbb41e515")
        _ <- `Greeter"`(stdout, name)(using `π-uuid`)
      } yield (),
      for {
        _ <- τ(`@`(r))("d9941f48-49a7-4ed3-9f1b-a65abbd25574")
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
            _93d5940e_7fde_45f7_b628_580911fd3280 = _root_.scala.collection.immutable.Set("6b96ae00-eb6b-4f97-b598-9d9ed1a8e527")
            _ <- `π-incr`(_93d5940e_7fde_45f7_b628_580911fd3280)
            _ <- for {
              _ <- stdout(null, "That's an unusual name.\n")("6b96ae00-eb6b-4f97-b598-9d9ed1a8e527")
              _ <- `𝟎`
            } yield ()
          } yield ()
          else `𝟎`
      ) yield (),
      for (
        _ <-
          if (name ==== "Voldemort") for {
            _ <- IO.unit
            _a0bc68dd_c9a0_49aa_b5bf_778377318060 = _root_.scala.collection.immutable.Set("14099e60-5bfa-4dbe-852a-215035c028b8")
            _ <- `π-incr`(_a0bc68dd_c9a0_49aa_b5bf_778377318060)
            _ <- for {
              _ <- stdout(null, "WARNING! LORD VOLDEMORT IS HERE!\n")("14099e60-5bfa-4dbe-852a-215035c028b8")
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
                _7fd57c4a_89f2_4aaa_bd35_04a71f375098 = _root_.scala.collection.immutable.Set("c3d090ed-5083-46d0-ad85-5cd0a934c0a7")
                _ <- `π-incr`(_7fd57c4a_89f2_4aaa_bd35_04a71f375098)
                _ <- for {
                  _ <- stdout(null, s"Hello $name!\n")("c3d090ed-5083-46d0-ad85-5cd0a934c0a7")
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
        _c175f4a0_7d70_4471_9b67_b5cc6d7f2eac = _root_.scala.collection.immutable.Set("590dd8da-23fc-4872-923e-fccd38cefb20")
        _ <- `π-incr`(_c175f4a0_7d70_4471_9b67_b5cc6d7f2eac)
        _ <- for {
          _ <- stdout(null, "That's an unusual name.\n")("590dd8da-23fc-4872-923e-fccd38cefb20")
          _ <- `𝟎`
        } yield ()
      } yield ()
      else for (
        _ <-
          if (name ==== "Voldemort") for {
            _ <- IO.unit
            _e08f24c2_4a2c_4045_b318_f52099cb96ad = _root_.scala.collection.immutable.Set("3439b552-c0c7-4bf7-8d5e-b22ffaa425ba")
            _ <- `π-incr`(_e08f24c2_4a2c_4045_b318_f52099cb96ad)
            _ <- for {
              _ <- stdout(null, "WARNING! LORD VOLDEMORT IS HERE!\n")("3439b552-c0c7-4bf7-8d5e-b22ffaa425ba")
              _ <- `𝟎`
            } yield ()
          } yield ()
          else for {
            _ <- IO.unit
            _480c0a69_aa15_4ced_a219_fc1cd5785bd6 = _root_.scala.collection.immutable.Set("aafbb929-cae5-4be0-a4c4-1228ff96dad7")
            _ <- `π-incr`(_480c0a69_aa15_4ced_a219_fc1cd5785bd6)
            _ <- for {
              _ <- stdout(null, s"Hello $name!\n")("aafbb929-cae5-4be0-a4c4-1228ff96dad7")
              _ <- `𝟎`
            } yield ()
          } yield ()
      ) yield ()
  ) yield ()

  def `Greeter"'`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, / : /, * : *): IO[Unit] = for (
    _ <-
      if (name.substring(0, 1).toUpperCase ==== "Q") for {
        _ <- IO.unit
        _2069d47f_fb35_48d5_8573_070bc238366b = _root_.scala.collection.immutable.Set("4b9cc54e-1e73-45fe-a8c2-e37c86f8fefc")
        _ <- `π-incr`(_2069d47f_fb35_48d5_8573_070bc238366b)
        _ <- for {
          _ <- stdout(null, "That's an unusual name.\n")("4b9cc54e-1e73-45fe-a8c2-e37c86f8fefc")
          _ <- `𝟎`
        } yield ()
      } yield ()
      else for (
        _ <-
          if (name ==== "Voldemort") for {
            _ <- IO.unit
            _011063c3_cd87_4183_874f_85d0b0b33221 = _root_.scala.collection.immutable.Set("88dbfef7-fe95-444b-9e50-2e42b05755b9")
            _ <- `π-incr`(_011063c3_cd87_4183_874f_85d0b0b33221)
            _ <- for {
              _ <- stdout(null, "WARNING! LORD VOLDEMORT IS HERE!\n")("88dbfef7-fe95-444b-9e50-2e42b05755b9")
              _ <- `𝟎`
            } yield ()
          } yield ()
          else for {
            _ <- IO.unit
            _c7cd86a5_f812_4f5a_975c_bd6fefb28f70 = _root_.scala.collection.immutable.Set("cd9217c5-aa2c-4ff6-abe9-268915ab1322")
            _ <- `π-incr`(_c7cd86a5_f812_4f5a_975c_bd6fefb28f70)
            _ <- for {
              _ <- stdout(null, s"Hello $name!\n")("cd9217c5-aa2c-4ff6-abe9-268915ab1322")
              _ <- `𝟎`
            } yield ()
          } yield ()
      ) yield ()
  ) yield ()
