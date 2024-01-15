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
    "73e0abc2-282c-4659-a49a-87001da962d8" -> _root_.scala.collection.immutable.Set("0fe23043-1224-4bb9-a27a-0723d665aedf", "75b125f5-671f-4124-8163-af19578dfb76"),
    "75b125f5-671f-4124-8163-af19578dfb76" -> _root_.scala.collection.immutable.Set("73e0abc2-282c-4659-a49a-87001da962d8", "0fe23043-1224-4bb9-a27a-0723d665aedf"),
    "0fe23043-1224-4bb9-a27a-0723d665aedf" -> _root_.scala.collection.immutable.Set("73e0abc2-282c-4659-a49a-87001da962d8", "75b125f5-671f-4124-8163-af19578dfb76")
  )

  val `π-spell`: `Π-Map`[String, `Π-Set`[String]] = _root_.scala.collection.immutable.Map(
    "bb65e089-fdbf-4c10-880d-b6dd0ae0893d" -> _root_.scala.collection.immutable.Set("ef895e46-55ab-4791-b568-1a0e09f92c0e"),
    "bcf4cc01-0a3e-4ef9-89a3-83f7a5fff38d" -> _root_.scala.collection.immutable.Set("00210bee-6f8b-4abb-985b-ffcfd6329c82"),
    "121feeb5-a3a0-496b-8a39-584e0d080cfe" -> _root_.scala.collection.immutable.Set("152a6f90-389c-4e68-a032-029a0ffff2b7"),
    "bf7db1e5-0558-4e9e-8930-a548278304ba" -> _root_.scala.collection.immutable.Set("ce657c99-bc52-491c-9140-a3a869a66cde"),
    "ce657c99-bc52-491c-9140-a3a869a66cde" -> _root_.scala.collection.immutable.Set("bb65e089-fdbf-4c10-880d-b6dd0ae0893d"),
    "ef895e46-55ab-4791-b568-1a0e09f92c0e" -> _root_.scala.collection.immutable.Set("bcf4cc01-0a3e-4ef9-89a3-83f7a5fff38d")
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
    _ca4a8b1e_0d60_4dd8_860d_366b09379bf1 = _root_.scala.collection.immutable.Set("121feeb5-a3a0-496b-8a39-584e0d080cfe", "bf7db1e5-0558-4e9e-8930-a548278304ba")
    _ <- %.update(_ca4a8b1e_0d60_4dd8_860d_366b09379bf1.foldLeft(_)(_ + _))
    _ <- (
      for {
        _         <- stdout(null, "What's your name?\n")("121feeb5-a3a0-496b-8a39-584e0d080cfe")
        (name, _) <- stdin(null)("152a6f90-389c-4e68-a032-029a0ffff2b7")
        _         <- Chooser(stdout, name)(using `π-uuid`)
      } yield (),
      for {
        (prompt, _)   <- stdout(null)("bf7db1e5-0558-4e9e-8930-a548278304ba")
        _             <- τ(∞)("ce657c99-bc52-491c-9140-a3a869a66cde")
        _             <- IO {
          print(prompt)
        }
        _             <- τ(∞)("bb65e089-fdbf-4c10-880d-b6dd0ae0893d")
        sc_name       <- IO {
          scala.io.StdIn.readLine
        }
        _             <- stdin(null, sc_name)("ef895e46-55ab-4791-b568-1a0e09f92c0e")
        (pi_greet, _) <- stdout(null)("bcf4cc01-0a3e-4ef9-89a3-83f7a5fff38d")
        _             <- τ(∞)("00210bee-6f8b-4abb-985b-ffcfd6329c82")
        _             <- IO {
          print(pi_greet)
        }
        _             <- pisc.fibonacci.π.Main()(using `π-uuid`)
      } yield ()
    ).parMapN { (_, _) => }
  } yield ()

  def Chooser(stdout: `()`, name: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *, + : +, - : -): IO[Unit] = for {
    _ <- IO.unit
    _89ce1184_c568_4879_a438_fb7139b8b523 = _root_.scala.collection.immutable.Set("73e0abc2-282c-4659-a49a-87001da962d8", "0fe23043-1224-4bb9-a27a-0723d665aedf", "75b125f5-671f-4124-8163-af19578dfb76")
    _ <- %.update(_89ce1184_c568_4879_a438_fb7139b8b523.foldLeft(_)(_ + _))
    _ <- (
      for {
        _ <- τ(`@`(r))("73e0abc2-282c-4659-a49a-87001da962d8")
        _ <- `Greeter'`(stdout, name)(using `π-uuid`)
      } yield (),
      for {
        _ <- τ(`@`(r))("0fe23043-1224-4bb9-a27a-0723d665aedf")
        _ <- `Greeter"`(stdout, name)(using `π-uuid`)
      } yield (),
      for {
        _ <- τ(`@`(r))("75b125f5-671f-4124-8163-af19578dfb76")
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
            _9ba4b391_7add_401e_ac6e_0db901f13a25 = _root_.scala.collection.immutable.Set("e03f89d4-3b93-4299-9ef7-0af5161e37ec")
            _ <- %.update(_9ba4b391_7add_401e_ac6e_0db901f13a25.foldLeft(_)(_ + _))
            _ <- for (_ <- stdout(null, "That's an unusual name.\n")("e03f89d4-3b93-4299-9ef7-0af5161e37ec")) yield ()
          } yield ()
          else `𝟎`
      ) yield (),
      for (
        _ <-
          if (name === "Voldemort") for {
            _ <- IO.unit
            _ad9dd3be_95b6_479d_8dfd_e19c4af28498 = _root_.scala.collection.immutable.Set("490fb075-6fc5-445d-9343-81de2b587686")
            _ <- %.update(_ad9dd3be_95b6_479d_8dfd_e19c4af28498.foldLeft(_)(_ + _))
            _ <- for (_ <- stdout(null, "WARNING! LORD VOLDEMORT IS HERE!\n")("490fb075-6fc5-445d-9343-81de2b587686")) yield ()
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
                _c106ef05_a766_44d9_acde_78dce81fc1b6 = _root_.scala.collection.immutable.Set("d30ae7fe-e627-425c-91a1-5b8011955636")
                _ <- %.update(_c106ef05_a766_44d9_acde_78dce81fc1b6.foldLeft(_)(_ + _))
                _ <- for {
                  _ <- stdout(null, s"Hello $name!\n")("d30ae7fe-e627-425c-91a1-5b8011955636")
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
        _5e6bc916_ee7c_4f13_a799_51ec7d89bb77 = _root_.scala.collection.immutable.Set("22a90fd2-3d4b-4eb9-a005-f867b51ac060")
        _ <- %.update(_5e6bc916_ee7c_4f13_a799_51ec7d89bb77.foldLeft(_)(_ + _))
        _ <- for {
          _ <- stdout(null, "That's an unusual name.\n")("22a90fd2-3d4b-4eb9-a005-f867b51ac060")
          _ <- `𝟎`
        } yield ()
      } yield ()
      else for (
        _ <-
          if (name === "Voldemort") for {
            _ <- IO.unit
            _7ffea3a2_7ab3_4957_944c_20d97931f503 = _root_.scala.collection.immutable.Set("6c5654b0-540e-4ab7-8544-89c3847b3def")
            _ <- %.update(_7ffea3a2_7ab3_4957_944c_20d97931f503.foldLeft(_)(_ + _))
            _ <- for {
              _ <- stdout(null, "WARNING! LORD VOLDEMORT IS HERE!\n")("6c5654b0-540e-4ab7-8544-89c3847b3def")
              _ <- `𝟎`
            } yield ()
          } yield ()
          else for {
            _ <- IO.unit
            _eab41f88_04fe_48ab_a22c_3fe16b3ee413 = _root_.scala.collection.immutable.Set("5a0a8d8e-7a1d-4e68-87c4-b3fb6da5ac0b")
            _ <- %.update(_eab41f88_04fe_48ab_a22c_3fe16b3ee413.foldLeft(_)(_ + _))
            _ <- for {
              _ <- stdout(null, s"Hello $name!\n")("5a0a8d8e-7a1d-4e68-87c4-b3fb6da5ac0b")
              _ <- `𝟎`
            } yield ()
          } yield ()
      ) yield ()
  ) yield ()

  def `Greeter"'`(stdout: `()`, name: `()`)(using ^ : String)(using % : %, \ : \, / : /, * : *, + : +, - : -): IO[Unit] = for (
    _ <-
      if (name.substring(0, 1).toUpperCase === "Q") for {
        _ <- IO.unit
        _e4b4e15c_1524_458b_a180_f95a140b3529 = _root_.scala.collection.immutable.Set("29262532-5f16-435a-9355-fda136b26400")
        _ <- %.update(_e4b4e15c_1524_458b_a180_f95a140b3529.foldLeft(_)(_ + _))
        _ <- for {
          _ <- stdout(null, "That's an unusual name.\n")("29262532-5f16-435a-9355-fda136b26400")
          _ <- `𝟎`
        } yield ()
      } yield ()
      else for (
        _ <-
          if (name === "Voldemort") for {
            _ <- IO.unit
            _04fc2cd7_543f_4baf_a7d9_1f371cd4df78 = _root_.scala.collection.immutable.Set("1da0b843-8992-4114-9533-23e9339d7d91")
            _ <- %.update(_04fc2cd7_543f_4baf_a7d9_1f371cd4df78.foldLeft(_)(_ + _))
            _ <- for {
              _ <- stdout(null, "WARNING! LORD VOLDEMORT IS HERE!\n")("1da0b843-8992-4114-9533-23e9339d7d91")
              _ <- `𝟎`
            } yield ()
          } yield ()
          else for {
            _ <- IO.unit
            _faeece69_f21d_46e3_9f7c_3f566f052a77 = _root_.scala.collection.immutable.Set("1d0f999d-1c7e-43a2-b542-4c219fe8c1f5")
            _ <- %.update(_faeece69_f21d_46e3_9f7c_3f566f052a77.foldLeft(_)(_ + _))
            _ <- for {
              _ <- stdout(null, s"Hello $name!\n")("1d0f999d-1c7e-43a2-b542-4c219fe8c1f5")
              _ <- `𝟎`
            } yield ()
          } yield ()
      ) yield ()
  ) yield ()
