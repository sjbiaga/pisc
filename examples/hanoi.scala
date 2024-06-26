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
    _root_.scala.collection.immutable.Map()

  val `π-spell`: `Π-Map`[String, `Π-Set`[String]] =
    _root_.scala.collection.immutable.Map(
      "8f607ca7-c033-4ac2-b406-65187b2d2fca" -> _root_.scala.collection.immutable
        .Set(),
      "9f9bfe93-c828-474d-8002-4877160c634e" -> _root_.scala.collection.immutable
        .Set("6c16acc1-776a-4937-a36b-ffa3af0291c0"),
      "cec5c4de-5c4d-4ac0-82ab-f9dc7dc92736" -> _root_.scala.collection.immutable
        .Set("04f33f5d-b689-4765-97db-a113b82c30e5"),
      "04f33f5d-b689-4765-97db-a113b82c30e5" -> _root_.scala.collection.immutable
        .Set(),
      "4d4a87d4-ec21-4415-aba7-6ced8ca4985e" -> _root_.scala.collection.immutable
        .Set("4d4a87d4-ec21-4415-aba7-6ced8ca4985e"),
      "752c6515-e3db-4617-8dad-7a35a8f34876" -> _root_.scala.collection.immutable
        .Set(),
      "731df9a8-7f94-4fa4-9319-601311fc31f1" -> _root_.scala.collection.immutable
        .Set("9f9bfe93-c828-474d-8002-4877160c634e"),
      "5a771948-d5c2-41b6-b238-0af9eda601c0" -> _root_.scala.collection.immutable
        .Set("8f607ca7-c033-4ac2-b406-65187b2d2fca"),
      "665f4e8e-6da2-49bb-8302-02b24902d5f8" -> _root_.scala.collection.immutable
        .Set("665f4e8e-6da2-49bb-8302-02b24902d5f8"),
      "cb1b0dbf-2333-4f32-a31c-00bae1e14aef" -> _root_.scala.collection.immutable
        .Set("6a1cbe42-6fc1-48ed-bcf3-561d6908441f"),
      "6a1cbe42-6fc1-48ed-bcf3-561d6908441f" -> _root_.scala.collection.immutable
        .Set("cec5c4de-5c4d-4ac0-82ab-f9dc7dc92736"),
      "6b46d943-c838-478a-a754-4243f38b2583" -> _root_.scala.collection.immutable
        .Set("06d60ab1-7cf9-4a4c-8a07-094b25d7e197"),
      "f42a7288-f402-4e00-9391-a179763cb3f4" -> _root_.scala.collection.immutable
        .Set(),
      "39cb9620-399f-49b6-900d-213f64ded6ef" -> _root_.scala.collection.immutable
        .Set("6b46d943-c838-478a-a754-4243f38b2583"),
      "51407f78-7d3a-4eb3-aba6-b770fab6055a" -> _root_.scala.collection.immutable
        .Set("51407f78-7d3a-4eb3-aba6-b770fab6055a"),
      "6c16acc1-776a-4937-a36b-ffa3af0291c0" -> _root_.scala.collection.immutable
        .Set("752c6515-e3db-4617-8dad-7a35a8f34876"),
      "cbeaf5cf-964d-45df-bc3f-93037229c675" -> _root_.scala.collection.immutable
        .Set("cb1b0dbf-2333-4f32-a31c-00bae1e14aef"),
      "06d60ab1-7cf9-4a4c-8a07-094b25d7e197" -> _root_.scala.collection.immutable
        .Set("f42a7288-f402-4e00-9391-a179763cb3f4")
    )

  implicit val `π-wand`
    : (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) =
    `π-trick` -> `π-spell`

  given Conversion[`()`, Int] = _.as[Int]

  val ! : String => IO[String] = { _ => IO.pure(null) }

  def Main(args: String*)(using ^ : String)(using % : %, / : /): IO[Unit] = for {
    _ <- IO.unit
    _ef9545fa_a2b0_40d0_903b_02a7d8b642e4 =
      _root_.scala.collection.immutable.Set(
        "665f4e8e-6da2-49bb-8302-02b24902d5f8",
        "51407f78-7d3a-4eb3-aba6-b770fab6055a",
        "4d4a87d4-ec21-4415-aba7-6ced8ca4985e"
      )
    _ <- `π-enable`(_ef9545fa_a2b0_40d0_903b_02a7d8b642e4)
    a <- ν
    b <- ν
    c <- ν
    _ <- (
      `π-supervised`(for {
        _58b7ecc6_4e3e_4632_bd35_84cdc2ef2d50 <- IO {
          lazy val _58b7ecc6_4e3e_4632_bd35_84cdc2ef2d50: String => IO[Unit] = {
            implicit ^ =>
              (
                IO.unit,
                `π-supervised`(for {
                  _d7dcaacd_4487_4931_8181_18815823d3ad <-
                    a(⊤(1L), "a")("665f4e8e-6da2-49bb-8302-02b24902d5f8")
                  _                                     <-
                    if (_d7dcaacd_4487_4931_8181_18815823d3ad == null) IO.cede
                    else _58b7ecc6_4e3e_4632_bd35_84cdc2ef2d50(`π-uuid`)
                } yield ())
              ).parMapN { (_, _) => }
          }
          _58b7ecc6_4e3e_4632_bd35_84cdc2ef2d50
        }
        _d7dcaacd_4487_4931_8181_18815823d3ad <-
          a(⊤(1L), "a")("665f4e8e-6da2-49bb-8302-02b24902d5f8")
        _                                     <-
          if (_d7dcaacd_4487_4931_8181_18815823d3ad == null) IO.cede
          else _58b7ecc6_4e3e_4632_bd35_84cdc2ef2d50(`π-uuid`)
      } yield ()),
      `π-supervised`(for {
        _2ce767da_4378_4f1d_a286_3d6da6dceee4 <- IO {
          lazy val _2ce767da_4378_4f1d_a286_3d6da6dceee4: String => IO[Unit] = {
            implicit ^ =>
              (
                IO.unit,
                `π-supervised`(for {
                  _b0ac1280_f575_4484_9b6c_e9f657a15908 <-
                    b(⊤(1L), "b")("51407f78-7d3a-4eb3-aba6-b770fab6055a")
                  _                                     <-
                    if (_b0ac1280_f575_4484_9b6c_e9f657a15908 == null) IO.cede
                    else _2ce767da_4378_4f1d_a286_3d6da6dceee4(`π-uuid`)
                } yield ())
              ).parMapN { (_, _) => }
          }
          _2ce767da_4378_4f1d_a286_3d6da6dceee4
        }
        _b0ac1280_f575_4484_9b6c_e9f657a15908 <-
          b(⊤(1L), "b")("51407f78-7d3a-4eb3-aba6-b770fab6055a")
        _                                     <-
          if (_b0ac1280_f575_4484_9b6c_e9f657a15908 == null) IO.cede
          else _2ce767da_4378_4f1d_a286_3d6da6dceee4(`π-uuid`)
      } yield ()),
      `π-supervised`(for {
        _4bbfa1c5_ddfe_4873_8b91_ef0c66d8f10f <- IO {
          lazy val _4bbfa1c5_ddfe_4873_8b91_ef0c66d8f10f: String => IO[Unit] = {
            implicit ^ =>
              (
                IO.unit,
                `π-supervised`(for {
                  _89b9b051_71c6_41c1_a7f3_434ed121d9a2 <-
                    c(⊤(1L), "c")("4d4a87d4-ec21-4415-aba7-6ced8ca4985e")
                  _                                     <-
                    if (_89b9b051_71c6_41c1_a7f3_434ed121d9a2 == null) IO.cede
                    else _4bbfa1c5_ddfe_4873_8b91_ef0c66d8f10f(`π-uuid`)
                } yield ())
              ).parMapN { (_, _) => }
          }
          _4bbfa1c5_ddfe_4873_8b91_ef0c66d8f10f
        }
        _89b9b051_71c6_41c1_a7f3_434ed121d9a2 <-
          c(⊤(1L), "c")("4d4a87d4-ec21-4415-aba7-6ced8ca4985e")
        _                                     <-
          if (_89b9b051_71c6_41c1_a7f3_434ed121d9a2 == null) IO.cede
          else _4bbfa1c5_ddfe_4873_8b91_ef0c66d8f10f(`π-uuid`)
      } yield ()),
      `π-supervised`(Hanoi(a, b, c, -1)(using `π-uuid`))
    ).parMapN { (_, _, _, _) => }
  } yield ()

  def Hanoi(
    a: `()`,
    b: `()`,
    c: `()`,
    n: `()`
  )(implicit
    ^ : String,
    % : %,
    / : /
  ): IO[Unit] = for {
    _ <- IO.unit
    _8dc3b8a1_b215_404e_ad4c_24b2c443e4d8 =
      _root_.scala.collection.immutable.Set(
        "22db36d8-6b09-4220-8582-be98cce4464b",
        "cbeaf5cf-964d-45df-bc3f-93037229c675"
      )
    _ <- `π-enable`(_8dc3b8a1_b215_404e_ad4c_24b2c443e4d8)
    w <- ν
    _ <- (
      `π-supervised`(for {
        _ <- τ(⊤(1L))("22db36d8-6b09-4220-8582-be98cce4464b")
        _ <- IO.print("n = ")
        n <- IO.blocking {
          try scala.io.StdIn.readLine.toInt
          catch _ => -1
        }
        _ <- Hanoi(a, b, c, n, w)(using `π-uuid`)
      } yield ()),
      `π-supervised`(for {
        (w, _) <- w(⊤(1L))("cbeaf5cf-964d-45df-bc3f-93037229c675")
        (a, _) <- a(⊤(1L))("cb1b0dbf-2333-4f32-a31c-00bae1e14aef")(!)
        (b, _) <- b(⊤(1L))("6a1cbe42-6fc1-48ed-bcf3-561d6908441f")(!)
        (c, _) <- c(⊤(1L))("cec5c4de-5c4d-4ac0-82ab-f9dc7dc92736")(!)
        _      <- τ(⊤(1L))("04f33f5d-b689-4765-97db-a113b82c30e5")
        _      <- IO {
          println(s"#${w} moves")
        }
      } yield ())
    ).parMapN { (_, _) => }
  } yield ()

  def Move(from: `()`, to: `()`, z: `()`)(using ^ : String)(using % : %, / : /)
    : IO[Unit] = for {
    _ <- IO.unit
    _130e7101_5541_49e2_aeb9_32981ec97125 = _root_.scala.collection.immutable
      .Set("39cb9620-399f-49b6-900d-213f64ded6ef")
    _      <- `π-enable`(_130e7101_5541_49e2_aeb9_32981ec97125)
    (x, _) <- from(⊤(1L))("39cb9620-399f-49b6-900d-213f64ded6ef")
    (y, _) <- to(⊤(1L))("6b46d943-c838-478a-a754-4243f38b2583")
    _      <- τ(⊤(1L))("06d60ab1-7cf9-4a4c-8a07-094b25d7e197")
    _      <- IO {
      println(s"${x} -> ${y}")
    }
    _      <- z(⊤(1L), 1)("f42a7288-f402-4e00-9391-a179763cb3f4")
  } yield ()

  def Hanoi(
    a: `()`,
    b: `()`,
    c: `()`,
    n: `()`,
    w: `()`
  )(implicit
    ^ : String,
    % : %,
    / : /
  ): IO[Unit] =
    if (n <= 1 ==== true) for {
      _ <- IO.unit
      _685483ba_f735_48dc_8ee6_324212d258f5 = _root_.scala.collection.immutable
        .Set("5a771948-d5c2-41b6-b238-0af9eda601c0")
      _ <- `π-enable`(_685483ba_f735_48dc_8ee6_324212d258f5)
      z <- ν
      _ <- (
        `π-supervised`(Move(a, c, z)(using `π-uuid`)),
        `π-supervised`(for {
          (z, _) <- z(⊤(1L))("5a771948-d5c2-41b6-b238-0af9eda601c0")
          _      <- w(⊤(1L), z)("8f607ca7-c033-4ac2-b406-65187b2d2fca")
        } yield ())
      ).parMapN { (_, _) => }
    } yield ()
    else for {
      _ <- IO.unit
      _4624e470_e6ef_43e7_81bc_c313abd8299c = _root_.scala.collection.immutable
        .Set("731df9a8-7f94-4fa4-9319-601311fc31f1")
      _ <- `π-enable`(_4624e470_e6ef_43e7_81bc_c313abd8299c)
      x <- ν
      _ <- (
        `π-supervised`(Hanoi(a, c, b, n - 1, x)(using `π-uuid`)),
        `π-supervised`(for {
          (x, _) <- x(⊤(1L))("731df9a8-7f94-4fa4-9319-601311fc31f1")
          y      <- ν
          _      <- (
            `π-supervised`(Move(a, c, y)(using `π-uuid`)),
            `π-supervised`(for {
              (y, _) <- y(⊤(1L))("9f9bfe93-c828-474d-8002-4877160c634e")
              z      <- ν
              _      <- (
                `π-supervised`(Hanoi(b, a, c, n - 1, z)(using `π-uuid`)),
                `π-supervised`(for {
                  (z, _) <- z(⊤(1L))("6c16acc1-776a-4937-a36b-ffa3af0291c0")
                  _      <-
                    w(⊤(1L), x + y + z)("752c6515-e3db-4617-8dad-7a35a8f34876")
                } yield ())
              ).parMapN { (_, _) => }
            } yield ())
          ).parMapN { (_, _) => }
        } yield ())
      ).parMapN { (_, _) => }
    } yield ()
