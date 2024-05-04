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
      "dd6de97f-6a8e-429b-8ea8-f60ff8fb0308" -> _root_.scala.collection.immutable
        .Set("dd6de97f-6a8e-429b-8ea8-f60ff8fb0308"),
      "076529c5-8cdf-4ee4-bfa0-23c2508ede30" -> _root_.scala.collection.immutable
        .Set(),
      "cdfe00fc-575d-4c0b-8bac-9b9fa0b386c1" -> _root_.scala.collection.immutable
        .Set("7873f008-4c8e-429a-8f26-c1943bd49471"),
      "379a0cfa-fe95-4d28-bebd-a9f7f068a3ab" -> _root_.scala.collection.immutable
        .Set("6da1a34e-8a03-4603-95f2-aa6ab7b7189f"),
      "9177d90f-37ad-4544-80c4-4e5f8fa4b2a4" -> _root_.scala.collection.immutable
        .Set("9177d90f-37ad-4544-80c4-4e5f8fa4b2a4"),
      "fa7e2152-6f5f-495e-903d-0b00ff51e459" -> _root_.scala.collection.immutable
        .Set("a6642012-ea01-4111-bca2-859ca4c66caf"),
      "7d12758d-10ee-4539-b76c-c3772651adaa" -> _root_.scala.collection.immutable
        .Set("cdfe00fc-575d-4c0b-8bac-9b9fa0b386c1"),
      "2e30c19b-f650-4a7d-9d1a-6fd5d65f4fcb" -> _root_.scala.collection.immutable
        .Set("cc1275a3-72fe-4e03-aaec-2877d1cce035"),
      "6da1a34e-8a03-4603-95f2-aa6ab7b7189f" -> _root_.scala.collection.immutable
        .Set("de43b72e-122d-4217-9d12-a7ba2f933ce0"),
      "25c2283f-7775-425d-bb78-9234a026d6a9" -> _root_.scala.collection.immutable
        .Set("25c2283f-7775-425d-bb78-9234a026d6a9"),
      "d3ecca01-71b5-49a6-860a-088248d4376b" -> _root_.scala.collection.immutable
        .Set(),
      "de43b72e-122d-4217-9d12-a7ba2f933ce0" -> _root_.scala.collection.immutable
        .Set("d3ecca01-71b5-49a6-860a-088248d4376b"),
      "a6642012-ea01-4111-bca2-859ca4c66caf" -> _root_.scala.collection.immutable
        .Set("2a4c43d5-c8e4-405f-b161-d72911c7b937"),
      "2a4c43d5-c8e4-405f-b161-d72911c7b937" -> _root_.scala.collection.immutable
        .Set("076529c5-8cdf-4ee4-bfa0-23c2508ede30"),
      "cc1275a3-72fe-4e03-aaec-2877d1cce035" -> _root_.scala.collection.immutable
        .Set(),
      "5eb2d991-afe9-4784-9471-9f098e230b8f" -> _root_.scala.collection.immutable
        .Set("7d12758d-10ee-4539-b76c-c3772651adaa"),
      "d4e6bdb0-e630-403a-9553-7e225107a310" -> _root_.scala.collection.immutable
        .Set(),
      "7873f008-4c8e-429a-8f26-c1943bd49471" -> _root_.scala.collection.immutable
        .Set("d4e6bdb0-e630-403a-9553-7e225107a310")
    )

  implicit val `π-wand`
    : (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) =
    `π-trick` -> `π-spell`

  given Conversion[`()`, Int] = _.as[Int]

  val ! : String => IO[String] = { _ => IO.pure(null) }

  def Main(args: String*)(using ^ : String)(using % : %, / : /): IO[Unit] = for {
    _ <- IO.unit
    _f7c93b3a_6ee6_461d_b5b2_c725fc489b70 =
      _root_.scala.collection.immutable.Set(
        "25c2283f-7775-425d-bb78-9234a026d6a9",
        "dd6de97f-6a8e-429b-8ea8-f60ff8fb0308",
        "9177d90f-37ad-4544-80c4-4e5f8fa4b2a4"
      )
    _ <- `π-enable`(_f7c93b3a_6ee6_461d_b5b2_c725fc489b70)
    a <- ν
    b <- ν
    c <- ν
    _ <- (
      `π-supervised`(for {
        _9f4a97ff_bd0c_4d86_9c58_de441bf0a8c7 <- IO {
          lazy val _9f4a97ff_bd0c_4d86_9c58_de441bf0a8c7: String => IO[Unit] = {
            implicit ^ =>
              (
                IO.unit,
                `π-supervised`(for {
                  _8f5c3d2d_6924_402e_b3e6_4495837e1f96 <-
                    a(⊤(1L), "a")("25c2283f-7775-425d-bb78-9234a026d6a9")
                  _                                     <-
                    if (_8f5c3d2d_6924_402e_b3e6_4495837e1f96 == null) IO.cede
                    else _9f4a97ff_bd0c_4d86_9c58_de441bf0a8c7(`π-uuid`)
                } yield ())
              ).parMapN { (_, _) => }
          }
          _9f4a97ff_bd0c_4d86_9c58_de441bf0a8c7
        }
        _8f5c3d2d_6924_402e_b3e6_4495837e1f96 <-
          a(⊤(1L), "a")("25c2283f-7775-425d-bb78-9234a026d6a9")
        _                                     <-
          if (_8f5c3d2d_6924_402e_b3e6_4495837e1f96 == null) IO.cede
          else _9f4a97ff_bd0c_4d86_9c58_de441bf0a8c7(`π-uuid`)
      } yield ()),
      `π-supervised`(for {
        _52dbf996_4833_4131_9396_4859925d0d1c <- IO {
          lazy val _52dbf996_4833_4131_9396_4859925d0d1c: String => IO[Unit] = {
            implicit ^ =>
              (
                IO.unit,
                `π-supervised`(for {
                  _b2aabb1b_fb01_48fc_9366_281daa3d9abd <-
                    b(⊤(1L), "b")("dd6de97f-6a8e-429b-8ea8-f60ff8fb0308")
                  _                                     <-
                    if (_b2aabb1b_fb01_48fc_9366_281daa3d9abd == null) IO.cede
                    else _52dbf996_4833_4131_9396_4859925d0d1c(`π-uuid`)
                } yield ())
              ).parMapN { (_, _) => }
          }
          _52dbf996_4833_4131_9396_4859925d0d1c
        }
        _b2aabb1b_fb01_48fc_9366_281daa3d9abd <-
          b(⊤(1L), "b")("dd6de97f-6a8e-429b-8ea8-f60ff8fb0308")
        _                                     <-
          if (_b2aabb1b_fb01_48fc_9366_281daa3d9abd == null) IO.cede
          else _52dbf996_4833_4131_9396_4859925d0d1c(`π-uuid`)
      } yield ()),
      `π-supervised`(for {
        _38dcfbcd_e8ee_41b7_9c1b_15872b10f3e9 <- IO {
          lazy val _38dcfbcd_e8ee_41b7_9c1b_15872b10f3e9: String => IO[Unit] = {
            implicit ^ =>
              (
                IO.unit,
                `π-supervised`(for {
                  _4393b159_ead1_4544_b05f_7233d7dc7ef1 <-
                    c(⊤(1L), "c")("9177d90f-37ad-4544-80c4-4e5f8fa4b2a4")
                  _                                     <-
                    if (_4393b159_ead1_4544_b05f_7233d7dc7ef1 == null) IO.cede
                    else _38dcfbcd_e8ee_41b7_9c1b_15872b10f3e9(`π-uuid`)
                } yield ())
              ).parMapN { (_, _) => }
          }
          _38dcfbcd_e8ee_41b7_9c1b_15872b10f3e9
        }
        _4393b159_ead1_4544_b05f_7233d7dc7ef1 <-
          c(⊤(1L), "c")("9177d90f-37ad-4544-80c4-4e5f8fa4b2a4")
        _                                     <-
          if (_4393b159_ead1_4544_b05f_7233d7dc7ef1 == null) IO.cede
          else _38dcfbcd_e8ee_41b7_9c1b_15872b10f3e9(`π-uuid`)
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
    _2b77e423_b916_4665_b98d_deeba1153dc6 =
      _root_.scala.collection.immutable.Set(
        "4d362b72-0065-4a15-ab88-0d99f4befef4",
        "5eb2d991-afe9-4784-9471-9f098e230b8f"
      )
    _ <- `π-enable`(_2b77e423_b916_4665_b98d_deeba1153dc6)
    w <- ν
    _ <- (
      `π-supervised`(for {
        _ <- τ(⊤(1L))("4d362b72-0065-4a15-ab88-0d99f4befef4")
        _ <- IO.print("n = ")
        n <- IO.blocking {
          try scala.io.StdIn.readLine.toInt
          catch _ => -1
        }
        _ <- Hanoi(a, b, c, n, w)(using `π-uuid`)
      } yield ()),
      `π-supervised`(for {
        (w, _) <- w(⊤(1L))("5eb2d991-afe9-4784-9471-9f098e230b8f")
        (a, _) <- a(⊤(1L))("7d12758d-10ee-4539-b76c-c3772651adaa")(!)
        (b, _) <- b(⊤(1L))("cdfe00fc-575d-4c0b-8bac-9b9fa0b386c1")(!)
        (c, _) <- c(⊤(1L))("7873f008-4c8e-429a-8f26-c1943bd49471")(!)
        _      <- τ(⊤(1L))("d4e6bdb0-e630-403a-9553-7e225107a310")
        _      <- IO {
          println(s"#${w} moves")
        }
      } yield ())
    ).parMapN { (_, _) => }
  } yield ()

  def Move(from: `()`, to: `()`, z: `()`)(using ^ : String)(using % : %, / : /)
    : IO[Unit] = for {
    _ <- IO.unit
    _a469c596_67ce_455c_a379_0b41659d5daa = _root_.scala.collection.immutable
      .Set("fa7e2152-6f5f-495e-903d-0b00ff51e459")
    _      <- `π-enable`(_a469c596_67ce_455c_a379_0b41659d5daa)
    (x, _) <- from(⊤(1L))("fa7e2152-6f5f-495e-903d-0b00ff51e459")
    (y, _) <- to(⊤(1L))("a6642012-ea01-4111-bca2-859ca4c66caf")
    _      <- τ(⊤(1L))("2a4c43d5-c8e4-405f-b161-d72911c7b937")
    _      <- IO {
      println(s"${x} -> ${y}")
    }
    _      <- z(⊤(1L), 1)("076529c5-8cdf-4ee4-bfa0-23c2508ede30")
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
      _a92c7ff5_5d50_4e06_b5b3_d8f2af6a92ee = _root_.scala.collection.immutable
        .Set("2e30c19b-f650-4a7d-9d1a-6fd5d65f4fcb")
      _ <- `π-enable`(_a92c7ff5_5d50_4e06_b5b3_d8f2af6a92ee)
      z <- ν
      _ <- (
        `π-supervised`(Move(a, c, z)(using `π-uuid`)),
        `π-supervised`(for {
          (z, _) <- z(⊤(1L))("2e30c19b-f650-4a7d-9d1a-6fd5d65f4fcb")
          _      <- w(⊤(1L), z)("cc1275a3-72fe-4e03-aaec-2877d1cce035")
        } yield ())
      ).parMapN { (_, _) => }
    } yield ()
    else for {
      _ <- IO.unit
      _d9df2474_a029_4c0d_8654_461499968e44 = _root_.scala.collection.immutable
        .Set("379a0cfa-fe95-4d28-bebd-a9f7f068a3ab")
      _ <- `π-enable`(_d9df2474_a029_4c0d_8654_461499968e44)
      x <- ν
      _ <- (
        `π-supervised`(Hanoi(a, c, b, n - 1, x)(using `π-uuid`)),
        `π-supervised`(for {
          (x, _) <- x(⊤(1L))("379a0cfa-fe95-4d28-bebd-a9f7f068a3ab")
          y      <- ν
          _      <- (
            `π-supervised`(Move(a, c, y)(using `π-uuid`)),
            `π-supervised`(for {
              (y, _) <- y(⊤(1L))("6da1a34e-8a03-4603-95f2-aa6ab7b7189f")
              z      <- ν
              _      <- (
                `π-supervised`(Hanoi(b, a, c, n - 1, z)(using `π-uuid`)),
                `π-supervised`(for {
                  (z, _) <- z(⊤(1L))("de43b72e-122d-4217-9d12-a7ba2f933ce0")
                  _      <-
                    w(⊤(1L), x + y + z)("d3ecca01-71b5-49a6-860a-088248d4376b")
                } yield ())
              ).parMapN { (_, _) => }
            } yield ())
          ).parMapN { (_, _) => }
        } yield ())
      ).parMapN { (_, _) => }
    } yield ()
