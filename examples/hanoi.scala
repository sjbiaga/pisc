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
      "48ca4d55-7bd4-49a3-80bc-255a054d966f" -> _root_.scala.collection.immutable
        .Set("2d7fb5de-8c06-4246-b8d0-5a34f3bfe7ec"),
      "65005e4c-0cfa-4a61-a2b1-631b9d15d174" -> _root_.scala.collection.immutable
        .Set(),
      "9a994bac-f34e-4505-8fd1-55d36395a412" -> _root_.scala.collection.immutable
        .Set("9747b929-89b5-49ff-9b73-66475f3b9549"),
      "0c1c629b-72ba-40fb-8754-b724d2e53d5e" -> _root_.scala.collection.immutable
        .Set(),
      "29d86ebe-f9af-4cc3-a042-df7e4ed341fc" -> _root_.scala.collection.immutable
        .Set("65005e4c-0cfa-4a61-a2b1-631b9d15d174"),
      "16531015-a5ae-4dac-8ea0-765ea3553fe9" -> _root_.scala.collection.immutable
        .Set(),
      "a88805b6-4dd9-42aa-aa0e-4522a795f49e" -> _root_.scala.collection.immutable
        .Set("824696f1-0117-45ec-9efc-f31ac265cf09"),
      "693984ee-837d-4197-a71b-57d47e1cc3dd" -> _root_.scala.collection.immutable
        .Set("cb7071a4-551f-412d-9c62-f62527313da5"),
      "451b077d-ffb1-4c69-bec3-dd70c008c429" -> _root_.scala.collection.immutable
        .Set("451b077d-ffb1-4c69-bec3-dd70c008c429"),
      "72aabaa1-c0ef-404d-8e62-a9e57c54a0b8" -> _root_.scala.collection.immutable
        .Set("a88805b6-4dd9-42aa-aa0e-4522a795f49e"),
      "9747b929-89b5-49ff-9b73-66475f3b9549" -> _root_.scala.collection.immutable
        .Set(),
      "2d7fb5de-8c06-4246-b8d0-5a34f3bfe7ec" -> _root_.scala.collection.immutable
        .Set("29d86ebe-f9af-4cc3-a042-df7e4ed341fc"),
      "252cebe1-144b-4ef2-bede-0c3964dd12cd" -> _root_.scala.collection.immutable
        .Set("252cebe1-144b-4ef2-bede-0c3964dd12cd"),
      "bd3e4137-e011-4bc2-860c-b398fede8d8b" -> _root_.scala.collection.immutable
        .Set("693984ee-837d-4197-a71b-57d47e1cc3dd"),
      "853fc36b-7265-4235-9579-20be5680dc03" -> _root_.scala.collection.immutable
        .Set("853fc36b-7265-4235-9579-20be5680dc03"),
      "cb742835-c4fd-4114-982d-6bea3521ddc7" -> _root_.scala.collection.immutable
        .Set("0c1c629b-72ba-40fb-8754-b724d2e53d5e"),
      "cb7071a4-551f-412d-9c62-f62527313da5" -> _root_.scala.collection.immutable
        .Set("cb742835-c4fd-4114-982d-6bea3521ddc7"),
      "824696f1-0117-45ec-9efc-f31ac265cf09" -> _root_.scala.collection.immutable
        .Set("16531015-a5ae-4dac-8ea0-765ea3553fe9")
    )

  implicit val `π-wand`
    : (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]) =
    `π-trick` -> `π-spell`

  given Conversion[`()`, Int] = _.as[Int]

  val ! : String => IO[String] = { _ => IO.pure(null) }

  def Main(args: String*)(using ^ : String)(using % : %, / : /): IO[Unit] = for {
    _ <- IO.unit
    _23d17176_d0fb_44ed_b241_9a21aeef3c06 =
      _root_.scala.collection.immutable.Set(
        "252cebe1-144b-4ef2-bede-0c3964dd12cd",
        "451b077d-ffb1-4c69-bec3-dd70c008c429",
        "853fc36b-7265-4235-9579-20be5680dc03"
      )
    _ <- `π-enable`(_23d17176_d0fb_44ed_b241_9a21aeef3c06)
    a <- ν
    b <- ν
    c <- ν
    _ <- (
      IO.unit,
      `π-supervised`(for {
        _b8eafdba_a47c_4f39_9b09_c01d28e60a12 <- IO {
          lazy val _b8eafdba_a47c_4f39_9b09_c01d28e60a12: String => IO[Unit] = {
            implicit ^ =>
              (
                IO.unit,
                `π-supervised`(for {
                  _51480183_49ee_43ac_88de_25d12daea1d2 <-
                    a(⊤(1L), "a")("252cebe1-144b-4ef2-bede-0c3964dd12cd")
                  _                                     <-
                    if (_51480183_49ee_43ac_88de_25d12daea1d2 == null) IO.cede
                    else _b8eafdba_a47c_4f39_9b09_c01d28e60a12(`π-uuid`)
                } yield ())
              ).parMapN { (_, _) => }
          }
          _b8eafdba_a47c_4f39_9b09_c01d28e60a12
        }
        _51480183_49ee_43ac_88de_25d12daea1d2 <-
          a(⊤(1L), "a")("252cebe1-144b-4ef2-bede-0c3964dd12cd")
        _                                     <-
          if (_51480183_49ee_43ac_88de_25d12daea1d2 == null) IO.cede
          else _b8eafdba_a47c_4f39_9b09_c01d28e60a12(`π-uuid`)
      } yield ()),
      `π-supervised`(for {
        _61456ca3_b2f3_45f9_adb2_9baf51d2ffd9 <- IO {
          lazy val _61456ca3_b2f3_45f9_adb2_9baf51d2ffd9: String => IO[Unit] = {
            implicit ^ =>
              (
                IO.unit,
                `π-supervised`(for {
                  _e9922a8e_27f9_450d_8381_e25dd5100538 <-
                    b(⊤(1L), "b")("451b077d-ffb1-4c69-bec3-dd70c008c429")
                  _                                     <-
                    if (_e9922a8e_27f9_450d_8381_e25dd5100538 == null) IO.cede
                    else _61456ca3_b2f3_45f9_adb2_9baf51d2ffd9(`π-uuid`)
                } yield ())
              ).parMapN { (_, _) => }
          }
          _61456ca3_b2f3_45f9_adb2_9baf51d2ffd9
        }
        _e9922a8e_27f9_450d_8381_e25dd5100538 <-
          b(⊤(1L), "b")("451b077d-ffb1-4c69-bec3-dd70c008c429")
        _                                     <-
          if (_e9922a8e_27f9_450d_8381_e25dd5100538 == null) IO.cede
          else _61456ca3_b2f3_45f9_adb2_9baf51d2ffd9(`π-uuid`)
      } yield ()),
      `π-supervised`(for {
        _40797462_5635_4bc8_9e94_8fa28fac7893 <- IO {
          lazy val _40797462_5635_4bc8_9e94_8fa28fac7893: String => IO[Unit] = {
            implicit ^ =>
              (
                IO.unit,
                `π-supervised`(for {
                  _fc72766f_1b94_4069_a927_686557dac177 <-
                    c(⊤(1L), "c")("853fc36b-7265-4235-9579-20be5680dc03")
                  _                                     <-
                    if (_fc72766f_1b94_4069_a927_686557dac177 == null) IO.cede
                    else _40797462_5635_4bc8_9e94_8fa28fac7893(`π-uuid`)
                } yield ())
              ).parMapN { (_, _) => }
          }
          _40797462_5635_4bc8_9e94_8fa28fac7893
        }
        _fc72766f_1b94_4069_a927_686557dac177 <-
          c(⊤(1L), "c")("853fc36b-7265-4235-9579-20be5680dc03")
        _                                     <-
          if (_fc72766f_1b94_4069_a927_686557dac177 == null) IO.cede
          else _40797462_5635_4bc8_9e94_8fa28fac7893(`π-uuid`)
      } yield ()),
      `π-supervised`(Hanoi(a, b, c, -1)(using `π-uuid`))
    ).parMapN { (_, _, _, _, _) => }
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
    _0f7bec5b_8d18_43b5_9554_79ab8450ba61 =
      _root_.scala.collection.immutable.Set(
        "43450e21-c997-4d61-b686-3ff9fc580194",
        "bd3e4137-e011-4bc2-860c-b398fede8d8b"
      )
    _ <- `π-enable`(_0f7bec5b_8d18_43b5_9554_79ab8450ba61)
    w <- ν
    _ <- (
      IO.unit,
      `π-supervised`(for {
        _ <- τ(⊤(1L))("43450e21-c997-4d61-b686-3ff9fc580194")
        _ <- IO.print("n = ")
        n <- IO.blocking {
          try scala.io.StdIn.readLine.toInt
          catch _ => -1
        }
        _ <- Hanoi(a, b, c, n, w)(using `π-uuid`)
      } yield ()),
      `π-supervised`(for {
        (w, _) <- w(⊤(1L))("bd3e4137-e011-4bc2-860c-b398fede8d8b")
        (a, _) <- a(⊤(1L))("693984ee-837d-4197-a71b-57d47e1cc3dd")(!)
        (b, _) <- b(⊤(1L))("cb7071a4-551f-412d-9c62-f62527313da5")(!)
        (c, _) <- c(⊤(1L))("cb742835-c4fd-4114-982d-6bea3521ddc7")(!)
        _      <- τ(⊤(1L))("0c1c629b-72ba-40fb-8754-b724d2e53d5e")
        _      <- IO {
          println(s"#${w} moves")
        }
      } yield ())
    ).parMapN { (_, _, _) => }
  } yield ()

  def Move(from: `()`, to: `()`, z: `()`)(using ^ : String)(using % : %, / : /)
    : IO[Unit] = for {
    _ <- IO.unit
    _e471ac9b_3b49_4d12_86e9_62c8d8607761 = _root_.scala.collection.immutable
      .Set("72aabaa1-c0ef-404d-8e62-a9e57c54a0b8")
    _      <- `π-enable`(_e471ac9b_3b49_4d12_86e9_62c8d8607761)
    (x, _) <- from(⊤(1L))("72aabaa1-c0ef-404d-8e62-a9e57c54a0b8")
    (y, _) <- to(⊤(1L))("a88805b6-4dd9-42aa-aa0e-4522a795f49e")
    _      <- τ(⊤(1L))("824696f1-0117-45ec-9efc-f31ac265cf09")
    _      <- IO {
      println(s"${x} -> ${y}")
    }
    _      <- z(⊤(1L), 1)("16531015-a5ae-4dac-8ea0-765ea3553fe9")
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
      _5a2b1c16_e393_4aac_93ab_279aa86891c1 = _root_.scala.collection.immutable
        .Set("9a994bac-f34e-4505-8fd1-55d36395a412")
      _ <- `π-enable`(_5a2b1c16_e393_4aac_93ab_279aa86891c1)
      z <- ν
      _ <- (
        `π-supervised`(Move(a, c, z)(using `π-uuid`)),
        `π-supervised`(for {
          (z, _) <- z(⊤(1L))("9a994bac-f34e-4505-8fd1-55d36395a412")
          _      <- w(⊤(1L), z)("9747b929-89b5-49ff-9b73-66475f3b9549")
        } yield ())
      ).parMapN { (_, _) => }
    } yield ()
    else for {
      _ <- IO.unit
      _d747f088_f64e_488e_8da5_76cb09038ebb = _root_.scala.collection.immutable
        .Set("48ca4d55-7bd4-49a3-80bc-255a054d966f")
      _ <- `π-enable`(_d747f088_f64e_488e_8da5_76cb09038ebb)
      x <- ν
      _ <- (
        IO.unit,
        `π-supervised`(Hanoi(a, c, b, n - 1, x)(using `π-uuid`)),
        `π-supervised`(for {
          (x, _) <- x(⊤(1L))("48ca4d55-7bd4-49a3-80bc-255a054d966f")
          y      <- ν
          _      <- (
            `π-supervised`(Move(a, c, y)(using `π-uuid`)),
            `π-supervised`(for {
              (y, _) <- y(⊤(1L))("2d7fb5de-8c06-4246-b8d0-5a34f3bfe7ec")
              z      <- ν
              _      <- (
                `π-supervised`(Hanoi(b, a, c, n - 1, z)(using `π-uuid`)),
                `π-supervised`(for {
                  (z, _) <- z(⊤(1L))("29d86ebe-f9af-4cc3-a042-df7e4ed341fc")
                  _      <-
                    w(⊤(1L), x + y + z)("65005e4c-0cfa-4a61-a2b1-631b9d15d174")
                } yield ())
              ).parMapN { (_, _) => }
            } yield ())
          ).parMapN { (_, _) => }
        } yield ())
      ).parMapN { (_, _, _) => }
    } yield ()
