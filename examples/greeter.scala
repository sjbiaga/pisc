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

import cats.effect.{IO, IOApp}

object App extends IOApp.Simple:

  override def run: IO[Unit] = π.Main()

object π:

  import cats.effect.syntax.all._
  import cats.syntax.all._
  import cats.effect.std.Semaphore

  import Π._

  val `𝟎` = IO.unit

  given Conversion[`()`, String] = _.name.asInstanceOf[String]

  def Main(): IO[Unit] = for {
    stdin  <- ν
    stdout <- ν
    _      <- Greeter(stdin, stdout, "")
  } yield ()

  def Greeter(stdin: `()`, stdout: `()`, line: `()`): IO[Unit] = for (
    _ <- (
      for {
        _    <- stdout("What's your name?\n")
        name <- stdin()
        _    <- Chooser(stdout, name)
      } yield (),
      for {
        prompt <- stdout()
        _      <- τ
        _ <- IO {
          print(prompt)
        }
        line <- IO.blocking {
          scala.io.StdIn.readLine
        }
        _     <- stdin(line)
        greet <- stdout()
        _     <- τ
        _ <- IO {
          print(greet)
        }
      } yield ()
    ).parMapN { (_, _) => }
  ) yield ()

  def Chooser(stdout: `()`, name: `()`): IO[Unit] = for {
    _4a23328b_4860_42cb_b991_ed6d659c7a78 <- Semaphore[IO](1)
    _ <- IO.race(
      for {
        _ <- _4a23328b_4860_42cb_b991_ed6d659c7a78.acquire
        _ <- `Greeter'`(stdout, name)
      } yield (),
      for {
        _ <- _4a23328b_4860_42cb_b991_ed6d659c7a78.acquire
        _261d76ca_8b4f_42db_ab9e_bb95adf43ad1 <- Semaphore[IO](1)
        _ <- IO.race(
          for {
            _ <- _261d76ca_8b4f_42db_ab9e_bb95adf43ad1.acquire
            _ <- `Greeter"`(stdout, name)
          } yield (),
          for {
            _ <- _261d76ca_8b4f_42db_ab9e_bb95adf43ad1.acquire
            _ <- `Greeter"'`(stdout, name)
          } yield ()
        )
      } yield ()
    )
  } yield ()

  def `Greeter'`(stdout: `()`, name: `()`): IO[Unit] = for (
    _ <- (
      `𝟎`,
      for (
        _ <-
          if (name.substring(0, 1).toUpperCase === "Q") {
            for (_ <- stdout("That's an unusual name.\n")) yield ()
          } else `𝟎`
      ) yield (),
      for (
        _ <-
          if (name === "Voldemort") {
            for (_ <- stdout("WARNING! LORD VOLDEMORT IS HERE!\n")) yield ()
          } else `𝟎`
      ) yield (),
      for (
        _ <-
          if (name.substring(0, 1).toUpperCase === "Q") {
            `𝟎`
          } else {
            for (
              _ <-
                if (name === "Voldemort") `𝟎`
                else for (_ <- stdout(s"Hello $name!\n")) yield ()
            ) yield ()
          }) yield ()
    ).parMapN { (_, _, _, _) => }
  ) yield ()

  def `Greeter"`(stdout: `()`, name: `()`): IO[Unit] = for (
    _ <-
      if (name.substring(0, 1).toUpperCase === "Q") {
        for (_ <- stdout("That's an unusual name.\n")) yield ()
      } else {
        for (
          _ <-
            if (name === "Voldemort") {
              for (_ <- stdout("WARNING! LORD VOLDEMORT IS HERE!\n")) yield ()
            } else for (_ <- stdout(s"Hello $name!\n")) yield ()
        ) yield ()
      }) yield ()

  def `Greeter"'`(stdout: `()`, name: `()`): IO[Unit] = for (
    _ <-
      if (name.substring(0, 1).toUpperCase === "Q") {
        for (_ <- stdout("That's an unusual name.\n")) yield ()
      } else {
        for (
          _ <-
            if (name === "Voldemort") {
              for (_ <- stdout("WARNING! LORD VOLDEMORT IS HERE!\n")) yield ()
            } else for (_ <- stdout(s"Hello $name!\n")) yield ()
        ) yield ()
      }) yield ()
