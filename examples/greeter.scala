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

  given Conversion[`()`, String] = _.name.asInstanceOf[String]

  def Main(): IO[Unit] = for {
    stdin  <- ν
    stdout <- ν
    _ <- (
      for {
        _    <- stdout("What's your name?\n")
        name <- stdin()
        _    <- Chooser(stdout, name)
      } yield (),
      for {
        prompt   <- stdout()
        _        <- IO.print(prompt)
        sc_name  <- IO { scala.io.StdIn.readLine }
        _        <- stdin(sc_name)
        pi_greet <- stdout()
        _        <- IO.print(pi_greet)
        _        <- for (_ <- IO.unit) yield ()
      } yield ()
    ).parMapN { (_, _) => }
  } yield ()

  def Chooser(stdout: `()`, name: `()`): IO[Unit] = for {
    `14b17611-2176-48f5-af1b-ba6a085f5ee8` <- Semaphore[IO](1)
    _ <- IO.race(
      for {
        _ <- `14b17611-2176-48f5-af1b-ba6a085f5ee8`.acquire
        _ <- `Greeter'`(stdout, name)
      } yield (),
      for {
        _ <- `14b17611-2176-48f5-af1b-ba6a085f5ee8`.acquire
        _ <- `Greeter'`(stdout, name)
      } yield ()
    )
  } yield ()

  def `Greeter'`(stdout: `()`, name: `()`): IO[Unit] = for (
    _ <- (
      for {
        _ <-
          if (!(name.substring(0, 1).toUpperCase === "Q")) IO.cede
          else {
            for {
              _ <- stdout("That's an unusual name.\n")
              _ <- for (_ <- IO.unit) yield ()
            } yield ()
          }
        _ <- for (_ <- IO.unit) yield ()
      } yield (),
      for {
        _ <-
          if (!(name === "Voldemort")) IO.cede
          else {
            for {
              _ <- stdout("WARNING! LORD VOLDEMORT IS HERE!\n")
              _ <- for (_ <- IO.unit) yield ()
            } yield ()
          }
        _ <- for (_ <- IO.unit) yield ()
      } yield (),
      for {
        _ <-
          if (name.substring(0, 1).toUpperCase === "Q") IO.cede
          else {
            for {
              _ <-
                if (name === "Voldemort") IO.cede
                else {
                  for {
                    _ <- stdout(s"Hello $name!\n")
                    _ <- for (_ <- IO.unit) yield ()
                  } yield ()
                }
              _ <- for (_ <- IO.unit) yield ()
            } yield ()
          }
        _ <- for (_ <- IO.unit) yield ()
      } yield ()
    ).parMapN { (_, _, _) => }
  ) yield ()

  def `Greeter"`(stdout: `()`, name: `()`): IO[Unit] = for {
    _ <-
      if (name.substring(0, 1).toUpperCase === "Q") for {
        _ <- stdout("That's an unusual name.\n")
        _ <- for (_ <- IO.unit) yield ()
      } yield ()
      else {
        for {
          _ <-
            if (name === "Voldemort") for {
              _ <- stdout("WARNING! LORD VOLDEMORT IS HERE!\n")
              _ <- for (_ <- IO.unit) yield ()
            } yield ()
            else {
              for {
                _ <- stdout(s"Hello $name!\n")
                _ <- for (_ <- IO.unit) yield ()
              } yield ()
            }
          _ <- for (_ <- IO.unit) yield ()
        } yield ()
      }
    _ <- for (_ <- IO.unit) yield ()
  } yield ()

  def `Greeter"'`(stdout: `()`, name: `()`): IO[Unit] = for {
    _ <-
      if (name.substring(0, 1).toUpperCase === "Q") for {
        _ <- stdout("That's an unusual name.\n")
        _ <- for (_ <- IO.unit) yield ()
      } yield ()
      else {
        for {
          _ <-
            if (name === "Voldemort") for {
              _ <- stdout("WARNING! LORD VOLDEMORT IS HERE!\n")
              _ <- for (_ <- IO.unit) yield ()
            } yield ()
            else {
              for {
                _ <- stdout(s"Hello $name!\n")
                _ <- for (_ <- IO.unit) yield ()
              } yield ()
            }
          _ <- for (_ <- IO.unit) yield ()
        } yield ()
      }
    _ <- for (_ <- IO.unit) yield ()
  } yield ()
