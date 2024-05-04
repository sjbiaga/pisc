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

object App extends IOApp:

  override def run(args: List[String]): IO[ExitCode] =
    π.Main(args*).as(ExitCode.Success)

object π:

  import _root_.cats.effect.syntax.all._
  import _root_.cats.syntax.all._
  import _root_.cats.effect.std.Semaphore

  import Π._

  given Conversion[`()`, String] = _.as[String]

  val nl: String => IO[String] = { it => IO { s"$it\n" } }

  def Main(args: String*): IO[Unit] = for {
    stdin  <- ν
    stdout <- ν
    _      <- Greeter(stdin, stdout, "")
  } yield ()

  def Greeter(stdin: `()`, stdout: `()`, line: `()`): IO[Unit] = (
    for {
      _    <- stdout("What's your name?")
      name <- stdin()
      _    <-
        if (name.isBlank ==== true) for {
          _ <- stdout(`()`(null))
          _ <- Greeter(stdin, stdout, line)
        } yield ()
        else Chooser(stdout, name)
    } yield (),
    for {
      prompt <- stdout()(nl)
      _      <- τ
      _      <- IO {
        print(prompt)
      }
      line   <- IO.blocking {
        scala.io.StdIn.readLine
      }
      _      <- stdin(line)
      greet  <- stdout()(nl)
      _      <-
        if (!greet ==== false) for {
          _ <- τ
          _ <- IO {
            print(greet)
          }
        } yield ()
        else IO.unit
    } yield ()
  ).parMapN { (_, _) => }

  def Chooser(stdout: `()`, name: `()`): IO[Unit] = for {
    _f6bbf037_fa43_4b19_ba87_695a1e27d7dd <- Semaphore[IO](1)
    _                                     <- (
      _f6bbf037_fa43_4b19_ba87_695a1e27d7dd.tryAcquire
        .ifM(`Greeter'`(stdout, name), IO.cede),
      _f6bbf037_fa43_4b19_ba87_695a1e27d7dd.tryAcquire
        .ifM(`Greeter"`(stdout, name), IO.cede),
      _f6bbf037_fa43_4b19_ba87_695a1e27d7dd.tryAcquire.ifM(
        `Greeter"'`(stdout, name),
        IO.cede
      )
    ).parMapN { (_, _, _) => }
  } yield ()

  def `Greeter'`(stdout: `()`, name: `()`): IO[Unit] = (
    if (name.substring(0, 1).toUpperCase ==== "Q") for (
      _ <- stdout("That's an unusual name.")
    ) yield ()
    else IO.unit,
    if (name ==== "Voldemort") for (
      _ <- stdout("WARNING! LORD VOLDEMORT IS HERE!")
    ) yield ()
    else IO.unit,
    if (name.substring(0, 1).toUpperCase ==== "Q") IO.unit
    else if (name ==== "Voldemort") IO.unit
    else for (_ <- stdout(s"Hello $name!")) yield ()
  ).parMapN { (_, _, _) => }

  def `Greeter"`(stdout: `()`, name: `()`): IO[Unit] =
    if (name.substring(0, 1).toUpperCase ==== "Q") for (
      _ <- stdout("That's an unusual name.")
    ) yield ()
    else if (name ==== "Voldemort") for (
      _ <- stdout("WARNING! LORD VOLDEMORT IS HERE!")
    ) yield ()
    else for (_ <- stdout(s"Hello $name!")) yield ()

  def `Greeter"'`(stdout: `()`, name: `()`): IO[Unit] =
    if (name.substring(0, 1).toUpperCase ==== "Q") for (
      _ <- stdout("That's an unusual name.")
    ) yield ()
    else if (name ==== "Voldemort") for (
      _ <- stdout("WARNING! LORD VOLDEMORT IS HERE!")
    ) yield ()
    else for (_ <- stdout(s"Hello $name!")) yield ()
