package dotarrow
package ex2

import cats.effect.{ IO, IOApp }


object App extends IOApp.Simple:

  override def run: IO[Unit] =

    for
      _ <- IO { println("started") }
      x: Int <- IO { 2 + "".replaceAll("$", "5").toInt }
      _ <- IO { println(s"computed x = $x") }
      y: Int <- IO { x * 4 }
      _ <- IO { println(s"computed y = $y") }
      z: Double <- IO { "1".toDouble * x * y }
      _ <- IO { println(s"computed z = $z") }
      _u: Unit <- IO { println(z); }
      _ <- IO { println("ended") }
      _ <- IO { }.void
    yield
      ()
