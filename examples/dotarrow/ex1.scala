package dotarrow
package ex1

import cats.effect.{ IO, IOApp }


object App extends IOApp.Simple:

  override def run: IO[Unit] =

    for
      x: Int <- IO { 2 + 5 }
      y: Int <- IO { x * 4 }
      z: Double <- IO { 1.0 * x * y }
      _u: Unit <- IO { println(z) }
      _ <- IO { }.void
    yield
      ()
