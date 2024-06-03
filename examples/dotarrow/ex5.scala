import cats.effect.{ IO, IOApp }


object App extends IOApp.Simple:

  override def run: IO[Unit] =

    for
      x: Int <- IO { 2 + 5 * 1 - 0 }
      y: Int <- IO { x * 4 * (x - x + 1) }
      z: Int <- IO { x * y * 1 * 1 }
      w: Int <- IO { (z - 0) * 1  }
      _u: Unit <- IO { println(w) }
      _ <- IO { }.void
    yield
      ()
