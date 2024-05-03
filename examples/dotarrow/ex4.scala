package dotarrow
package ex4

import cats.effect.{ IO, IOApp }


object App extends IOApp.Simple:

  override def run: IO[Unit] =

    for
      _ <- IO { println("started") }
      t: (Int, Int) <- IO { 2 -> "\\".replaceAll("\\\\$", "5").toInt }
      (a, b) = t
      x: Int <- IO { a + b }
      _ <- IO { println(s"computed x = $x") }
      c: C <- IO { C(x) }
      _ <- IO { println(s"computed c = $c") }
      C(n) <- IO { c }
      d: ` D ` <- IO { new ` D `(c) }
      _ <- IO { println(s"computed d = $d") }
      y: Int <- IO { d.c.n * 4 }
      _ <- IO { println(s"computed y = $y") }
      z: Double <- IO { "1".toDouble * x * y }
      _ <- IO { println(s"computed z = $z") }
      _u: Unit <- IO { println(z) }
      _ <- IO { println("ended") }
      _ <- IO { }.void
    yield
      ()

  case class C(n: Int = 0)

  class ` D `(val c: C):
    override def toString(): String = s"D($c)"

  import io.circe.generic.auto._

  import io.circe.{ Decoder, Encoder }

  given Encoder[` D `] = Encoder.encoderContravariant.contramap(implicitly[Encoder[C]])(_.c)

  given Decoder[` D `] = implicitly[Decoder[C]].map(` D `(_))
