/*
 * Copyright (c) 2023-2025 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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

package object Π:

  import _root_.cats.effect.{ Ref, IO }
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.{ CyclicBarrier, Queue, Supervisor }

  import `Π-magic`.*


  /**
    * Supervised [[code]].
    * @param code
    */
  private def exec[T](code: => IO[T]): IO[T] =
    Supervisor[IO](await = true)
      .use(_.supervise(code))
      .flatMap(_.join
                .flatMap
                { case Succeeded(it) => it
                  case _ => IO(null.asInstanceOf[T]) }
              )


  /**
    * restriction aka new name
    */
  object ν:

    def map[B](f: `()` => B): IO[B] = flatMap(f andThen IO.pure)
    def flatMap[B](f: `()` => IO[B]): IO[B] =
      ( for
          q <- Queue.bounded[IO, (Seq[Any], CyclicBarrier[IO])](1)
          ref <- Ref.of[IO, ><](><(q, false))
        yield
          f(ref)
      ).flatten


  /**
    * silent transition
    */
  val τ: IO[Option[Unit]] = IO(Some(()))


  /**
    * prefix
    */
  implicit final class `()`(private val name: Any) extends AnyVal:

    private def ref = `()`[>*<]

    def ====(that: `()`) =
      try
        this.ref eq that.ref
      catch
        case _ =>
          this.name == that.name

    inline def unary_! : Boolean = name == null
    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`*): IO[Option[Unit]] = ><(value.map(_.name))(ref)

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`*)(code: => IO[Any]): IO[Option[Unit]] = ><(value.map(_.name))(ref)(code)

    /**
      * positive prefix i.e. input
      */
    def apply(): IO[Seq[`()`]] = ><()(ref).map(_.map(new `()`(_)))

    /**
      * positive prefix i.e. input
      */
    def apply()(code: Seq[Any] => IO[Seq[Any]]): IO[Seq[`()`]] = ><()(ref)(code).map(_.map(new `()`(_)))

    override def toString: String = if name == null then "null" else name.toString


  private object `Π-magic`:

    final case class ><(queue: Queue[IO, (Seq[Any], CyclicBarrier[IO])], stop: Boolean)

    type >*< = Ref[IO, ><]

    object >< :

      def apply(names: Seq[Any])(`>R`: >*<): IO[Option[Unit]] =
        CyclicBarrier[IO](2).flatMap { b2 =>
          `>R`.flatModify { case it @ ><(q, _) =>
            it -> q.offer(names -> b2)
          } >> b2.await >>
          `>R`.modify { case it @ ><(_, stop) =>
            it -> (if stop then None else Some(()))
          }
        }

      def apply(names: Seq[Any])(`>R`: >*<)(code: => IO[Any]): IO[Option[Unit]] =
        CyclicBarrier[IO](2).flatMap { b2 =>
          `>R`.flatModify { case it @ ><(q, _) =>
            it -> q.offer(names -> b2)
          } >> exec(code) >> b2.await >>
          `>R`.modify { case it @ ><(_, stop) =>
            it -> (if stop then None else Some(()))
          }
        }

      def apply()(`<R`: >*<): IO[Seq[Any]] =
        `<R`.flatModify { case it @ ><(q, _) =>
          it -> q.take
        }.flatMap { (names, b2) =>
          IO.pure(names) <* b2.await
        }

      def apply()(`<R`: >*<)(code: Seq[Any] => IO[Seq[Any]]): IO[Seq[Any]] =
        `<R`.flatModify { case it @ ><(q, _) =>
          it -> q.take
        }.flatMap {
          case it@ (Seq(null, _*), _) => IO.pure(it)
          case (it, b2) => (code andThen exec)(it)
                             .flatTap {
                               case Seq(null, _*) => `<R`.update(_.copy(stop = true))
                               case _ => IO.unit
                             }.map(_ -> b2)
        }.flatMap { (names, b2) =>
          IO.pure(names) <* b2.await
        }
