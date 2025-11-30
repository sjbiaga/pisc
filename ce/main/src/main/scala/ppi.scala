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

  import _root_.cats.effect.IO
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.{ Queue, Supervisor }

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
          q <- Queue.synchronous[IO, Seq[Any]]
        yield
          f(q)
      ).flatten


  /**
    * silent transition
    */
  val τ: IO[Option[Unit]] = IO(Some(()))


  /**
    * prefix
    */
  implicit final class `()`(private val name: Any) extends AnyVal:

    private def q = `()`[><]

    def ====(that: `()`) =
      try
        this.q eq that.q
      catch
        case _ =>
          this.name == that.name

    inline def unary_! : Boolean = name == null
    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`*): IO[Option[Unit]] = ><(value.map(_.name))(q)

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`*)(code: => IO[Any]): IO[Option[Unit]] = apply(value) <* exec(code)

    /**
      * positive prefix i.e. input
      */
    def apply(): IO[Seq[`()`]] = ><()(q).map(_.map(new `()`(_)))

    /**
      * positive prefix i.e. input
      */
    def apply()(code: Seq[Any] => IO[Seq[Any]]): IO[Seq[`()`]] = ><()(q)(code).map(_.map(new `()`(_)))

    override def toString: String = if name == null then "null" else name.toString


  private object `Π-magic`:

    type >< = Queue[IO, Seq[Any]]

    object >< :

      inline def apply(names: Seq[Any])(`>Q`: ><): IO[Option[Unit]] =
        `>Q`.offer(names).as(Some(()))

      inline def apply()(`<Q`: ><): IO[Seq[Any]] =
        `<Q`.take

      inline def apply[T]()(`<Q`: ><)(code: Seq[Any] => IO[Seq[Any]]): IO[Seq[Any]] =
        `<Q`.take.flatMap {
          case it @ Seq(null, _*) => IO.pure(it)
          case it => (code andThen exec)(it)
        }
