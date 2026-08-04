/*
 * Copyright (c) 2023-2026 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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

  import _root_.scala.reflect.{ ClassTag, classTag }

  import _root_.cats.effect.std.Queue
  import _root_.zio.interop.catz.concurrentInstance
  import _root_.zio.{ Exit, Promise, Task, UIO, ZIO }

  import `Π-magic`.*


  given [A]: Conversion[Task[A], UIO[A]] =
    _.either.map {
      case Right(it) => it
      case _         => null.asInstanceOf[A]
    }

  extension (self: ZIO.type)
    def apply[A](a: => A): UIO[A] =
      ZIO.attempt(a)


  private def exec[T](code: Task[T]): UIO[T] =
    code.fork.flatMap(_.join.exit).map {
      case Exit.Success(it) => it
      case _                => null.asInstanceOf[T]
    }


  /**
    * restriction aka new name
    */
  object ν:

    def map[B](f: `()` => B): UIO[B] = flatMap(f andThen ZIO.succeed)
    def flatMap[B](f: `()` => UIO[B]): UIO[B] =
      Queue.synchronous[Task, Seq[Any]].map(`()`).flatMap(f)


  /**
    * silent transition
    */
  val τ: UIO[Option[Unit]] = ZIO.succeed(Some(()))


  /**
    * prefix
    */
  implicit final class `()`(private val name: Any) extends AnyVal:

    private def q = `()`[><]

    def ====(that: `()`) =
      try
        this.q eq that.q
      catch _ =>
        this.name == that.name

    inline def unary_! : Boolean = name == null
    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(value: => S*)(using DummyImplicit): UIO[Option[Unit]] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(value.map(_.asInstanceOf[`()`])*)
      else
        apply(false)(value.map(ZIO.attempt)*)

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(value: => S*)(code: => Task[Any])(using DummyImplicit): UIO[Option[Unit]] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(value.map(_.asInstanceOf[`()`])*)(code)
      else
        apply(true)(value.map(ZIO.attempt)*)(code)

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(value: => Task[S]*): UIO[Option[Unit]] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.suspendSucceed(ZIO.collectAll(value.map(_.asInstanceOf[Task[`()`]])).flatMap(apply(_*)))
      else
        ZIO.suspendSucceed(ZIO.collectAll(value).map(_.map(new `()`(_))).flatMap(apply(_*)))

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(value: => Task[S]*)(code: => Task[Any]): UIO[Option[Unit]] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.suspendSucceed(ZIO.collectAll(value.map(_.asInstanceOf[Task[`()`]])).flatMap(apply(_*)))
      else
        ZIO.suspendSucceed(ZIO.collectAll(value).map(_.map(new `()`(_))).flatMap(apply(_*)(code)))

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`*): UIO[Option[Unit]] = ><(value.map(_.name))(q)

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`*)(code: => Task[Any]): UIO[Option[Unit]] = apply(value) <* exec(code)

    /**
      * positive prefix i.e. input
      */
    def apply(): UIO[Seq[`()`]] = ><()(q).map(_.map(new `()`(_)))

    /**
      * positive prefix i.e. input
      */
    def apply[T]()(code: Seq[T] => Task[Seq[T]]): UIO[Seq[`()`]] = ><()(q)(code).map(_.map(new `()`(_)))

    override def toString: String = if name == null then "null" else name.toString


  private object `Π-magic`:

    type >< = Queue[Task, Seq[Any]]

    object >< :

      inline def apply(names: Seq[Any])(`>Q`: ><): UIO[Option[Unit]] =
        `>Q`.offer(names).as(Some(()))

      inline def apply()(`<Q`: ><): UIO[Seq[Any]] =
        `<Q`.take

      inline def apply[T]()(`<Q`: ><)(code: Seq[T] => Task[Seq[T]]): UIO[Seq[Any]] =
        `<Q`.take.flatMap {
          case it @ Seq(null, _*) => ZIO.succeed(it.asInstanceOf[Seq[T]])
          case it: Seq[T]         => (code andThen exec)(it)
        }
