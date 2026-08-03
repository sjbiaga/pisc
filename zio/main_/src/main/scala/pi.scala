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
  import _root_.zio.{ Promise, Ref, Task, UIO, ZIO }
  import _root_.zio.concurrent.CyclicBarrier

  import `Π-magic`.*


  given [A]: Conversion[ZIO[Any, Throwable, A], ZIO[Any, Nothing, A]] = _.either.map(_.right.get)


  private def exec[T](code: => Task[T]): UIO[T] =
    code.absorb.either.map {
      case Right(it) => it
      case _         => null.asInstanceOf[T]
    }


  /**
    * restriction aka new name
    */
  object ν:

    def map[B](f: `()` => B): UIO[B] = flatMap(f andThen ZIO.succeed)
    def flatMap[B](f: `()` => UIO[B]): UIO[B] =
      Queue.synchronous[Task, (Any, CyclicBarrier)].map(><(_, false)).flatMap(Ref.make).map(`()`).flatMap(f)


  /**
    * silent transition
    */
  val τ: UIO[Option[Unit]] = ZIO.succeed(Some(()))


  /**
    * prefix
    */
  implicit final class `()`(private val name: Any) extends AnyVal:

    private def ref = `()`[>*<]

    def ====(that: `()`) =
      try
        this.ref eq that.ref
      catch _ =>
        this.name == that.name

    inline def unary_! : Boolean = name == null
    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(value: => S)(using DummyImplicit): UIO[Option[Unit]] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(value.asInstanceOf[`()`])
      else
        apply(false)(ZIO.attempt(value))

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(value: => S)(code: => Task[Any])(using DummyImplicit): UIO[Option[Unit]] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(value.asInstanceOf[`()`])(code)
      else
        apply(true)(ZIO.attempt(value))(code)

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(value: => Task[S]): UIO[Option[Unit]] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.attempt(value.asInstanceOf[UIO[`()`]].flatMap(apply(_))).flatten
      else
        ZIO.attempt(value.map(new `()`(_)).flatMap(apply(_))).flatten

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(value: => Task[S])(code: => Task[Any]): UIO[Option[Unit]] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.attempt(value.asInstanceOf[UIO[`()`]].flatMap(apply(_)(code))).flatten
      else
        ZIO.attempt(value.map(new `()`(_)).flatMap(apply(_)(code))).flatten

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`): UIO[Option[Unit]] = ><(value.name)(ref)

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`)(code: => Task[Any]): UIO[Option[Unit]] = ><(value.name)(ref)(code)

    /**
      * positive prefix i.e. input
      */
    def apply(): UIO[`()`] = ><()(ref).map(new `()`(_))

    /**
      * positive prefix i.e. input
      */
    def apply[T]()(code: T => Task[T]): UIO[`()`] = ><()(ref)(code).map(new `()`(_))

    override def toString: String = if name == null then "null" else name.toString


  private object `Π-magic`:

    final case class ><(queue: Queue[Task, (Any, CyclicBarrier)], stop: Boolean)

    type >*< = Ref[><]

    object >< :

      def apply(name: Any)(`>R`: >*<): UIO[Option[Unit]] =
        CyclicBarrier.make(2).flatMap { b2 =>
          `>R`.modify { case it @ ><(q, _) =>
            q.offer(name -> b2) -> it
          }.flatten *> b2.await.exit *>
          `>R`.modify { case it @ ><(_, stop) =>
            (if stop then None else Some(())) -> it
          }
        }

      def apply(name: Any)(`>R`: >*<)(code: => Task[Any]): UIO[Option[Unit]] =
        CyclicBarrier.make(2).flatMap { b2 =>
          `>R`.modify { case it @ ><(q, _) =>
            q.offer(name -> b2) -> it
          }.flatten *> exec(code) *> b2.await.exit *>
          `>R`.modify { case it @ ><(_, stop) =>
            (if stop then None else Some(())) -> it
          }
        }

      def apply()(`<R`: >*<): UIO[Any] =
        `<R`.modify { case it @ ><(q, _) =>
          q.take -> it
        }.flatten.flatMap { (name, b2) =>
          ZIO.succeed(name) <* b2.await.exit
        }

      def apply[T]()(`<R`: >*<)(code: T => Task[T]): UIO[Any] =
        `<R`.modify { case it @ ><(q, _) =>
          q.take -> it
        }.flatten.flatMap {
          case it @ (null, _) => ZIO.succeed(it)
          case (it: T, b2)    => (code andThen exec)(it)
                                   .tap {
                                     case null => `<R`.update(_.copy(stop = true))
                                     case _    => ZIO.unit
                                   }.map(_ -> b2)
        }.flatMap { (name, b2) =>
          ZIO.succeed(name) <* b2.await.exit
        }
