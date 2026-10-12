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

  import _root_.zio.{ Duration, Exit, Queue, Task, UIO, ZIO }

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
      Queue.unbounded[Any].map(`()`).flatMap(f)


  /**
    * silent transition
    */
  object τ extends τ:

    def apply(): UIO[Option[Unit]] = ZIO.yieldNow.as(Some(()))

    /**
      * linear replication guard
      */
    def apply(_f: false)(parallelism: Int)(body: UIO[Any]): UIO[Unit] =
      super.silent(false)(parallelism)(body)

    /**
      * linear replication guard w/ pace
      */
    def apply(_f: false)(pace: Duration, parallelism: Int)(body: UIO[Any]): UIO[Unit] =
      super.silent(false)(pace, parallelism)(body)

    /**
      * linear replication guard w/ code
      */
    def apply(_t: true)(parallelism: Int)(code: UIO[Any])(body: UIO[Any]): UIO[Unit] =
      super.silent(true)(parallelism)(code)(body)

    /**
      * linear replication guard w/ pace w/ code
      */
    def apply(_t: true)(pace: Duration, parallelism: Int)(code: UIO[Any])(body: UIO[Any]): UIO[Unit] =
      super.silent(true)(pace, parallelism)(code)(body)


  /**
    * prefix
    */
  implicit final class `()`(private[Π] val name: Any) extends AnyVal with Macros:

    protected def q = `()`[><]

    def ====(that: `()`) =
      try
        this.q eq that.q
      catch _ =>
        this.name == that.name

    inline def unary_! : Boolean = name == null
    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

    // LINEAR REPLICATION //////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////// BOUND //

    /**
      * linear replication bound output guard
      */
    def apply(_nu: "ν")(_f: false)(parallelism: Int)(body: `()` => UIO[Any]): UIO[Unit] =
      super.output("ν")(false)(parallelism)(body)

    /**
      * linear replication bound output guard w/ pace
      */
    def apply(_nu: "ν")(_f: false)(pace: Duration, parallelism: Int)(body: `()` => UIO[Any]): UIO[Unit] =
      super.output("ν")(false)(pace, parallelism)(body)

    /**
      * linear replication bound output guard w/ code
      */
    def apply(_nu: "ν")(_t: true)(parallelism: Int)(code: UIO[Any])(body: `()` => UIO[Any]): UIO[Unit] =
      super.output("ν")(true)(parallelism)(code)(body)

    /**
      * linear replication bound output guard w/ pace w/ code
      */
    def apply(_nu: "ν")(_t: true)(pace: Duration, parallelism: Int)(code: UIO[Any])(body: `()` => UIO[Any]): UIO[Unit] =
      super.output("ν")(true)(pace, parallelism)(code)(body)

    //////////////////////////////////////////////////////////////// CONSTANT //

    /**
      * linear constant replication output guard
      */
    def apply(_f: false)(parallelism: Int, value: `()`)(body: UIO[Any]): UIO[Unit] =
      super.output(false)(parallelism, value)(body)

    /**
      * linear constant replication output guard w/ pace
      */
    def apply(_f: false)(pace: Duration, parallelism: Int, value: `()`)(body: UIO[Any]): UIO[Unit] =
      super.output(false)(pace, parallelism, value)(body)

    /**
      * linear constant replication output guard w/ code
      */
    def apply(_t: true)(parallelism: Int, value: `()`)(code: UIO[Any])(body: UIO[Any]): UIO[Unit] =
      super.output(true)(parallelism, value)(code)(body)

    /**
      * linear constant replication output guard w/ pace w/ code
      */
    def apply(_t: true)(pace: Duration, parallelism: Int, value: `()`)(code: UIO[Any])(body: UIO[Any]): UIO[Unit] =
      super.output(true)(pace, parallelism, value)(code)(body)

    //////////////////////////////////////////////////////////////// VARIABLE //

    /**
      * linear variable replication output guard
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(parallelism: Int, value: => S)(body: UIO[Any])(using DummyImplicit): UIO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(false)(parallelism, value.asInstanceOf[`()`])(body)
      else
        apply("*")(false)(parallelism, ZIO.attempt(value))(body)

    /**
      * linear variable replication output guard w/ pace
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(pace: Duration, parallelism: Int, value: => S)(body: UIO[Any])(using DummyImplicit): UIO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(false)(pace, parallelism, value.asInstanceOf[`()`])(body)
      else
        apply("*")(false)(pace, parallelism, ZIO.attempt(value))(body)

    /**
      * linear variable replication output guard w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(parallelism: Int, value: => S)(code: UIO[Any])(body: UIO[Any])(using DummyImplicit): UIO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(true)(parallelism, value.asInstanceOf[`()`])(code)(body)
      else
        apply("*")(true)(parallelism, ZIO.attempt(value))(code)(body)

    /**
      * linear variable replication output guard w/ pace w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(pace: Duration, parallelism: Int, value: => S)(code: UIO[Any])(body: UIO[Any])(using DummyImplicit): UIO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(true)(pace, parallelism, value.asInstanceOf[`()`])(code)(body)
      else
        apply("*")(true)(pace, parallelism, ZIO.attempt(value))(code)(body)

    /**
      * linear variable replication output guard
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(parallelism: Int, value: => Task[S])(body: UIO[Any]): UIO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]].flatMap(apply(false)(parallelism, _)(body)))
      else
        super.output("*")(false)(parallelism, value)(body)

    /**
      * linear variable replication output guard w/ pace
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(pace: Duration, parallelism: Int, value: => Task[S])(body: UIO[Any]): UIO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]].flatMap(apply(false)(pace, parallelism, _)(body)))
      else
        super.output("*")(false)(pace, parallelism, value)(body)

    /**
      * linear variable replication output guard w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(parallelism: Int, value: => Task[S])(code: UIO[Any])(body: UIO[Any]): UIO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]].flatMap(apply(true)(parallelism, _)(code)(body)))
      else
        super.output("*")(true)(parallelism, value)(code)(body)

    /**
      * linear variable replication output guard w/ pace w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(pace: Duration, parallelism: Int, value: => Task[S])(code: UIO[Any])(body: UIO[Any]): UIO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]].flatMap(apply(true)(pace, parallelism, _)(code)(body)))
      else
        super.output("*")(true)(pace, parallelism, value)(code)(body)

    /////////////////////////////////////////////////////////////////// INPUT //

    /**
      * linear replication input guard
      */
    def apply(_n: Null)(_f: false)(parallelism: Int)(body: `()` => UIO[Any]): UIO[Unit] =
      super.input(false)(parallelism)(body)

    /**
      * linear replication input guard w/ pace
      */
    def apply(_n: Null)(_f: false)(pace: Duration, parallelism: Int)(body: `()` => UIO[Any]): UIO[Unit] =
      super.input(false)(pace, parallelism)(body)

    /**
      * linear replication input guard w/ code
      */
    def apply[T](_n: Null)(_t: true)(parallelism: Int)(code: T => Task[T])(body: `()` => UIO[Any]): UIO[Unit] =
      super.input(true)(parallelism)(code)(body)

    /**
      * linear replication input guard w/ pace w/ code
      */
    def apply[T](_n: Null)(_t: true)(pace: Duration, parallelism: Int)(code: T => Task[T])(body: `()` => UIO[Any]): UIO[Unit] =
      super.input(true)(pace, parallelism)(code)(body)

    ////////////////////////////////////////////////////// linear replication //

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
        ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]].flatMap(apply(_)))
      else
        ZIO.suspendSucceed(value.map(new `()`(_)).flatMap(apply(_)))

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(value: => Task[S])(code: => Task[Any]): UIO[Option[Unit]] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]].flatMap(apply(_)(code)))
      else
        ZIO.suspendSucceed(value.map(new `()`(_)).flatMap(apply(_)(code)))

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`): UIO[Option[Unit]] = ><(value.name)(q)

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`)(code: => Task[Any]): UIO[Option[Unit]] = apply(value) <* exec(code)

    /**
      * positive prefix i.e. input
      */
    def apply(): UIO[`()`] = ><()(q).map(new `()`(_))

    /**
      * positive prefix i.e. input
      */
    def apply[T]()(code: T => Task[T]): UIO[`()`] = ><()(q)(code).map(new `()`(_))

    override def toString: String = if name == null then "null" else name.toString


  private object `Π-magic`:

    type >< = Queue[Any]

    object >< :

      inline def apply(name: Any)(`>Q`: ><): UIO[Option[Unit]] =
        `>Q`.offer(name).as(Some(()))

      inline def apply()(`<Q`: ><): UIO[Any] =
        `<Q`.take

      inline def apply[T]()(`<Q`: ><)(code: T => Task[T]): UIO[Any] =
        `<Q`.take.flatMap {
          case null  => ZIO.succeed(null)
          case it: T => (code andThen exec)(it)
        }
