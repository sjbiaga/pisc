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

  import _root_.scala.concurrent.duration.FiniteDuration

  import _root_.scala.reflect.{ ClassTag, classTag }

  import _root_.cats.effect.IO
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.{ CyclicBarrier, Queue, Semaphore, Supervisor }

  import `Π-magic`.*


  /**
    * Supervised [[code]].
    * @param code
    */
  private def exec[T](code: => IO[T]): IO[T] =
    Supervisor[IO](await = true)
      .use(_.supervise(code))
      .flatMap(_.join)
      .flatMap {
        case Succeeded(it) => it
        case _             => IO.pure(null.asInstanceOf[T])
      }


  /**
    * restriction aka new name
    */
  object ν:

    def map[B](f: `()` => B): IO[B] = flatMap(f andThen IO.pure)
    def flatMap[B](f: `()` => IO[B]): IO[B] =
      Queue.unbounded[IO, Any].map(`()`).flatMap(f)


  /**
    * silent transition
    */
  object τ extends τ:

    def apply(): IO[Option[Unit]] = IO.cede.as(Some(()))

    /**
      * linear replication guard
      */
    def apply(_f: false)(parallelism: Int)(body: IO[Any]): IO[Unit] =
      super.silent(false)(parallelism)(body)

    /**
      * linear replication guard w/ pace
      */
    def apply(_f: false)(pace: FiniteDuration, parallelism: Int)(body: IO[Any]): IO[Unit] =
      super.silent(false)(pace, parallelism)(body)

    /**
      * linear replication guard w/ code
      */
    def apply(_t: true)(parallelism: Int)(code: IO[Any])(body: IO[Any]): IO[Unit] =
      super.silent(true)(parallelism)(code)(body)

    /**
      * linear replication guard w/ pace w/ code
      */
    def apply(_t: true)(pace: FiniteDuration, parallelism: Int)(code: IO[Any])(body: IO[Any]): IO[Unit] =
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
    def apply(_nu: "ν")(_f: false)(parallelism: Int)(body: `()` => IO[Any]): IO[Unit] =
      super.output("ν")(false)(parallelism)(body)

    /**
      * linear replication bound output guard w/ pace
      */
    def apply(_nu: "ν")(_f: false)(pace: FiniteDuration, parallelism: Int)(body: `()` => IO[Any]): IO[Unit] =
      super.output("ν")(false)(pace, parallelism)(body)

    /**
      * linear replication bound output guard w/ code
      */
    def apply(_nu: "ν")(_t: true)(parallelism: Int)(code: IO[Any])(body: `()` => IO[Any]): IO[Unit] =
      super.output("ν")(true)(parallelism)(code)(body)

    /**
      * linear replication bound output guard w/ pace w/ code
      */
    def apply(_nu: "ν")(_t: true)(pace: FiniteDuration, parallelism: Int)(code: IO[Any])(body: `()` => IO[Any]): IO[Unit] =
      super.output("ν")(true)(pace, parallelism)(code)(body)

    //////////////////////////////////////////////////////////////// CONSTANT //

    /**
      * linear constant replication output guard
      */
    def apply(_f: false)(parallelism: Int, value: `()`)(body: IO[Any]): IO[Unit] =
      super.output(false)(parallelism, value)(body)

    /**
      * linear constant replication output guard w/ pace
      */
    def apply(_f: false)(pace: FiniteDuration, parallelism: Int, value: `()`)(body: IO[Any]): IO[Unit] =
      super.output(false)(pace, parallelism, value)(body)

    /**
      * linear constant replication output guard w/ code
      */
    def apply(_t: true)(parallelism: Int, value: `()`)(code: IO[Any])(body: IO[Any]): IO[Unit] =
      super.output(true)(parallelism, value)(code)(body)

    /**
      * linear constant replication output guard w/ pace w/ code
      */
    def apply(_t: true)(pace: FiniteDuration, parallelism: Int, value: `()`)(code: IO[Any])(body: IO[Any]): IO[Unit] =
      super.output(true)(pace, parallelism, value)(code)(body)

    //////////////////////////////////////////////////////////////// VARIABLE //

    /**
      * linear variable replication output guard
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(parallelism: Int, value: => S)(body: IO[Any])(using DummyImplicit): IO[Unit] =
     if classTag[S].runtimeClass eq getClass
     then
       apply(false)(parallelism, value.asInstanceOf[`()`])(body)
     else
       apply("*")(false)(parallelism, IO.delay(value))(body)

    /**
      * linear variable replication output guard w/ pace
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(pace: FiniteDuration, parallelism: Int, value: => S)(body: IO[Any])(using DummyImplicit): IO[Unit] =
     if classTag[S].runtimeClass eq getClass
     then
       apply(false)(pace, parallelism, value.asInstanceOf[`()`])(body)
     else
       apply("*")(false)(pace, parallelism, IO.delay(value))(body)

    /**
      * linear variable replication output guard w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(parallelism: Int, value: => S)(code: IO[Any])(body: IO[Any])(using DummyImplicit): IO[Unit] =
     if classTag[S].runtimeClass eq getClass
     then
       apply(true)(parallelism, value.asInstanceOf[`()`])(code)(body)
     else
       apply("*")(true)(parallelism, IO.delay(value))(code)(body)

    /**
      * linear variable replication output guard w/ pace w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(pace: FiniteDuration, parallelism: Int, value: => S)(code: IO[Any])(body: IO[Any])(using DummyImplicit): IO[Unit] =
     if classTag[S].runtimeClass eq getClass
     then
       apply(true)(pace, parallelism, value.asInstanceOf[`()`])(code)(body)
     else
       apply("*")(true)(pace, parallelism, IO.delay(value))(code)(body)

    /**
      * linear variable replication output guard
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(parallelism: Int, value: => IO[S])(body: IO[Any]): IO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        IO.defer(value.asInstanceOf[IO[`()`]].flatMap(apply(false)(parallelism, _)(body)))
      else
        super.output("*")(false)(parallelism, value)(body)

    /**
      * linear variable replication output guard w/ pace
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(pace: FiniteDuration, parallelism: Int, value: => IO[S])(body: IO[Any]): IO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        IO.defer(value.asInstanceOf[IO[`()`]].flatMap(apply(false)(pace, parallelism, _)(body)))
      else
        super.output("*")(false)(pace, parallelism, value)(body)

    /**
      * linear variable replication output guard w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(parallelism: Int, value: => IO[S])(code: IO[Any])(body: IO[Any]): IO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        IO.defer(value.asInstanceOf[IO[`()`]].flatMap(apply(true)(parallelism, _)(code)(body)))
      else
        super.output("*")(true)(parallelism, value)(code)(body)

    /**
      * linear variable replication output guard w/ pace w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(pace: FiniteDuration, parallelism: Int, value: => IO[S])(code: IO[Any])(body: IO[Any]): IO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        IO.defer(value.asInstanceOf[IO[`()`]].flatMap(apply(true)(pace, parallelism, _)(code)(body)))
      else
        super.output("*")(true)(pace, parallelism, value)(code)(body)

    /////////////////////////////////////////////////////////////////// INPUT //

    /**
      * linear replication input guard
      */
    def apply(_n: Null)(_f: false)(parallelism: Int)(body: `()` => IO[Any]): IO[Unit] =
      super.input(false)(parallelism)(body)

    /**
      * linear replication input guard w/ pace
      */
    def apply(_n: Null)(_f: false)(pace: FiniteDuration, parallelism: Int)(body: `()` => IO[Any]): IO[Unit] =
      super.input(false)(pace, parallelism)(body)

    /**
      * linear replication input guard w/ code
      */
    def apply[T](_n: Null)(_t: true)(parallelism: Int)(code: T => IO[T])(body: `()` => IO[Any]): IO[Unit] =
      super.input(true)(parallelism)(code)(body)

    /**
      * linear replication input guard w/ pace w/ code
      */
    def apply[T](_n: Null)(_t: true)(pace: FiniteDuration, parallelism: Int)(code: T => IO[T])(body: `()` => IO[Any]): IO[Unit] =
      super.input(true)(pace, parallelism)(code)(body)

    ////////////////////////////////////////////////////// linear replication //

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(value: => S)(using DummyImplicit): IO[Option[Unit]] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(value.asInstanceOf[`()`])
      else
        apply(false)(IO.delay(value))

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(value: => S)(code: => IO[Any])(using DummyImplicit): IO[Option[Unit]] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(value.asInstanceOf[`()`])(code)
      else
        apply(true)(IO.delay(value))(code)

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(value: => IO[S]): IO[Option[Unit]] =
      if classTag[S].runtimeClass eq getClass
      then
        IO.defer(value.asInstanceOf[IO[`()`]].flatMap(apply(_)))
      else
        IO.defer(value.map(new `()`(_)).flatMap(apply(_)))

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(value: => IO[S])(code: => IO[Any]): IO[Option[Unit]] =
      if classTag[S].runtimeClass eq getClass
      then
        IO.defer(value.asInstanceOf[IO[`()`]].flatMap(apply(_)(code)))
      else
        IO.defer(value.map(new `()`(_)).flatMap(apply(_)(code)))

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`): IO[Option[Unit]] = ><(value.name)(q)

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`)(code: => IO[Any]): IO[Option[Unit]] = apply(value) <* exec(code)

    /**
      * positive prefix i.e. input
      */
    def apply(): IO[`()`] = ><()(q).map(new `()`(_))

    /**
      * positive prefix i.e. input
      */
    def apply[T]()(code: T => IO[T]): IO[`()`] = ><()(q)(code).map(new `()`(_))

    override def toString: String = if name == null then "null" else name.toString


  protected object `Π-magic`:

    type >< = Queue[IO, Any]

    object >< :

      inline def apply(name: Any)(`>Q`: ><): IO[Option[Unit]] =
        `>Q`.offer(name).as(Some(()))

      inline def apply()(`<Q`: ><): IO[Any] =
        `<Q`.take

      inline def apply[T]()(`<Q`: ><)(code: T => IO[T]): IO[Any] =
        `<Q`.take.flatMap {
          case null  => IO.pure(null)
          case it: T => (code andThen exec)(it)
        }
