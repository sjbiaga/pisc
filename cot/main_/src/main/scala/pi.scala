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

  import _root_.cats.syntax.functor.*
  import _root_.cats.syntax.flatMap.*

  import _root_.cats.{ Defer, Monad }
  import _root_.cats.data.OptionT
  import _root_.cats.effect.{ Concurrent, Ref, Temporal }
  import _root_.cats.effect.std.{ CyclicBarrier, Queue }

  import `Π-magic`.*


  /**
    * restriction aka new name
    */
  final class ν[F[_]: Concurrent: Defer: Temporal]:

    def map[B](f: `()`[F] => B): OptionT[F, B] = flatMap(f andThen OptionT.pure[F].apply)
    def flatMap[B](f: `()`[F] => OptionT[F, B]): OptionT[F, B] =
      ( for
          queue <- OptionT.liftF(Queue.unbounded[F, (Any, CyclicBarrier[F])])
          stop  <- OptionT.liftF(Ref[F].of(false))
         yield
          f(`()`[F](><(queue, stop)))
      ).flatten

  /**
    * silent transition
    */
  final class τ[F[_]: Concurrent: Temporal] extends τMacros[F]:

    def apply(): OptionT[F, Unit] = OptionT.liftF(Concurrent[F].cede)

    /**
      * modulo replication guard
      */
    def apply(_f: false)(parallelism: Int)(body: OptionT[F, Unit]): OptionT[F, Unit] =
      super.silent(false)(parallelism)(body)

    /**
      * modulo replication guard w/ pace
      */
    def apply(_f: false)(pace: FiniteDuration, parallelism: Int)(body: OptionT[F, Unit]): OptionT[F, Unit] =
      super.silent(false)(pace, parallelism)(body)

    /**
      * modulo replication guard w/ code
      */
    def apply(_t: true)(parallelism: Int)(code: OptionT[F, Any])(body: OptionT[F, Unit]): OptionT[F, Unit] =
      super.silent(true)(parallelism)(code)(body)

    /**
      * modulo replication guard w/ pace w/ code
      */
    def apply(_t: true)(pace: FiniteDuration, parallelism: Int)(code: OptionT[F, Any])(body: OptionT[F, Unit]): OptionT[F, Unit] =
      super.silent(true)(pace, parallelism)(code)(body)


  /**
    * prefix
    */
  implicit final class `()`[F[_]: Concurrent: Defer: Temporal](/*private[Π] */val name: Any) extends Macros[F]:

    protected def eth = `()`[><[F]]

    def ====(that: `()`[F]) =
      try
        this.eth eq that.eth
      catch _ =>
        this.name == that.name

    inline def unary_! : Boolean = name == null
    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()`[F] = this

    // MODULO REPLICATION //////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////// BOUND //

    /**
      * modulo replication bound output guard
      */
    def apply(_nu: "ν")(_f: false)(parallelism: Int)(body: `()`[F] => OptionT[F, Unit]): OptionT[F, Unit] =
      super.output("ν")(false)(parallelism)(body)

    /**
      * modulo replication bound output guard w/ pace
      */
    def apply(_nu: "ν")(_f: false)(pace: FiniteDuration, parallelism: Int)(body: `()`[F] => OptionT[F, Unit]): OptionT[F, Unit] =
      super.output("ν")(false)(pace, parallelism)(body)

    /**
      * modulo replication bound output guard w/ code
      */
    def apply(_nu: "ν")(_t: true)(parallelism: Int)(code: => OptionT[F, Any])(body: `()`[F] => OptionT[F, Unit]): OptionT[F, Unit] =
      super.output("ν")(true)(parallelism)(code)(body)

    /**
      * modulo replication bound output guard w/ pace w/ code
      */
    def apply(_nu: "ν")(_t: true)(pace: FiniteDuration, parallelism: Int)(code: => OptionT[F, Any])(body: `()`[F] => OptionT[F, Unit]): OptionT[F, Unit] =
      super.output("ν")(true)(pace, parallelism)(code)(body)

    //////////////////////////////////////////////////////////////// CONSTANT //

    /**
      * modulo constant replication output guard
      */
    def apply(_f: false)(parallelism: Int, value: `()`[F])(body: OptionT[F, Unit]): OptionT[F, Unit] =
      super.output(false)(parallelism, value)(body)

    /**
      * modulo constant replication output guard w/ pace
      */
    def apply(_f: false)(pace: FiniteDuration, parallelism: Int, value: `()`[F])(body: OptionT[F, Unit]): OptionT[F, Unit] =
      super.output(false)(pace, parallelism, value)(body)

    /**
      * modulo constant replication output guard w/ code
      */
    def apply(_t: true)(parallelism: Int, value: `()`[F])(code: => OptionT[F, Any])(body: OptionT[F, Unit]): OptionT[F, Unit] =
      super.output(true)(parallelism, value)(code)(body)

    /**
      * modulo constant replication output guard w/ pace w/ code
      */
    def apply(_t: true)(pace: FiniteDuration, parallelism: Int, value: `()`[F])(code: => OptionT[F, Any])(body: OptionT[F, Unit]): OptionT[F, Unit] =
      super.output(true)(pace, parallelism, value)(code)(body)

    //////////////////////////////////////////////////////////////// VARIABLE //

    /**
      * modulo variable replication output guard
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(parallelism: Int, value: => S)(body: OptionT[F, Unit])(using DummyImplicit): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(false)(parallelism, value.asInstanceOf[`()`[F]])(body)
      else
        Defer[[X] =>> OptionT[F, X]].defer(apply("*")(false)(parallelism, OptionT.pure[F](value))(body))

    /**
      * modulo variable replication output guard w/ pace
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(pace: FiniteDuration, parallelism: Int, value: => S)(body: OptionT[F, Unit])(using DummyImplicit): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(false)(pace, parallelism, value.asInstanceOf[`()`[F]])(body)
      else
        Defer[[X] =>> OptionT[F, X]].defer(apply("*")(false)(pace, parallelism, OptionT.pure[F](value))(body))

    /**
      * modulo variable replication output guard w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(parallelism: Int, value: => S)(code: => OptionT[F, Any])(body: OptionT[F, Unit])(using DummyImplicit): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(true)(parallelism, value.asInstanceOf[`()`[F]])(code)(body)
      else
        Defer[[X] =>> OptionT[F, X]].defer(apply("*")(true)(parallelism, OptionT.pure[F](value))(code)(body))

    /**
      * modulo variable replication output guard w/ pace w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(pace: FiniteDuration, parallelism: Int, value: => S)(code: => OptionT[F, Any])(body: OptionT[F, Unit])(using DummyImplicit): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(true)(pace, parallelism, value.asInstanceOf[`()`[F]])(code)(body)
      else
        Defer[[X] =>> OptionT[F, X]].defer(apply("*")(true)(pace, parallelism, OptionT.pure[F](value))(code)(body))

    /**
      * modulo variable replication output guard
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(parallelism: Int, value: => OptionT[F, S])(body: OptionT[F, Unit]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        Defer[[X] =>> OptionT[F, X]].defer(value.asInstanceOf[OptionT[F, `()`[F]]].flatMap(apply(false)(parallelism, _)(body)))
      else
        super.output("*")(false)(parallelism, value)(body)

    /**
      * modulo variable replication output guard w/ pace
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(pace: FiniteDuration, parallelism: Int, value: => OptionT[F, S])(body: OptionT[F, Unit]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        Defer[[X] =>> OptionT[F, X]].defer(value.asInstanceOf[OptionT[F, `()`[F]]].flatMap(apply(false)(pace, parallelism, _)(body)))
      else
        super.output("*")(false)(pace, parallelism, value)(body)

    /**
      * modulo variable replication output guard w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(parallelism: Int, value: => OptionT[F, S])(code: => OptionT[F, Any])(body: OptionT[F, Unit]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        Defer[[X] =>> OptionT[F, X]].defer(value.asInstanceOf[OptionT[F, `()`[F]]].flatMap(apply(true)(parallelism, _)(code)(body)))
      else
        super.output("*")(true)(parallelism, value)(code)(body)

    /**
      * modulo variable replication output guard w/ pace w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(pace: FiniteDuration, parallelism: Int, value: => OptionT[F, S])(code: => OptionT[F, Any])(body: OptionT[F, Unit]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        Defer[[X] =>> OptionT[F, X]].defer(value.asInstanceOf[OptionT[F, `()`[F]]].flatMap(apply(true)(pace, parallelism, _)(code)(body)))
      else
        super.output("*")(true)(pace, parallelism, value)(code)(body)

    /////////////////////////////////////////////////////////////////// INPUT //

    /**
      * modulo replication input guard
      */
    def apply(_n: Null)(_f: false)(parallelism: Int)(body: `()`[F] => OptionT[F, Unit]): OptionT[F, Unit] =
      super.input(false)(parallelism)(body)

    /**
      * modulo replication input guard w/ pace
      */
    def apply(_n: Null)(_f: false)(pace: FiniteDuration, parallelism: Int)(body: `()`[F] => OptionT[F, Unit]): OptionT[F, Unit] =
      super.input(false)(pace, parallelism)(body)

    /**
      * modulo replication input guard w/ code
      */
    def apply[T: ClassTag](_n: Null)(_t: true)(parallelism: Int)(code: T => OptionT[F, Any])(body: `()`[F] => OptionT[F, Unit]): OptionT[F, Unit] =
      super.input(true)(parallelism)(code)(body)

    /**
      * modulo replication input guard w/ pace w/ code
      */
    def apply[T: ClassTag](_n: Null)(_t: true)(pace: FiniteDuration, parallelism: Int)(code: T => OptionT[F, Any])(body: `()`[F] => OptionT[F, Unit]): OptionT[F, Unit] =
      super.input(true)(pace, parallelism)(code)(body)

    ////////////////////////////////////////////////////// modulo replication //

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(value: => S)(using DummyImplicit): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(value.asInstanceOf[`()`[F]])
      else
        OptionT.catsDataDeferForOptionT[F].defer(apply(false)(OptionT.pure[F](value)))

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(value: => S)(code: => OptionT[F, Any])(using DummyImplicit): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(value.asInstanceOf[`()`[F]])(code)
      else
        OptionT.catsDataDeferForOptionT[F].defer(apply(true)(OptionT.pure[F](value))(code))

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(value: => OptionT[F, S]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        OptionT.catsDataDeferForOptionT[F].defer(value.asInstanceOf[OptionT[F, `()`[F]]].flatMap(apply(_)))
      else
        OptionT.catsDataDeferForOptionT[F].defer(value.map(new `()`[F](_)).flatMap(apply(_)))

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(value: => OptionT[F, S])(code: => OptionT[F, Any]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        OptionT.catsDataDeferForOptionT[F].defer(value.asInstanceOf[OptionT[F, `()`[F]]].flatMap(apply(_)(code)))
      else
        OptionT.catsDataDeferForOptionT[F].defer(value.map(new `()`[F](_)).flatMap(apply(_)(code)))

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`[F]): OptionT[F, Unit] =
      ><(value.name)(eth)

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`[F])(code: => OptionT[F, Any]): OptionT[F, Unit] =
      apply(value).flatMap(_ => code).void

    /**
      * positive prefix i.e. input
      */
    def apply(): OptionT[F, `()`[F]] =
      ><()(eth).map(new `()`[F](_))

    /**
      * positive prefix i.e. input
      */
    def apply[T: ClassTag]()(code: T => OptionT[F, Any]): OptionT[F, `()`[F]] =
      ><()(code)(eth).map(new `()`[F](_))

    override def toString: String = if name == null then "null" else name.toString


  protected object `Π-magic`:

    final case class ><[F[_]](queue: Queue[F, (Any, CyclicBarrier[F])], stop: Ref[F, Boolean])

    object >< :

      def apply[F[_]: Concurrent](name: Any)(`>Q`: ><[F]): OptionT[F, Unit] =
        OptionT.liftF(CyclicBarrier[F](2)).flatMap { b2 =>
          OptionT.liftF(`>Q`.queue.offer(name -> b2))
            .semiflatTap(_ => b2.await)
            .flatMap(_ => OptionT.unlessM(`>Q`.stop.get)(Concurrent[F].unit))
          }

      def apply[F[_]: Monad]()(`<Q`: ><[F]): OptionT[F, Any] =
        OptionT.liftF(`<Q`.queue.take).flatMap { (name, b2) =>
          OptionT.liftF(`<Q`.stop.set(name == null))
            .semiflatTap(_ => b2.await)
            .flatMap(_ => OptionT.unless(name == null)(name))
        }

      def apply[F[_]: Monad, T: ClassTag]()(code: T => OptionT[F, Any])(`<Q`: ><[F]): OptionT[F, Any] =
        OptionT.liftF(`<Q`.queue.take).flatMap {
          case it @ (null, _) => OptionT.pure(it)
          case (it: T, b2) => code(it).orElse(OptionT.pure(null)).map(_ -> b2)
        }.flatMap { (name, b2) =>
          OptionT.liftF(`<Q`.stop.set(name == null))
            .semiflatTap(_ => b2.await)
            .flatMap(_ => OptionT.unless(name == null)(name))
        }
