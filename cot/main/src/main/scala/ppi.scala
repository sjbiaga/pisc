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

  import _root_.cats.instances.seq.*
  import _root_.cats.syntax.functor.*
  import _root_.cats.syntax.traverse.*

  import _root_.cats.{ Defer, Monad }
  import _root_.cats.data.OptionT
  import _root_.cats.effect.{ Deferred, Temporal }
  import _root_.cats.effect.std.Queue

  import `Π-magic`.*


  extension [F[_]: Temporal, A](self: OptionT[F, A])
    def race(using signal: Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, A] =
      OptionT(Temporal[[X] =>> OptionT[F, X]].race(signal.get.flatMap(_ => OptionT.none[F, A]), self).map(_.toOption).value.map(_.flatten))

  extension (_self: OptionT.type)
    def sleep[F[_]: Temporal](time: FiniteDuration)(using signal: Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      Temporal[[X] =>> OptionT[F, X]].timeoutTo(signal.get.flatMap(_ => OptionT.none), time, unit)
    def unit[F[_]: Monad]: OptionT[F, Unit] =
      Monad[[X] =>> OptionT[F, X]].unit


  /**
    * restriction aka new name
    */
  final class ν[F[_]: Defer: Temporal]:

    def map[B](f: `()`[F] => B): OptionT[F, B] = flatMap(f andThen OptionT.pure[F].apply)
    def flatMap[B](f: `()`[F] => OptionT[F, B]): OptionT[F, B] =
      OptionT.liftF(Queue.unbounded[F, Any]).map(`()`[F]).flatMap(f)


  /**
    * silent transition
    */
  final class τ[F[_]: Temporal] extends τMacros[F]:

    def apply(): OptionT[F, Unit] = OptionT.liftF(Temporal[F].cede)

    /**
      * modulo replication guard
      */
    def apply(_f: false)(parallelism: Int)(body: OptionT[F, Unit]): OptionT[F, Unit] =
      super.silent(false)(parallelism)(body)

    /**
      * modulo replication guard w/ pace
      */
    def apply(_f: false)(pace: FiniteDuration, parallelism: Int)(body: OptionT[F, Unit])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      super.silent(false)(pace, parallelism)(body)

    /**
      * modulo replication guard w/ code
      */
    def apply(_t: true)(parallelism: Int)(code: => OptionT[F, Any])(body: OptionT[F, Unit]): OptionT[F, Unit] =
      super.silent(true)(parallelism)(code)(body)

    /**
      * modulo replication guard w/ pace w/ code
      */
    def apply(_t: true)(pace: FiniteDuration, parallelism: Int)(code: => OptionT[F, Any])(body: OptionT[F, Unit])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      super.silent(true)(pace, parallelism)(code)(body)


  /**
    * prefix
    */
  implicit final class `()`[F[_]: Defer: Temporal](/*private[Π] */val name: Any) extends Macros[F]:

    protected def q = `()`[><[F]]

    def ====(that: `()`[F]) =
      try
        this.q eq that.q
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
    def apply(_nu: "ν")(_f: false)(arity: Int)(parallelism: Int)(body: Seq[`()`[F]] => OptionT[F, Unit])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      super.output("ν")(false)(arity)(parallelism)(body)

    /**
      * modulo replication bound output guard w/ pace
      */
    def apply(_nu: "ν")(_f: false)(arity: Int)(pace: FiniteDuration, parallelism: Int)(body: Seq[`()`[F]] => OptionT[F, Unit])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      super.output("ν")(false)(arity)(pace, parallelism)(body)

    /**
      * modulo replication bound output guard w/ code
      */
    def apply(_nu: "ν")(_t: true)(arity: Int)(parallelism: Int)(code: => OptionT[F, Any])(body: Seq[`()`[F]] => OptionT[F, Unit])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      super.output("ν")(true)(arity)(parallelism)(code)(body)

    /**
      * modulo replication bound output guard w/ pace w/ code
      */
    def apply(_nu: "ν")(_t: true)(arity: Int)(pace: FiniteDuration, parallelism: Int)(code: => OptionT[F, Any])(body: Seq[`()`[F]] => OptionT[F, Unit])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      super.output("ν")(true)(arity)(pace, parallelism)(code)(body)

    //////////////////////////////////////////////////////////////// CONSTANT //

    /**
      * modulo constant replication output guard
      */
    def apply(_m: "-")(_f: false)(parallelism: Int, value: `()`[F]*)(body: OptionT[F, Unit])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      super.output(false)(parallelism, value*)(body)

    /**
      * modulo constant replication output guard w/ pace
      */
    def apply(_m: "-")(_f: false)(pace: FiniteDuration, parallelism: Int, value: `()`[F]*)(body: OptionT[F, Unit])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      super.output(false)(pace, parallelism, value*)(body)

    /**
      * modulo constant replication output guard w/ code
      */
    def apply(_m: "-")(_t: true)(parallelism: Int, value: `()`[F]*)(code: => OptionT[F, Any])(body: OptionT[F, Unit])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      super.output(true)(parallelism, value*)(code)(body)

    /**
      * modulo constant replication output guard w/ pace w/ code
      */
    def apply(_m: "-")(_t: true)(pace: FiniteDuration, parallelism: Int, value: `()`[F]*)(code: => OptionT[F, Any])(body: OptionT[F, Unit])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      super.output(true)(pace, parallelism, value*)(code)(body)

    //////////////////////////////////////////////////////////////// VARIABLE //

    /**
      * modulo variable replication output guard
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(parallelism: Int, value: => S*)(body: OptionT[F, Unit])(using DummyImplicit)(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply("-")(false)(parallelism, value.map(_.asInstanceOf[`()`[F]])*)(body)
      else
        Defer[[X] =>> OptionT[F, X]].defer(apply("*")(false)(parallelism, value.map(OptionT.pure[F].apply(_))*)(body))

    /**
      * modulo variable replication output guard w/ pace
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(pace: FiniteDuration, parallelism: Int, value: => S*)(body: OptionT[F, Unit])(using DummyImplicit)(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply("-")(false)(pace, parallelism, value.map(_.asInstanceOf[`()`[F]])*)(body)
      else
        Defer[[X] =>> OptionT[F, X]].defer(apply("*")(false)(pace, parallelism, value.map(OptionT.pure[F].apply(_))*)(body))

    /**
      * modulo variable replication output guard w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(parallelism: Int, value: => S*)(code: => OptionT[F, Any])(body: OptionT[F, Unit])(using DummyImplicit)(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply("-")(true)(parallelism, value.map(_.asInstanceOf[`()`[F]])*)(code)(body)
      else
        Defer[[X] =>> OptionT[F, X]].defer(apply("*")(true)(parallelism, value.map(OptionT.pure[F].apply(_))*)(code)(body))

    /**
      * modulo variable replication output guard w/ pace w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(pace: FiniteDuration, parallelism: Int, value: => S*)(code: => OptionT[F, Any])(body: OptionT[F, Unit])(using DummyImplicit)(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply("-")(true)(pace, parallelism, value.map(_.asInstanceOf[`()`[F]])*)(code)(body)
      else
        Defer[[X] =>> OptionT[F, X]].defer(apply("*")(true)(pace, parallelism, value.map(OptionT.pure[F].apply(_))*)(code)(body))

    /**
      * modulo variable replication output guard
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(parallelism: Int, value: => OptionT[F, S]*)(body: OptionT[F, Unit])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        Defer[[X] =>> OptionT[F, X]].defer(value.map(_.asInstanceOf[OptionT[F, `()`[F]]]).sequence.flatMap(apply("-")(false)(parallelism, _*)(body)))
      else
        Defer[[X] =>> OptionT[F, X]].defer(super.output("*")(false)(parallelism, value.sequence)(body))

    /**
      * modulo variable replication output guard w/ pace
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(pace: FiniteDuration, parallelism: Int, value: => OptionT[F, S]*)(body: OptionT[F, Unit])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        Defer[[X] =>> OptionT[F, X]].defer(value.map(_.asInstanceOf[OptionT[F, `()`[F]]]).sequence.flatMap(apply("-")(false)(pace, parallelism, _*)(body)))
      else
        Defer[[X] =>> OptionT[F, X]].defer(super.output("*")(false)(pace, parallelism, value.sequence)(body))

    /**
      * modulo variable replication output guard w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(parallelism: Int, value: => OptionT[F, S]*)(code: => OptionT[F, Any])(body: OptionT[F, Unit])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        Defer[[X] =>> OptionT[F, X]].defer(value.map(_.asInstanceOf[OptionT[F, `()`[F]]]).sequence.flatMap(apply("-")(true)(parallelism, _*)(code)(body)))
      else
        Defer[[X] =>> OptionT[F, X]].defer(super.output("*")(true)(parallelism, value.sequence)(code)(body))

    /**
      * modulo variable replication output guard w/ pace w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(pace: FiniteDuration, parallelism: Int, value: => OptionT[F, S]*)(code: => OptionT[F, Any])(body: OptionT[F, Unit])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        Defer[[X] =>> OptionT[F, X]].defer(value.map(_.asInstanceOf[OptionT[F, `()`[F]]]).sequence.flatMap(apply("-")(true)(pace, parallelism, _*)(code)(body)))
      else
        Defer[[X] =>> OptionT[F, X]].defer(super.output("*")(true)(pace, parallelism, value.sequence)(code)(body))

    /////////////////////////////////////////////////////////////////// INPUT //

    /**
      * modulo replication input guard
      */
    def apply(_n: Null)(_f: false)(parallelism: Int)(body: Seq[`()`[F]] => OptionT[F, Unit])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      super.input(false)(parallelism)(body)

    /**
      * modulo replication input guard w/ pace
      */
    def apply(_n: Null)(_f: false)(pace: FiniteDuration, parallelism: Int)(body: Seq[`()`[F]] => OptionT[F, Unit])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      super.input(false)(pace, parallelism)(body)

    /**
      * modulo replication input guard w/ code
      */
    def apply[T: ClassTag](_n: Null)(_t: true)(parallelism: Int)(code: Seq[T] => OptionT[F, Seq[Any]])(body: Seq[`()`[F]] => OptionT[F, Unit])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      super.input(true)(parallelism)(code)(body)

    /**
      * modulo replication input guard w/ pace w/ code
      */
    def apply[T: ClassTag](_n: Null)(_t: true)(pace: FiniteDuration, parallelism: Int)(code: Seq[T] => OptionT[F, Seq[Any]])(body: Seq[`()`[F]] => OptionT[F, Unit])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      super.input(true)(pace, parallelism)(code)(body)

    ////////////////////////////////////////////////////// modulo replication //

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(value: => S*)(using DummyImplicit)(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(value.map(_.asInstanceOf[`()`[F]])*)
      else
        Defer[[X] =>> OptionT[F, X]].defer(apply(false)(value.map(OptionT.pure[F].apply(_))*))

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(value: => S*)(code: => OptionT[F, Any])(using DummyImplicit)(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(value.map(_.asInstanceOf[`()`[F]])*)(code)
      else
        Defer[[X] =>> OptionT[F, X]].defer(apply(true)(value.map(OptionT.pure[F].apply(_))*)(code))

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(value: => OptionT[F, S]*)(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        Defer[[X] =>> OptionT[F, X]].defer(value.map(_.asInstanceOf[OptionT[F, `()`[F]]]).sequence.flatMap(apply(_*)))
      else
        Defer[[X] =>> OptionT[F, X]].defer(value.sequence.map(_.map(new `()`[F](_))).flatMap(apply(_*)))

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(value: => OptionT[F, S]*)(code: => OptionT[F, Any])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        Defer[[X] =>> OptionT[F, X]].defer(value.map(_.asInstanceOf[OptionT[F, `()`[F]]]).sequence.flatMap(apply(_*)(code)))
      else
        Defer[[X] =>> OptionT[F, X]].defer(value.sequence.map(_.map(new `()`[F](_))).flatMap(apply(_*)(code)))

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`[F]*)(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      ><(value.map(_.name))(q)

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`[F]*)(code: => OptionT[F, Any])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
      apply(value.map(_.name)).flatMap(_ => code).void

    /**
      * positive prefix i.e. input
      */
    def apply()(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Seq[`()`[F]]] =
      ><()(q).map(_.map(new `()`[F](_)))

    /**
      * positive prefix i.e. input
      */
    def apply[T: ClassTag]()(code: Seq[T] => OptionT[F, Seq[Any]])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Seq[`()`[F]]] =
      ><[F, T]()(q)(code).map(_.map(new `()`[F](_)))

    override def toString: String = if name == null then "null" else name.toString


  protected object `Π-magic`:

    type ><[F[_]] = Queue[F, Seq[Any]]

    object >< :

      inline def apply[F[_]: Temporal](names: Seq[Any])(`>Q`: ><[F])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Unit] =
        OptionT.liftF(`>Q`.offer(names)).race

      inline def apply[F[_]: Temporal]()(`<Q`: ><[F])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Seq[Any]] =
        OptionT.liftF(`<Q`.take).race

      inline def apply[F[_]: Temporal, T: ClassTag]()(`<Q`: ><[F])(code: Seq[T] => OptionT[F, Seq[Any]])(using Deferred[[X] =>> OptionT[F, X], Unit]): OptionT[F, Seq[Any]] =
        ( OptionT.liftF(`<Q`.take).flatMap {
            case it @ Seq(null, _*) => OptionT.pure(it)
            case it: Seq[T] => code(it)
          }
        ).race
