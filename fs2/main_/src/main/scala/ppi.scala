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

  import _root_.scala.collection.immutable.Seq
  import _root_.scala.concurrent.duration.FiniteDuration

  import _root_.cats.syntax.apply.*
  import _root_.cats.syntax.functor.*
  import _root_.cats.syntax.flatMap.*
  import _root_.cats.syntax.traverse.*

  import _root_.cats.effect.{ Deferred, Ref, Resource, Temporal }
  import _root_.cats.effect.std.Queue

  import _root_.fs2.Stream
  import _root_.fs2.concurrent.Topic

  import `Π-magic`.*


  /**
    * restriction aka new name
    */
  final class ν[F[_]: Temporal]:

    def map[B](f: `()`[F] => B): Stream[F, B] = flatMap(f andThen Stream.emit[F, B])
    def flatMap[B](f: `()`[F] => Stream[F, B]): Stream[F, B] =
      ( for
          topic <- Stream.eval(Topic[F, (Seq[`()`[F]], Deferred[F, Unit])])
          stop  <- Stream.eval(Deferred[F, Either[Throwable, Unit]])
          queue <- Stream.eval(Queue.unbounded[F, Unit])
          limit <- Stream.eval(Ref[F].of(false))
        yield
          f(><(topic, stop, queue, limit))
      ).flatten


  /**
    * silent transition
    */
  final class τ[F[_]: Temporal]:

    object ! :

      /**
        * replication guard
        */
      def apply(): Stream[F, Unit] =
        Stream.unit.repeat

      /**
        * replication guard w/ pace
        */
      def apply(pace: FiniteDuration): Stream[F, Unit] =
        Stream.awakeEvery(pace).as(())

      /**
        * replication guard w/ code
        */
      def apply[T]()(code: => F[T]): Stream[F, Unit] =
        apply().evalTap(_ => code)

      /**
        * replication guard w/ pace w/ code
        */
      def apply[T](pace: FiniteDuration)(code: => F[T]): Stream[F, Unit] =
        apply(pace).evalTap(_ => code)

    /**
      * prefix
      */
    def apply(): Stream[F, Unit] =
      Stream.unit

    /**
      * prefix w/ code
      */
    def apply[T]()(code: => F[T]): Stream[F, Unit] =
      apply().evalTap(_ => code)

  /**
    * events, i.e., names (topics) and values
    */
  implicit final class `()`[F[_]: Temporal](private val name: Any) { self =>

    private inline def t = `()`[><[F]].topic
    private inline def d = `()`[><[F]].stop
    private inline def q = `()`[><[F]].queue
    private inline def r = `()`[><[F]].limit
    private implicit def a: F[Unit] = q.take >> r.set(false)
    private def o =
      for
        b <- r.get
        s <- q.size
        _ <- if !b || s == 0 then q.offer(()) >> r.set(true) else Temporal[F].unit
      yield
        ()
    private def s = Stream.resource(t.subscribeAwaitUnbounded <* Resource.eval(o)).flatten.evalFilter(_._2.complete(())).map(_._1)

    def ====(that: `()`[F]) =
      try
        this.t eq that.t
      catch
        case _ =>
          this.name == that.name

    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()`[F] = this

    object ! :

      object ν:

        /**
          * replication bound output guard
          */
        def apply(arity: Int): Stream[F, Seq[`()`[F]]] =
          (Stream.unit.repeat >> self.ν(arity)).interruptWhen(d)

        /**
          * replication bound output guard w/ code
          */
        def apply[T](arity: Int)(code: => F[T]): Stream[F, Seq[`()`[F]]] =
          (Stream.unit.repeat >> self.ν(arity)(code)).interruptWhen(d)

        /**
          * replication bound output guard w/ pace
          */
        def apply(arity: Int, pace: FiniteDuration): Stream[F, Seq[`()`[F]]] =
          (Stream.awakeEvery(pace) >> self.ν(arity)).interruptWhen(d)

        /**
          * replication bound output guard w/ pace w/ code
          */
        def apply[T](arity: Int, pace: FiniteDuration)(code: => F[T]): Stream[F, Seq[`()`[F]]] =
          (Stream.awakeEvery(pace) >> self.ν(arity)(code)).interruptWhen(d)

      /**
        * constant replication output guard
        */
      def apply(value: `()`[F]*): Stream[F, Unit] =
        Stream.repeatEval(Deferred[F, Unit].map(value -> _)).through1(t).interruptWhen(d)

      /**
        * constant replication output guard w/ pace
        */
      def apply(pace: FiniteDuration, value: `()`[F]*): Stream[F, Unit] =
        Stream.awakeEvery(pace).evalMap(_ => Deferred[F, Unit].map(value -> _)).through1(t).interruptWhen(d)

      /**
        * constant replication output guard w/ code
        */
      def apply[T](value: `()`[F]*)(code: => F[T]): Stream[F, Unit] =
        Stream.repeatEval(Deferred[F, Unit].map(value -> _)).through1(t).evalTap(_ => code).interruptWhen(d)

      /**
        * constant replication output guard w/ pace w/ code
        */
      def apply[T](pace: FiniteDuration, value: `()`[F]*)(code: => F[T]): Stream[F, Unit] =
        Stream.awakeEvery(pace).evalMap(_ => Deferred[F, Unit].map(value -> _)).through1(t).evalTap(_ => code).interruptWhen(d)

      object `null`:

        /**
          * `null` replication output guard
          */
        inline def apply(_arity: Int): Stream[F, Unit] =
          self.`null`(_arity)

        /**
          * `null` replication output guard w/ pace
          */
        inline def apply(_pace: FiniteDuration, _arity: Int): Stream[F, Unit] =
          self.`null`(_arity)

        /**
          * `null` replication output guard w/ code
          */
        inline def apply[T](_arity: Int)(code: => F[T]): Stream[F, Unit] =
          self.`null`(_arity)(code)

        /**
          * `null` replication output guard w/ pace w/ code
          */
        inline def apply[T](_pace: FiniteDuration, _arity: Int)(code: => F[T]): Stream[F, Unit] =
          self.`null`(_arity)(code)

      object * :

        /**
          * variable replication output guard
          */
        def apply[S](value: => F[S]*): Stream[F, Unit] =
          value.traverse(Stream.eval).repeat.evalMap { it => Deferred[F, Unit].map(it.map(new `()`[F](_)) -> _) }.through1(t).interruptWhen(d)

        /**
          * variable replication output guard w/ pace
          */
        def apply[S](pace: FiniteDuration, value: => F[S]*): Stream[F, Unit] =
          value.traverse(Stream.eval).repeat.spaced(pace).evalMap { it => Deferred[F, Unit].map(it.map(new `()`[F](_)) -> _) }.through1(t).interruptWhen(d)

        /**
          * variable replication output guard w/ code
          */
        def apply[S, T](value: => F[S]*)(code: => F[T]): Stream[F, Unit] =
          value.traverse(Stream.eval).repeat.evalMap { it => Deferred[F, Unit].map(it.map(new `()`[F](_)) -> _) }.through1(t).evalTap(_ => code).interruptWhen(d)

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S, T](pace: FiniteDuration, value: => F[S]*)(code: => F[T]): Stream[F, Unit] =
          value.traverse(Stream.eval).repeat.spaced(pace).evalMap { it => Deferred[F, Unit].map(it.map(new `()`[F](_)) -> _) }.through1(t).evalTap(_ => code).interruptWhen(d)

      /**
        * replication input guard
        */
      def apply(): Stream[F, Seq[`()`[F]]] =
        s.evalTap { case Seq(it, _*) if it.name == null => d.complete(Right(())).void case _ => o }.interruptWhen(d)

      /**
        * replication input guard w/ pace
        */
      def apply(pace: FiniteDuration): Stream[F, Seq[`()`[F]]] =
        s.spaced(pace).evalTap { case Seq(it, _*) if it.name == null => d.complete(Right(())).void case _ => o }.interruptWhen(d)

      /**
        * replication input guard w/ code
        */
      def apply[T]()(code: Seq[T] => F[Seq[T]]): Stream[F, Seq[`()`[F]]] =
        s.evalMap { it => code(it.`()`[Seq[T]]).map(_.map(new `()`[F](_))) }.evalTap { case Seq(it, _*) if it.name == null => d.complete(Right(())).void case _ => o }.interruptWhen(d)

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](pace: FiniteDuration)(code: Seq[T] => F[Seq[T]]): Stream[F, Seq[`()`[F]]] =
        s.spaced(pace).evalMap { it => code(it.`()`[Seq[T]]).map(_.map(new `()`[F](_))) }.evalTap { case Seq(it, _*) if it.name == null => d.complete(Right(())).void case _ => o }.interruptWhen(d)

    object ν:

      /**
        * bound output prefix
        */
      def apply(arity: Int): Stream[F, Seq[`()`[F]]] =
        for
          names <- Seq.fill(arity)(Π.ν[F].map(identity)).sequence
          _     <- Stream.eval(Deferred[F, Unit].map(names -> _)).through1(t)
        yield
          names

      /**
        * bound output prefix w/ code
        */
      def apply[T](arity: Int)(code: => F[T]): Stream[F, Seq[`()`[F]]] =
        for
          names <- Seq.fill(arity)(Π.ν[F].map(identity)).sequence
          _     <- Stream.eval(Deferred[F, Unit].map(names -> _)).through1(t).evalTap(_ => code)
        yield
          names

    /**
      * constant output prefix
      */
    def apply(value: `()`[F]*): Stream[F, Unit] =
      Stream.eval(Deferred[F, Unit].map(value -> _)).through1(t)

    /**
      * constant output prefix w/ code
      */
    def apply[T](value: `()`[F]*)(code: => F[T]): Stream[F, Unit] =
      Stream.eval(Deferred[F, Unit].map(value -> _)).through1(t).evalTap(_ => code)

    object `null`:

      /**
        * `null` output prefix
        */
      def apply(_arity: Int): Stream[F, Unit] =
        Stream.eval(d.complete(Right(())).void)

      /**
        * `null` output prefix w/ code
        */
      def apply[T](_arity: Int)(code: => F[T]): Stream[F, Unit] =
        Stream.eval(d.complete(Right(())).void).evalTap(_ => code)

    object * :

      /**
        * variable output prefix
        */
      def apply[S](value: => F[S]*): Stream[F, Unit] =
        value.traverse(Stream.eval).evalMap { it => Deferred[F, Unit].map(it.map(new `()`[F](_)) -> _) }.through1(t)

      /**
        * variable output prefix w/ code
        */
      def apply[S, T](value: => F[S]*)(code: => F[T]): Stream[F, Unit] =
        value.traverse(Stream.eval).evalMap { it => Deferred[F, Unit].map(it.map(new `()`[F](_)) -> _) }.through1(t).evalTap(_ => code)

    /**
      * input prefix
      */
    def apply(): Stream[F, Seq[`()`[F]]] =
      s.head

    /**
      * input prefix w/ code
      */
    def apply[T]()(code: Seq[T] => F[Seq[T]]): Stream[F, Seq[`()`[F]]] =
      s.head.evalMap { it => code(it.`()`[Seq[T]]).map(_.map(new `()`[F](_))) }

    override def toString: String = if name == null then "null" else name.toString

  }


  private object `Π-magic`:

    case class ><[F[_]](topic: Topic[F, (Seq[`()`[F]], Deferred[F, Unit])],
                        stop: Deferred[F, Either[Throwable, Unit]],
                        queue: Queue[F, Unit],
                        limit: Ref[F, Boolean])

    extension [F[_]: Temporal, O](self: Stream[F, O])
      inline def through1(topic: Topic[F, O])
                         (using await: F[Unit]): Stream[F, Unit] =
        self.evalMap(await >> topic.publish1(_)).takeWhile(_.isRight).void
