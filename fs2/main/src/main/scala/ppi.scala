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

  import _root_.scala.collection.immutable.Seq
  import _root_.scala.concurrent.duration.FiniteDuration

  import _root_.scala.reflect.{ ClassTag, classTag }

  import _root_.cats.instances.seq.*
  import _root_.cats.syntax.applicative.*
  import _root_.cats.syntax.apply.*
  import _root_.cats.syntax.functor.*
  import _root_.cats.syntax.flatMap.*
  import _root_.cats.syntax.traverse.*

  import _root_.cats.effect.{ Async, Deferred, Resource }
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.{ CyclicBarrier, Semaphore, Supervisor }

  import _root_.fs2.{ Pull, Stream }
  import _root_.fs2.concurrent.Topic

  import `Π-magic`.*


  /**
    * Supervised [[code]].
    * @param code
    */
  private def exec[F[_]: Async, T](code: => F[T]): F[T] =
    Supervisor[F](await = true)
      .use(_.supervise(code))
      .flatMap(_.join)
      .flatMap {
        case Succeeded(it) => it
        case _             => Async[F].pure(null.asInstanceOf[T])
      }


  /**
    * restriction aka new name
    */
  final class ν[F[_]: Async]:

    private[Π] def apply(): F[`()`[F]] =
      for
        topic <- Topic[F, (Seq[`()`[F]], Deferred[F, Unit])]
        limit <- Semaphore[F](0)
      yield
        ><[F](topic, limit)

    def map[B](f: `()`[F] => B): Stream[F, B] = flatMap(f andThen Stream.emit[F, B])
    def flatMap[B](f: `()`[F] => Stream[F, B]): Stream[F, B] =
      Stream.eval(apply()).flatMap(f)


  /**
    * silent transition
    */
  final class τ[F[_]: Async]:

    object `(!)`:

      object `(+)`:

        /**
          * linear replication guard
          */
        def apply()(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
          Stream.repeatEval(-.await >> *.fold(Async[F].unit)(_.acquire) >> +.release)

        /**
          * linear replication guard w/ pace
          */
        def apply(pace: FiniteDuration)(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
          apply()(-, *, +).spaced(pace)

        /**
          * linear replication guard w/ code
          */
        def apply[T]()(code: => F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
          apply()(-, *, +).evalTap(_ => exec(code))

        /**
          * linear replication guard w/ pace w/ code
          */
        def apply[T](pace: FiniteDuration)(code: => F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
          apply(pace)(-, *, +).evalTap(_ => exec(code))

      /**
        * replication guard
        */
      def apply(): Stream[F, Unit] =
        Stream.unit.repeat

      /**
        * replication guard w/ pace
        */
      def apply(pace: FiniteDuration): Stream[F, Unit] =
        Stream.awakeEvery(pace).void

      /**
        * replication guard w/ code
        */
      def apply[T]()(code: => F[T]): Stream[F, Unit] =
        apply().evalTap(_ => exec(code))

      /**
        * replication guard w/ pace w/ code
        */
      def apply[T](pace: FiniteDuration)(code: => F[T]): Stream[F, Unit] =
        apply(pace).evalTap(_ => exec(code))

    /**
      * prefix
      */
    def apply(): Stream[F, Unit] =
      Stream.unit

    /**
      * prefix w/ pace
      */
    def apply(pace: FiniteDuration): Stream[F, Unit] =
      apply() <* Stream.sleep(pace)

    /**
      * prefix w/ code
      */
    def apply[T]()(code: => F[T]): Stream[F, Unit] =
      apply().evalTap(_ => exec(code))

    /**
      * prefix w/ pace w/ code
      */
    def apply[T](pace: FiniteDuration)(code: => F[T]): Stream[F, Unit] =
      apply(pace).evalTap(_ => exec(code))

  /**
    * events, i.e., names (topics) and values
    */
  implicit final class `()`[F[_]: Async](private val name: Any) { self =>

    private inline def t = `()`[><[F]].topic
    private inline def l = `()`[><[F]].limit
    private implicit def a: F[Unit] = l.acquire
    private def _s = Stream.resource(t.subscribeAwaitUnbounded <* Resource.eval(l.release)).flatten

    private def s: Stream[F, Seq[`()`[F]]] = _s.filter(true).stream
    private def `s.head`: Stream[F, Seq[`()`[F]]] = _s.filter(false).stream

    extension (self: Stream[F, (Seq[`()`[F]], Deferred[F, Unit])])
      private def filter(r: Boolean): Pull[F, Seq[`()`[F]], Unit] =
        self.pull.uncons1.flatMap {
          case Some((it, d), its) =>
            Pull.eval(d.complete(())).flatMap {
              if _ then Pull.output1(it) >> (Stream.eval(l.release) >> its).filter(r).whenA(r)
              else its.filter(r)
            }
          case _ =>
            Pull.done
        }

    def ====(that: `()`[F]) =
      try
        this.t eq that.t
      catch _ =>
        this.name == that.name

    inline def unary_! : Boolean = name == null
    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()`[F] = this

    object `(!)`:

      object `(+)`:

        object `(ν)`:

          /**
            * linear replication bound output guard
            */
          def apply(arity: Int)(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Seq[`()`[F]]] =
            (Stream.repeatEval(-.await >> *.fold(Async[F].unit)(_.acquire)) >> self.`(ν)`(arity)).evalTap(_ => +.release)

          /**
            * linear replication bound output guard w/ pace
            */
          def apply(arity: Int, pace: FiniteDuration)(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Seq[`()`[F]]] =
            apply(arity)(-, *, +).spaced(pace)

          /**
            * linear replication bound output guard w/ code
            */
          def apply[T](arity: Int)(code: => F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Seq[`()`[F]]] =
            apply(arity)(-, *, +).evalTap(_ => exec(code))

          /**
            * linear replication bound output guard w/ pace w/ code
            */
          def apply[T](arity: Int, pace: FiniteDuration)(code: => F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Seq[`()`[F]]] =
            apply(arity, pace)(-, *, +).evalTap(_ => exec(code))

        /**
          * linear constant replication output guard
          */
        def apply(_1: 1)(value: `()`[F]*)(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
          Stream.repeatEval(-.await >> *.fold(Async[F].unit)(_.acquire) >> Deferred[F, Unit].map(value -> _)).through1(t).evalTap(_ => +.release)

        /**
          * linear constant replication output guard w/ pace
          */
        def apply(_2: 2)(pace: FiniteDuration, value: `()`[F]*)(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
          apply(1)(value*)(-, *, +).spaced(pace)

        /**
          * linear constant replication output guard w/ code
          */
        def apply[T](_3: 3)(value: `()`[F]*)(code: => F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
          apply(1)(value*)(-, *, +).evalTap(_ => exec(code))

        /**
          * linear constant replication output guard w/ pace w/ code
          */
        def apply[T](_4: 4)(pace: FiniteDuration, value: `()`[F]*)(code: => F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
          apply(2)(pace, value*)(-, *, +).evalTap(_ => exec(code))

        object `(null)`:

          /**
            * linear `null` replication output guard
            */
          def apply(arity: Int)(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
            self.`(!)`.`(+)`.apply(1)(Seq.fill(arity)(new `()`[F](null))*)(-, *, +)

          /**
            * linear `null` replication output guard w/ pace
            */
          def apply(arity: Int, pace: FiniteDuration)(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
            self.`(!)`.`(+)`.apply(2)(pace, Seq.fill(arity)(new `()`[F](null))*)(-, *, +)

          /**
            * linear `null` replication output guard w/ code
            */
          def apply[T](arity: Int)(code: => F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
            self.`(!)`.`(+)`.apply(3)(Seq.fill(arity)(new `()`[F](null))*)(code)(-, *, +)

          /**
            * linear `null` replication output guard w/ pace w/ code
            */
          def apply[T](arity: Int, pace: FiniteDuration)(code: => F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
            self.`(!)`.`(+)`.apply(4)(pace, Seq.fill(arity)(new `()`[F](null))*)(code)(-, *, +)

        object `(*)`:

          /**
            * linear variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(value: => S*)(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])(using DummyImplicit): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(1)(value.map(_.asInstanceOf[`()`[F]]*))(-, *, +)
            else
              apply[S](1)(value.map(Async[F].delay)*)(-, *, +)

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(pace: FiniteDuration, value: => S*)(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])(using DummyImplicit): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(2)(pace, value.map(_.asInstanceOf[`()`[F]]*))(-, *, +)
            else
              apply[S](2)(pace, value.map(Async[F].delay)*)(-, *, +)

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(value: => S*)(code: F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])(using DummyImplicit): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(3)(value.map(_.asInstanceOf[`()`[F]]*))(code)(-, *, +)
            else
              apply[S, T](3)(value.map(Async[F].delay)*)(code)(-, *, +)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(pace: FiniteDuration, value: => S*)(code: F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])(using DummyImplicit): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(4)(pace, value.map(_.asInstanceOf[`()`[F]]*))(code)(-, *, +)
            else
              apply[S, T](4)(pace, value.map(Async[F].delay)*)(code)(-, *, +)

          /**
            * linear variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(value: => F[S]*)(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              Stream.eval(Async[F].defer(value.map(_.asInstanceOf[F[`()`[F]]]).sequence)).flatMap(self.`(!)`.`(+)`(1)(_*)(-, *, +))
            else
              Stream.repeatEval {
                for
                  _  <- -.await
                  _  <- *.fold(Async[F].unit)(_.acquire)
                  it <- value.sequence >>= { it => Deferred[F, Unit].map(it.map(new `()`[F](_)) -> _) }
                yield
                  it
              }.through1(t).evalTap(_ => +.release)

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(pace: FiniteDuration, value: => F[S]*)(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
            apply[S](1)(value*)(-, *, +).spaced(pace)

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(value: => F[S]*)(code: F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
            apply[S](1)(value*)(-, *, +).evalTap(_ => exec(code))

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(pace: FiniteDuration, value: => F[S]*)(code: F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
            apply[S](2)(pace, value*)(-, *, +).evalTap(_ => exec(code))

        /**
          * linear replication input guard
          */
        def apply(_1: 1)()(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Seq[`()`[F]]] =
          (Stream.repeatEval(-.await >> *.fold(Async[F].unit)(_.acquire)) zipRight s).evalTap(_ => +.release)

        /**
          * linear replication input guard w/ pace
          */
        def apply(_2: 2)(pace: FiniteDuration)(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Seq[`()`[F]]] =
          apply(1)()(-, *, +).spaced(pace)

        /**
          * linear replication input guard w/ code
          */
        def apply[T](_3: 3)()(code: Seq[T] => F[Seq[T]])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Seq[`()`[F]]] =
          apply(1)()(-, *, +).map(_.map(_.`()`[T])).evalMap((code andThen exec)(_).map(_.map(new `()`[F](_))))

        /**
          * linear replication input guard w/ pace w/ code
          */
        def apply[T](_4: 4)(pace: FiniteDuration)(code: Seq[T] => F[Seq[T]])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Seq[`()`[F]]] =
          apply(2)(pace)(-, *, +).map(_.map(_.`()`[T])).evalMap((code andThen exec)(_).map(_.map(new `()`[F](_))))

      object `(ν)`:

        /**
          * replication bound output guard
          */
        def apply(arity: Int): Stream[F, Seq[`()`[F]]] =
          Stream.unit.repeat >> self.`(ν)`(arity)

        /**
          * replication bound output guard w/ pace
          */
        def apply(arity: Int, pace: FiniteDuration): Stream[F, Seq[`()`[F]]] =
          Stream.awakeEvery(pace) >> self.`(ν)`(arity)

        /**
          * replication bound output guard w/ code
          */
        def apply[T](arity: Int)(code: => F[T]): Stream[F, Seq[`()`[F]]] =
          Stream.unit.repeat >> self.`(ν)`[T](arity)(code)

        /**
          * replication bound output guard w/ pace w/ code
          */
        def apply[T](arity: Int, pace: FiniteDuration)(code: => F[T]): Stream[F, Seq[`()`[F]]] =
          Stream.awakeEvery(pace) >> self.`(ν)`[T](arity)(code)

      /**
        * constant replication output guard
        */
      def apply(_1: 1)(value: `()`[F]*): Stream[F, Unit] =
        Stream.repeatEval(Deferred[F, Unit].map(value -> _)).through1(t)

      /**
        * constant replication output guard w/ pace
        */
      def apply(_2: 2)(pace: FiniteDuration, value: `()`[F]*): Stream[F, Unit] =
        apply(1)(value*).spaced(pace)

      /**
        * constant replication output guard w/ code
        */
      def apply[T](_3: 3)(value: `()`[F]*)(code: => F[T]): Stream[F, Unit] =
        apply(1)(value*).evalTap(_ => exec(code))

      /**
        * constant replication output guard w/ pace w/ code
        */
      def apply[T](_4: 4)(pace: FiniteDuration, value: `()`[F]*)(code: => F[T]): Stream[F, Unit] =
        apply(2)(pace, value*).evalTap(_ => exec(code))

      object `(null)`:

        /**
          * `null` replication output guard
          */
        def apply(arity: Int): Stream[F, Unit] =
          self.`(!)`.apply(1)(Seq.fill(arity)(new `()`[F](null))*)

        /**
          * `null` replication output guard w/ pace
          */
        def apply(arity: Int, pace: FiniteDuration): Stream[F, Unit] =
          self.`(!)`.apply(2)(pace, Seq.fill(arity)(new `()`[F](null))*)

        /**
          * `null` replication output guard w/ code
          */
        def apply[T](arity: Int)(code: => F[T]): Stream[F, Unit] =
          self.`(!)`.apply[T](3)(Seq.fill(arity)(new `()`[F](null))*)(code)

        /**
          * `null` replication output guard w/ pace w/ code
          */
        def apply[T](arity: Int, pace: FiniteDuration)(code: => F[T]): Stream[F, Unit] =
          self.`(!)`.apply[T](4)(pace, Seq.fill(arity)(new `()`[F](null))*)(code)

      object `(*)`:

        /**
          * variable replication output guard
          */
        def apply[S: ClassTag](_1: 1)(value: => S*)(using DummyImplicit): Stream[F, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(1)(value.map(_.asInstanceOf[`()`[F]]*))
          else
            apply[S](1)(value.map(Async[F].delay)*)

        /**
          * variable replication output guard w/ pace
          */
        def apply[S: ClassTag](_2: 2)(pace: FiniteDuration, value: => S*)(using DummyImplicit): Stream[F, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(2)(pace, value.map(_.asInstanceOf[`()`[F]]*))
          else
            apply[S](2)(pace, value.map(Async[F].delay)*)

        /**
          * variable replication output guard w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(value: => S*)(code: => F[T])(using DummyImplicit): Stream[F, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(3)(value.map(_.asInstanceOf[`()`[F]]*))(code)
          else
            apply[S](1)(value*).evalTap(_ => exec(code))

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(pace: FiniteDuration, value: => S*)(code: => F[T])(using DummyImplicit): Stream[F, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(4)(pace, value.map(_.asInstanceOf[`()`[F]]*))(code)
          else
            apply[S](2)(pace, value*).evalTap(_ => exec(code))

        /**
          * variable replication output guard
          */
        def apply[S: ClassTag](_1: 1)(value: => F[S]*): Stream[F, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            Stream.eval(Async[F].defer(value.map(_.asInstanceOf[F[`()`[F]]]).sequence)).flatMap(self.`(!)`(1)(_*))
          else
            Stream.repeatEval(Async[F].defer(value.sequence >>= { it => Deferred[F, Unit].map(it.map(new `()`[F](_)) -> _) })).through1(t)

        /**
          * variable replication output guard w/ pace
          */
        def apply[S: ClassTag](_2: 2)(pace: FiniteDuration, value: => F[S]*): Stream[F, Unit] =
          apply[S](1)(value*).spaced(pace)

        /**
          * variable replication output guard w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(value: => F[S]*)(code: => F[T]): Stream[F, Unit] =
          apply[S](1)(value*).evalTap(_ => exec(code))

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(pace: FiniteDuration, value: => F[S]*)(code: => F[T]): Stream[F, Unit] =
          apply[S](2)(pace, value*).evalTap(_ => exec(code))

      /**
        * replication input guard
        */
      def apply(_1: 1)(): Stream[F, Seq[`()`[F]]] =
        s

      /**
        * replication input guard w/ pace
        */
      def apply(_2: 2)(pace: FiniteDuration): Stream[F, Seq[`()`[F]]] =
        s.spaced(pace)

      /**
        * replication input guard w/ code
        */
      def apply[T](_3: 3)()(code: Seq[T] => F[Seq[T]]): Stream[F, Seq[`()`[F]]] =
        s.map(_.map(_.`()`[T])).evalMap((code andThen exec)(_).map(_.map(new `()`[F](_))))

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](_4: 4)(pace: FiniteDuration)(code: Seq[T] => F[Seq[T]]): Stream[F, Seq[`()`[F]]] =
        s.spaced(pace).map(_.map(_.`()`[T])).evalMap((code andThen exec)(_).map(_.map(new `()`[F](_))))

    object `(ν)`:

      /**
        * bound output prefix
        */
      def apply(arity: Int): Stream[F, Seq[`()`[F]]] =
        for
          names <- Stream.eval(Seq.fill(arity)(Π.ν[F]()()).sequence)
          _     <- Stream.eval(Deferred[F, Unit].map(names -> _)).through1(t)
        yield
          names

      /**
        * bound output prefix w/ pace
        */
      def apply(arity: Int, pace: FiniteDuration): Stream[F, Seq[`()`[F]]] =
        apply(arity) <* Stream.sleep(pace)

      /**
        * bound output prefix w/ code
        */
      def apply[T](arity: Int)(code: => F[T]): Stream[F, Seq[`()`[F]]] =
        apply(arity).evalTap(_ => exec(code))

      /**
        * bound output prefix w/ pace w/ code
        */
      def apply[T](arity: Int, pace: FiniteDuration)(code: => F[T]): Stream[F, Seq[`()`[F]]] =
        apply(arity, pace).evalTap(_ => exec(code))

    /**
      * constant output prefix
      */
    def apply(_1: 1)(value: `()`[F]*): Stream[F, Unit] =
      Stream.eval(Deferred[F, Unit].map(value -> _)).through1(t)

    /**
      * constant output prefix w/ pace
      */
    def apply(_2: 2)(pace: FiniteDuration, value: `()`[F]*): Stream[F, Unit] =
      apply(1)(value*) <* Stream.sleep(pace)

    /**
      * constant output prefix w/ code
      */
    def apply[T](_3: 3)(value: `()`[F]*)(code: => F[T]): Stream[F, Unit] =
      apply(1)(value*).evalTap(_ => exec(code))

    /**
      * constant output prefix w/ pace w/ code
      */
    def apply[T](_4: 4)(pace: FiniteDuration, value: `()`[F]*)(code: => F[T]): Stream[F, Unit] =
      apply(2)(pace, value*).evalTap(_ => exec(code))

    object `(null)`:

      /**
        * `null` output prefix
        */
      def apply(arity: Int): Stream[F, Unit] =
        self.apply(1)(Seq.fill(arity)(new `()`[F](null))*)

      /**
        * `null` output prefix w/ pace
        */
      def apply(arity: Int, pace: FiniteDuration): Stream[F, Unit] =
        self.apply(2)(pace, Seq.fill(arity)(new `()`[F](null))*)

      /**
        * `null` output prefix w/ code
        */
      def apply[T](arity: Int)(code: => F[T]): Stream[F, Unit] =
        self.apply[T](3)(Seq.fill(arity)(new `()`[F](null))*)(code)

      /**
        * `null` output prefix w/ pace w/ code
        */
      def apply[T](arity: Int, pace: FiniteDuration)(code: => F[T]): Stream[F, Unit] =
        self.apply[T](4)(pace, Seq.fill(arity)(new `()`[F](null))*)(code)

    object `(*)`:

      /**
        * variable output prefix
        */
      def apply[S: ClassTag](_1: 1)(value: => S*)(using DummyImplicit): Stream[F, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(1)(value.map(_.asInstanceOf[`()`[F]]*))
        else
          apply[S](1)(value.map(Async[F].delay)*)

      /**
        * variable output prefix w/ pace
        */
      def apply[S: ClassTag](_2: 2)(pace: FiniteDuration, value: => S*)(using DummyImplicit): Stream[F, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(2)(pace, value.map(_.asInstanceOf[`()`[F]]*))
        else
          apply[S](1)(value*) <* Stream.sleep(pace)

      /**
        * variable output prefix w/ code
        */
      def apply[S: ClassTag, T](_3: 3)(value: => S*)(code: => F[T])(using DummyImplicit): Stream[F, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(3)(value.map(_.asInstanceOf[`()`[F]]*))(code)
        else
          apply[S](1)(value*).evalTap(_ => exec(code))

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S: ClassTag, T](_4: 4)(pace: FiniteDuration, value: => S*)(code: => F[T])(using DummyImplicit): Stream[F, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(4)(pace, value.map(_.asInstanceOf[`()`[F]]*))(code)
        else
          apply[S](2)(pace, value*).evalTap(_ => exec(code))

      /**
        * variable output prefix
        */
      def apply[S: ClassTag](_1: 1)(value: => F[S]*): Stream[F, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          Stream.eval(Async[F].defer(value.map(_.asInstanceOf[F[`()`[F]]]).sequence)).flatMap(self(1)(_*))
        else
          Stream.eval(Async[F].defer(value.sequence >>= { it => Deferred[F, Unit].map(it.map(new `()`[F](_)) -> _) })).through1(t)

      /**
        * variable output prefix w/ pace
        */
      def apply[S: ClassTag](_2: 2)(pace: FiniteDuration, value: => F[S]*): Stream[F, Unit] =
        apply[S](1)(value*) <* Stream.sleep(pace)

      /**
        * variable output prefix w/ code
        */
      def apply[S: ClassTag, T](_3: 3)(value: => F[S]*)(code: => F[T]): Stream[F, Unit] =
        apply[S](1)(value*).evalTap(_ => exec(code))

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S: ClassTag, T](_4: 4)(pace: FiniteDuration, value: => F[S]*)(code: => F[T]): Stream[F, Unit] =
        apply[S](2)(pace, value*).evalTap(_ => exec(code))

    /**
      * input prefix
      */
    def apply(_1: 1)(): Stream[F, Seq[`()`[F]]] =
      `s.head`

    /**
      * input prefix w/ pace
      */
    def apply(_2: 2)(pace: FiniteDuration): Stream[F, Seq[`()`[F]]] =
      apply(1)() <* Stream.sleep(pace)

    /**
      * input prefix w/ code
      */
    def apply[T](_3: 3)()(code: Seq[T] => F[Seq[T]]): Stream[F, Seq[`()`[F]]] =
      apply(1)().map(_.map(_.`()`[T])).evalMap((code andThen exec)(_).map(_.map(new `()`[F](_))))

    /**
      * input prefix w/ pace w/ code
      */
    def apply[T](_4: 4)(pace: FiniteDuration)(code: Seq[T] => F[Seq[T]]): Stream[F, Seq[`()`[F]]] =
      apply(2)(pace).map(_.map(_.`()`[T])).evalMap((code andThen exec)(_).map(_.map(new `()`[F](_))))

    override def toString: String = if name == null then "null" else name.toString

  }


  private object `Π-magic`:

    case class ><[F[_]](topic: Topic[F, (Seq[`()`[F]], Deferred[F, Unit])],
                        limit: Semaphore[F])

    extension [F[_]: Async, O](self: Stream[F, O])
      def through1(topic: Topic[F, O])
                  (using await: F[Unit]): Stream[F, Unit] =
        self.evalMap(await >> topic.publish1(_)).takeWhile(_.isRight).void
