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

  import _root_.cats.syntax.applicative.*
  import _root_.cats.syntax.apply.*
  import _root_.cats.syntax.functor.*
  import _root_.cats.syntax.flatMap.*

  import _root_.cats.effect.{ Async, Deferred, Resource }
  import _root_.cats.effect.std.{ CyclicBarrier, Semaphore }

  import _root_.fs2.{ Pull, Stream }
  import _root_.fs2.concurrent.Topic

  import `Π-magic`.*


  /**
    * restriction aka new name
    */
  final class ν[F[_]: Async]:

    def map[B](f: `()`[F] => B): Stream[F, B] = flatMap(f andThen Stream.emit[F, B])
    def flatMap[B](f: `()`[F] => Stream[F, B]): Stream[F, B] =
      ( for
          topic <- Stream.eval(Topic[F, (`()`[F], Deferred[F, Unit])])
          limit <- Stream.eval(Semaphore[F](0))
        yield
          f(><[F](topic, limit))
      ).flatten


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
          apply()(-, * ,+).spaced(pace)

        /**
          * linear replication guard w/ code
          */
        def apply[T]()(code: => F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
          apply()(-, * ,+).evalTap(_ => code)

        /**
          * linear replication guard w/ pace w/ code
          */
        def apply[T](pace: FiniteDuration)(code: => F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
          apply(pace)(-, * ,+).evalTap(_ => code)

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
      * prefix w/ pace
      */
    def apply(pace: FiniteDuration): Stream[F, Unit] =
      apply() <* Stream.sleep(pace)

    /**
      * prefix w/ code
      */
    def apply[T]()(code: => F[T]): Stream[F, Unit] =
      apply().evalTap(_ => code)

    /**
      * prefix w/ pace w/ code
      */
    def apply[T](pace: FiniteDuration)(code: => F[T]): Stream[F, Unit] =
      apply(pace).evalTap(_ => code)

  /**
    * events, i.e., names (topics) and values
    */
  implicit final class `()`[F[_]: Async](private val name: Any) { self =>

    private inline def t = `()`[><[F]].topic
    private inline def l = `()`[><[F]].limit
    private implicit def a: F[Unit] = l.acquire
    private def _s = Stream.resource(t.subscribeAwaitUnbounded <* Resource.eval(l.release)).flatten

    private def s: Stream[F, `()`[F]] = _s.filter(true).stream
    private def `s.head`: Stream[F, `()`[F]] = _s.filter(false).stream

    extension (self: Stream[F, (`()`[F], Deferred[F, Unit])])
      private def filter(r: Boolean): Pull[F, `()`[F], Unit] =
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
          def apply()(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, `()`[F]] =
            (Stream.repeatEval(-.await >> *.fold(Async[F].unit)(_.acquire)) >> self.`(ν)`()).evalTap(_ => +.release)

          /**
            * linear replication bound output guard w/ pace
            */
          def apply(pace: FiniteDuration)(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, `()`[F]] =
            apply()(-, * ,+).spaced(pace)

          /**
            * linear replication bound output guard w/ code
            */
          def apply[T]()(code: => F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, `()`[F]] =
            apply()(-, * ,+).evalTap(_ => code)

          /**
            * linear replication bound output guard w/ pace w/ code
            */
          def apply[T](pace: FiniteDuration)(code: => F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, `()`[F]] =
            apply(pace)(-, * ,+).evalTap(_ => code)

        /**
          * linear constant replication output guard
          */
        def apply(value: `()`[F])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
          Stream.repeatEval(-.await >> *.fold(Async[F].unit)(_.acquire) >> Deferred[F, Unit].map(value -> _)).through1(t).evalTap(_ => +.release)

        /**
          * linear constant replication output guard w/ pace
          */
        def apply(pace: FiniteDuration, value: `()`[F])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
          apply(value)(-, * ,+).spaced(pace)

        /**
          * linear constant replication output guard w/ code
          */
        def apply[T](value: `()`[F])(code: => F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
          apply(value)(-, * ,+).evalTap(_ => code)

        /**
          * linear constant replication output guard w/ pace w/ code
          */
        def apply[T](pace: FiniteDuration, value: `()`[F])(code: => F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
          apply(pace, value)(-, * ,+).evalTap(_ => code)

        object `(null)`:

          /**
            * linear `null` replication output guard
            */
          def apply()(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
            self.`(!)`.`(+)`.apply(new `()`[F](null))(-, * ,+)

          /**
            * linear `null` replication output guard w/ pace
            */
          def apply(pace: FiniteDuration)(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
            self.`(!)`.`(+)`.apply(pace, new `()`[F](null))(-, * ,+)

          /**
            * linear `null` replication output guard w/ code
            */
          def apply[T]()(code: => F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
            self.`(!)`.`(+)`.apply(new `()`[F](null))(code)(-, * ,+)

          /**
            * linear `null` replication output guard w/ pace w/ code
            */
          def apply[T](pace: FiniteDuration)(code: => F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
            self.`(!)`.`(+)`.apply(pace, new `()`[F](null))(code)(-, * ,+)

        object `(*)`:

          /**
            * linear variable replication output guard
            */
          def apply[S](_1: 1)(value: => S)(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])(using DummyImplicit): Stream[F, Unit] =
            apply[S](1)(Async[F].delay(value))(-, * ,+)

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S](_2: 2)(pace: FiniteDuration, value: => S)(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])(using DummyImplicit): Stream[F, Unit] =
            apply[S](2)(pace, Async[F].delay(value))(-, * ,+)

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S, T](_3: 3)(value: => S)(code: F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])(using DummyImplicit): Stream[F, Unit] =
            apply[S, T](3)(Async[F].delay(value))(code)(-, * ,+)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S, T](_4: 4)(pace: FiniteDuration, value: => S)(code: F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])(using DummyImplicit): Stream[F, Unit] =
            apply[S, T](4)(pace, Async[F].delay(value))(code)(-, * ,+)

          /**
            * linear variable replication output guard
            */
          def apply[S](_1: 1)(value: => F[S])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
            Stream.repeatEval {
              for
                _  <- -.await
                _  <- *.fold(Async[F].unit)(_.acquire)
                it <- value
                d  <- Deferred[F, Unit]
              yield
                new `()`[F](it) -> d
            }.through1(t).evalTap(_ => +.release)

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S](_2: 2)(pace: FiniteDuration, value: => F[S])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
            apply[S](1)(value)(-, * ,+).spaced(pace)

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S, T](_3: 3)(value: => F[S])(code: F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
            apply[S](1)(value)(-, * ,+).evalTap(_ => code)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S, T](_4: 4)(pace: FiniteDuration, value: => F[S])(code: F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, Unit] =
            apply[S](2)(pace, value)(-, * ,+).evalTap(_ => code)

        /**
          * linear replication input guard
          */
        def apply()(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, `()`[F]] =
          (Stream.repeatEval(-.await >> *.fold(Async[F].unit)(_.acquire)) zipRight s).evalTap(_ => +.release)

        /**
          * linear replication input guard w/ pace
          */
        def apply(pace: FiniteDuration)(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, `()`[F]] =
          apply()(-, * ,+).spaced(pace)

        /**
          * linear replication input guard w/ code
          */
        def apply[T]()(code: T => F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, `()`[F]] =
          apply()(-, * ,+).map(_.`()`[T]).evalMap(code(_).map(new `()`[F](_)))

        /**
          * linear replication input guard w/ pace w/ code
          */
        def apply[T](pace: FiniteDuration)(code: T => F[T])(- : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F]): Stream[F, `()`[F]] =
          apply(pace)(-, * ,+).map(_.`()`[T]).evalMap(code(_).map(new `()`[F](_)))

      object `(ν)`:

        /**
          * replication bound output guard
          */
        def apply(): Stream[F, `()`[F]] =
          Stream.unit.repeat >> self.`(ν)`()

        /**
          * replication bound output guard w/ pace
          */
        def apply(pace: FiniteDuration): Stream[F, `()`[F]] =
          Stream.awakeEvery(pace) >> self.`(ν)`()

        /**
          * replication bound output guard w/ code
          */
        def apply[T]()(code: => F[T]): Stream[F, `()`[F]] =
          Stream.unit.repeat >> self.`(ν)`[T]()(code)

        /**
          * replication bound output guard w/ pace w/ code
          */
        def apply[T](pace: FiniteDuration)(code: => F[T]): Stream[F, `()`[F]] =
          Stream.awakeEvery(pace) >> self.`(ν)`[T]()(code)

      /**
        * constant replication output guard
        */
      def apply(value: `()`[F]): Stream[F, Unit] =
        Stream.repeatEval(Deferred[F, Unit].map(value -> _)).through1(t)

      /**
        * constant replication output guard w/ pace
        */
      def apply(pace: FiniteDuration, value: `()`[F]): Stream[F, Unit] =
        apply(value).spaced(pace)

      /**
        * constant replication output guard w/ code
        */
      def apply[T](value: `()`[F])(code: => F[T]): Stream[F, Unit] =
        apply(value).evalTap(_ => code)

      /**
        * constant replication output guard w/ pace w/ code
        */
      def apply[T](pace: FiniteDuration, value: `()`[F])(code: => F[T]): Stream[F, Unit] =
        apply(pace, value).evalTap(_ => code)

      object `(null)`:

        /**
          * `null` replication output guard
          */
        def apply(): Stream[F, Unit] =
          self.`(!)`.apply(new `()`[F](null))

        /**
          * `null` replication output guard w/ pace
          */
        def apply(pace: FiniteDuration): Stream[F, Unit] =
          self.`(!)`.apply(pace, new `()`[F](null))

        /**
          * `null` replication output guard w/ code
          */
        def apply[T]()(code: => F[T]): Stream[F, Unit] =
          self.`(!)`.apply[T](new `()`[F](null))(code)

        /**
          * `null` replication output guard w/ pace w/ code
          */
        def apply[T](pace: FiniteDuration)(code: => F[T]): Stream[F, Unit] =
          self.`(!)`.apply[T](pace, new `()`[F](null))(code)

      object `(*)`:

        /**
          * variable replication output guard
          */
        def apply[S](_1: 1)(value: => S)(using DummyImplicit): Stream[F, Unit] =
          apply[S](1)(Async[F].delay(value))

        /**
          * variable replication output guard w/ pace
          */
        def apply[S](_2: 2)(pace: FiniteDuration, value: => S)(using DummyImplicit): Stream[F, Unit] =
          apply[S](2)(pace, Async[F].delay(value))

        /**
          * variable replication output guard w/ code
          */
        def apply[S, T](_3: 3)(value: => S)(code: => F[T])(using DummyImplicit): Stream[F, Unit] =
          apply[S](1)(value).evalTap(_ => code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S, T](_4: 4)(pace: FiniteDuration, value: => S)(code: => F[T])(using DummyImplicit): Stream[F, Unit] =
          apply[S](2)(pace, value).evalTap(_ => code)

        /**
          * variable replication output guard
          */
        def apply[S](_1: 1)(value: => F[S]): Stream[F, Unit] =
          Stream.repeatEval(value >>= { it => Deferred[F, Unit].map(new `()`[F](it) -> _) }).through1(t)

        /**
          * variable replication output guard w/ pace
          */
        def apply[S](_2: 2)(pace: FiniteDuration, value: => F[S]): Stream[F, Unit] =
          apply[S](1)(value).spaced(pace)

        /**
          * variable replication output guard w/ code
          */
        def apply[S, T](_3: 3)(value: => F[S])(code: => F[T]): Stream[F, Unit] =
          apply[S](1)(value).evalTap(_ => code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S, T](_4: 4)(pace: FiniteDuration, value: => F[S])(code: => F[T]): Stream[F, Unit] =
          apply[S](2)(pace, value).evalTap(_ => code)

      /**
        * replication input guard
        */
      def apply(): Stream[F, `()`[F]] =
        s

      /**
        * replication input guard w/ pace
        */
      def apply(pace: FiniteDuration): Stream[F, `()`[F]] =
        s.spaced(pace)

      /**
        * replication input guard w/ code
        */
      def apply[T]()(code: T => F[T]): Stream[F, `()`[F]] =
        s.map(_.`()`[T]).evalMap(code(_).map(new `()`[F](_)))

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](pace: FiniteDuration)(code: T => F[T]): Stream[F, `()`[F]] =
        s.spaced(pace).map(_.`()`[T]).evalMap(code(_).map(new `()`[F](_)))

    object `(ν)`:

      /**
        * bound output prefix
        */
      def apply(): Stream[F, `()`[F]] =
        for
          name <- Π.ν[F]
          _    <- Stream.eval(Deferred[F, Unit].map(name -> _)).through1(t)
        yield
          name

      /**
        * bound output prefix w/ pace
        */
      def apply(pace: FiniteDuration): Stream[F, `()`[F]] =
        apply() <* Stream.sleep(pace)

      /**
        * bound output prefix w/ code
        */
      def apply[T]()(code: => F[T]): Stream[F, `()`[F]] =
        apply().evalTap(_ => code)

      /**
        * bound output prefix w/ pace w/ code
        */
      def apply[T](pace: FiniteDuration)(code: => F[T]): Stream[F, `()`[F]] =
        apply(pace).evalTap(_ => code)

    /**
      * constant output prefix
      */
    def apply(value: `()`[F]): Stream[F, Unit] =
      Stream.eval(Deferred[F, Unit].map(value -> _)).through1(t)

    /**
      * constant output prefix w/ pace
      */
    def apply(pace: FiniteDuration, value: `()`[F]): Stream[F, Unit] =
      apply(value) <* Stream.sleep(pace)

    /**
      * constant output prefix w/ code
      */
    def apply[T](value: `()`[F])(code: => F[T]): Stream[F, Unit] =
      apply(value).evalTap(_ => code)

    /**
      * constant output prefix w/ pace w/ code
      */
    def apply[T](pace: FiniteDuration, value: `()`[F])(code: => F[T]): Stream[F, Unit] =
      apply(pace, value).evalTap(_ => code)

    object `(null)`:

      /**
        * `null` output prefix
        */
      def apply(): Stream[F, Unit] =
        self.apply(new `()`[F](null))

      /**
        * `null` output prefix w/ pace
        */
      def apply(pace: FiniteDuration): Stream[F, Unit] =
        self.apply(pace, new `()`[F](null))

      /**
        * `null` output prefix w/ code
        */
      def apply[T]()(code: => F[T]): Stream[F, Unit] =
        self.apply[T](new `()`[F](null))(code)

      /**
        * `null` output prefix w/ pace w/ code
        */
      def apply[T](pace: FiniteDuration)(code: => F[T]): Stream[F, Unit] =
        self.apply[T](pace, new `()`[F](null))(code)

    object `(*)`:

      /**
        * variable output prefix
        */
      def apply[S](_1: 1)(value: => S)(using DummyImplicit): Stream[F, Unit] =
        apply[S](1)(Async[F].delay(value))

      /**
        * variable output prefix w/ pace
        */
      def apply[S](_2: 2)(pace: FiniteDuration, value: => S)(using DummyImplicit): Stream[F, Unit] =
        apply[S](1)(value) <* Stream.sleep(pace)

      /**
        * variable output prefix w/ code
        */
      def apply[S, T](_3: 3)(value: => S)(code: => F[T])(using DummyImplicit): Stream[F, Unit] =
        apply[S](1)(value).evalTap(_ => code)

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S, T](_4: 4)(pace: FiniteDuration, value: => S)(code: => F[T])(using DummyImplicit): Stream[F, Unit] =
        apply[S](2)(pace, value).evalTap(_ => code)

      /**
        * variable output prefix
        */
      def apply[S](_1: 1)(value: => F[S]): Stream[F, Unit] =
        Stream.eval(value >>= { it => Deferred[F, Unit].map(new `()`[F](it) -> _) }).through1(t)

      /**
        * variable output prefix w/ pace
        */
      def apply[S](_2: 2)(pace: FiniteDuration, value: => F[S]): Stream[F, Unit] =
        apply[S](1)(value) <* Stream.sleep(pace)

      /**
        * variable output prefix w/ code
        */
      def apply[S, T](_3: 3)(value: => F[S])(code: => F[T]): Stream[F, Unit] =
        apply[S](1)(value).evalTap(_ => code)

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S, T](_4: 4)(pace: FiniteDuration, value: => F[S])(code: => F[T]): Stream[F, Unit] =
        apply[S](2)(pace, value).evalTap(_ => code)

    /**
      * input prefix
      */
    def apply(): Stream[F, `()`[F]] =
      `s.head`

    /**
      * input prefix w/ pace
      */
    def apply(pace: FiniteDuration): Stream[F, `()`[F]] =
      apply() <* Stream.sleep(pace)

    /**
      * input prefix w/ code
      */
    def apply[T]()(code: T => F[T]): Stream[F, `()`[F]] =
      apply().map(_.`()`[T]).evalMap(code(_).map(new `()`[F](_)))

    /**
      * input prefix w/ pace w/ code
      */
    def apply[T](pace: FiniteDuration)(code: T => F[T]): Stream[F, `()`[F]] =
      apply(pace).map(_.`()`[T]).evalMap(code(_).map(new `()`[F](_)))

    override def toString: String = if name == null then "null" else name.toString

  }


  private object `Π-magic`:

    case class ><[F[_]](topic: Topic[F, (`()`[F], Deferred[F, Unit])],
                        limit: Semaphore[F])

    extension [F[_]: Async, O](self: Stream[F, O])
      def through1(topic: Topic[F, O])
                  (using await: F[Unit]): Stream[F, Unit] =
        self.evalMap(await >> topic.publish1(_)).takeWhile(_.isRight).void
