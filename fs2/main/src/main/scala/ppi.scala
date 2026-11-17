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

  import _root_.cats.Defer
  import _root_.cats.instances.seq.*
  import _root_.cats.syntax.applicative.*
  import _root_.cats.syntax.apply.*
  import _root_.cats.syntax.functor.*
  import _root_.cats.syntax.flatMap.*
  import _root_.cats.syntax.traverse.*

  import _root_.cats.effect.{ Async, Deferred, Resource }
  import _root_.cats.effect.kernel.Outcome
  import _root_.cats.effect.std.{ Semaphore, Supervisor }

  import _root_.fs2.{ Pull, Stream }
  import _root_.fs2.concurrent.Topic

  import `Π-magic`.*


  /**
    * Supervised [[code]].
    * @param code
    */
  private def exec[F[_]: Async, T](code: => F[T]): F[T] =
    Supervisor[F](await = true)
      .use(_.supervise(code).flatMap(_.join))
      .flatMap {
        case Outcome.Succeeded(it) => it
        case _                     => Async[F].pure(null.asInstanceOf[T])
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
          * modulo replication guard
          */
        def apply()(- : Semaphore[F]): Stream[F, Unit] =
          Stream.repeatEval(-.acquire >> -.release)

        /**
          * modulo replication guard w/ pace
          */
        def apply(pace: FiniteDuration)(- : Semaphore[F]): Stream[F, Unit] =
          Stream.repeatEval(-.acquire >> Async[F].sleep(pace) >> -.release)

        /**
          * modulo replication guard w/ code
          */
        def apply[T]()(code: => F[T])(- : Semaphore[F]): Stream[F, Unit] =
          Stream.repeatEval(-.acquire >> exec(code) >> -.release)

        /**
          * modulo replication guard w/ pace w/ code
          */
        def apply[T](pace: FiniteDuration)(code: => F[T])(- : Semaphore[F]): Stream[F, Unit] =
          Stream.repeatEval(-.acquire >> exec(code) >> Async[F].sleep(pace) >> -.release)

      /**
        * replication guard
        */
      def apply(): Stream[F, Unit] =
        Stream.unit.repeat

      /**
        * replication guard w/ pace
        */
      def apply(pace: FiniteDuration): Stream[F, Unit] =
        apply() <* Stream.sleep(pace)

      /**
        * replication guard w/ code
        */
      def apply[T]()(code: => F[T]): Stream[F, Unit] =
        apply().evalTap(_ => exec(code))

      /**
        * replication guard w/ pace w/ code
        */
      def apply[T](pace: FiniteDuration)(code: => F[T]): Stream[F, Unit] =
        apply[T]()(code) <* Stream.sleep(pace)

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
      apply[T]()(code) <* Stream.sleep(pace)


  /**
    * names (topics) and values
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
            * modulo replication bound output guard
            */
          def apply(arity: Int)(- : Semaphore[F]): Stream[F, Seq[`()`[F]]] =
            (Stream.repeatEval(-.acquire) >> self.`(ν)`(arity)).evalTap(_ => -.release)

          /**
            * modulo replication bound output guard w/ pace
            */
          def apply(arity: Int, pace: FiniteDuration)(- : Semaphore[F]): Stream[F, Seq[`()`[F]]] =
            (Stream.repeatEval(-.acquire) >> self.`(ν)`(arity, pace)).evalTap(_ => -.release)

          /**
            * modulo replication bound output guard w/ code
            */
          def apply[T](arity: Int)(code: => F[T])(- : Semaphore[F]): Stream[F, Seq[`()`[F]]] =
            (Stream.repeatEval(-.acquire) >> self.`(ν)`[T](arity)(code)).evalTap(_ => -.release)

          /**
            * modulo replication bound output guard w/ pace w/ code
            */
          def apply[T](arity: Int, pace: FiniteDuration)(code: => F[T])(- : Semaphore[F]): Stream[F, Seq[`()`[F]]] =
            (Stream.repeatEval(-.acquire) >> self.`(ν)`[T](arity, pace)(code)).evalTap(_ => -.release)

        /**
          * modulo constant replication output guard
          */
        def apply(_1: 1)(value: `()`[F]*)(- : Semaphore[F]): Stream[F, Unit] =
          Stream.repeatEval(-.acquire >> Deferred[F, Unit].map(value -> _)).through1(t).evalTap(_ => -.release)

        /**
          * modulo constant replication output guard w/ pace
          */
        def apply(_2: 2)(pace: FiniteDuration, value: `()`[F]*)(- : Semaphore[F]): Stream[F, Unit] =
          Stream.repeatEval(-.acquire >> Deferred[F, Unit].map(value -> _)).through1(t).evalTap(_ => Async[F].sleep(pace) >> -.release)

        /**
          * modulo constant replication output guard w/ code
          */
        def apply[T](_3: 3)(value: `()`[F]*)(code: => F[T])(- : Semaphore[F]): Stream[F, Unit] =
          Stream.repeatEval(-.acquire >> Deferred[F, Unit].map(value -> _)).through1(t).evalTap(_ => exec(code) >> -.release)

        /**
          * modulo constant replication output guard w/ pace w/ code
          */
        def apply[T](_4: 4)(pace: FiniteDuration, value: `()`[F]*)(code: => F[T])(- : Semaphore[F]): Stream[F, Unit] =
          Stream.repeatEval(-.acquire >> Deferred[F, Unit].map(value -> _)).through1(t).evalTap(_ => exec(code) >> Async[F].sleep(pace) >> -.release)

        object `(null)`:

          /**
            * modulo `null` replication output guard
            */
          def apply(arity: Int)(- : Semaphore[F]): Stream[F, Unit] =
            self.`(!)`.`(+)`.apply(1)(Seq.fill(arity)(new `()`[F](null))*)(-)

          /**
            * modulo `null` replication output guard w/ pace
            */
          def apply(arity: Int, pace: FiniteDuration)(- : Semaphore[F]): Stream[F, Unit] =
            self.`(!)`.`(+)`.apply(2)(pace, Seq.fill(arity)(new `()`[F](null))*)(-)

          /**
            * modulo `null` replication output guard w/ code
            */
          def apply[T](arity: Int)(code: => F[T])(- : Semaphore[F]): Stream[F, Unit] =
            self.`(!)`.`(+)`.apply(3)(Seq.fill(arity)(new `()`[F](null))*)(code)(-)

          /**
            * modulo `null` replication output guard w/ pace w/ code
            */
          def apply[T](arity: Int, pace: FiniteDuration)(code: => F[T])(- : Semaphore[F]): Stream[F, Unit] =
            self.`(!)`.`(+)`.apply(4)(pace, Seq.fill(arity)(new `()`[F](null))*)(code)(-)

        object `(*)`:

          /**
            * modulo variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(value: => S*)(- : Semaphore[F])(using DummyImplicit): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(1)(value.map(_.asInstanceOf[`()`[F]]*))(-)
            else
              Defer[[X] =>> Stream[F, X]].defer(apply[S](1)(value.map(Async[F].pure)*)(-))

          /**
            * modulo variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(pace: FiniteDuration, value: => S*)(- : Semaphore[F])(using DummyImplicit): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(2)(pace, value.map(_.asInstanceOf[`()`[F]]*))(-)
            else
              Defer[[X] =>> Stream[F, X]].defer(apply[S](2)(pace, value.map(Async[F].pure)*)(-))

          /**
            * modulo variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(value: => S*)(code: F[T])(- : Semaphore[F])(using DummyImplicit): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(3)(value.map(_.asInstanceOf[`()`[F]]*))(code)(-)
            else
              Defer[[X] =>> Stream[F, X]].defer(apply[S, T](3)(value.map(Async[F].pure)*)(code)(-))

          /**
            * modulo variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(pace: FiniteDuration, value: => S*)(code: F[T])(- : Semaphore[F])(using DummyImplicit): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(4)(pace, value.map(_.asInstanceOf[`()`[F]]*))(code)(-)
            else
              Defer[[X] =>> Stream[F, X]].defer(apply[S, T](4)(pace, value.map(Async[F].pure)*)(code)(-))

          /**
            * modulo variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(value: => F[S]*)(- : Semaphore[F]): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              Stream.eval(Async[F].defer(value.map(_.asInstanceOf[F[`()`[F]]]).sequence)).flatMap(self.`(!)`.`(+)`(1)(_*)(-))
            else
              Stream.repeatEval {
                for
                  _  <- -.acquire
                  it <- value.sequence >>= { it => Deferred[F, Unit].map(it.map(new `()`[F](_)) -> _) }
                yield
                  it
              }.through1(t).evalTap(_ => -.release)

          /**
            * modulo variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(pace: FiniteDuration, value: => F[S]*)(- : Semaphore[F]): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              Stream.eval(Async[F].defer(value.map(_.asInstanceOf[F[`()`[F]]]).sequence)).flatMap(self.`(!)`.`(+)`(2)(pace, _*)(-))
            else
              Stream.repeatEval {
                for
                  _  <- -.acquire
                  it <- value.sequence >>= { it => Deferred[F, Unit].map(it.map(new `()`[F](_)) -> _) }
                yield
                  it
              }.through1(t).evalTap(_ => Async[F].sleep(pace) >> -.release)

          /**
            * modulo variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(value: => F[S]*)(code: F[T])(- : Semaphore[F]): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              Stream.eval(Async[F].defer(value.map(_.asInstanceOf[F[`()`[F]]]).sequence)).flatMap(self.`(!)`.`(+)`(3)(_*)(code)(-))
            else
              Stream.repeatEval {
                for
                  _  <- -.acquire
                  it <- value.sequence >>= { it => Deferred[F, Unit].map(it.map(new `()`[F](_)) -> _) }
                yield
                  it
              }.through1(t).evalTap(_ => exec(code) >> -.release)

          /**
            * modulo variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(pace: FiniteDuration, value: => F[S]*)(code: F[T])(- : Semaphore[F]): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              Stream.eval(Async[F].defer(value.map(_.asInstanceOf[F[`()`[F]]]).sequence)).flatMap(self.`(!)`.`(+)`(4)(pace, _*)(code)(-))
            else
              Stream.repeatEval {
                for
                  _  <- -.acquire
                  it <- value.sequence >>= { it => Deferred[F, Unit].map(it.map(new `()`[F](_)) -> _) }
                yield
                  it
              }.through1(t).evalTap(_ => exec(code) >> Async[F].sleep(pace) >> -.release)

        /**
          * modulo replication input guard
          */
        def apply(_1: 1)()(- : Semaphore[F]): Stream[F, Seq[`()`[F]]] =
          (Stream.repeatEval(-.acquire) zipRight s).evalTap(_ => -.release)

        /**
          * modulo replication input guard w/ pace
          */
        def apply(_2: 2)(pace: FiniteDuration)(- : Semaphore[F]): Stream[F, Seq[`()`[F]]] =
          (Stream.repeatEval(-.acquire) zipRight s).evalTap(_ => Async[F].sleep(pace) >> -.release)

        /**
          * modulo replication input guard w/ code
          */
        def apply[T](_3: 3)()(code: Seq[T] => F[Seq[T]])(- : Semaphore[F]): Stream[F, Seq[`()`[F]]] =
          (Stream.repeatEval(-.acquire) zipRight s).map(_.map(_.`()`[T])).evalMap((code andThen exec)(_).map(_.map(new `()`[F](_)))).evalTap(_ => -.release)

        /**
          * modulo replication input guard w/ pace w/ code
          */
        def apply[T](_4: 4)(pace: FiniteDuration)(code: Seq[T] => F[Seq[T]])(- : Semaphore[F]): Stream[F, Seq[`()`[F]]] =
          (Stream.repeatEval(-.acquire) zipRight s).map(_.map(_.`()`[T])).evalMap((code andThen exec)(_).map(_.map(new `()`[F](_)))).evalTap(_ => Async[F].sleep(pace) >> -.release)

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
          Stream.unit.repeat >> self.`(ν)`(arity, pace)

        /**
          * replication bound output guard w/ code
          */
        def apply[T](arity: Int)(code: => F[T]): Stream[F, Seq[`()`[F]]] =
          Stream.unit.repeat >> self.`(ν)`[T](arity)(code)

        /**
          * replication bound output guard w/ pace w/ code
          */
        def apply[T](arity: Int, pace: FiniteDuration)(code: => F[T]): Stream[F, Seq[`()`[F]]] =
          Stream.unit.repeat >> self.`(ν)`[T](arity, pace)(code)

      /**
        * constant replication output guard
        */
      def apply(_1: 1)(value: `()`[F]*): Stream[F, Unit] =
        Stream.repeatEval(Deferred[F, Unit].map(value -> _)).through1(t)

      /**
        * constant replication output guard w/ pace
        */
      def apply(_2: 2)(pace: FiniteDuration, value: `()`[F]*): Stream[F, Unit] =
        apply(1)(value*) <* Stream.sleep(pace)

      /**
        * constant replication output guard w/ code
        */
      def apply[T](_3: 3)(value: `()`[F]*)(code: => F[T]): Stream[F, Unit] =
        apply(1)(value*).evalTap(_ => exec(code))

      /**
        * constant replication output guard w/ pace w/ code
        */
      def apply[T](_4: 4)(pace: FiniteDuration, value: `()`[F]*)(code: => F[T]): Stream[F, Unit] =
        apply[T](3)(value*)(code) <* Stream.sleep(pace)

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
            Defer[[X] =>> Stream[F, X]].defer(apply[S](1)(value.map(Async[F].pure)*))

        /**
          * variable replication output guard w/ pace
          */
        def apply[S: ClassTag](_2: 2)(pace: FiniteDuration, value: => S*)(using DummyImplicit): Stream[F, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(2)(pace, value.map(_.asInstanceOf[`()`[F]]*))
          else
            Defer[[X] =>> Stream[F, X]].defer(apply[S](2)(pace, value.map(Async[F].pure)*))

        /**
          * variable replication output guard w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(value: => S*)(code: => F[T])(using DummyImplicit): Stream[F, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(3)(value.map(_.asInstanceOf[`()`[F]]*))(code)
          else
            Defer[[X] =>> Stream[F, X]].defer(apply[S, T](3)(value.map(Async[F].pure)*)(code))

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(pace: FiniteDuration, value: => S*)(code: => F[T])(using DummyImplicit): Stream[F, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(4)(pace, value.map(_.asInstanceOf[`()`[F]]*))(code)
          else
            Defer[[X] =>> Stream[F, X]].defer(apply[S, T](4)(pace, value.map(Async[F].pure)*)(code))

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
          apply[S](1)(value*) <* Stream.sleep(pace)

        /**
          * variable replication output guard w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(value: => F[S]*)(code: => F[T]): Stream[F, Unit] =
          apply[S](1)(value*).evalTap(_ => exec(code))

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(pace: FiniteDuration, value: => F[S]*)(code: => F[T]): Stream[F, Unit] =
          apply[S, T](3)(value*)(code) <* Stream.sleep(pace)

      /**
        * replication input guard
        */
      def apply(_1: 1)(): Stream[F, Seq[`()`[F]]] =
        s

      /**
        * replication input guard w/ pace
        */
      def apply(_2: 2)(pace: FiniteDuration): Stream[F, Seq[`()`[F]]] =
        apply(1)() <* Stream.sleep(pace)

      /**
        * replication input guard w/ code
        */
      def apply[T](_3: 3)()(code: Seq[T] => F[Seq[T]]): Stream[F, Seq[`()`[F]]] =
        s.map(_.map(_.`()`[T])).evalMap((code andThen exec)(_).map(_.map(new `()`[F](_))))

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](_4: 4)(pace: FiniteDuration)(code: Seq[T] => F[Seq[T]]): Stream[F, Seq[`()`[F]]] =
        apply[T](3)()(code) <* Stream.sleep(pace)

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
        apply[T](arity)(code) <* Stream.sleep(pace)

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
      apply[T](3)(value*)(code) <* Stream.sleep(pace)

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
          Defer[[X] =>> Stream[F, X]].defer(apply[S](1)(value.map(Async[F].pure)*))

      /**
        * variable output prefix w/ pace
        */
      def apply[S: ClassTag](_2: 2)(pace: FiniteDuration, value: => S*)(using DummyImplicit): Stream[F, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(2)(pace, value.map(_.asInstanceOf[`()`[F]]*))
        else
          Defer[[X] =>> Stream[F, X]].defer(apply[S](2)(pace, value.map(Async[F].pure)*))

      /**
        * variable output prefix w/ code
        */
      def apply[S: ClassTag, T](_3: 3)(value: => S*)(code: => F[T])(using DummyImplicit): Stream[F, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(3)(value.map(_.asInstanceOf[`()`[F]]*))(code)
        else
          Defer[[X] =>> Stream[F, X]].defer(apply[S, T](3)(value.map(Async[F].pure)*)(code))

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S: ClassTag, T](_4: 4)(pace: FiniteDuration, value: => S*)(code: => F[T])(using DummyImplicit): Stream[F, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(4)(pace, value.map(_.asInstanceOf[`()`[F]]*))(code)
        else
          Defer[[X] =>> Stream[F, X]].defer(apply[S, T](4)(pace, value.map(Async[F].pure)*)(code))

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
        apply[S, T](3)(value*)(code) <* Stream.sleep(pace)

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
      `s.head`.map(_.map(_.`()`[T])).evalMap((code andThen exec)(_).map(_.map(new `()`[F](_))))

    /**
      * input prefix w/ pace w/ code
      */
    def apply[T](_4: 4)(pace: FiniteDuration)(code: Seq[T] => F[Seq[T]]): Stream[F, Seq[`()`[F]]] =
      apply[T](3)()(code) <* Stream.sleep(pace)

    override def toString: String = if name == null then "null" else name.toString

  }


  private object `Π-magic`:

    case class ><[F[_]](topic: Topic[F, (Seq[`()`[F]], Deferred[F, Unit])],
                        limit: Semaphore[F])

    extension [F[_]: Async, O](self: Stream[F, O])
      def through1(topic: Topic[F, O])
                  (using await: F[Unit]): Stream[F, Unit] =
        self.evalMap(await >> topic.publish1(_)).takeWhile(_.isRight).void
