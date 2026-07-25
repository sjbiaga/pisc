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
  import _root_.cats.syntax.applicativeError.*
  import _root_.cats.syntax.functor.*
  import _root_.cats.syntax.flatMap.*

  import _root_.cats.effect.{ Concurrent, ContextShift, Resource, Sync, Timer }
  import _root_.cats.effect.concurrent.{ Deferred, Semaphore }

  import _root_.monix.catnap.{ ConcurrentChannel, ConcurrentQueue }
  import _root_.monix.catnap.ConsumerF.Config
  import _root_.monix.execution.BufferCapacity.Unbounded
  import _root_.monix.execution.ChannelType.{ MultiConsumer, MultiProducer }
  import _root_.monix.tail.Iterant

  import `Π-magic`.*


  /**
    * restriction aka new name
    */
  final class ν[F[_]: Concurrent: ContextShift: Timer]:

    def map[B](f: `()`[F] => B): Iterant[F, B] = flatMap(f andThen Iterant.pure[F, B])
    def flatMap[B](f: `()`[F] => Iterant[F, B]): Iterant[F, B] =
      val unbounded = Config(capacity = Some(Unbounded()), consumerType = Some(MultiConsumer))
      ( for
          channel <- Iterant.liftF(ConcurrentChannel.withConfig[F, Option[Throwable], (`()`[F], Deferred[F, Unit])](unbounded, MultiProducer))
          limit   <- Iterant.liftF(Semaphore[F](0))
        yield
          f(><[F](channel, limit))
      ).flatten


  /**
    * silent transition
    */
  final class τ[F[_]: Concurrent: Timer]:

    import `()`.tapEval

    object `(!)`:

      /**
        * replication guard
        */
      def apply(): Iterant[F, Unit] =
        Iterant.repeatEval(())

      /**
        * replication guard w/ pace
        */
      def apply(pace: FiniteDuration): Iterant[F, Unit] =
        Iterant.intervalAtFixedRate(pace).void

      /**
        * replication guard w/ code
        */
      def apply[T]()(code: => F[T]): Iterant[F, Unit] =
        apply().tapEval(_ => code)

      /**
        * replication guard w/ pace w/ code
        */
      def apply[T](pace: FiniteDuration)(code: => F[T]): Iterant[F, Unit] =
        apply(pace).tapEval(_ => code)

    /**
      * prefix
      */
    def apply(): Iterant[F, Unit] =
      Iterant.eval(())

    /**
      * prefix w/ pace
      */
    def apply(pace: FiniteDuration): Iterant[F, Unit] =
      apply() <* Iterant.intervalAtFixedRate(pace, pace).take(1)

    /**
      * prefix w/ code
      */
    def apply[T]()(code: => F[T]): Iterant[F, Unit] =
      apply().tapEval(_ => code)

    /**
      * prefix w/ pace w/ code
      */
    def apply[T](pace: FiniteDuration)(code: => F[T]): Iterant[F, Unit] =
      apply(pace).tapEval(_ => code)

  /**
    * events, i.e., names (channels) and values
    */
  implicit final class `()`[F[_]: Concurrent: ContextShift: Timer](private val name: Any) { self =>

    import Π.`()`.tapEval

    private inline def ch = `()`[><[F]].channel
    private inline def l = `()`[><[F]].limit
    private implicit def a: F[Unit] = l.acquire
    private def o = l.release
    private def s = Iterant
      .liftF(ch.consume.flatTap(_ => Resource.eval(o)).use(_.pull.map(_.right.getOrElse(new `()`[F](null) -> null))))
      .repeat
      .mapEval {
        case (it, null) => Concurrent[F].pure(it -> false)
        case (it, d) => d.complete(()).attempt.map(it -> _.isRight)
      }
      .filter(_._2)
      .map(_._1)

    def ====(that: `()`[F]) =
      try
        this.ch eq that.ch
      catch _ =>
        this.name == that.name

    inline def unary_! : Boolean = name == null
    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()`[F] = this

    object `(!)`:

      object `(ν)`:

        /**
          * replication bound output guard
          */
        def apply(): Iterant[F, `()`[F]] =
          Iterant.repeatEval(()) >> self.`(ν)`()

        /**
          * replication bound output guard w/ pace
          */
        def apply(pace: FiniteDuration): Iterant[F, `()`[F]] =
          Iterant.intervalAtFixedRate(pace) >> self.`(ν)`()

        /**
          * replication bound output guard w/ code
          */
        def apply[T]()(code: => F[T]): Iterant[F, `()`[F]] =
          Iterant.repeatEval(()) >> self.`(ν)`[T]()(code)

        /**
          * replication bound output guard w/ pace w/ code
          */
        def apply[T](pace: FiniteDuration)(code: => F[T]): Iterant[F, `()`[F]] =
          Iterant.intervalAtFixedRate(pace) >> self.`(ν)`[T]()(code)

      /**
        * constant replication output guard
        */
      def apply(value: `()`[F]): Iterant[F, Unit] =
        Iterant.repeatEvalF(Deferred[F, Unit].map(value -> _)).through1(ch)

      /**
        * constant replication output guard w/ pace
        */
      def apply(pace: FiniteDuration, value: `()`[F]): Iterant[F, Unit] =
        Iterant.intervalAtFixedRate(pace).mapEval(_ => Deferred[F, Unit].map(value -> _)).through1(ch)

      /**
        * constant replication output guard w/ code
        */
      def apply[T](value: `()`[F])(code: => F[T]): Iterant[F, Unit] =
        apply(value).tapEval(_ => code)

      /**
        * constant replication output guard w/ pace w/ code
        */
      def apply[T](pace: FiniteDuration, value: `()`[F])(code: => F[T]): Iterant[F, Unit] =
        apply(pace, value).tapEval(_ => code)

      object `(null)`:

        /**
          * `null` replication output guard
          */
        def apply(): Iterant[F, Unit] =
          self.`(null)`()

        /**
          * `null` replication output guard w/ pace
          */
        def apply(_pace: FiniteDuration): Iterant[F, Unit] =
         apply()

        /**
          * `null` replication output guard w/ code
          */
        def apply[T]()(code: => F[T]): Iterant[F, Unit] =
          self.`(null)`[T]()(code)

        /**
          * `null` replication output guard w/ pace w/ code
          */
        def apply[T](_pace: FiniteDuration)(code: => F[T]): Iterant[F, Unit] =
          apply[T]()(code)

      object `(*)`:

        /**
          * variable replication output guard
          */
        def apply[S](_1: 1)(value: => S)(using DummyImplicit): Iterant[F, Unit] =
          apply[S](1)(Concurrent[F].delay(value))

        /**
          * variable replication output guard w/ pace
          */
        def apply[S](_2: 2)(pace: FiniteDuration, value: => S)(using DummyImplicit): Iterant[F, Unit] =
          apply[S](2)(pace, Concurrent[F].delay(value))

        /**
          * variable replication output guard w/ code
          */
        def apply[S, T](_3: 3)(value: => S)(code: => F[T])(using DummyImplicit): Iterant[F, Unit] =
          apply[S](1)(value).tapEval(_ => code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S, T](_4: 4)(pace: FiniteDuration, value: => S)(code: => F[T])(using DummyImplicit): Iterant[F, Unit] =
          apply[S](2)(pace, value).tapEval(_ => code)

        /**
          * variable replication output guard
          */
        def apply[S](_1: 1)(value: => F[S]): Iterant[F, Unit] =
          Iterant.repeatEvalF(value.flatMap { it => Deferred[F, Unit].map(new `()`[F](it) -> _) }).through1(ch)

        /**
          * variable replication output guard w/ pace
          */
        def apply[S](_2: 2)(pace: FiniteDuration, value: => F[S]): Iterant[F, Unit] =
          (apply[S](1)(value) zip Iterant.intervalAtFixedRate(pace)).map(_._1)

        /**
          * variable replication output guard w/ code
          */
        def apply[S, T](_3: 3)(value: => F[S])(code: => F[T]): Iterant[F, Unit] =
          apply[S](1)(value).tapEval(_ => code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S, T](_4: 4)(pace: FiniteDuration, value: => F[S])(code: => F[T]): Iterant[F, Unit] =
          apply[S](2)(pace, value).tapEval(_ => code)

      /**
        * replication input guard
        */
      def apply(): Iterant[F, `()`[F]] =
        halt(s)

      /**
        * replication input guard w/ pace
        */
      def apply(pace: FiniteDuration): Iterant[F, `()`[F]] =
        halt((s zip Iterant.intervalAtFixedRate(pace)).map(_._1))

      /**
        * replication input guard w/ code
        */
      def apply[T]()(code: T => F[T]): Iterant[F, `()`[F]] =
        haltWithCode[T](s)(code)

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](pace: FiniteDuration)(code: T => F[T]): Iterant[F, `()`[F]] =
        haltWithCode[T]((s zip Iterant.intervalAtFixedRate(pace)).map(_._1))(code)

    object `(ν)`:

      /**
        * bound output prefix
        */
      def apply(): Iterant[F, `()`[F]] =
        for
          name <- Π.ν[F]
          _    <- Iterant.liftF(Deferred[F, Unit].map(name -> _)).through1(ch)
        yield
          name

      /**
        * bound output prefix w/ pace
        */
      def apply(pace: FiniteDuration): Iterant[F, `()`[F]] =
        apply() <* Iterant.intervalAtFixedRate(pace, pace).take(1)

      /**
        * bound output prefix w/ code
        */
      def apply[T]()(code: => F[T]): Iterant[F, `()`[F]] =
        apply().tapEval(_ => code)

      /**
        * bound output prefix w/ pace w/ code
        */
      def apply[T](pace: FiniteDuration)(code: => F[T]): Iterant[F, `()`[F]] =
        apply(pace).tapEval(_ => code)

    /**
      * constant output prefix
      */
    def apply(value: `()`[F]): Iterant[F, Unit] =
      Iterant.liftF(Deferred[F, Unit].map(value -> _)).through1(ch)

    /**
      * constant output prefix w/ pace
      */
    def apply(pace: FiniteDuration, value: `()`[F]): Iterant[F, Unit] =
      apply(value) <* Iterant.intervalAtFixedRate(pace, pace).take(1)

    /**
      * constant output prefix w/ code
      */
    def apply[T](value: `()`[F])(code: => F[T]): Iterant[F, Unit] =
      apply(value).tapEval(_ => code)

    /**
      * constant output prefix w/ pace w/ code
      */
    def apply[T](pace: FiniteDuration, value: `()`[F])(code: => F[T]): Iterant[F, Unit] =
      apply(pace, value).tapEval(_ => code)

    object `(null)`:

      /**
        * `null` output prefix
        */
      def apply(): Iterant[F, Unit] =
        Iterant.liftF(ch.halt(None))

      /**
        * `null` output prefix w/ pace
        */
      def apply(_pace: FiniteDuration): Iterant[F, Unit] =
        apply()

      /**
        * `null` output prefix w/ code
        */
      def apply[T]()(code: => F[T]): Iterant[F, Unit] =
        apply().tapEval(_ => code)

      /**
        * `null` output prefix w/ pace w/ code
        */
      def apply[T](_pace: FiniteDuration)(code: => F[T]): Iterant[F, Unit] =
        apply[T]()(code)

    object `(*)`:

      /**
        * variable output prefix
        */
      def apply[S](_1: 1)(value: => S)(using DummyImplicit): Iterant[F, Unit] =
        apply[S](1)(Concurrent[F].delay(value))

      /**
        * variable output prefix w/ pace
        */
      def apply[S](_2: 2)(pace: FiniteDuration, value: => S)(using DummyImplicit): Iterant[F, Unit] =
        apply[S](1)(value) <* Iterant.intervalAtFixedRate(pace, pace).take(1)

      /**
        * variable output prefix w/ code
        */
      def apply[S, T](_3: 3)(value: => S)(code: => F[T])(using DummyImplicit): Iterant[F, Unit] =
        apply[S](1)(value).tapEval(_ => code)

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S, T](_4: 4)(pace: FiniteDuration, value: => S)(code: => F[T])(using DummyImplicit): Iterant[F, Unit] =
        apply[S](2)(pace, value).tapEval(_ => code)

      /**
        * variable output prefix
        */
      def apply[S](_1: 1)(value: => F[S]): Iterant[F, Unit] =
        Iterant.liftF(value.flatMap { it => Deferred[F, Unit].map(new `()`[F](it) -> _) }).through1(ch)

      /**
        * variable output prefix w/ pace
        */
      def apply[S](_2: 2)(pace: FiniteDuration, value: => F[S]): Iterant[F, Unit] =
        apply[S](1)(value) <* Iterant.intervalAtFixedRate(pace, pace).take(1)

      /**
        * variable output prefix w/ code
        */
      def apply[S, T](_3: 3)(value: => F[S])(code: => F[T]): Iterant[F, Unit] =
        apply[S](1)(value).tapEval(_ => code)

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S, T](_4: 4)(pace: FiniteDuration, value: => F[S])(code: => F[T]): Iterant[F, Unit] =
        apply[S](2)(pace, value).tapEval(_ => code)

    /**
      * input prefix
      */
    def apply(): Iterant[F, `()`[F]] =
      halt(s.take(1))

    /**
      * input prefix w/ pace
      */
    def apply(pace: FiniteDuration): Iterant[F, `()`[F]] =
      halt(s.take(1) <* Iterant.intervalAtFixedRate(pace, pace).take(1))

    /**
      * input prefix w/ code
      */
    def apply[T]()(code: T => F[T]): Iterant[F, `()`[F]] =
      haltWithCode[T](s.take(1))(code)

    /**
      * input prefix w/ pace w/ code
      */
    def apply[T](pace: FiniteDuration)(code: T => F[T]): Iterant[F, `()`[F]] =
      haltWithCode[T](s.take(1) <* Iterant.intervalAtFixedRate(pace, pace).take(1))(code)

    private def halt(s: Iterant[F, `()`[F]]): Iterant[F, `()`[F]] =
      s.tapEval(ch.halt(None).whenA(_))

    private def haltWithCode[T](s: Iterant[F, `()`[F]])(code: T => F[T]): Iterant[F, `()`[F]] =
      halt(s.map(_.`()`[T]).mapEval(code(_).map(new `()`[F](_))))

    override def toString: String = if name == null then "null" else name.toString

  }


  private object `()`:

    given [F[_]: Concurrent: ContextShift: Timer]: Conversion[`()`[F], Boolean] = !_

    extension [F[_]: Sync, A](self: Iterant[F, A])
      def tapEval[B](f: A => F[B]): Iterant[F, A] =
        self.mapEval(a => f(a).as(a))


  private object `Π-magic`:

    case class ><[F[_]](channel: ConcurrentChannel[F, Option[Throwable], (`()`[F], Deferred[F, Unit])],
                        limit: Semaphore[F])

    final implicit class IterantOps[F[_]: Concurrent, O](self: Iterant[F, O]):
      def through1(channel: ConcurrentChannel[F, Option[Throwable], O])
                  (implicit await: F[Unit]): Iterant[F, Unit] =
        self.mapEval(await >> channel.push(_)).takeWhile(identity).void
