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

  import _root_.scala.concurrent.duration.FiniteDuration

  import _root_.cats.syntax.apply.*
  import _root_.cats.syntax.applicativeError.*
  import _root_.cats.syntax.functor.*
  import _root_.cats.syntax.flatMap.*

  import _root_.cats.effect.{ Concurrent, ContextShift, Resource, Timer }
  import _root_.cats.effect.concurrent.{ Deferred, Ref }

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
          queue   <- Iterant.liftF(ConcurrentQueue.unbounded[F, Unit]())
          limit   <- Iterant.liftF(Ref[F].of(false))
        yield
          f(><[F](channel, queue, limit))
      ).flatten


  /**
    * silent transition
    */
  final class τ[F[_]: Concurrent: Timer]:

    object ! :

      /**
        * replication guard
        */
      def apply(): Iterant[F, Unit] =
        Iterant.repeatEval(())

      /**
        * replication guard w/ pace
        */
      def apply(pace: FiniteDuration): Iterant[F, Unit] =
        Iterant.intervalAtFixedRate(pace).as(())

      /**
        * replication guard w/ code
        */
      def apply[T]()(code: => F[T]): Iterant[F, Unit] =
        apply().mapEval(code.as(_))

      /**
        * replication guard w/ pace w/ code
        */
      def apply[T](pace: FiniteDuration)(code: => F[T]): Iterant[F, Unit] =
        apply(pace).mapEval(code.as(_))

    /**
      * prefix
      */
    def apply(): Iterant[F, Unit] =
      Iterant.eval(())

    /**
      * prefix w/ code
      */
    def apply[T]()(code: => F[T]): Iterant[F, Unit] =
      apply().mapEval(code.as(_))

  /**
    * events, i.e., names (topics) and values
    */
  implicit final class `()`[F[_]: Concurrent: ContextShift: Timer](private val name: Any) { self =>

    private inline def ch = `()`[><[F]].channel
    private inline def q = `()`[><[F]].queue
    private inline def r = `()`[><[F]].limit
    private implicit def a: F[Unit] = q.poll >> r.set(false)
    private def o =
      for
        b <- r.get
        e <- q.isEmpty
        _ <- if !b || e then q.offer(()) >> r.set(true) else Concurrent[F].unit
      yield
        ()
    private def s = Iterant.resource((ch.consume <* Resource.eval(o)).use(_.pull.map(_.right.get)))(_ => Concurrent[F].unit).repeat.mapEval { (it, d) => d.complete(()).attempt.map(it -> _.isRight) }.filter(_._2).map(_._1)

    def ====(that: `()`[F]) =
      try
        this.ch eq that.ch
      catch
        case _ =>
          this.name == that.name

    inline def unary_! : Boolean = name == null
    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()`[F] = this

    object ! :

      object ν:

        /**
          * replication bound output guard
          */
        def apply(): Iterant[F, `()`[F]] =
          Iterant.repeatEval(()) >> self.ν()

        /**
          * replication bound output guard w/ code
          */
        def apply[T]()(code: => F[T]): Iterant[F, `()`[F]] =
          Iterant.repeatEval(()) >> self.ν[T]()(code)

        /**
          * replication bound output guard w/ pace
          */
        def apply(pace: FiniteDuration): Iterant[F, `()`[F]] =
          Iterant.intervalAtFixedRate(pace) >> self.ν()

        /**
          * replication bound output guard w/ pace w/ code
          */
        def apply[T](pace: FiniteDuration)(code: => F[T]): Iterant[F, `()`[F]] =
          Iterant.intervalAtFixedRate(pace) >> self.ν[T]()(code)

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
        apply(value).mapEval(code.as(_))

      /**
        * constant replication output guard w/ pace w/ code
        */
      def apply[T](pace: FiniteDuration, value: `()`[F])(code: => F[T]): Iterant[F, Unit] =
        apply(pace, value).mapEval(code.as(_))

      object `null`:

        /**
          * `null` replication output guard
          */
        inline def apply(): Iterant[F, Unit] =
          self.!.apply(new `()`[F](null))

        /**
          * `null` replication output guard w/ pace
          */
        inline def apply(pace: FiniteDuration): Iterant[F, Unit] =
          self.!.apply(pace, new `()`[F](null))

        /**
          * `null` replication output guard w/ code
          */
        inline def apply[T]()(code: => F[T]): Iterant[F, Unit] =
          self.!.apply[T](new `()`[F](null))(code)

        /**
          * `null` replication output guard w/ pace w/ code
          */
        inline def apply[T](pace: FiniteDuration)(code: => F[T]): Iterant[F, Unit] =
          self.!.apply[T](pace, new `()`[F](null))(code)

      object * :

        /**
          * variable replication output guard
          */
        def apply[S](value: => S): Iterant[F, Unit] =
          apply[S](Concurrent[F].delay(value))

        /**
          * variable replication output guard w/ pace
          */
        def apply[S](pace: FiniteDuration, value: => S): Iterant[F, Unit] =
          apply[S](pace, Concurrent[F].delay(value))

        /**
          * variable replication output guard w/ code
          */
        def apply[S, T](value: => S)(code: => F[T]): Iterant[F, Unit] =
          apply[S](value).mapEval(code.as(_))

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S, T](pace: FiniteDuration, value: => S)(code: => F[T]): Iterant[F, Unit] =
          apply[S](pace, value).mapEval(code.as(_))

        /**
          * variable replication output guard
          */
        @annotation.targetName("applyF")
        def apply[S](value: => F[S]): Iterant[F, Unit] =
          Iterant.repeatEvalF(value).mapEval { it => Deferred[F, Unit].map(new `()`[F](it) -> _) }.through1(ch)

        /**
          * variable replication output guard w/ pace
          */
        @annotation.targetName("applyF")
        def apply[S](pace: FiniteDuration, value: => F[S]): Iterant[F, Unit] =
          Iterant.intervalAtFixedRate(pace).mapEval(_ => value).mapEval { it => Deferred[F, Unit].map(new `()`[F](it) -> _) }.through1(ch)

        /**
          * variable replication output guard w/ code
          */
        @annotation.targetName("applyF")
        def apply[S, T](value: => F[S])(code: => F[T]): Iterant[F, Unit] =
          apply[S](value).mapEval(code.as(_))

        /**
          * variable replication output guard w/ pace w/ code
          */
        @annotation.targetName("applyF")
        def apply[S, T](pace: FiniteDuration, value: => F[S])(code: => F[T]): Iterant[F, Unit] =
          apply[S](pace, value).mapEval(code.as(_))

      /**
        * replication input guard
        */
      def apply(): Iterant[F, `()`[F]] =
        s

      /**
        * replication input guard w/ pace
        */
      def apply(pace: FiniteDuration): Iterant[F, `()`[F]] =
        (Iterant.intervalAtFixedRate(pace) zip s).map(_._2)

      /**
        * replication input guard w/ code
        */
      def apply[T]()(code: T => F[T]): Iterant[F, `()`[F]] =
        apply().mapEval { it => code(it.`()`[T]).map(new `()`[F](_)) }

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](pace: FiniteDuration)(code: T => F[T]): Iterant[F, `()`[F]] =
        apply(pace).mapEval { it => code(it.`()`[T]).map(new `()`[F](_)) }

    object ν:

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
        * bound output prefix w/ code
        */
      def apply[T]()(code: => F[T]): Iterant[F, `()`[F]] =
        apply().mapEval(code.as(_))

    /**
      * constant output prefix
      */
    def apply(value: `()`[F]): Iterant[F, Unit] =
      Iterant.liftF(Deferred[F, Unit].map(value -> _)).through1(ch)

    /**
      * constant output prefix w/ code
      */
    def apply[T](value: `()`[F])(code: => F[T]): Iterant[F, Unit] =
      apply(value).mapEval(code.as(_))

    object `null`:

      /**
        * `null` output prefix
        */
      inline def apply(): Iterant[F, Unit] =
        self.apply(new `()`[F](null))

      /**
        * `null` output prefix w/ code
        */
      inline def apply[T]()(code: => F[T]): Iterant[F, Unit] =
        self.apply[T](new `()`[F](null))(code)

    object * :

      /**
        * variable output prefix
        */
      def apply[S](value: => S): Iterant[F, Unit] =
        apply[S](Concurrent[F].delay(value))

      /**
        * variable output prefix w/ code
        */
      def apply[S, T](value: => S)(code: => F[T]): Iterant[F, Unit] =
        apply[S](value).mapEval(code.as(_))

      /**
        * variable output prefix
        */
      @annotation.targetName("applyF")
      def apply[S](value: => F[S]): Iterant[F, Unit] =
        Iterant.liftF(value).mapEval { it => Deferred[F, Unit].map(new `()`[F](it) -> _) }.through1(ch)

      /**
        * variable output prefix w/ code
        */
      @annotation.targetName("applyF")
      def apply[S, T](value: => F[S])(code: => F[T]): Iterant[F, Unit] =
        apply[S](value).mapEval(code.as(_))

    /**
      * input prefix
      */
    def apply(): Iterant[F, `()`[F]] =
      s.take(1)

    /**
      * input prefix w/ code
      */
    def apply[T]()(code: T => F[T]): Iterant[F, `()`[F]] =
      apply().mapEval { it => code(it.`()`[T]).map(new `()`[F](_)) }

    override def toString: String = if name == null then "null" else name.toString

  }


  private object `Π-magic`:

    case class ><[F[_]](channel: ConcurrentChannel[F, Option[Throwable], (`()`[F], Deferred[F, Unit])],
                        queue: ConcurrentQueue[F, Unit],
                        limit: Ref[F, Boolean])

    final implicit class IterantOps[F[_]: Concurrent, O](self: Iterant[F, O]):
      inline def through1(channel: ConcurrentChannel[F, Option[Throwable], O])
                         (implicit await: F[Unit]): Iterant[F, Unit] =
        self.mapEval(await >> channel.push(_)).void
