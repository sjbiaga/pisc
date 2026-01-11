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

  import _root_.cats.effect.{ Async, Deferred, Ref, Resource }
  import _root_.cats.effect.std.{ CyclicBarrier, Queue }

  import _root_.fs2.Stream
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
          stop  <- Stream.eval(Deferred[F, Unit])
          queue <- Stream.eval(Queue.unbounded[F, Unit])
          limit <- Stream.eval(Ref[F].of(false))
        yield
          f(><[F](topic, stop, queue, limit))
      ).flatten


  /**
    * silent transition
    */
  final class τ[F[_]: Async]:

    object ! :

      object + :

        /**
          * linear replication guard
          */
        def apply()(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, Unit] =
          Stream.repeatEval(-.await >> +.fold(Async[F].unit)(_.take) >> *.fold(Async[F].unit)(_.offer(())))

        /**
          * linear replication guard w/ pace
          */
        def apply(pace: FiniteDuration)(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, Unit] =
          apply()(-, +, *).spaced(pace)

        /**
          * linear replication guard w/ code
          */
        def apply[T]()(code: => F[T])(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, Unit] =
          apply()(-, +, *).evalTap(_ => code)

        /**
          * linear replication guard w/ pace w/ code
          */
        def apply[T](pace: FiniteDuration)(code: => F[T])(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, Unit] =
          apply(pace)(-, +, *).evalTap(_ => code)

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
    private inline def d = `()`[><[F]].stop
    private inline def q = `()`[><[F]].queue
    private inline def r = `()`[><[F]].limit
    private implicit def a: F[Unit] = q.take >> r.set(false)
    private def o =
      for
        b <- r.get
        s <- q.size
        _ <- if !b || s == 0 then q.offer(()) >> r.set(true) else Async[F].unit
      yield
        ()
    private def s = Stream.resource(t.subscribeAwaitUnbounded <* Resource.eval(o)).flatten.evalFilter(_._2.complete(())).map(_._1)
    private def s(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, `()`[F]] =
      (Stream.repeatEval(-.await >> +.fold(Async[F].unit)(_.take)) zipRight s).evalTap(_ => *.fold(Async[F].unit)(_.offer(())))

    def ====(that: `()`[F]) =
      try
        this.t eq that.t
      catch
        case _ =>
          this.name == that.name

    inline def unary_! : Boolean = name == null
    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()`[F] = this

    object ! :

      object + :

        object ν:

          /**
            * linear replication bound output guard
            */
          def apply()(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, `()`[F]] =
            (Stream.repeatEval(-.await >> +.fold(Async[F].unit)(_.take)) >> self.ν()).evalTap(_ => *.fold(Async[F].unit)(_.offer(()))).interruptWhen(d.get.attempt)

          /**
            * linear replication bound output guard w/ pace
            */
          def apply(pace: FiniteDuration)(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, `()`[F]] =
            apply()(-, +, *).spaced(pace)

          /**
            * linear replication bound output guard w/ code
            */
          def apply[T]()(code: => F[T])(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, `()`[F]] =
            apply()(-, +, *).evalTap(_ => code)

          /**
            * linear replication bound output guard w/ pace w/ code
            */
          def apply[T](pace: FiniteDuration)(code: => F[T])(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, `()`[F]] =
            apply(pace)(-, +, *).evalTap(_ => code)

        /**
          * linear constant replication output guard
          */
        def apply(value: `()`[F])(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, Unit] =
          Stream.repeatEval(-.await >> +.fold(Async[F].unit)(_.take) >> Deferred[F, Unit].map(value -> _)).through1(t).evalTap(_ => *.fold(Async[F].unit)(_.offer(()))).interruptWhen(d.get.attempt)

        /**
          * linear constant replication output guard w/ pace
          */
        def apply(pace: FiniteDuration, value: `()`[F])(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, Unit] =
          apply(value)(-, +, *).spaced(pace)

        /**
          * linear constant replication output guard w/ code
          */
        def apply[T](value: `()`[F])(code: => F[T])(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, Unit] =
          apply(value)(-, +, *).evalTap(_ => code)

        /**
          * linear constant replication output guard w/ pace w/ code
          */
        def apply[T](pace: FiniteDuration, value: `()`[F])(code: => F[T])(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, Unit] =
          apply(pace, value)(-, +, *).evalTap(_ => code)

        object `null`:

          /**
            * linear `null` replication output guard
            */
          def apply()(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, Unit] =
            self.`null`()

          /**
            * linear `null` replication output guard w/ pace
            */
          def apply(_pace: FiniteDuration)(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, Unit] =
            apply()(-, +, *)

          /**
            * linear `null` replication output guard w/ code
            */
          def apply[T]()(code: => F[T])(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, Unit] =
            self.`null`[T]()(code)

          /**
            * linear `null` replication output guard w/ pace w/ code
            */
          def apply[T](_pace: FiniteDuration)(code: => F[T])(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, Unit] =
            apply[T]()(code)(-, +, *)

        object * :

          /**
            * linear variable replication output guard
            */
          def apply[S](value: => S)(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, Unit] =
            apply[S](Async[F].delay(value))(-, +, *)

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S](pace: FiniteDuration, value: => S)(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, Unit] =
            apply[S](pace, Async[F].delay(value))(-, +, *)

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S, T](value: => S)(code: F[T])(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, Unit] =
            apply[S, T](Async[F].delay(value))(code)(-, +, *)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S, T](pace: FiniteDuration, value: => S)(code: F[T])(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, Unit] =
            apply[S, T](pace, Async[F].delay(value))(code)(-, +, *)

          /**
            * linear variable replication output guard
            */
          @annotation.targetName("applyF")
          def apply[S](value: => F[S])(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, Unit] =
            Stream.repeatEval {
              for
                _  <- -.await
                _  <- +.fold(Async[F].unit)(_.take)
                it <- value
                d  <- Deferred[F, Unit]
              yield
                new `()`[F](it) -> d
            }.through1(t).evalTap(_ => *.fold(Async[F].unit)(_.offer(()))).interruptWhen(d.get.attempt)

          /**
            * linear variable replication output guard w/ pace
            */
          @annotation.targetName("applyF")
          def apply[S](pace: FiniteDuration, value: => F[S])(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, Unit] =
            apply[S](value)(-, +, *).spaced(pace)

          /**
            * linear variable replication output guard w/ code
            */
          @annotation.targetName("applyF")
          def apply[S, T](value: => F[S])(code: F[T])(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, Unit] =
            apply[S](value)(-, +, *).evalTap(_ => code)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          @annotation.targetName("applyF")
          def apply[S, T](pace: FiniteDuration, value: => F[S])(code: F[T])(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, Unit] =
            apply[S](pace, value)(-, +, *).evalTap(_ => code)

        /**
          * linear replication input guard
          */
        def apply()(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, `()`[F]] =
          stop(s(-, +, *))

        /**
          * linear replication input guard w/ pace
          */
        def apply(pace: FiniteDuration)(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, `()`[F]] =
          stop(s(-, +, *).spaced(pace))

        /**
          * linear replication input guard w/ code
          */
        def apply[T]()(code: T => F[T])(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, `()`[F]] =
          stopWithCode[T](s(-, +, *))(code)

        /**
          * linear replication input guard w/ pace w/ code
          */
        def apply[T](pace: FiniteDuration)(code: T => F[T])(- : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]]): Stream[F, `()`[F]] =
          stopWithCode[T](s(-, +, *).spaced(pace))(code)

      object ν:

        /**
          * replication bound output guard
          */
        def apply(): Stream[F, `()`[F]] =
          Stream.unit.repeat >> self.ν()

        /**
          * replication bound output guard w/ pace
          */
        def apply(pace: FiniteDuration): Stream[F, `()`[F]] =
          Stream.awakeEvery(pace) >> self.ν()

        /**
          * replication bound output guard w/ code
          */
        def apply[T]()(code: => F[T]): Stream[F, `()`[F]] =
          Stream.unit.repeat >> self.ν[T]()(code)

        /**
          * replication bound output guard w/ pace w/ code
          */
        def apply[T](pace: FiniteDuration)(code: => F[T]): Stream[F, `()`[F]] =
          Stream.awakeEvery(pace) >> self.ν[T]()(code)

      /**
        * constant replication output guard
        */
      def apply(value: `()`[F]): Stream[F, Unit] =
        Stream.repeatEval(Deferred[F, Unit].map(value -> _)).through1(t).interruptWhen(d.get.attempt)

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

      object `null`:

        /**
          * `null` replication output guard
          */
        def apply(): Stream[F, Unit] =
          self.`null`()

        /**
          * `null` replication output guard w/ pace
          */
        def apply(_pace: FiniteDuration): Stream[F, Unit] =
          apply()

        /**
          * `null` replication output guard w/ code
          */
        def apply[T]()(code: => F[T]): Stream[F, Unit] =
          self.`null`[T]()(code)

        /**
          * `null` replication output guard w/ pace w/ code
          */
        def apply[T](_pace: FiniteDuration)(code: => F[T]): Stream[F, Unit] =
          apply[T]()(code)

      object * :

        /**
          * variable replication output guard
          */
        def apply[S](value: => S): Stream[F, Unit] =
          apply[S](Async[F].delay(value))

        /**
          * variable replication output guard w/ pace
          */
        def apply[S](pace: FiniteDuration, value: => S): Stream[F, Unit] =
          apply[S](pace, Async[F].delay(value))

        /**
          * variable replication output guard w/ code
          */
        def apply[S, T](value: => S)(code: => F[T]): Stream[F, Unit] =
          apply[S](value).evalTap(_ => code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S, T](pace: FiniteDuration, value: => S)(code: => F[T]): Stream[F, Unit] =
          apply[S](pace, value).evalTap(_ => code)

        /**
          * variable replication output guard
          */
        @annotation.targetName("applyF")
        def apply[S](value: => F[S]): Stream[F, Unit] =
          Stream.repeatEval(value >>= { it => Deferred[F, Unit].map(new `()`[F](it) -> _) }).through1(t).interruptWhen(d.get.attempt)

        /**
          * variable replication output guard w/ pace
          */
        @annotation.targetName("applyF")
        def apply[S](pace: FiniteDuration, value: => F[S]): Stream[F, Unit] =
          apply[S](value).spaced(pace)

        /**
          * variable replication output guard w/ code
          */
        @annotation.targetName("applyF")
        def apply[S, T](value: => F[S])(code: => F[T]): Stream[F, Unit] =
          apply[S](value).evalTap(_ => code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        @annotation.targetName("applyF")
        def apply[S, T](pace: FiniteDuration, value: => F[S])(code: => F[T]): Stream[F, Unit] =
          apply[S](pace, value).evalTap(_ => code)

      /**
        * replication input guard
        */
      def apply(): Stream[F, `()`[F]] =
        stop(s).evalTap(_ => o)

      /**
        * replication input guard w/ pace
        */
      def apply(pace: FiniteDuration): Stream[F, `()`[F]] =
        stop(s.spaced(pace)).evalTap(_ => o)

      /**
        * replication input guard w/ code
        */
      def apply[T]()(code: T => F[T]): Stream[F, `()`[F]] =
        stopWithCode[T](s)(code).evalTap(_ => o)

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](pace: FiniteDuration)(code: T => F[T]): Stream[F, `()`[F]] =
        stopWithCode[T](s.spaced(pace))(code).evalTap(_ => o)

    object ν:

      /**
        * bound output prefix
        */
      def apply(): Stream[F, `()`[F]] =
        ( for
            name <- Π.ν[F]
            _    <- Stream.eval(Deferred[F, Unit].map(name -> _)).through1(t)
          yield
            name
        ).interruptWhen(d.get.attempt)

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
      Stream.eval(Deferred[F, Unit].map(value -> _)).through1(t).interruptWhen(d.get.attempt)

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

    object `null`:

      /**
        * `null` output prefix
        */
      def apply(): Stream[F, Unit] =
        Stream.eval(d.complete(()).void).interruptWhen(d.get.attempt)

      /**
        * `null` output prefix w/ pace
        */
      def apply(_pace: FiniteDuration): Stream[F, Unit] =
        apply()

      /**
        * `null` output prefix w/ code
        */
      def apply[T]()(code: => F[T]): Stream[F, Unit] =
        apply().evalTap(_ => code)

      /**
        * `null` output prefix w/ pace w/ code
        */
      def apply[T](_pace: FiniteDuration)(code: => F[T]): Stream[F, Unit] =
        apply[T]()(code)

    object * :

      /**
        * variable output prefix
        */
      def apply[S](value: => S): Stream[F, Unit] =
        apply[S](Async[F].delay(value))

      /**
        * variable output prefix w/ pace
        */
      def apply[S](pace: FiniteDuration, value: => S): Stream[F, Unit] =
        apply[S](value) <* Stream.sleep(pace)

      /**
        * variable output prefix w/ code
        */
      def apply[S, T](value: => S)(code: => F[T]): Stream[F, Unit] =
        apply[S](value).evalTap(_ => code)

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S, T](pace: FiniteDuration, value: => S)(code: => F[T]): Stream[F, Unit] =
        apply[S](pace, value).evalTap(_ => code)

      /**
        * variable output prefix
        */
      @annotation.targetName("applyF")
      def apply[S](value: => F[S]): Stream[F, Unit] =
        Stream.eval(value >>= { it => Deferred[F, Unit].map(new `()`[F](it) -> _) }).through1(t).interruptWhen(d.get.attempt)

      /**
        * variable output prefix w/ pace
        */
      @annotation.targetName("applyF")
      def apply[S](pace: FiniteDuration, value: => F[S]): Stream[F, Unit] =
        apply[S](value) <* Stream.sleep(pace)

      /**
        * variable output prefix w/ code
        */
      @annotation.targetName("applyF")
      def apply[S, T](value: => F[S])(code: => F[T]): Stream[F, Unit] =
        apply[S](value).evalTap(_ => code)

      /**
        * variable output prefix w/ pace w/ code
        */
      @annotation.targetName("applyF")
      def apply[S, T](pace: FiniteDuration, value: => F[S])(code: => F[T]): Stream[F, Unit] =
        apply[S](pace, value).evalTap(_ => code)

    /**
      * input prefix
      */
    def apply(): Stream[F, `()`[F]] =
      stop(s.head)

    /**
      * input prefix w/ pace
      */
    def apply(pace: FiniteDuration): Stream[F, `()`[F]] =
      stop(s.head <* Stream.sleep(pace))

    /**
      * input prefix w/ code
      */
    def apply[T]()(code: T => F[T]): Stream[F, `()`[F]] =
      stopWithCode[T](s.head)(code)

    /**
      * input prefix w/ pace w/ code
      */
    def apply[T](pace: FiniteDuration)(code: T => F[T]): Stream[F, `()`[F]] =
      stopWithCode[T](s.head <* Stream.sleep(pace))(code)

    private def stop(s: Stream[F, `()`[F]]): Stream[F, `()`[F]] =
      s.evalTap { case it if it.name == null => d.complete(()).void case _ => Async[F].unit }.interruptWhen(d.get.attempt)

    private def stopWithCode[T](s: Stream[F, `()`[F]])(code: T => F[T]): Stream[F, `()`[F]] =
      stop(s.evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) })

    override def toString: String = if name == null then "null" else name.toString

  }


  private object `Π-magic`:

    case class ><[F[_]](topic: Topic[F, (`()`[F], Deferred[F, Unit])],
                        stop: Deferred[F, Unit],
                        queue: Queue[F, Unit],
                        limit: Ref[F, Boolean])

    extension [F[_]: Async, O](self: Stream[F, O])
      def through1(topic: Topic[F, O])
                  (using await: F[Unit]): Stream[F, Unit] =
        self.evalMap(await >> topic.publish1(_)).takeWhile(_.isRight).void
