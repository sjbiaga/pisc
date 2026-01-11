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

  import _root_.zio.{ Duration, Hub, Promise, Ref, Queue, Schedule, Task, UIO, ZIO }
  import _root_.zio.concurrent.CyclicBarrier
  import _root_.zio.stream.ZStream

  import `Π-magic`.*


  /**
    * restriction aka new name
    */
  object ν:

    def map[B](f: `()` => B): ZStream[Any, Throwable, B] = flatMap(f andThen ZStream.succeed)
    def flatMap[B](f: `()` => ZStream[Any, Throwable, B]): ZStream[Any, Throwable, B] =
      ( for
          hub   <- ZStream.fromZIO(Hub.unbounded[(`()`, Promise[Throwable, Unit])])
          stop  <- ZStream.fromZIO(Promise.make[Throwable, Unit])
          queue <- ZStream.fromZIO(Queue.unbounded[Unit])
          limit <- ZStream.fromZIO(Ref.make(false))
        yield
          f(><(hub, stop, queue, limit))
      ).flatten


  /**
    * silent transition
    */
  object τ:

    object ! :

      object + :

        /**
          * linear replication guard
          */
        def apply()(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, Unit] =
          ZStream.fromZIO(-.await.exit *> +.fold(ZIO.unit)(_.take) *> *.fold(ZIO.unit)(_.offer(())).unit).repeat(Schedule.forever)

        /**
          * linear replication guard w/ pace
          */
        def apply(pace: Duration)(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, Unit] =
          apply()(-, +, *) zipLeft ZStream.tick(pace)

        /**
          * linear replication guard w/ code
          */
        def apply[T]()(code: => Task[T])(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, Unit] =
          apply()(-, +, *).tap(_ => code)

        /**
          * linear replication guard w/ pace w/ code
          */
        def apply[T](pace: Duration)(code: => Task[T])(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, Unit] =
          apply(pace)(-, +, *).tap(_ => code)

      /**
        * replication guard
        */
      def apply(): ZStream[Any, Throwable, Unit] =
        ZStream.unit.repeat(Schedule.forever)

      /**
        * replication guard w/ pace
        */
      def apply(pace: Duration): ZStream[Any, Throwable, Unit] =
        ZStream.tick(pace)

      /**
        * replication guard w/ code
        */
      def apply[T]()(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        apply().tap(_ => code)

      /**
        * replication guard w/ pace w/ code
        */
      def apply[T](pace: Duration)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        apply(pace).tap(_ => code)

    /**
      * prefix
      */
    def apply(): ZStream[Any, Throwable, Unit] =
      ZStream.unit

    /**
      * prefix w/ pace
      */
    def apply(pace: Duration): ZStream[Any, Throwable, Unit] =
      apply() <* ZStream.unit.repeat(Schedule.fromDuration(pace))

    /**
      * prefix w/ code
      */
    def apply[T]()(code: => Task[T]): ZStream[Any, Throwable, Unit] =
      apply().tap(_ => code)

    /**
      * prefix w/ pace w/ code
      */
    def apply[T](pace: Duration)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
      apply(pace).tap(_ => code)

  /**
    * events, i.e., names (hubs) and values
    */
  implicit final class `()`(private val name: Any) { self =>

    private inline def h = `()`[><].hub
    private inline def p = `()`[><].stop
    private inline def q = `()`[><].queue
    private inline def r = `()`[><].limit
    private implicit def a: Task[Unit] = q.take *> r.set(false)
    private def o =
      for
        b <- r.get
        s <- q.size
        _ <- if !b || s <= 0 then q.offer(()) *> r.set(true) else ZIO.unit
      yield
        ()
    private def s = ZStream.unwrapScoped(ZStream.fromHubScoped(h).tap(_ => o)).filterZIO(_._2.succeed(())).map(_._1)
    private def s(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, `()`] =
      (ZStream.fromZIO(-.await.exit *> +.fold(ZIO.unit)(_.take)).repeat(Schedule.forever) zipRight s).tap(_ => *.fold(ZIO.unit)(_.offer(())))

    def ====(that: `()`) =
      try
        this.h eq that.h
      catch
        case _ =>
          this.name == that.name

    inline def unary_! : Boolean = name == null
    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

    object ! :

      object + :

        object ν:

          /**
            * linear replication bound output guard
            */
          def apply()(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, `()`] =
            (ZStream.fromZIO(-.await.exit *> +.fold(ZIO.unit)(_.take)).repeat(Schedule.forever) *> self.ν()).tap(_ => *.fold(ZIO.unit)(_.offer(()))).interruptWhen(p)

          /**
            * linear replication bound output guard w/ pace
            */
          def apply(pace: Duration)(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, `()`] =
            apply()(-, +, *) zipLeft ZStream.tick(pace)

          /**
            * linear replication bound output guard w/ code
            */
          def apply[T]()(code: => Task[T])(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, `()`] =
            apply()(-, +, *).tap(_ => code)

          /**
            * linear replication bound output guard w/ pace w/ code
            */
          def apply[T](pace: Duration)(code: => Task[T])(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, `()`] =
            apply(pace)(-, +, *).tap(_ => code)

        /**
          * linear constant replication output guard
          */
        def apply(value: `()`)(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, Unit] =
          ZStream.fromZIO(-.await.exit *> +.fold(ZIO.unit)(_.take) *> Promise.make[Throwable, Unit].map(value -> _)).repeat(Schedule.forever).through1(h).tap(_ => *.fold(ZIO.unit)(_.offer(()))).interruptWhen(p)

        /**
          * linear constant replication output guard w/ pace
          */
        def apply(pace: Duration, value: `()`)(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, Unit] =
          apply(value)(-, +, *) zipLeft ZStream.tick(pace)

        /**
          * linear constant replication output guard w/ code
          */
        def apply[T](value: `()`)(code: => Task[T])(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, Unit] =
          apply(value)(-, +, *).tap(_ => code)

        /**
          * linear constant replication output guard w/ pace w/ code
          */
        def apply[T](pace: Duration, value: `()`)(code: => Task[T])(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, Unit] =
          apply(pace, value)(-, +, *).tap(_ => code)

        object `null`:

          /**
            * linear `null` replication output guard
            */
          def apply()(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, Unit] =
            self.`null`()

          /**
            * linear `null` replication output guard w/ pace
            */
          def apply(_pace: Duration)(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, Unit] =
            apply()(-, +, *)

          /**
            * linear `null` replication output guard w/ code
            */
          def apply[T]()(code: => Task[T])(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, Unit] =
            self.`null`[T]()(code)

          /**
            * linear `null` replication output guard w/ pace w/ code
            */
          def apply[T](_pace: Duration)(code: => Task[T])(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, Unit] =
            apply[T]()(code)(-, +, *)

        object * :

          /**
            * linear variable replication output guard
            */
          def apply[S](value: => S)(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, Unit] =
            apply[S](ZIO.attempt(value))(-, +, *)

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S](pace: Duration, value: => S)(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, Unit] =
            apply[S](pace, ZIO.attempt(value))(-, +, *)

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S, T](value: => S)(code: => Task[T])(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, Unit] =
            apply[S, T](ZIO.attempt(value))(code)(-, +, *)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S, T](pace: Duration, value: => S)(code: => Task[T])(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, Unit] =
            apply[S, T](pace, ZIO.attempt(value))(code)(-, +, *)

          /**
            * linear variable replication output guard
            */
          @annotation.targetName("applyTask")
          def apply[S](value: => Task[S])(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, Unit] =
            ZStream.fromZIO {
              for
                _  <- -.await.exit
                _  <- +.fold(ZIO.unit)(_.take)
                it <- value
                p  <- Promise.make[Throwable, Unit]
              yield
                new `()`(it) -> p
            }.repeat(Schedule.forever).through1(h).tap(_ => *.fold(ZIO.unit)(_.offer(()))).interruptWhen(p)

          /**
            * linear variable replication output guard w/ pace
            */
          @annotation.targetName("applyTask")
          def apply[S](pace: Duration, value: => Task[S])(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, Unit] =
            apply[S](value)(-, +, *) zipLeft ZStream.tick(pace)

          /**
            * linear variable replication output guard w/ code
            */
          @annotation.targetName("applyTask")
          def apply[S, T](value: => Task[S])(code: => Task[T])(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, Unit] =
            apply[S](value)(-, +, *).tap(_ => code)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          @annotation.targetName("applyTask")
          def apply[S, T](pace: Duration, value: => Task[S])(code: => Task[T])(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, Unit] =
            apply[S](pace, value)(-, +, *).tap(_ => code)

        /**
          * linear replication input guard
          */
        def apply()(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, `()`] =
          stop(s(-, +, *))

        /**
          * linear replication input guard w/ pace
          */
        def apply(pace: Duration)(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, `()`] =
          stop(s(-, +, *) zipLeft ZStream.tick(pace))

        /**
          * linear replication input guard w/ code
          */
        def apply[T]()(code: T => Task[T])(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, `()`] =
          stopWithCode[T](s(-, +, *))(code)

        /**
          * linear replication input guard w/ pace w/ code
          */
        def apply[T](pace: Duration)(code: T => Task[T])(- : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]]): ZStream[Any, Throwable, `()`] =
          stopWithCode[T](s(-, +, *) zipLeft ZStream.tick(pace))(code)

      object ν:

        /**
          * replication bound output guard
          */
        def apply(): ZStream[Any, Throwable, `()`] =
          τ.!() *> self.ν()

        /**
          * replication bound output guard w/ code
          */
        def apply[T]()(code: => Task[T]): ZStream[Any, Throwable, `()`] =
          τ.!() *> self.ν[T]()(code)

        /**
          * replication bound output guard w/ pace
          */
        def apply(pace: Duration): ZStream[Any, Throwable, `()`] =
          τ.!(pace) *> self.ν()

        /**
          * replication bound output guard w/ pace w/ code
          */
        def apply[T](pace: Duration)(code: => Task[T]): ZStream[Any, Throwable, `()`] =
          τ.!(pace) *> self.ν[T]()(code)

      /**
        * constant replication output guard
        */
      def apply(value: `()`): ZStream[Any, Throwable, Unit] =
        ZStream.fromZIO(Promise.make[Throwable, Unit].map(value -> _)).repeat(Schedule.forever).through1(h).interruptWhen(p)

      /**
        * constant replication output guard w/ pace
        */
      def apply(pace: Duration, value: `()`): ZStream[Any, Throwable, Unit] =
        apply(value) zipLeft ZStream.tick(pace)

      /**
        * constant replication output guard w/ code
        */
      def apply[T](value: `()`)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        apply(value).tap(_ => code)

      /**
        * constant replication output guard w/ pace w/ code
        */
      def apply[T](pace: Duration, value: `()`)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        apply(pace, value).tap(_ => code)

      object `null`:

        /**
          * `null` replication output guard
          */
        def apply(): ZStream[Any, Throwable, Unit] =
          self.`null`()

        /**
          * `null` replication output guard w/ pace
          */
        def apply(_pace: Duration): ZStream[Any, Throwable, Unit] =
          apply()

        /**
          * `null` replication output guard w/ code
          */
        def apply[T]()(code: => Task[T]): ZStream[Any, Throwable, Unit] =
          self.`null`[T]()(code)

        /**
          * `null` replication output guard w/ pace w/ code
          */
        def apply[T](_pace: Duration)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
          apply[T]()(code)

      object * :

        /**
          * variable replication output guard
          */
        def apply[S](value: => S): ZStream[Any, Throwable, Unit] =
          apply[S](ZIO.attempt(value))

        /**
          * variable replication output guard w/ pace
          */
        def apply[S](pace: Duration, value: => S): ZStream[Any, Throwable, Unit] =
          apply[S](pace, ZIO.attempt(value))

        /**
          * variable replication output guard w/ code
          */
        def apply[S, T](value: => S)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
          apply[S](value).tap(_ => code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S, T](pace: Duration, value: => S)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
          apply[S](pace, value).tap(_ => code)

        /**
          * variable replication output guard
          */
        @annotation.targetName("applyTask")
        def apply[S](value: => Task[S]): ZStream[Any, Throwable, Unit] =
          ZStream.fromZIO(value.flatMap { it => Promise.make[Throwable, Unit].map(new `()`(it) -> _) }).repeat(Schedule.forever).through1(h).interruptWhen(p)

        /**
          * variable replication output guard w/ pace
          */
        @annotation.targetName("applyTask")
        def apply[S](pace: Duration, value: => Task[S]): ZStream[Any, Throwable, Unit] =
          apply[S](value) zipLeft ZStream.tick(pace)

        /**
          * variable replication output guard w/ code
          */
        @annotation.targetName("applyTask")
        def apply[S, T](value: => Task[S])(code: => Task[T]): ZStream[Any, Throwable, Unit] =
          apply[S](value).tap(_ => code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        @annotation.targetName("applyTask")
        def apply[S, T](pace: Duration, value: => Task[S])(code: => Task[T]): ZStream[Any, Throwable, Unit] =
          apply[S](pace, value).tap(_ => code)

      /**
        * replication input guard
        */
      def apply(): ZStream[Any, Throwable, `()`] =
        stop(s).tap(_ => o)

      /**
        * replication input guard w/ pace
        */
      def apply(pace: Duration): ZStream[Any, Throwable, `()`] =
        stop(s zipLeft ZStream.tick(pace)).tap(_ => o)

      /**
        * replication input guard w/ code
        */
      def apply[T]()(code: T => Task[T]): ZStream[Any, Throwable, `()`] =
        stopWithCode[T](s)(code).tap(_ => o)

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](pace: Duration)(code: T => Task[T]): ZStream[Any, Throwable, `()`] =
        stopWithCode[T](s zipLeft ZStream.tick(pace))(code).tap(_ => o)

    object ν:

      /**
        * bound output prefix
        */
      def apply(): ZStream[Any, Throwable, `()`] =
        ( for
            name <- Π.ν
            _    <- ZStream.fromZIO(Promise.make[Throwable, Unit].map(name -> _)).through1(h)
          yield
            name
        ).interruptWhen(p)

      /**
        * bound output prefix w/ pace
        */
      def apply(pace: Duration): ZStream[Any, Throwable, `()`] =
        apply() <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * bound output prefix w/ code
        */
      def apply[T]()(code: => Task[T]): ZStream[Any, Throwable, `()`] =
        apply().tap(_ => code)

      /**
        * bound output prefix w/ pace w/ code
        */
      def apply[T](pace: Duration)(code: => Task[T]): ZStream[Any, Throwable, `()`] =
        apply(pace).tap(_ => code)

    /**
      * constant output prefix
      */
    def apply(value: `()`): ZStream[Any, Throwable, Unit] =
      ZStream.fromZIO(Promise.make[Throwable, Unit].map(value -> _)).through1(h).interruptWhen(p)

    /**
      * constant output prefix w/ pace
      */
    def apply(pace: Duration, value: `()`): ZStream[Any, Throwable, Unit] =
      apply(value) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

    /**
      * constant output prefix w/ code
      */
    def apply[T](value: `()`)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
      apply(value).tap(_ => code)

    /**
      * constant output prefix w/ pace w/ code
      */
    def apply[T](pace: Duration, value: `()`)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
      apply(pace, value).tap(_ => code)

    object `null`:

      /**
        * `null` output prefix
        */
      def apply(): ZStream[Any, Throwable, Unit] =
        ZStream.fromZIO(p.succeed(()).unit).interruptWhen(p)

      /**
        * `null` output prefix w/ pace
        */
      def apply(_pace: Duration): ZStream[Any, Throwable, Unit] =
        apply()

      /**
        * `null` output prefix w/ code
        */
      def apply[T]()(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        apply().tap(_ => code)

      /**
        * `null` output prefix w/ pace w/ code
        */
      def apply[T](_pace: Duration)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        apply[T]()(code)

    object * :

      /**
        * variable output prefix
        */
      def apply[S](value: => S): ZStream[Any, Throwable, Unit] =
        apply[S](ZIO.attempt(value))

      /**
        * variable output prefix w/ pace
        */
      def apply[S](pace: Duration, value: => S): ZStream[Any, Throwable, Unit] =
        apply[S](value) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * variable output prefix w/ code
        */
      def apply[S, T](value: => S)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        apply[S](value).tap(_ => code)

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S, T](pace: Duration, value: => S)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        apply[S](pace, value).tap(_ => code)

      /**
        * variable output prefix
        */
      @annotation.targetName("applyTask")
      def apply[S](value: => Task[S]): ZStream[Any, Throwable, Unit] =
        ZStream.fromZIO(value.flatMap { it => Promise.make[Throwable, Unit].map(new `()`(it) -> _) }).through1(h).interruptWhen(p)

      /**
        * variable output prefix w/ pace
        */
      @annotation.targetName("applyTask")
      def apply[S](pace: Duration, value: => Task[S]): ZStream[Any, Throwable, Unit] =
        apply[S](value) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * variable output prefix w/ code
        */
      @annotation.targetName("applyTask")
      def apply[S, T](value: => Task[S])(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        apply[S](value).tap(_ => code)

      /**
        * variable output prefix w/ pace w/ code
        */
      @annotation.targetName("applyTask")
      def apply[S, T](pace: Duration, value: => Task[S])(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        apply[S](pace, value).tap(_ => code)

    /**
      * input prefix
      */
    def apply(): ZStream[Any, Throwable, `()`] =
      stop(s.take(1))

    /**
      * input prefix w/ pace
      */
    def apply(pace: Duration): ZStream[Any, Throwable, `()`] =
      stop(s.take(1) <* ZStream.unit.repeat(Schedule.fromDuration(pace)))

    /**
      * input prefix w/ code
      */
    def apply[T]()(code: T => Task[T]): ZStream[Any, Throwable, `()`] =
      stopWithCode[T](s.take(1))(code)

    /**
      * input prefix w/ pace w/ code
      */
    def apply[T](pace: Duration)(code: T => Task[T]): ZStream[Any, Throwable, `()`] =
      stopWithCode[T](s.take(1) <* ZStream.unit.repeat(Schedule.fromDuration(pace)))(code)

    private def stop(s: ZStream[Any, Throwable, `()`]): ZStream[Any, Throwable, `()`] =
      s.tap { case it if it.name == null => p.succeed(()) case _ => ZIO.unit }.interruptWhen(p)

    private def stopWithCode[T](s: ZStream[Any, Throwable, `()`])(code: T => Task[T]): ZStream[Any, Throwable, `()`] =
      stop(s.mapZIO { it => code(it.`()`[T]).map(new `()`(_)) })

    override def toString: String = if name == null then "null" else name.toString

  }


  private object `Π-magic`:

    case class ><(hub: Hub[(`()`, Promise[Throwable, Unit])],
                  stop: Promise[Throwable, Unit],
                  queue: Queue[Unit],
                  limit: Ref[Boolean])

    extension [O](self: ZStream[Any, Throwable, O])
      def through1(hub: Hub[O])
                  (using await: Task[Unit]): ZStream[Any, Throwable, Unit] =
        self.mapZIO(await *> hub.publish(_)).takeWhile(identity).as(())
