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

  import _root_.scala.reflect.{ ClassTag, classTag }

  import _root_.cats.effect.std.Semaphore
  import _root_.zio.interop.catz.generic.*
  import _root_.zio.{ Duration, Exit, Hub, Promise, Schedule, Task, UIO, ZIO }
  import _root_.zio.concurrent.CyclicBarrier
  import _root_.zio.stream.{ ZSink, ZStream }

  import `Π-magic`.*


  given [A]: Conversion[Task[A], UIO[A]] =
    _.either.map {
      case Right(it) => it
      case _         => null.asInstanceOf[A]
    }

  private def exec[T](code: Task[T]): UIO[T] =
    code.fork.flatMap(_.join.exit).map {
      case Exit.Success(it) => it
      case _                => null.asInstanceOf[T]
    }


  /**
    * restriction aka new name
    */
  object ν:

    def map[B](f: `()` => B): ZStream[Any, Nothing, B] = flatMap(f andThen ZStream.succeed)
    def flatMap[B](f: `()` => ZStream[Any, Nothing, B]): ZStream[Any, Nothing, B] =
      ( for
          hub   <- ZStream.fromZIO(Hub.unbounded[(`()`, Promise[Nothing, Unit])])
          stop  <- ZStream.fromZIO(Promise.make[Nothing, Unit])
          limit <- ZStream.fromZIO(Semaphore[UIO](0))
        yield
          f(><(hub, stop, limit))
      ).flatten


  /**
    * silent transition
    */
  object τ:

    object `(!)`:

      object `(+)`:

        /**
          * linear replication guard
          */
        def apply()(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          ZStream.fromZIO(-.await.exit *> *.fold(ZIO.unit)(_.acquire) *> +.release).repeat(Schedule.forever)

        /**
          * linear replication guard w/ pace
          */
        def apply(pace: Duration)(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          apply()(-, * ,+) zipLeft ZStream.tick(pace)

        /**
          * linear replication guard w/ code
          */
        def apply[T]()(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          apply()(-, * ,+).tap(_ => exec(code))

        /**
          * linear replication guard w/ pace w/ code
          */
        def apply[T](pace: Duration)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          apply(pace)(-, * ,+).tap(_ => exec(code))

      /**
        * replication guard
        */
      def apply(): ZStream[Any, Nothing, Unit] =
        ZStream.unit.repeat(Schedule.forever)

      /**
        * replication guard w/ pace
        */
      def apply(pace: Duration): ZStream[Any, Nothing, Unit] =
        ZStream.tick(pace)

      /**
        * replication guard w/ code
        */
      def apply[T]()(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply().tap(_ => exec(code))

      /**
        * replication guard w/ pace w/ code
        */
      def apply[T](pace: Duration)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply(pace).tap(_ => exec(code))

    /**
      * prefix
      */
    def apply(): ZStream[Any, Nothing, Unit] =
      ZStream.unit

    /**
      * prefix w/ pace
      */
    def apply(pace: Duration): ZStream[Any, Nothing, Unit] =
      apply() <* ZStream.unit.repeat(Schedule.fromDuration(pace))

    /**
      * prefix w/ code
      */
    def apply[T]()(code: => Task[T]): ZStream[Any, Nothing, Unit] =
      apply().tap(_ => exec(code))

    /**
      * prefix w/ pace w/ code
      */
    def apply[T](pace: Duration)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
      apply(pace).tap(_ => exec(code))

  /**
    * events, i.e., names (hubs) and values
    */
  implicit final class `()`(private val name: Any) { self =>

    private inline def h = `()`[><].hub
    private inline def p = `()`[><].stop
    private inline def l = `()`[><].limit
    private implicit def a: UIO[Unit] = l.acquire
    private def _s = ZStream.unwrapScoped(ZStream.fromHubScoped(h).tap(_ => l.release))

    private def s: ZStream[Any , Nothing, `()`] = _s.filter(true)
    private def `s.head`: ZStream[Any , Nothing, `()`] = _s.filter(false)

    extension (self: ZStream[Any, Nothing, (`()`, Promise[Nothing, Unit])])
      private def filter(r: Boolean): ZStream[Any, Nothing, `()`] =
        ZStream.unwrapScoped {
          self.peel(ZSink.head).map {
            case (Some((it, p)), its) =>
              ZStream.fromZIO(p.succeed(())).flatMap {
                if _ then ZStream(it) ++ (ZStream.fromZIO(l.release) *> its).filter(r).when(r)
                else its.filter(r)
              }
            case _ =>
              ZStream.empty
          }
        }

    private def s(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, `()`] =
      (ZStream.fromZIO(-.await.exit *> *.fold(ZIO.unit)(_.acquire)).repeat(Schedule.forever) zipRight s).tap(_ => +.release)

    def ====(that: `()`) =
      try
        this.h eq that.h
      catch _ =>
        this.name == that.name

    inline def unary_! : Boolean = name == null
    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

    object `(!)`:

      object `(+)`:

        object `(ν)`:

          /**
            * linear replication bound output guard
            */
          def apply()(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, `()`] =
            (ZStream.fromZIO(-.await.exit *> *.fold(ZIO.unit)(_.acquire)).repeat(Schedule.forever) *> self.`(ν)`()).tap(_ => +.release).interruptWhen(p)

          /**
            * linear replication bound output guard w/ pace
            */
          def apply(pace: Duration)(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, `()`] =
            apply()(-, * ,+) zipLeft ZStream.tick(pace)

          /**
            * linear replication bound output guard w/ code
            */
          def apply[T]()(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, `()`] =
            apply()(-, * ,+).tap(_ => exec(code))

          /**
            * linear replication bound output guard w/ pace w/ code
            */
          def apply[T](pace: Duration)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, `()`] =
            apply(pace)(-, * ,+).tap(_ => exec(code))

        /**
          * linear constant replication output guard
          */
        def apply(value: `()`)(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          ZStream.fromZIO(-.await.exit *> *.fold(ZIO.unit)(_.acquire) *> Promise.make[Nothing, Unit].map(value -> _)).repeat(Schedule.forever).through1(h).tap(_ => +.release).interruptWhen(p)

        /**
          * linear constant replication output guard w/ pace
          */
        def apply(pace: Duration, value: `()`)(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          apply(value)(-, * ,+) zipLeft ZStream.tick(pace)

        /**
          * linear constant replication output guard w/ code
          */
        def apply[T](value: `()`)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          apply(value)(-, * ,+).tap(_ => exec(code))

        /**
          * linear constant replication output guard w/ pace w/ code
          */
        def apply[T](pace: Duration, value: `()`)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          apply(pace, value)(-, * ,+).tap(_ => exec(code))

        object `(null)`:

          /**
            * linear `null` replication output guard
            */
          def apply()(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            self.`(null)`()

          /**
            * linear `null` replication output guard w/ pace
            */
          def apply(_pace: Duration)(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            apply()(-, * ,+)

          /**
            * linear `null` replication output guard w/ code
            */
          def apply[T]()(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            self.`(null)`[T]()(code)

          /**
            * linear `null` replication output guard w/ pace w/ code
            */
          def apply[T](_pace: Duration)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            apply[T]()(code)(-, * ,+)

        object `(*)`:

          /**
            * linear variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(value: => S)(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(value.asInstanceOf[`()`])(-, * ,+)
            else
              apply[S](1)(ZIO.attempt(value))(-, * ,+)

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(pace: Duration, value: => S)(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(pace, value.asInstanceOf[`()`])(-, * ,+)
            else
              apply[S](2)(pace, ZIO.attempt(value))(-, * ,+)

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(value: => S)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(value.asInstanceOf[`()`])(code)(-, * ,+)
            else
              apply[S, T](3)(ZIO.attempt(value))(code)(-, * ,+)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => S)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(pace, value.asInstanceOf[`()`])(code)(-, * ,+)
            else
              apply[S, T](4)(pace, ZIO.attempt(value))(code)(-, * ,+)

          /**
            * linear variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(value: => Task[S])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              ZStream.fromZIO(ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]]: UIO[`()`])).flatMap(self.`(!)`.`(+)`(_)(-, *, +))
            else
              ZStream.fromZIO {
                for
                  _  <- -.await.exit
                  _  <- *.fold(ZIO.unit)(_.acquire)
                  it <- (value: UIO[S]).flatMap { it => Promise.make[Nothing, Unit].map(new `()`(it) -> _) }
                yield
                  new `()`(it) -> p
              }.repeat(Schedule.forever).through1(h).tap(_ => +.release).interruptWhen(p)

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(pace: Duration, value: => Task[S])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            apply[S](1)(value)(-, * ,+) zipLeft ZStream.tick(pace)

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(value: => Task[S])(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            apply[S](1)(value)(-, * ,+).tap(_ => exec(code))

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => Task[S])(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            apply[S](2)(pace, value)(-, * ,+).tap(_ => exec(code))

        /**
          * linear replication input guard
          */
        def apply()(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, `()`] =
          stop(s(-, * ,+))

        /**
          * linear replication input guard w/ pace
          */
        def apply(pace: Duration)(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, `()`] =
          stop(s(-, * ,+) zipLeft ZStream.tick(pace))

        /**
          * linear replication input guard w/ code
          */
        def apply[T]()(code: T => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, `()`] =
          stopWithCode[T](s(-, * ,+))(code)

        /**
          * linear replication input guard w/ pace w/ code
          */
        def apply[T](pace: Duration)(code: T => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, `()`] =
          stopWithCode[T](s(-, * ,+) zipLeft ZStream.tick(pace))(code)

      object `(ν)`:

        /**
          * replication bound output guard
          */
        def apply(): ZStream[Any, Nothing, `()`] =
          τ.`(!)`() *> self.`(ν)`()

        /**
          * replication bound output guard w/ code
          */
        def apply[T]()(code: => Task[T]): ZStream[Any, Nothing, `()`] =
          τ.`(!)`() *> self.`(ν)`[T]()(code)

        /**
          * replication bound output guard w/ pace
          */
        def apply(pace: Duration): ZStream[Any, Nothing, `()`] =
          τ.`(!)`(pace) *> self.`(ν)`()

        /**
          * replication bound output guard w/ pace w/ code
          */
        def apply[T](pace: Duration)(code: => Task[T]): ZStream[Any, Nothing, `()`] =
          τ.`(!)`(pace) *> self.`(ν)`[T]()(code)

      /**
        * constant replication output guard
        */
      def apply(value: `()`): ZStream[Any, Nothing, Unit] =
        ZStream.fromZIO(Promise.make[Nothing, Unit].map(value -> _)).repeat(Schedule.forever).through1(h).interruptWhen(p)

      /**
        * constant replication output guard w/ pace
        */
      def apply(pace: Duration, value: `()`): ZStream[Any, Nothing, Unit] =
        apply(value) zipLeft ZStream.tick(pace)

      /**
        * constant replication output guard w/ code
        */
      def apply[T](value: `()`)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply(value).tap(_ => exec(code))

      /**
        * constant replication output guard w/ pace w/ code
        */
      def apply[T](pace: Duration, value: `()`)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply(pace, value).tap(_ => exec(code))

      object `(null)`:

        /**
          * `null` replication output guard
          */
        def apply(): ZStream[Any, Nothing, Unit] =
          self.`(null)`()

        /**
          * `null` replication output guard w/ pace
          */
        def apply(_pace: Duration): ZStream[Any, Nothing, Unit] =
          apply()

        /**
          * `null` replication output guard w/ code
          */
        def apply[T]()(code: => Task[T]): ZStream[Any, Nothing, Unit] =
          self.`(null)`[T]()(code)

        /**
          * `null` replication output guard w/ pace w/ code
          */
        def apply[T](_pace: Duration)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
          apply[T]()(code)

      object `(*)`:

        /**
          * variable replication output guard
          */
        def apply[S: ClassTag](_1: 1)(value: => S)(using DummyImplicit): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(value.asInstanceOf[`()`])
          else
            apply[S](1)(ZIO.attempt(value))

        /**
          * variable replication output guard w/ pace
          */
        def apply[S: ClassTag](_2: 2)(pace: Duration, value: => S)(using DummyImplicit): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(pace, value.asInstanceOf[`()`])
          else
            apply[S](2)(pace, ZIO.attempt(value))

        /**
          * variable replication output guard w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(value: => S)(code: => Task[T])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(value.asInstanceOf[`()`])(code)
          else
            apply[S](1)(value).tap(_ => exec(code))

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => S)(code: => Task[T])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(pace, value.asInstanceOf[`()`])(code)
          else
            apply[S](2)(pace, value).tap(_ => exec(code))

        /**
          * variable replication output guard
          */
        def apply[S: ClassTag](_1: 1)(value: => Task[S]): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            ZStream.fromZIO(ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]]: UIO[`()`])).flatMap(self.`(!)`(_))
          else
            ZStream.fromZIO((value: UIO[S]).flatMap { it => Promise.make[Nothing, Unit].map(new `()`(it) -> _) }).repeat(Schedule.forever).through1(h).interruptWhen(p)

        /**
          * variable replication output guard w/ pace
          */
        def apply[S: ClassTag](_2: 2)(pace: Duration, value: => Task[S]): ZStream[Any, Nothing, Unit] =
          apply[S](1)(value) zipLeft ZStream.tick(pace)

        /**
          * variable replication output guard w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(value: => Task[S])(code: => Task[T]): ZStream[Any, Nothing, Unit] =
          apply[S](1)(value).tap(_ => exec(code))

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => Task[S])(code: => Task[T]): ZStream[Any, Nothing, Unit] =
          apply[S](2)(pace, value).tap(_ => exec(code))

      /**
        * replication input guard
        */
      def apply(): ZStream[Any, Nothing, `()`] =
        stop(s)

      /**
        * replication input guard w/ pace
        */
      def apply(pace: Duration): ZStream[Any, Nothing, `()`] =
        stop(s zipLeft ZStream.tick(pace))

      /**
        * replication input guard w/ code
        */
      def apply[T]()(code: T => Task[T]): ZStream[Any, Nothing, `()`] =
        stopWithCode[T](s)(code)

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](pace: Duration)(code: T => Task[T]): ZStream[Any, Nothing, `()`] =
        stopWithCode[T](s zipLeft ZStream.tick(pace))(code)

    object `(ν)`:

      /**
        * bound output prefix
        */
      def apply(): ZStream[Any, Nothing, `()`] =
        ( for
            name <- Π.ν
            _    <- ZStream.fromZIO(Promise.make[Nothing, Unit].map(name -> _)).through1(h)
          yield
            name
        ).interruptWhen(p)

      /**
        * bound output prefix w/ pace
        */
      def apply(pace: Duration): ZStream[Any, Nothing, `()`] =
        apply() <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * bound output prefix w/ code
        */
      def apply[T]()(code: => Task[T]): ZStream[Any, Nothing, `()`] =
        apply().tap(_ => exec(code))

      /**
        * bound output prefix w/ pace w/ code
        */
      def apply[T](pace: Duration)(code: => Task[T]): ZStream[Any, Nothing, `()`] =
        apply(pace).tap(_ => exec(code))

    /**
      * constant output prefix
      */
    def apply(value: `()`): ZStream[Any, Nothing, Unit] =
      ZStream.fromZIO(Promise.make[Nothing, Unit].map(value -> _)).through1(h).interruptWhen(p)

    /**
      * constant output prefix w/ pace
      */
    def apply(pace: Duration, value: `()`): ZStream[Any, Nothing, Unit] =
      apply(value) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

    /**
      * constant output prefix w/ code
      */
    def apply[T](value: `()`)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
      apply(value).tap(_ => exec(code))

    /**
      * constant output prefix w/ pace w/ code
      */
    def apply[T](pace: Duration, value: `()`)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
      apply(pace, value).tap(_ => exec(code))

    object `(null)`:

      /**
        * `null` output prefix
        */
      def apply(): ZStream[Any, Nothing, Unit] =
        ZStream.fromZIO(p.succeed(()).unit).interruptWhen(p)

      /**
        * `null` output prefix w/ pace
        */
      def apply(_pace: Duration): ZStream[Any, Nothing, Unit] =
        apply()

      /**
        * `null` output prefix w/ code
        */
      def apply[T]()(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply().tap(_ => exec(code))

      /**
        * `null` output prefix w/ pace w/ code
        */
      def apply[T](_pace: Duration)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply[T]()(code)

    object `(*)`:

      /**
        * variable output prefix
        */
      def apply[S: ClassTag](_1: 1)(value: => S)(using DummyImplicit): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(value.asInstanceOf[`()`])
        else
          apply[S](1)(ZIO.attempt(value))

      /**
        * variable output prefix w/ pace
        */
      def apply[S: ClassTag](_2: 2)(pace: Duration, value: => S)(using DummyImplicit): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(pace, value.asInstanceOf[`()`])
        else
          apply[S](1)(value) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * variable output prefix w/ code
        */
      def apply[S: ClassTag, T](_3: 3)(value: => S)(code: => Task[T])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(value.asInstanceOf[`()`])(code)
        else
          apply[S](1)(value).tap(_ => exec(code))

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => S)(code: => Task[T])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(pace, value.asInstanceOf[`()`])(code)
        else
          apply[S](2)(pace, value).tap(_ => exec(code))

      /**
        * variable output prefix
        */
      def apply[S: ClassTag](_1: 1)(value: => Task[S]): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          ZStream.fromZIO(ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]]: UIO[`()`])).flatMap(self(_))
        else
          ZStream.fromZIO((value: UIO[S]).flatMap { it => Promise.make[Nothing, Unit].map(new `()`(it) -> _) }).through1(h).interruptWhen(p)

      /**
        * variable output prefix w/ pace
        */
      def apply[S: ClassTag](_2: 2)(pace: Duration, value: => Task[S]): ZStream[Any, Nothing, Unit] =
        apply[S](1)(value) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * variable output prefix w/ code
        */
      def apply[S: ClassTag, T](_3: 3)(value: => Task[S])(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply[S](1)(value).tap(_ => exec(code))

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => Task[S])(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply[S](2)(pace, value).tap(_ => exec(code))

    /**
      * input prefix
      */
    def apply(): ZStream[Any, Nothing, `()`] =
      stop(`s.head`)

    /**
      * input prefix w/ pace
      */
    def apply(pace: Duration): ZStream[Any, Nothing, `()`] =
      stop(`s.head` <* ZStream.unit.repeat(Schedule.fromDuration(pace)))

    /**
      * input prefix w/ code
      */
    def apply[T]()(code: T => Task[T]): ZStream[Any, Nothing, `()`] =
      stopWithCode[T](`s.head`)(code)

    /**
      * input prefix w/ pace w/ code
      */
    def apply[T](pace: Duration)(code: T => Task[T]): ZStream[Any, Nothing, `()`] =
      stopWithCode[T](`s.head` <* ZStream.unit.repeat(Schedule.fromDuration(pace)))(code)

    private def stop(s: ZStream[Any, Nothing, `()`]): ZStream[Any, Nothing, `()`] =
      s.tap(p.succeed(()).when(_)).interruptWhen(p)

    private def stopWithCode[T](s: ZStream[Any, Nothing, `()`])(code: T => Task[T]): ZStream[Any, Nothing, `()`] =
      stop(s.map(_.`()`[T]).mapZIO((code andThen exec)(_).map(new `()`(_))))

    override def toString: String = if name == null then "null" else name.toString

  }


  private object `()`:

    given Conversion[`()`, Boolean] = !_


  private object `Π-magic`:

    case class ><(hub: Hub[(`()`, Promise[Nothing, Unit])],
                  stop: Promise[Nothing, Unit],
                  limit: Semaphore[UIO])

    extension [O](self: ZStream[Any, Nothing, O])
      def through1(hub: Hub[O])
                  (using await: UIO[Unit]): ZStream[Any, Nothing, Unit] =
        self.mapZIO(await *> hub.publish(_)).takeWhile(identity).as(())
