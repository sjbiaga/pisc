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
  import _root_.zio.interop.catz.concurrentInstance
  import _root_.zio.{ Duration, Hub, Promise, Schedule, Task, UIO, ZIO }
  import _root_.zio.concurrent.CyclicBarrier
  import _root_.zio.stream.{ ZSink, ZStream }

  import `Π-magic`.*


  /**
    * restriction aka new name
    */
  object ν:

    def map[B](f: `()` => B): ZStream[Any, Throwable, B] = flatMap(f andThen ZStream.succeed)
    def flatMap[B](f: `()` => ZStream[Any, Throwable, B]): ZStream[Any, Throwable, B] =
      ( for
          hub   <- ZStream.fromZIO(Hub.unbounded[(`()`, Promise[Throwable, Unit])])
          limit <- ZStream.fromZIO(Semaphore[Task](0))
        yield
          f(><(hub, limit))
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
        def apply()(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
          ZStream.fromZIO(-.await.exit *> *.fold(ZIO.unit)(_.acquire) *> +.release).repeat(Schedule.forever)

        /**
          * linear replication guard w/ pace
          */
        def apply(pace: Duration)(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
          apply()(-, * ,+) zipLeft ZStream.tick(pace)

        /**
          * linear replication guard w/ code
          */
        def apply[T]()(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
          apply()(-, * ,+).tap(_ => code)

        /**
          * linear replication guard w/ pace w/ code
          */
        def apply[T](pace: Duration)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
          apply(pace)(-, * ,+).tap(_ => code)

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
      apply().repeat(Schedule.fromDuration(pace))

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
    private inline def l = `()`[><].limit
    private implicit def a: Task[Unit] = l.acquire
    private def _s = ZStream.unwrapScoped(ZStream.fromHubScoped(h).tap(_ => l.release))

    private def s: ZStream[Any , Throwable, `()`] = _s.filter(true)
    private def `s.head`: ZStream[Any , Throwable, `()`] = _s.filter(false)

    extension (self: ZStream[Any, Throwable, (`()`, Promise[Throwable, Unit])])
      private def filter(r: Boolean): ZStream[Any, Throwable, `()`] =
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
          def apply()(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, `()`] =
            (ZStream.fromZIO(-.await.exit *> *.fold(ZIO.unit)(_.acquire)).repeat(Schedule.forever) *> self.`(ν)`()).tap(_ => +.release)

          /**
            * linear replication bound output guard w/ pace
            */
          def apply(pace: Duration)(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, `()`] =
            apply()(-, * ,+) zipLeft ZStream.tick(pace)

          /**
            * linear replication bound output guard w/ code
            */
          def apply[T]()(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, `()`] =
            apply()(-, * ,+).tap(_ => code)

          /**
            * linear replication bound output guard w/ pace w/ code
            */
          def apply[T](pace: Duration)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, `()`] =
            apply(pace)(-, * ,+).tap(_ => code)

        /**
          * linear constant replication output guard
          */
        def apply(value: `()`)(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
          ZStream.fromZIO(-.await.exit *> *.fold(ZIO.unit)(_.acquire) *> Promise.make[Throwable, Unit].map(value -> _)).repeat(Schedule.forever).through1(h).tap(_ => +.release)

        /**
          * linear constant replication output guard w/ pace
          */
        def apply(pace: Duration, value: `()`)(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
          apply(value)(-, * ,+) zipLeft ZStream.tick(pace)

        /**
          * linear constant replication output guard w/ code
          */
        def apply[T](value: `()`)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
          apply(value)(-, * ,+).tap(_ => code)

        /**
          * linear constant replication output guard w/ pace w/ code
          */
        def apply[T](pace: Duration, value: `()`)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
          apply(pace, value)(-, * ,+).tap(_ => code)

        object `(null)`:

          /**
            * linear `null` replication output guard
            */
          def apply()(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
            self.`(!)`.`(+)`.apply(new `()`(null))(-, * ,+)

          /**
            * linear `null` replication output guard w/ pace
            */
          def apply(pace: Duration)(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
            self.`(!)`.`(+)`.apply(pace, new `()`(null))(-, * ,+)

          /**
            * linear `null` replication output guard w/ code
            */
          def apply[T]()(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
            self.`(!)`.`(+)`.apply[T](new `()`(null))(code)(-, * ,+)

          /**
            * linear `null` replication output guard w/ pace w/ code
            */
          def apply[T](pace: Duration)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
            self.`(!)`.`(+)`.apply[T](pace, new `()`(null))(code)(-, * ,+)

        object `(*)`:

          /**
            * linear variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(value: => S)(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])(using DummyImplicit): ZStream[Any, Throwable, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(value.asInstanceOf[`()`])(-, * ,+)
            else
              apply[S](1)(ZIO.attempt(value))(-, * ,+)

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(pace: Duration, value: => S)(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])(using DummyImplicit): ZStream[Any, Throwable, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(pace, value.asInstanceOf[`()`])(-, * ,+)
            else
              apply[S](2)(pace, ZIO.attempt(value))(-, * ,+)

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(value: => S)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])(using DummyImplicit): ZStream[Any, Throwable, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(value.asInstanceOf[`()`])(code)(-, * ,+)
            else
              apply[S, T](3)(ZIO.attempt(value))(code)(-, * ,+)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => S)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])(using DummyImplicit): ZStream[Any, Throwable, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(pace, value.asInstanceOf[`()`])(code)(-, * ,+)
            else
              apply[S, T](4)(pace, ZIO.attempt(value))(code)(-, * ,+)

          /**
            * linear variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(value: => Task[S])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              ZStream.fromZIO(ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]])).flatMap(self.`(!)`.`(+)`(_)(-, *, +))
            else
              ZStream.fromZIO {
                for
                  _  <- -.await.exit
                  _  <- *.fold(ZIO.unit)(_.acquire)
                  it <- value.flatMap { it => Promise.make[Throwable, Unit].map(new `()`(it) -> _) }
                yield
                  it
              }.repeat(Schedule.forever).through1(h).tap(_ => +.release)

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(pace: Duration, value: => Task[S])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
            apply[S](1)(value)(-, * ,+) zipLeft ZStream.tick(pace)

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(value: => Task[S])(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
            apply[S](1)(value)(-, * ,+).tap(_ => code)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => Task[S])(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
            apply[S](2)(pace, value)(-, * ,+).tap(_ => code)

        /**
          * linear replication input guard
          */
        def apply()(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, `()`] =
          (ZStream.fromZIO(-.await.exit *> *.fold(ZIO.unit)(_.acquire)).repeat(Schedule.forever) zipRight s).tap(_ => +.release)

        /**
          * linear replication input guard w/ pace
          */
        def apply(pace: Duration)(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, `()`] =
          apply()(-, * ,+) zipLeft ZStream.tick(pace)

        /**
          * linear replication input guard w/ code
          */
        def apply[T]()(code: T => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, `()`] =
          apply()(-, * ,+).map(_.`()`[T]).mapZIO(code(_).map(new `()`(_)))

        /**
          * linear replication input guard w/ pace w/ code
          */
        def apply[T](pace: Duration)(code: T => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, `()`] =
          apply(pace)(-, * ,+).map(_.`()`[T]).mapZIO(code(_).map(new `()`(_)))

      object `(ν)`:

        /**
          * replication bound output guard
          */
        def apply(): ZStream[Any, Throwable, `()`] =
          τ.`(!)`() *> self.`(ν)`()

        /**
          * replication bound output guard w/ code
          */
        def apply[T]()(code: => Task[T]): ZStream[Any, Throwable, `()`] =
          τ.`(!)`() *> self.`(ν)`[T]()(code)

        /**
          * replication bound output guard w/ pace
          */
        def apply(pace: Duration): ZStream[Any, Throwable, `()`] =
          τ.`(!)`(pace) *> self.`(ν)`()

        /**
          * replication bound output guard w/ pace w/ code
          */
        def apply[T](pace: Duration)(code: => Task[T]): ZStream[Any, Throwable, `()`] =
          τ.`(!)`(pace) *> self.`(ν)`[T]()(code)

      /**
        * constant replication output guard
        */
      def apply(value: `()`): ZStream[Any, Throwable, Unit] =
        ZStream.fromZIO(Promise.make[Throwable, Unit].map(value -> _)).repeat(Schedule.forever).through1(h)

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

      object `(null)`:

        /**
          * `null` replication output guard
          */
        def apply(): ZStream[Any, Throwable, Unit] =
          self.`(!)`.apply(new `()`(null))

        /**
          * `null` replication output guard w/ pace
          */
        def apply(pace: Duration): ZStream[Any, Throwable, Unit] =
          self.`(!)`.apply(pace, new `()`(null))

        /**
          * `null` replication output guard w/ code
          */
        def apply[T]()(code: => Task[T]): ZStream[Any, Throwable, Unit] =
          self.`(!)`.apply[T](new `()`(null))(code)

        /**
          * `null` replication output guard w/ pace w/ code
          */
        def apply[T](pace: Duration)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
          self.`(!)`.apply[T](pace, new `()`(null))(code)

      object `(*)`:

        /**
          * variable replication output guard
          */
        def apply[S: ClassTag](_1: 1)(value: => S)(using DummyImplicit): ZStream[Any, Throwable, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(value.asInstanceOf[`()`])
          else
            apply[S](1)(ZIO.attempt(value))

        /**
          * variable replication output guard w/ pace
          */
        def apply[S: ClassTag](_2: 2)(pace: Duration, value: => S)(using DummyImplicit): ZStream[Any, Throwable, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(pace, value.asInstanceOf[`()`])
          else
            apply[S](2)(pace, ZIO.attempt(value))

        /**
          * variable replication output guard w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(value: => S)(code: => Task[T])(using DummyImplicit): ZStream[Any, Throwable, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(value.asInstanceOf[`()`])(code)
          else
            apply[S](1)(value).tap(_ => code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => S)(code: => Task[T])(using DummyImplicit): ZStream[Any, Throwable, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(pace, value.asInstanceOf[`()`])(code)
          else
            apply[S](2)(pace, value).tap(_ => code)

        /**
          * variable replication output guard
          */
        def apply[S: ClassTag](_1: 1)(value: => Task[S]): ZStream[Any, Throwable, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            ZStream.fromZIO(ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]])).flatMap(self.`(!)`(_))
          else
            ZStream.fromZIO(value.flatMap { it => Promise.make[Throwable, Unit].map(new `()`(it) -> _) }).repeat(Schedule.forever).through1(h)

        /**
          * variable replication output guard w/ pace
          */
        def apply[S: ClassTag](_2: 2)(pace: Duration, value: => Task[S]): ZStream[Any, Throwable, Unit] =
          apply[S](1)(value) zipLeft ZStream.tick(pace)

        /**
          * variable replication output guard w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(value: => Task[S])(code: => Task[T]): ZStream[Any, Throwable, Unit] =
          apply[S](1)(value).tap(_ => code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => Task[S])(code: => Task[T]): ZStream[Any, Throwable, Unit] =
          apply[S](2)(pace, value).tap(_ => code)

      /**
        * replication input guard
        */
      def apply(): ZStream[Any, Throwable, `()`] =
        s

      /**
        * replication input guard w/ pace
        */
      def apply(pace: Duration): ZStream[Any, Throwable, `()`] =
        (s zipLeft ZStream.tick(pace))

      /**
        * replication input guard w/ code
        */
      def apply[T]()(code: T => Task[T]): ZStream[Any, Throwable, `()`] =
        s.map(_.`()`[T]).mapZIO(code(_).map(new `()`(_)))

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](pace: Duration)(code: T => Task[T]): ZStream[Any, Throwable, `()`] =
        (s zipLeft ZStream.tick(pace)).map(_.`()`[T]).mapZIO(code(_).map(new `()`(_)))

    object `(ν)`:

      /**
        * bound output prefix
        */
      def apply(): ZStream[Any, Throwable, `()`] =
        for
          name <- Π.ν
          _    <- ZStream.fromZIO(Promise.make[Throwable, Unit].map(name -> _)).through1(h)
        yield
          name

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
      ZStream.fromZIO(Promise.make[Throwable, Unit].map(value -> _)).through1(h)

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

    object `(null)`:

      /**
        * `null` output prefix
        */
      def apply(): ZStream[Any, Throwable, Unit] =
        self.apply(new `()`(null))

      /**
        * `null` output prefix w/ pace
        */
      def apply(pace: Duration): ZStream[Any, Throwable, Unit] =
        self.apply(pace, new `()`(null))

      /**
        * `null` output prefix w/ code
        */
      def apply[T]()(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        self.apply[T](new `()`(null))(code)

      /**
        * `null` output prefix w/ pace w/ code
        */
      def apply[T](pace: Duration)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        self.apply[T](pace, new `()`(null))(code)

    object `(*)`:

      /**
        * variable output prefix
        */
      def apply[S: ClassTag](_1: 1)(value: => S)(using DummyImplicit): ZStream[Any, Throwable, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(value.asInstanceOf[`()`])
        else
          apply[S](1)(ZIO.attempt(value))

      /**
        * variable output prefix w/ pace
        */
      def apply[S: ClassTag](_2: 2)(pace: Duration, value: => S)(using DummyImplicit): ZStream[Any, Throwable, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(pace, value.asInstanceOf[`()`])
        else
          apply[S](1)(value) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * variable output prefix w/ code
        */
      def apply[S: ClassTag, T](_3: 3)(value: => S)(code: => Task[T])(using DummyImplicit): ZStream[Any, Throwable, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(value.asInstanceOf[`()`])(code)
        else
          apply[S](1)(value).tap(_ => code)

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => S)(code: => Task[T])(using DummyImplicit): ZStream[Any, Throwable, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(pace, value.asInstanceOf[`()`])(code)
        else
          apply[S](2)(pace, value).tap(_ => code)

      /**
        * variable output prefix
        */
      def apply[S: ClassTag](_1: 1)(value: => Task[S]): ZStream[Any, Throwable, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          ZStream.fromZIO(ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]])).flatMap(self(_))
        else
          ZStream.fromZIO(value).mapZIO { it => Promise.make[Throwable, Unit].map(new `()`(it) -> _) }.through1(h)

      /**
        * variable output prefix w/ pace
        */
      def apply[S: ClassTag](_2: 2)(pace: Duration, value: => Task[S]): ZStream[Any, Throwable, Unit] =
        apply[S](1)(value) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * variable output prefix w/ code
        */
      def apply[S: ClassTag, T](_3: 3)(value: => Task[S])(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        apply[S](1)(value).tap(_ => code)

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => Task[S])(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        apply[S](2)(pace, value).tap(_ => code)

    /**
      * input prefix
      */
    def apply(): ZStream[Any, Throwable, `()`] =
      `s.head`

    /**
      * input prefix w/ pace
      */
    def apply(pace: Duration): ZStream[Any, Throwable, `()`] =
      apply() <* ZStream.unit.repeat(Schedule.fromDuration(pace))

    /**
      * input prefix w/ code
      */
    def apply[T]()(code: T => Task[T]): ZStream[Any, Throwable, `()`] =
      apply().map(_.`()`[T]).mapZIO(code(_).map(new `()`(_)))

    /**
      * input prefix w/ pace w/ code
      */
    def apply[T](pace: Duration)(code: T => Task[T]): ZStream[Any, Throwable, `()`] =
      apply(pace).map(_.`()`[T]).mapZIO(code(_).map(new `()`(_)))

    override def toString: String = if name == null then "null" else name.toString

  }


  private object `Π-magic`:

    case class ><(hub: Hub[(`()`, Promise[Throwable, Unit])],
                  limit: Semaphore[Task])

    extension [O](self: ZStream[Any, Throwable, O])
      def through1(hub: Hub[O])
                  (using await: Task[Unit]): ZStream[Any, Throwable, Unit] =
        self.mapZIO(await *> hub.publish(_)).takeWhile(identity).as(())
