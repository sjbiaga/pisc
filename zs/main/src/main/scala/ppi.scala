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

  import _root_.cats.effect.std.Semaphore
  import _root_.zio.interop.catz.concurrentInstance
  import _root_.zio.{ Duration, Hub, Promise, Schedule, Task, ZIO }
  import _root_.zio.concurrent.CyclicBarrier
  import _root_.zio.stream.{ ZSink, ZStream }

  import `Π-magic`.*


  /**
    * restriction aka new name
    */
  object ν:

    private[Π] def make: ZIO[Any, Throwable, `()`] =
      for
        hub   <- Hub.unbounded[(Seq[`()`], Promise[Throwable, Unit])]
        limit <- Semaphore[Task](0)
      yield
        ><(hub, limit)

    def map[B](f: `()` => B): ZStream[Any, Throwable, B] = flatMap(f andThen ZStream.succeed)
    def flatMap[B](f: `()` => ZStream[Any, Throwable, B]): ZStream[Any, Throwable, B] =
      ZStream.fromZIO(make).flatMap(f)


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
          ZStream.fromZIO(-.await.exit *> *.fold(ZIO.unit)(_.acquire) *> +.release.unit).repeat(Schedule.forever)

        /**
          * linear replication guard w/ pace
          */
        def apply(pace: Duration)(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
          apply()(-, *, +) zipLeft ZStream.tick(pace)

        /**
          * linear replication guard w/ code
          */
        def apply[T]()(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
          apply()(-, *, +).tap(_ => code)

        /**
          * linear replication guard w/ pace w/ code
          */
        def apply[T](pace: Duration)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
          apply(pace)(-, *, +).tap(_ => code)

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

    private def s: ZStream[Any , Throwable, Seq[`()`]] = _s.filter(true)
    private def `s.head`: ZStream[Any , Throwable, Seq[`()`]] = _s.filter(false)

    extension (self: ZStream[Any, Throwable, (Seq[`()`], Promise[Throwable, Unit])])
      private def filter(r: Boolean): ZStream[Any, Throwable, Seq[`()`]] =
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
          def apply(arity: Int)(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Seq[`()`]] =
            (ZStream.fromZIO(-.await.exit *> *.fold(ZIO.unit)(_.acquire)).repeat(Schedule.forever) *> self.`(ν)`(arity)).tap(_ => +.release)

          /**
            * linear replication bound output guard w/ pace
            */
          def apply(arity: Int, pace: Duration)(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Seq[`()`]] =
            apply(arity)(-, *, +) zipLeft ZStream.tick(pace)

          /**
            * linear replication bound output guard w/ code
            */
          def apply[T](arity: Int)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Seq[`()`]] =
            apply(arity)(-, *, +).tap(_ => code)

          /**
            * linear replication bound output guard w/ pace w/ code
            */
          def apply[T](arity: Int, pace: Duration)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Seq[`()`]] =
            apply(arity, pace)(-, *, +).tap(_ => code)

        /**
          * linear constant replication output guard
          */
        def apply(_1: 1)(value: `()`*)(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
          ZStream.fromZIO(-.await.exit *> *.fold(ZIO.unit)(_.acquire) *> Promise.make[Throwable, Unit].map(value -> _)).repeat(Schedule.forever).through1(h).tap(_ => +.release)

        /**
          * linear constant replication output guard w/ pace
          */
        def apply(_2: 2)(pace: Duration, value: `()`*)(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
          apply(1)(value*)(-, *, +) zipLeft ZStream.tick(pace)

        /**
          * linear constant replication output guard w/ code
          */
        def apply[T](_3: 3)(value: `()`*)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
          apply(1)(value*)(-, *, +).tap(_ => code)

        /**
          * linear constant replication output guard w/ pace w/ code
          */
        def apply[T](_4: 4)(pace: Duration, value: `()`*)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
          apply(2)(pace, value*)(-, *, +).tap(_ => code)

        object `(null)`:

          /**
            * linear `null` replication output guard
            */
          def apply(arity: Int)(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
            self.`(!)`.`(+)`.apply(1)(Seq.fill(arity)(new `()`(null))*)(-, *, +)

          /**
            * linear `null` replication output guard w/ pace
            */
          def apply(arity: Int, pace: Duration)(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
            self.`(!)`.`(+)`.apply(2)(pace, Seq.fill(arity)(new `()`(null))*)(-, *, +)

          /**
            * linear `null` replication output guard w/ code
            */
          def apply[T](arity: Int)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
            self.`(!)`.`(+)`.apply[T](3)(Seq.fill(arity)(new `()`(null))*)(code)(-, *, +)

          /**
            * linear `null` replication output guard w/ pace w/ code
            */
          def apply[T](arity: Int, pace: Duration)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
            self.`(!)`.`(+)`.apply[T](4)(pace, Seq.fill(arity)(new `()`(null))*)(code)(-, *, +)

        object `(*)`:

          /**
            * linear variable replication output guard
            */
          def apply[S](_1: 1)(value: () => S*)(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])(using DummyImplicit): ZStream[Any, Throwable, Unit] =
            apply[S](1)(value.map { it => ZIO.attempt(it()) }*)(-, *, +)

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S](_2: 2)(pace: Duration, value: () => S*)(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])(using DummyImplicit): ZStream[Any, Throwable, Unit] =
            apply[S](2)(pace, value.map { it => ZIO.attempt(it()) }*)(-, *, +)

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S, T](_3: 3)(value: () => S*)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])(using DummyImplicit): ZStream[Any, Throwable, Unit] =
            apply[S, T](3)(value.map { it => ZIO.attempt(it()) }*)(code)(-, *, +)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S, T](_4: 4)(pace: Duration, value: () => S*)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])(using DummyImplicit): ZStream[Any, Throwable, Unit] =
            apply[S, T](4)(pace, value.map { it => ZIO.attempt(it()) }*)(code)(-, *, +)

          /**
            * linear variable replication output guard
            */
          def apply[S](_1: 1)(value: => Task[S]*)(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
            ZStream.fromZIO {
              for
                _  <- -.await.exit
                _  <- *.fold(ZIO.unit)(_.acquire)
                it <- ZIO.collectAllSuccesses(value)
                p  <- Promise.make[Throwable, Unit]
              yield
                it.map(new `()`(_)) -> p
            }.repeat(Schedule.forever).through1(h).tap(_ => +.release)

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S](_2: 2)(pace: Duration, value: => Task[S]*)(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
            apply[S](1)(value*)(-, *, +) zipLeft ZStream.tick(pace)

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S, T](_3: 3)(value: => Task[S]*)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
            apply[S](1)(value*)(-, *, +).tap(_ => code)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S, T](_4: 4)(pace: Duration, value: => Task[S]*)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Unit] =
            apply[S](2)(pace, value*)(-, *, +).tap(_ => code)

        /**
          * linear replication input guard
          */
        def apply(_1: 1)()(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Seq[`()`]] =
          (ZStream.fromZIO(-.await.exit *> *.fold(ZIO.unit)(_.acquire)).repeat(Schedule.forever) zipRight s).tap(_ => +.release)

        /**
          * linear replication input guard w/ pace
          */
        def apply(_2: 2)(pace: Duration)(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Seq[`()`]] =
          apply(1)()(-, *, +) zipLeft ZStream.tick(pace)

        /**
          * linear replication input guard w/ code
          */
        def apply[T](_3: 3)()(code: Seq[T] => Task[Seq[T]])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Seq[`()`]] =
          apply(1)()(-, *, +).map(_.map(_.`()`[T])).mapZIO(code(_).map(_.map(new `()`(_))))

        /**
          * linear replication input guard w/ pace w/ code
          */
        def apply[T](_4: 4)(pace: Duration)(code: Seq[T] => Task[Seq[T]])(- : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task]): ZStream[Any, Throwable, Seq[`()`]] =
          apply(2)(pace)(-, *, +).map(_.map(_.`()`[T])).mapZIO(code(_).map(_.map(new `()`(_))))

      object `(ν)`:

        /**
          * replication bound output guard
          */
        def apply(arity: Int): ZStream[Any, Throwable, Seq[`()`]] =
          τ.`(!)`() *> self.`(ν)`(arity)

        /**
          * replication bound output guard w/ code
          */
        def apply[T](arity: Int)(code: => Task[T]): ZStream[Any, Throwable, Seq[`()`]] =
          τ.`(!)`() *> self.`(ν)`[T](arity)(code)

        /**
          * replication bound output guard w/ pace
          */
        def apply(arity: Int, pace: Duration): ZStream[Any, Throwable, Seq[`()`]] =
          τ.`(!)`(pace) *> self.`(ν)`(arity)

        /**
          * replication bound output guard w/ pace w/ code
          */
        def apply[T](arity: Int, pace: Duration)(code: => Task[T]): ZStream[Any, Throwable, Seq[`()`]] =
          τ.`(!)`(pace) *> self.`(ν)`[T](arity)(code)

      /**
        * constant replication output guard
        */
      def apply(_1: 1)(value: `()`*): ZStream[Any, Throwable, Unit] =
        ZStream.fromZIO(Promise.make[Throwable, Unit].map(value -> _)).repeat(Schedule.forever).through1(h)

      /**
        * constant replication output guard w/ pace
        */
      def apply(_2: 2)(pace: Duration, value: `()`*): ZStream[Any, Throwable, Unit] =
        apply(1)(value*) zipLeft ZStream.tick(pace)

      /**
        * constant replication output guard w/ code
        */
      def apply[T](_3: 3)(value: `()`*)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        apply(1)(value*).tap(_ => code)

      /**
        * constant replication output guard w/ pace w/ code
        */
      def apply[T](_4: 4)(pace: Duration, value: `()`*)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        apply(2)(pace, value*).tap(_ => code)

      object `(null)`:

        /**
          * `null` replication output guard
          */
        def apply(arity: Int): ZStream[Any, Throwable, Unit] =
          self.`(!)`.apply(1)(Seq.fill(arity)(new `()`(null))*)

        /**
          * `null` replication output guard w/ pace
          */
        def apply(arity: Int, pace: Duration): ZStream[Any, Throwable, Unit] =
          self.`(!)`.apply(2)(pace, Seq.fill(arity)(new `()`(null))*)

        /**
          * `null` replication output guard w/ code
          */
        def apply[T](arity: Int)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
          self.`(!)`.apply[T](3)(Seq.fill(arity)(new `()`(null))*)(code)

        /**
          * `null` replication output guard w/ pace w/ code
          */
        def apply[T](arity: Int, pace: Duration)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
          self.`(!)`.apply[T](4)(pace, Seq.fill(arity)(new `()`(null))*)(code)

      object `(*)`:

        /**
          * variable replication output guard
          */
        def apply[S](_1: 1)(value: () => S*)(using DummyImplicit): ZStream[Any, Throwable, Unit] =
          apply[S](1)(value.map { it => ZIO.attempt(it()) }*)

        /**
          * variable replication output guard w/ pace
          */
        def apply[S](_2: 2)(pace: Duration, value: () => S*)(using DummyImplicit): ZStream[Any, Throwable, Unit] =
          apply[S](2)(pace, value.map { it => ZIO.attempt(it()) }*)

        /**
          * variable replication output guard w/ code
          */
        def apply[S, T](_3: 3)(value: () => S*)(code: => Task[T])(using DummyImplicit): ZStream[Any, Throwable, Unit] =
          apply[S](1)(value*).tap(_ => code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S, T](_4: 4)(pace: Duration, value: () => S*)(code: => Task[T])(using DummyImplicit): ZStream[Any, Throwable, Unit] =
          apply[S](2)(pace, value*).tap(_ => code)

        /**
          * variable replication output guard
          */
        def apply[S](_1: 1)(value: => Task[S]*): ZStream[Any, Throwable, Unit] =
          ZStream.fromZIO(ZIO.collectAllSuccesses(value).flatMap { it => Promise.make[Throwable, Unit].map(it.map(new `()`(_)) -> _) }).repeat(Schedule.forever).through1(h)

        /**
          * variable replication output guard w/ pace
          */
        def apply[S](_2: 2)(pace: Duration, value: => Task[S]*): ZStream[Any, Throwable, Unit] =
          apply[S](1)(value*) zipLeft ZStream.tick(pace)

        /**
          * variable replication output guard w/ code
          */
        def apply[S, T](_3: 3)(value: => Task[S]*)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
          apply[S](1)(value*).tap(_ => code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S, T](_4: 4)(pace: Duration, value: => Task[S]*)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
          apply[S](2)(pace, value*).tap(_ => code)

      /**
        * replication input guard
        */
      def apply(_1: 1)(): ZStream[Any, Throwable, Seq[`()`]] =
        s

      /**
        * replication input guard w/ pace
        */
      def apply(_2: 2)(pace: Duration): ZStream[Any, Throwable, Seq[`()`]] =
        (s zipLeft ZStream.tick(pace))

      /**
        * replication input guard w/ code
        */
      def apply[T](_3: 3)()(code: Seq[T] => Task[Seq[T]]): ZStream[Any, Throwable, Seq[`()`]] =
        s.map(_.map(_.`()`[T])).mapZIO(code(_).map(_.map(new `()`(_))))

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](_4: 4)(pace: Duration)(code: Seq[T] => Task[Seq[T]]): ZStream[Any, Throwable, Seq[`()`]] =
        (s zipLeft ZStream.tick(pace)).map(_.map(_.`()`[T])).mapZIO(code(_).map(_.map(new `()`(_))))

    object `(ν)`:

      /**
        * bound output prefix
        */
      def apply(arity: Int): ZStream[Any, Throwable, Seq[`()`]] =
        for
          names   <- ZStream.fromZIO(ZIO.collectAllSuccesses(Seq.fill(arity)(Π.ν.make)))
          promise <- ZStream.fromZIO(Promise.make[Throwable, Unit])
          _       <- ZStream.succeed(names -> promise).through1(h)
        yield
          names

      /**
        * bound output prefix w/ pace
        */
      def apply(arity: Int, pace: Duration): ZStream[Any, Throwable, Seq[`()`]] =
        apply(arity) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * bound output prefix w/ code
        */
      def apply[T](arity: Int)(code: => Task[T]): ZStream[Any, Throwable, Seq[`()`]] =
        apply(arity).tap(_ => code)

      /**
        * bound output prefix w/ pace w/ code
        */
      def apply[T](arity: Int, pace: Duration)(code: => Task[T]): ZStream[Any, Throwable, Seq[`()`]] =
        apply(arity, pace).tap(_ => code)

    /**
      * constant output prefix
      */
    def apply(_1: 1)(value: `()`*): ZStream[Any, Throwable, Unit] =
      ZStream.fromZIO(Promise.make[Throwable, Unit].map(value -> _)).through1(h)

    /**
      * constant output prefix w/ pace
      */
    def apply(_2: 2)(pace: Duration, value: `()`*): ZStream[Any, Throwable, Unit] =
      apply(1)(value*) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

    /**
      * constant output prefix w/ code
      */
    def apply[T](_3: 3)(value: `()`*)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
      apply(1)(value*).tap(_ => code)

    /**
      * constant output prefix w/ pace w/ code
      */
    def apply[T](_4: 4)(pace: Duration, value: `()`*)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
      apply(2)(pace, value*).tap(_ => code)

    object `(null)`:

      /**
        * `null` output prefix
        */
      def apply(arity: Int): ZStream[Any, Throwable, Unit] =
        self.apply(1)(Seq.fill(arity)(new `()`(null))*)

      /**
        * `null` output prefix w/ pace
        */
      def apply(arity: Int, pace: Duration): ZStream[Any, Throwable, Unit] =
        self.apply(2)(pace, Seq.fill(arity)(new `()`(null))*)

      /**
        * `null` output prefix w/ code
        */
      def apply[T](arity: Int)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        self.apply[T](3)(Seq.fill(arity)(new `()`(null))*)(code)

      /**
        * `null` output prefix w/ pace w/ code
        */
      def apply[T](arity: Int, pace: Duration)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        self.apply[T](4)(pace, Seq.fill(arity)(new `()`(null))*)(code)

    object `(*)`:

      /**
        * variable output prefix
        */
      def apply[S](_1: 1)(value: () => S*)(using DummyImplicit): ZStream[Any, Throwable, Unit] =
        apply[S](1)(value.map { it => ZIO.attempt(it()) }*)

      /**
        * variable output prefix w/ pace
        */
      def apply[S](_2: 2)(pace: Duration, value: () => S*)(using DummyImplicit): ZStream[Any, Throwable, Unit] =
        apply[S](1)(value*) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * variable output prefix w/ code
        */
      def apply[S, T](_3: 3)(value: () => S*)(code: => Task[T])(using DummyImplicit): ZStream[Any, Throwable, Unit] =
        apply[S](1)(value*).tap(_ => code)

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S, T](_4: 4)(pace: Duration, value: () => S*)(code: => Task[T])(using DummyImplicit): ZStream[Any, Throwable, Unit] =
        apply[S](2)(pace, value*).tap(_ => code)

      /**
        * variable output prefix
        */
      def apply[S](_1: 1)(value: => Task[S]*): ZStream[Any, Throwable, Unit] =
        ZStream.fromZIO(ZIO.collectAllSuccesses(value).flatMap { it => Promise.make[Throwable, Unit].map(it.map(new `()`(_)) -> _) }).through1(h)

      /**
        * variable output prefix w/ pace
        */
      def apply[S](_2: 2)(pace: Duration, value: => Task[S]*): ZStream[Any, Throwable, Unit] =
        apply[S](1)(value*) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * variable output prefix w/ code
        */
      def apply[S, T](_3: 3)(value: => Task[S]*)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        apply[S](1)(value*).tap(_ => code)

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S, T](_4: 4)(pace: Duration, value: => Task[S]*)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        apply[S](2)(pace, value*).tap(_ => code)

    /**
      * input prefix
      */
    def apply(_1: 1)(): ZStream[Any, Throwable, Seq[`()`]] =
      `s.head`

    /**
      * input prefix w/ pace
      */
    def apply(_2: 2)(pace: Duration): ZStream[Any, Throwable, Seq[`()`]] =
      apply(1)() <* ZStream.unit.repeat(Schedule.fromDuration(pace))

    /**
      * input prefix w/ code
      */
    def apply[T](_3: 3)()(code: Seq[T] => Task[Seq[T]]): ZStream[Any, Throwable, Seq[`()`]] =
      apply(1)().map(_.map(_.`()`[T])).mapZIO(code(_).map(_.map(new `()`(_))))

    /**
      * input prefix w/ pace w/ code
      */
    def apply[T](_4: 4)(pace: Duration)(code: Seq[T] => Task[Seq[T]]): ZStream[Any, Throwable, Seq[`()`]] =
      apply(2)(pace).map(_.map(_.`()`[T])).mapZIO(code(_).map(_.map(new `()`(_))))

    override def toString: String = if name == null then "null" else name.toString

  }


  private object `Π-magic`:

    case class ><(hub: Hub[(Seq[`()`], Promise[Throwable, Unit])],
                  limit: Semaphore[Task])

    extension [O](self: ZStream[Any, Throwable, O])
      def through1(hub: Hub[O])
                  (using await: Task[Unit]): ZStream[Any, Throwable, Unit] =
        self.mapZIO(await *> hub.publish(_)).takeWhile(identity).as(())
