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

  import _root_.scala.reflect.{ ClassTag, classTag }

  import _root_.cats.effect.std.Semaphore
  import _root_.zio.interop.catz.generic.*
  import _root_.zio.{ Duration, Exit, Hub, Promise, Schedule, Task, ZIO, UIO }
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

    private[Π] def make: ZIO[Any, Nothing, `()`] =
      for
        hub   <- Hub.unbounded[(Seq[`()`], Promise[Nothing, Unit])]
        stop  <- Promise.make[Nothing, Unit]
        limit <- Semaphore[UIO](0)
      yield
        ><(hub, stop, limit)

    def map[B](f: `()` => B): ZStream[Any, Nothing, B] = flatMap(f andThen ZStream.succeed)
    def flatMap[B](f: `()` => ZStream[Any, Nothing, B]): ZStream[Any, Nothing, B] =
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
        def apply()(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          ZStream.fromZIO(-.await.exit *> *.fold(ZIO.unit)(_.acquire) *> +.release.unit).repeat(Schedule.forever)

        /**
          * linear replication guard w/ pace
          */
        def apply(pace: Duration)(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          apply()(-, *, +) zipLeft ZStream.tick(pace)

        /**
          * linear replication guard w/ code
          */
        def apply[T]()(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          apply()(-, *, +).tap(_ => exec(code))

        /**
          * linear replication guard w/ pace w/ code
          */
        def apply[T](pace: Duration)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          apply(pace)(-, *, +).tap(_ => exec(code))

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
      apply().repeat(Schedule.fromDuration(pace))

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

    private def s: ZStream[Any , Nothing, Seq[`()`]] = _s.filter(true)
    private def `s.head`: ZStream[Any , Nothing, Seq[`()`]] = _s.filter(false)

    extension (self: ZStream[Any, Nothing, (Seq[`()`], Promise[Nothing, Unit])])
      private def filter(r: Boolean): ZStream[Any, Nothing, Seq[`()`]] =
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

    private def s(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Seq[`()`]] =
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
          def apply(arity: Int)(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Seq[`()`]] =
            (ZStream.fromZIO(-.await.exit *> *.fold(ZIO.unit)(_.acquire)).repeat(Schedule.forever) *> self.`(ν)`(arity)).tap(_ => +.release).interruptWhen(p)

          /**
            * linear replication bound output guard w/ pace
            */
          def apply(arity: Int, pace: Duration)(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Seq[`()`]] =
            apply(arity)(-, *, +) zipLeft ZStream.tick(pace)

          /**
            * linear replication bound output guard w/ code
            */
          def apply[T](arity: Int)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Seq[`()`]] =
            apply(arity)(-, *, +).tap(_ => exec(code))

          /**
            * linear replication bound output guard w/ pace w/ code
            */
          def apply[T](arity: Int, pace: Duration)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Seq[`()`]] =
            apply(arity, pace)(-, *, +).tap(_ => exec(code))

        /**
          * linear constant replication output guard
          */
        def apply(_1: 1)(value: `()`*)(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          ZStream.fromZIO(-.await.exit *> *.fold(ZIO.unit)(_.acquire) *> Promise.make[Nothing, Unit].map(value -> _)).repeat(Schedule.forever).through1(h).tap(_ => +.release).interruptWhen(p)

        /**
          * linear constant replication output guard w/ pace
          */
        def apply(_2: 2)(pace: Duration, value: `()`*)(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          apply(1)(value*)(-, *, +) zipLeft ZStream.tick(pace)

        /**
          * linear constant replication output guard w/ code
          */
        def apply[T](_3: 3)(value: `()`*)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          apply(1)(value*)(-, *, +).tap(_ => exec(code))

        /**
          * linear constant replication output guard w/ pace w/ code
          */
        def apply[T](_4: 4)(pace: Duration, value: `()`*)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          apply(2)(pace, value*)(-, *, +).tap(_ => exec(code))

        object `(null)`:

          /**
            * linear `null` replication output guard
            */
          def apply(_arity: Int)(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            self.`(null)`(_arity)

          /**
            * linear `null` replication output guard w/ pace
            */
          def apply(_arity: Int, _pace: Duration)(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            apply(_arity)(-, *, +)

          /**
            * linear `null` replication output guard w/ code
            */
          def apply[T](_arity: Int)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            self.`(null)`[T](_arity)(code)

          /**
            * linear `null` replication output guard w/ pace w/ code
            */
          def apply[T](_arity: Int, _pace: Duration)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            apply[T](_arity)(code)(-, *, +)

        object `(*)`:

          /**
            * linear variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(value: => S*)(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(1)(value.map(_.asInstanceOf[`()`]*))(-, *, +)
            else
              apply[S](1)(value.map(ZIO.attempt)*)(-, *, +)

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(pace: Duration, value: => S*)(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(2)(pace, value.map(_.asInstanceOf[`()`]*))(-, *, +)
            else
              apply[S](2)(pace, value.map(ZIO.attempt)*)(-, *, +)

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(value: => S*)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(3)(value.map(_.asInstanceOf[`()`]*))(code)(-, *, +)
            else
              apply[S, T](3)(value.map(ZIO.attempt)*)(code)(-, *, +)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => S*)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(4)(pace, value.map(_.asInstanceOf[`()`]*))(code)(-, *, +)
            else
              apply[S, T](4)(pace, value.map(ZIO.attempt)*)(code)(-, *, +)

          /**
            * linear variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(value: => Task[S]*)(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              ZStream.fromZIO(ZIO.suspendSucceed(ZIO.collectAllSuccesses(value.map(_.asInstanceOf[Task[`()`]])))).flatMap(self.`(!)`.`(+)`(1)(_*)(-, *, +))
            else
              ZStream.fromZIO {
                for
                  _  <- -.await.exit
                  _  <- *.fold(ZIO.unit)(_.acquire)
                  it <- ZIO.collectAllSuccesses(value).flatMap { it => Promise.make[Nothing, Unit].map(it.map(new `()`(_)) -> _) }
                yield
                  it
              }.repeat(Schedule.forever).through1(h).tap(_ => +.release).interruptWhen(p)

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(pace: Duration, value: => Task[S]*)(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            apply[S](1)(value*)(-, *, +) zipLeft ZStream.tick(pace)

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(value: => Task[S]*)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            apply[S](1)(value*)(-, *, +).tap(_ => exec(code))

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => Task[S]*)(code: => Task[T])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            apply[S](2)(pace, value*)(-, *, +).tap(_ => exec(code))

        /**
          * linear replication input guard
          */
        def apply(_1: 1)()(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Seq[`()`]] =
          stop(s(-, *, +))

        /**
          * linear replication input guard w/ pace
          */
        def apply(_2: 2)(pace: Duration)(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Seq[`()`]] =
          stop(s(-, *, +) zipLeft ZStream.tick(pace))

        /**
          * linear replication input guard w/ code
          */
        def apply[T](_3: 3)()(code: Seq[T] => Task[Seq[T]])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Seq[`()`]] =
          stopWithCode[T](s(-, *, +))(code)

        /**
          * linear replication input guard w/ pace w/ code
          */
        def apply[T](_4: 4)(pace: Duration)(code: Seq[T] => Task[Seq[T]])(- : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO]): ZStream[Any, Nothing, Seq[`()`]] =
          stopWithCode[T](s(-, *, +) zipLeft ZStream.tick(pace))(code)

      object `(ν)`:

        /**
          * replication bound output guard
          */
        def apply(arity: Int): ZStream[Any, Nothing, Seq[`()`]] =
          τ.`(!)`() *> self.`(ν)`(arity)

        /**
          * replication bound output guard w/ code
          */
        def apply[T](arity: Int)(code: => Task[T]): ZStream[Any, Nothing, Seq[`()`]] =
          τ.`(!)`() *> self.`(ν)`[T](arity)(code)

        /**
          * replication bound output guard w/ pace
          */
        def apply(arity: Int, pace: Duration): ZStream[Any, Nothing, Seq[`()`]] =
          τ.`(!)`(pace) *> self.`(ν)`(arity)

        /**
          * replication bound output guard w/ pace w/ code
          */
        def apply[T](arity: Int, pace: Duration)(code: => Task[T]): ZStream[Any, Nothing, Seq[`()`]] =
          τ.`(!)`(pace) *> self.`(ν)`[T](arity)(code)

      /**
        * constant replication output guard
        */
      def apply(_1: 1)(value: `()`*): ZStream[Any, Nothing, Unit] =
        ZStream.fromZIO(Promise.make[Nothing, Unit].map(value -> _)).repeat(Schedule.forever).through1(h).interruptWhen(p)

      /**
        * constant replication output guard w/ pace
        */
      def apply(_2: 2)(pace: Duration, value: `()`*): ZStream[Any, Nothing, Unit] =
        apply(1)(value*) zipLeft ZStream.tick(pace)

      /**
        * constant replication output guard w/ code
        */
      def apply[T](_3: 3)(value: `()`*)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply(1)(value*).tap(_ => exec(code))

      /**
        * constant replication output guard w/ pace w/ code
        */
      def apply[T](_4: 4)(pace: Duration, value: `()`*)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply(2)(pace, value*).tap(_ => exec(code))

      object `(null)`:

        /**
          * `null` replication output guard
          */
        def apply(_arity: Int): ZStream[Any, Nothing, Unit] =
          self.`(null)`(_arity)

        /**
          * `null` replication output guard w/ pace
          */
        def apply(_arity: Int, _pace: Duration): ZStream[Any, Nothing, Unit] =
          apply(_arity)

        /**
          * `null` replication output guard w/ code
          */
        def apply[T](_arity: Int)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
          self.`(null)`[T](_arity)(code)

        /**
          * `null` replication output guard w/ pace w/ code
          */
        def apply[T](_arity: Int, _pace: Duration)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
          apply[T](_arity)(code)

      object `(*)`:

        /**
          * variable replication output guard
          */
        def apply[S: ClassTag](_1: 1)(value: => S*)(using DummyImplicit): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(1)(value.map(_.asInstanceOf[`()`]*))
          else
            apply[S](1)(value.map(ZIO.attempt)*)

        /**
          * variable replication output guard w/ pace
          */
        def apply[S: ClassTag](_2: 2)(pace: Duration, value: => S*)(using DummyImplicit): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(2)(pace, value.map(_.asInstanceOf[`()`]*))
          else
            apply[S](2)(pace, value.map(ZIO.attempt)*)

        /**
          * variable replication output guard w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(value: => S*)(code: => Task[T])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(3)(value.map(_.asInstanceOf[`()`]*))(code)
          else
            apply[S](1)(value*).tap(_ => exec(code))

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => S*)(code: => Task[T])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(4)(pace, value.map(_.asInstanceOf[`()`]*))(code)
          else
            apply[S](2)(pace, value*).tap(_ => exec(code))

        /**
          * variable replication output guard
          */
        def apply[S: ClassTag](_1: 1)(value: => Task[S]*): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            ZStream.fromZIO(ZIO.suspendSucceed(ZIO.collectAllSuccesses(value.map(_.asInstanceOf[Task[`()`]])))).flatMap(self.`(!)`(1)(_*))
          else
            ZStream.fromZIO(ZIO.suspendSucceed(ZIO.collectAllSuccesses(value).flatMap { it => Promise.make[Nothing, Unit].map(it.map(new `()`(_)) -> _) })).repeat(Schedule.forever).through1(h).interruptWhen(p)

        /**
          * variable replication output guard w/ pace
          */
        def apply[S: ClassTag](_2: 2)(pace: Duration, value: => Task[S]*): ZStream[Any, Nothing, Unit] =
          apply[S](1)(value*) zipLeft ZStream.tick(pace)

        /**
          * variable replication output guard w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(value: => Task[S]*)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
          apply[S](1)(value*).tap(_ => exec(code))

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => Task[S]*)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
          apply[S](2)(pace, value*).tap(_ => exec(code))

      /**
        * replication input guard
        */
      def apply(_1: 1)(): ZStream[Any, Nothing, Seq[`()`]] =
        stop(s)

      /**
        * replication input guard w/ pace
        */
      def apply(_2: 2)(pace: Duration): ZStream[Any, Nothing, Seq[`()`]] =
        stop(s zipLeft ZStream.tick(pace))

      /**
        * replication input guard w/ code
        */
      def apply[T](_3: 3)()(code: Seq[T] => Task[Seq[T]]): ZStream[Any, Nothing, Seq[`()`]] =
        stopWithCode[T](s)(code)

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](_4: 4)(pace: Duration)(code: Seq[T] => Task[Seq[T]]): ZStream[Any, Nothing, Seq[`()`]] =
        stopWithCode[T](s zipLeft ZStream.tick(pace))(code)

    object `(ν)`:

      /**
        * bound output prefix
        */
      def apply(arity: Int): ZStream[Any, Nothing, Seq[`()`]] =
        ( for
            names   <- ZStream.fromZIO(ZIO.collectAllSuccesses(Seq.fill(arity)(Π.ν.make)))
            promise <- ZStream.fromZIO(Promise.make[Nothing, Unit])
            _       <- ZStream.succeed(names -> promise).through1(h)
          yield
            names
        ).interruptWhen(p)

      /**
        * bound output prefix w/ pace
        */
      def apply(arity: Int, pace: Duration): ZStream[Any, Nothing, Seq[`()`]] =
        apply(arity) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * bound output prefix w/ code
        */
      def apply[T](arity: Int)(code: => Task[T]): ZStream[Any, Nothing, Seq[`()`]] =
        apply(arity).tap(_ => exec(code))

      /**
        * bound output prefix w/ pace w/ code
        */
      def apply[T](arity: Int, pace: Duration)(code: => Task[T]): ZStream[Any, Nothing, Seq[`()`]] =
        apply(arity, pace).tap(_ => exec(code))

    /**
      * constant output prefix
      */
    def apply(_1: 1)(value: `()`*): ZStream[Any, Nothing, Unit] =
      ZStream.fromZIO(Promise.make[Nothing, Unit].map(value -> _)).through1(h).interruptWhen(p)

    /**
      * constant output prefix w/ pace
      */
    def apply(_2: 2)(pace: Duration, value: `()`*): ZStream[Any, Nothing, Unit] =
      apply(1)(value*) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

    /**
      * constant output prefix w/ code
      */
    def apply[T](_3: 3)(value: `()`*)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
      apply(1)(value*).tap(_ => exec(code))

    /**
      * constant output prefix w/ pace w/ code
      */
    def apply[T](_4: 4)(pace: Duration, value: `()`*)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
      apply(2)(pace, value*).tap(_ => exec(code))

    object `(null)`:

      /**
        * `null` output prefix
        */
      def apply(_arity: Int): ZStream[Any, Nothing, Unit] =
        ZStream.fromZIO(p.succeed(()).unit).interruptWhen(p)

      /**
        * `null` output prefix w/ pace
        */
      def apply(_arity: Int, _pace: Duration): ZStream[Any, Nothing, Unit] =
        apply(_arity)

      /**
        * `null` output prefix w/ code
        */
      def apply[T](_arity: Int)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply(_arity).tap(_ => exec(code))
      /**
        * `null` output prefix w/ pace w/ code
        */
      def apply[T](_arity: Int, _pace: Duration)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply[T](_arity)(code)

    object `(*)`:

      /**
        * variable output prefix
        */
      def apply[S: ClassTag](_1: 1)(value: => S*)(using DummyImplicit): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(1)(value.map(_.asInstanceOf[`()`]*))
        else
          apply[S](1)(value.map(ZIO.attempt)*)

      /**
        * variable output prefix w/ pace
        */
      def apply[S: ClassTag](_2: 2)(pace: Duration, value: => S*)(using DummyImplicit): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(2)(pace, value.map(_.asInstanceOf[`()`]*))
        else
          apply[S](1)(value*) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * variable output prefix w/ code
        */
      def apply[S: ClassTag, T](_3: 3)(value: => S*)(code: => Task[T])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(3)(value.map(_.asInstanceOf[`()`]*))(code)
        else
          apply[S](1)(value*).tap(_ => exec(code))

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => S*)(code: => Task[T])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(4)(pace, value.map(_.asInstanceOf[`()`]*))(code)
        else
          apply[S](2)(pace, value*).tap(_ => exec(code))

      /**
        * variable output prefix
        */
      def apply[S: ClassTag](_1: 1)(value: => Task[S]*): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          ZStream.fromZIO(ZIO.suspendSucceed(ZIO.collectAllSuccesses(value.map(_.asInstanceOf[Task[`()`]])))).flatMap(self(1)(_*))
        else
          ZStream.fromZIO(ZIO.suspendSucceed(ZIO.collectAllSuccesses(value).flatMap { it => Promise.make[Nothing, Unit].map(it.map(new `()`(_)) -> _) })).through1(h).interruptWhen(p)

      /**
        * variable output prefix w/ pace
        */
      def apply[S: ClassTag](_2: 2)(pace: Duration, value: => Task[S]*): ZStream[Any, Nothing, Unit] =
        apply[S](1)(value*) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * variable output prefix w/ code
        */
      def apply[S: ClassTag, T](_3: 3)(value: => Task[S]*)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply[S](1)(value*).tap(_ => exec(code))

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => Task[S]*)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply[S](2)(pace, value*).tap(_ => exec(code))

    /**
      * input prefix
      */
    def apply(_1: 1)(): ZStream[Any, Nothing, Seq[`()`]] =
      stop(`s.head`)

    /**
      * input prefix w/ pace
      */
    def apply(_2: 2)(pace: Duration): ZStream[Any, Nothing, Seq[`()`]] =
      stop(`s.head` <* ZStream.unit.repeat(Schedule.fromDuration(pace)))

    /**
      * input prefix w/ code
      */
    def apply[T](_3: 3)()(code: Seq[T] => Task[Seq[T]]): ZStream[Any, Nothing, Seq[`()`]] =
      stopWithCode[T](`s.head`)(code)

    /**
      * input prefix w/ pace w/ code
      */
    def apply[T](_4: 4)(pace: Duration)(code: Seq[T] => Task[Seq[T]]): ZStream[Any, Nothing, Seq[`()`]] =
      stopWithCode[T](`s.head` <* ZStream.unit.repeat(Schedule.fromDuration(pace)))(code)

    private def stop(s: ZStream[Any, Nothing, Seq[`()`]]): ZStream[Any, Nothing, Seq[`()`]] =
      s.tap(p.succeed(()).when(_)).interruptWhen(p)

    private def stopWithCode[T](s: ZStream[Any, Nothing, Seq[`()`]])(code: Seq[T] => Task[Seq[T]]): ZStream[Any, Nothing, Seq[`()`]] =
      stop(s.map(_.map(_.`()`[T])).mapZIO((code andThen exec)(_).map(_.map(new `()`(_)))))

    override def toString: String = if name == null then "null" else name.toString

  }


  private object `()`:

    given Conversion[Seq[`()`], Boolean] = !_.head


  private object `Π-magic`:

    case class ><(hub: Hub[(Seq[`()`], Promise[Nothing, Unit])],
                  stop: Promise[Nothing, Unit],
                  limit: Semaphore[UIO])

    extension [O](self: ZStream[Any, Nothing, O])
      def through1(hub: Hub[O])
                  (using await: UIO[Unit]): ZStream[Any, Nothing, Unit] =
        self.mapZIO(await *> hub.publish(_)).takeWhile(identity).as(())
