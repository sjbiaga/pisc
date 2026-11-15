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
          * modulo replication guard
          */
        def apply()(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          ZStream.fromZIO(-.acquire *> -.release).repeat(Schedule.forever)

        /**
          * modulo replication guard w/ pace
          */
        def apply(pace: Duration)(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          ZStream.fromZIO(-.acquire *> ZIO.sleep(pace) *> -.release).repeat(Schedule.forever)

        /**
          * modulo replication guard w/ code
          */
        def apply[T]()(code: => Task[T])(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          ZStream.fromZIO(-.acquire *> exec(code) *> -.release).repeat(Schedule.forever)

        /**
          * modulo replication guard w/ pace w/ code
          */
        def apply[T](pace: Duration)(code: => Task[T])(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          ZStream.fromZIO(-.acquire *> exec(code) *> ZIO.sleep(pace) *> -.release).repeat(Schedule.forever)

      /**
        * replication guard
        */
      def apply(): ZStream[Any, Nothing, Unit] =
        ZStream.unit.repeat(Schedule.forever)

      /**
        * replication guard w/ pace
        */
      def apply(pace: Duration): ZStream[Any, Nothing, Unit] =
        ZStream.unit.repeat(Schedule.forever) <* ZStream.fromZIO(ZIO.sleep(pace))

      /**
        * replication guard w/ code
        */
      def apply[T]()(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply().tap(_ => exec(code))

      /**
        * replication guard w/ pace w/ code
        */
      def apply[T](pace: Duration)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply[T]()(code) <* ZStream.fromZIO(ZIO.sleep(pace))

    /**
      * prefix
      */
    def apply(): ZStream[Any, Nothing, Unit] =
      ZStream.unit

    /**
      * prefix w/ pace
      */
    def apply(pace: Duration): ZStream[Any, Nothing, Unit] =
      apply() <* ZStream.fromZIO(ZIO.sleep(pace))

    /**
      * prefix w/ code
      */
    def apply[T]()(code: => Task[T]): ZStream[Any, Nothing, Unit] =
      apply().tap(_ => exec(code))

    /**
      * prefix w/ pace w/ code
      */
    def apply[T](pace: Duration)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
      apply[T]()(code) <* ZStream.fromZIO(ZIO.sleep(pace))


  /**
    * names (hubs) and values
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
            * modulo replication bound output guard
            */
          def apply()(- : Semaphore[UIO]): ZStream[Any, Nothing, `()`] =
            (ZStream.fromZIO(-.acquire).repeat(Schedule.forever) *> self.`(ν)`()).tap(_ => -.release)

          /**
            * modulo replication bound output guard w/ pace
            */
          def apply(pace: Duration)(- : Semaphore[UIO]): ZStream[Any, Nothing, `()`] =
            (ZStream.fromZIO(-.acquire).repeat(Schedule.forever) *> self.`(ν)`(pace)).tap(_ => -.release)

          /**
            * modulo replication bound output guard w/ code
            */
          def apply[T]()(code: => Task[T])(- : Semaphore[UIO]): ZStream[Any, Nothing, `()`] =
            (ZStream.fromZIO(-.acquire).repeat(Schedule.forever) *> self.`(ν)`[T]()(code)).tap(_ => -.release)

          /**
            * modulo replication bound output guard w/ pace w/ code
            */
          def apply[T](pace: Duration)(code: => Task[T])(- : Semaphore[UIO]): ZStream[Any, Nothing, `()`] =
            (ZStream.fromZIO(-.acquire).repeat(Schedule.forever) *> self.`(ν)`[T](pace)(code)).tap(_ => -.release)

        /**
          * modulo constant replication output guard
          */
        def apply(value: `()`)(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          ZStream.fromZIO(-.acquire *> Promise.make[Nothing, Unit].map(value -> _)).repeat(Schedule.forever).through1(h).tap(_ => -.release).interruptWhen(p)

        /**
          * modulo constant replication output guard w/ pace
          */
        def apply(pace: Duration, value: `()`)(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          ZStream.fromZIO(-.acquire *> Promise.make[Nothing, Unit].map(value -> _)).repeat(Schedule.forever).through1(h).tap(_ => ZIO.sleep(pace) *> -.release).interruptWhen(p)

        /**
          * modulo constant replication output guard w/ code
          */
        def apply[T](value: `()`)(code: => Task[T])(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          ZStream.fromZIO(-.acquire *> Promise.make[Nothing, Unit].map(value -> _)).repeat(Schedule.forever).through1(h).tap(_ => exec(code) *> -.release).interruptWhen(p)

        /**
          * modulo constant replication output guard w/ pace w/ code
          */
        def apply[T](pace: Duration, value: `()`)(code: => Task[T])(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          ZStream.fromZIO(-.acquire *> Promise.make[Nothing, Unit].map(value -> _)).repeat(Schedule.forever).through1(h).tap(_ => exec(code) *> ZIO.sleep(pace) *> -.release).interruptWhen(p)

        object `(null)`:

          /**
            * modulo `null` replication output guard
            */
          def apply()(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            self.`(null)`()

          /**
            * modulo `null` replication output guard w/ pace
            */
          def apply(_pace: Duration)(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            apply()(-)

          /**
            * modulo `null` replication output guard w/ code
            */
          def apply[T]()(_code: => Task[T])(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            apply()(-)

          /**
            * modulo `null` replication output guard w/ pace w/ code
            */
          def apply[T](_pace: Duration)(_code: => Task[T])(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            apply()(-)

        object `(*)`:

          /**
            * modulo variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(value: => S)(- : Semaphore[UIO])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(value.asInstanceOf[`()`])(-)
            else
              apply[S](1)(ZIO.attempt(value))(-)

          /**
            * modulo variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(pace: Duration, value: => S)(- : Semaphore[UIO])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(pace, value.asInstanceOf[`()`])(-)
            else
              apply[S](2)(pace, ZIO.attempt(value))(-)

          /**
            * modulo variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(value: => S)(code: => Task[T])(- : Semaphore[UIO])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(value.asInstanceOf[`()`])(code)(-)
            else
              apply[S, T](3)(ZIO.attempt(value))(code)(-)

          /**
            * modulo variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => S)(code: => Task[T])(- : Semaphore[UIO])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(pace, value.asInstanceOf[`()`])(code)(-)
            else
              apply[S, T](4)(pace, ZIO.attempt(value))(code)(-)

          /**
            * modulo variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(value: => Task[S])(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              ZStream.fromZIO(ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]]: UIO[`()`])).flatMap(self.`(!)`.`(+)`(_)(-))
            else
              ZStream.fromZIO {
                for
                  _  <- -.acquire
                  it <- (value: UIO[S]).flatMap { it => Promise.make[Nothing, Unit].map(new `()`(it) -> _) }
                yield
                  it
              }.repeat(Schedule.forever).through1(h).tap(_ => -.release).interruptWhen(p)

          /**
            * modulo variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(pace: Duration, value: => Task[S])(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              ZStream.fromZIO(ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]]: UIO[`()`])).flatMap(self.`(!)`.`(+)`(pace, _)(-))
            else
              ZStream.fromZIO {
                for
                  _  <- -.acquire
                  it <- (value: UIO[S]).flatMap { it => Promise.make[Nothing, Unit].map(new `()`(it) -> _) }
                yield
                  it
              }.repeat(Schedule.forever).through1(h).tap(_ => ZIO.sleep(pace) *> -.release).interruptWhen(p)

          /**
            * modulo variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(value: => Task[S])(code: => Task[T])(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              ZStream.fromZIO(ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]]: UIO[`()`])).flatMap(self.`(!)`.`(+)`(_)(code)(-))
            else
              ZStream.fromZIO {
                for
                  _  <- -.acquire
                  it <- (value: UIO[S]).flatMap { it => Promise.make[Nothing, Unit].map(new `()`(it) -> _) }
                yield
                  it
              }.repeat(Schedule.forever).through1(h).tap(_ => exec(code) *> -.release).interruptWhen(p)

          /**
            * modulo variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => Task[S])(code: => Task[T])(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              ZStream.fromZIO(ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]]: UIO[`()`])).flatMap(self.`(!)`.`(+)`(pace, _)(code)(-))
            else
              ZStream.fromZIO {
                for
                  _  <- -.acquire
                  it <- (value: UIO[S]).flatMap { it => Promise.make[Nothing, Unit].map(new `()`(it) -> _) }
                yield
                  it
              }.repeat(Schedule.forever).through1(h).tap(_ => exec(code) *> ZIO.sleep(pace) *> -.release).interruptWhen(p)

        /**
          * modulo replication input guard
          */
        def apply()(- : Semaphore[UIO]): ZStream[Any, Nothing, `()`] =
          stop((ZStream.fromZIO(-.acquire).repeat(Schedule.forever) zipRight s).tap(_ => -.release))

        /**
          * modulo replication input guard w/ pace
          */
        def apply(pace: Duration)(- : Semaphore[UIO]): ZStream[Any, Nothing, `()`] =
          stop((ZStream.fromZIO(-.acquire).repeat(Schedule.forever) zipRight s).tap(_ => ZIO.sleep(pace) *> -.release))

        /**
          * modulo replication input guard w/ code
          */
        def apply[T]()(code: T => Task[T])(- : Semaphore[UIO]): ZStream[Any, Nothing, `()`] =
          stop((ZStream.fromZIO(-.acquire).repeat(Schedule.forever) zipRight s).map(_.`()`[T]).mapZIO((code andThen exec)(_).map(new `()`(_))).tap(_ => -.release))

        /**
          * modulo replication input guard w/ pace w/ code
          */
        def apply[T](pace: Duration)(code: T => Task[T])(- : Semaphore[UIO]): ZStream[Any, Nothing, `()`] =
          stop((ZStream.fromZIO(-.acquire).repeat(Schedule.forever) zipRight s).map(_.`()`[T]).mapZIO((code andThen exec)(_).map(new `()`(_))).tap(_ => ZIO.sleep(pace) *> -.release))

      object `(ν)`:

        /**
          * replication bound output guard
          */
        def apply(): ZStream[Any, Nothing, `()`] =
          ZStream.unit.repeat(Schedule.forever) *> self.`(ν)`()

        /**
          * replication bound output guard w/ pace
          */
        def apply(pace: Duration): ZStream[Any, Nothing, `()`] =
          ZStream.unit.repeat(Schedule.forever) *> self.`(ν)`(pace)

        /**
          * replication bound output guard w/ code
          */
        def apply[T]()(code: => Task[T]): ZStream[Any, Nothing, `()`] =
          ZStream.unit.repeat(Schedule.forever) *> self.`(ν)`[T]()(code)

        /**
          * replication bound output guard w/ pace w/ code
          */
        def apply[T](pace: Duration)(code: => Task[T]): ZStream[Any, Nothing, `()`] =
          ZStream.unit.repeat(Schedule.forever) *> self.`(ν)`[T](pace)(code)

      /**
        * constant replication output guard
        */
      def apply(value: `()`): ZStream[Any, Nothing, Unit] =
        ZStream.fromZIO(Promise.make[Nothing, Unit].map(value -> _)).repeat(Schedule.forever).through1(h).interruptWhen(p)

      /**
        * constant replication output guard w/ pace
        */
      def apply(pace: Duration, value: `()`): ZStream[Any, Nothing, Unit] =
        apply(value) <* ZStream.fromZIO(ZIO.sleep(pace))

      /**
        * constant replication output guard w/ code
        */
      def apply[T](value: `()`)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply(value).tap(_ => exec(code))

      /**
        * constant replication output guard w/ pace w/ code
        */
      def apply[T](pace: Duration, value: `()`)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply[T](value)(code) <* ZStream.fromZIO(ZIO.sleep(pace))

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
        def apply[T]()(_code: => Task[T]): ZStream[Any, Nothing, Unit] =
          apply()

        /**
          * `null` replication output guard w/ pace w/ code
          */
        def apply[T](_pace: Duration)(_code: => Task[T]): ZStream[Any, Nothing, Unit] =
          apply()

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
            apply[S, T](3)(ZIO.attempt(value))(code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => S)(code: => Task[T])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(pace, value.asInstanceOf[`()`])(code)
          else
            apply[S, T](4)(pace, ZIO.attempt(value))(code)

        /**
          * variable replication output guard
          */
        def apply[S: ClassTag](_1: 1)(value: => Task[S]): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            ZStream.fromZIO(ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]]: UIO[`()`])).flatMap(self.`(!)`(_))
          else
            ZStream.fromZIO(ZIO.suspendSucceed((value: UIO[S]).flatMap { it => Promise.make[Nothing, Unit].map(new `()`(it) -> _) })).repeat(Schedule.forever).through1(h).interruptWhen(p)

        /**
          * variable replication output guard w/ pace
          */
        def apply[S: ClassTag](_2: 2)(pace: Duration, value: => Task[S]): ZStream[Any, Nothing, Unit] =
          apply[S](1)(value) <* ZStream.fromZIO(ZIO.sleep(pace))

        /**
          * variable replication output guard w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(value: => Task[S])(code: => Task[T]): ZStream[Any, Nothing, Unit] =
          apply[S](1)(value).tap(_ => exec(code))

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => Task[S])(code: => Task[T]): ZStream[Any, Nothing, Unit] =
          apply[S, T](3)(value)(code) <* ZStream.fromZIO(ZIO.sleep(pace))

      /**
        * replication input guard
        */
      def apply(): ZStream[Any, Nothing, `()`] =
        stop(s)

      /**
        * replication input guard w/ pace
        */
      def apply(pace: Duration): ZStream[Any, Nothing, `()`] =
        apply() <* ZStream.fromZIO(ZIO.sleep(pace))

      /**
        * replication input guard w/ code
        */
      def apply[T]()(code: T => Task[T]): ZStream[Any, Nothing, `()`] =
        stop(s.map(_.`()`[T]).mapZIO((code andThen exec)(_).map(new `()`(_))))

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](pace: Duration)(code: T => Task[T]): ZStream[Any, Nothing, `()`] =
        apply[T]()(code) <* ZStream.fromZIO(ZIO.sleep(pace))

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
        apply() <* ZStream.fromZIO(ZIO.sleep(pace))

      /**
        * bound output prefix w/ code
        */
      def apply[T]()(code: => Task[T]): ZStream[Any, Nothing, `()`] =
        apply().tap(_ => exec(code))

      /**
        * bound output prefix w/ pace w/ code
        */
      def apply[T](pace: Duration)(code: => Task[T]): ZStream[Any, Nothing, `()`] =
        apply[T]()(code) <* ZStream.fromZIO(ZIO.sleep(pace))

    /**
      * constant output prefix
      */
    def apply(value: `()`): ZStream[Any, Nothing, Unit] =
      ZStream.fromZIO(Promise.make[Nothing, Unit].map(value -> _)).through1(h).interruptWhen(p)

    /**
      * constant output prefix w/ pace
      */
    def apply(pace: Duration, value: `()`): ZStream[Any, Nothing, Unit] =
      apply(value) <* ZStream.fromZIO(ZIO.sleep(pace))

    /**
      * constant output prefix w/ code
      */
    def apply[T](value: `()`)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
      apply(value).tap(_ => exec(code))

    /**
      * constant output prefix w/ pace w/ code
      */
    def apply[T](pace: Duration, value: `()`)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
      apply[T](value)(code) <* ZStream.fromZIO(ZIO.sleep(pace))

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
      def apply[T]()(_code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply()

      /**
        * `null` output prefix w/ pace w/ code
        */
      def apply[T](_pace: Duration)(_code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply()

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
          apply[S](2)(pace, ZIO.attempt(value))

      /**
        * variable output prefix w/ code
        */
      def apply[S: ClassTag, T](_3: 3)(value: => S)(code: => Task[T])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(value.asInstanceOf[`()`])(code)
        else
          apply[S, T](3)(ZIO.attempt(value))(code)

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => S)(code: => Task[T])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(pace, value.asInstanceOf[`()`])(code)
        else
          apply[S, T](4)(pace, ZIO.attempt(value))(code)

      /**
        * variable output prefix
        */
      def apply[S: ClassTag](_1: 1)(value: => Task[S]): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          ZStream.fromZIO(ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]]: UIO[`()`])).flatMap(self(_))
        else
          ZStream.fromZIO(ZIO.suspendSucceed((value: UIO[S]).flatMap { it => Promise.make[Nothing, Unit].map(new `()`(it) -> _) })).through1(h).interruptWhen(p)

      /**
        * variable output prefix w/ pace
        */
      def apply[S: ClassTag](_2: 2)(pace: Duration, value: => Task[S]): ZStream[Any, Nothing, Unit] =
        apply[S](1)(value) <* ZStream.fromZIO(ZIO.sleep(pace))

      /**
        * variable output prefix w/ code
        */
      def apply[S: ClassTag, T](_3: 3)(value: => Task[S])(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply[S](1)(value).tap(_ => exec(code))

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => Task[S])(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply[S, T](3)(value)(code) <* ZStream.fromZIO(ZIO.sleep(pace))

    /**
      * input prefix
      */
    def apply(): ZStream[Any, Nothing, `()`] =
      stop(`s.head`)

    /**
      * input prefix w/ pace
      */
    def apply(pace: Duration): ZStream[Any, Nothing, `()`] =
      apply() <* ZStream.fromZIO(ZIO.sleep(pace))

    /**
      * input prefix w/ code
      */
    def apply[T]()(code: T => Task[T]): ZStream[Any, Nothing, `()`] =
      stop(`s.head`.map(_.`()`[T]).mapZIO((code andThen exec)(_).map(new `()`(_))))

    /**
      * input prefix w/ pace w/ code
      */
    def apply[T](pace: Duration)(code: T => Task[T]): ZStream[Any, Nothing, `()`] =
      apply[T]()(code) <* ZStream.fromZIO(ZIO.sleep(pace))

    private def stop(s: ZStream[Any, Nothing, `()`]): ZStream[Any, Nothing, `()`] =
      s.tap(p.succeed(()).when(_)).interruptWhen(p)

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
