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
        limit <- Semaphore[UIO](0)
      yield
        ><(hub, limit)

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
          def apply(arity: Int)(- : Semaphore[UIO]): ZStream[Any, Nothing, Seq[`()`]] =
            (ZStream.fromZIO(-.acquire).repeat(Schedule.forever) *> self.`(ν)`(arity)).tap(_ => -.release)

          /**
            * modulo replication bound output guard w/ pace
            */
          def apply(arity: Int, pace: Duration)(- : Semaphore[UIO]): ZStream[Any, Nothing, Seq[`()`]] =
            (ZStream.fromZIO(-.acquire).repeat(Schedule.forever) *> self.`(ν)`(arity, pace)).tap(_ => -.release)

          /**
            * modulo replication bound output guard w/ code
            */
          def apply[T](arity: Int)(code: => Task[T])(- : Semaphore[UIO]): ZStream[Any, Nothing, Seq[`()`]] =
            (ZStream.fromZIO(-.acquire).repeat(Schedule.forever) *> self.`(ν)`[T](arity)(code)).tap(_ => -.release)

          /**
            * modulo replication bound output guard w/ pace w/ code
            */
          def apply[T](arity: Int, pace: Duration)(code: => Task[T])(- : Semaphore[UIO]): ZStream[Any, Nothing, Seq[`()`]] =
            (ZStream.fromZIO(-.acquire).repeat(Schedule.forever) *> self.`(ν)`[T](arity, pace)(code)).tap(_ => -.release)

        /**
          * modulo constant replication output guard
          */
        def apply(_1: 1)(value: `()`*)(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          ZStream.fromZIO(-.acquire *> Promise.make[Nothing, Unit].map(value -> _)).repeat(Schedule.forever).through1(h).tap(_ => -.release)

        /**
          * modulo constant replication output guard w/ pace
          */
        def apply(_2: 2)(pace: Duration, value: `()`*)(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          ZStream.fromZIO(-.acquire *> Promise.make[Nothing, Unit].map(value -> _)).repeat(Schedule.forever).through1(h).tap(_ => ZIO.sleep(pace) *> -.release)

        /**
          * modulo constant replication output guard w/ code
          */
        def apply[T](_3: 3)(value: `()`*)(code: => Task[T])(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          ZStream.fromZIO(-.acquire *> Promise.make[Nothing, Unit].map(value -> _)).repeat(Schedule.forever).through1(h).tap(_ => exec(code) *> -.release)

        /**
          * modulo constant replication output guard w/ pace w/ code
          */
        def apply[T](_4: 4)(pace: Duration, value: `()`*)(code: => Task[T])(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
          ZStream.fromZIO(-.acquire *> Promise.make[Nothing, Unit].map(value -> _)).repeat(Schedule.forever).through1(h).tap(_ => exec(code) *> ZIO.sleep(pace) *> -.release)

        object `(null)`:

          /**
            * modulo `null` replication output guard
            */
          def apply(arity: Int)(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            self.`(!)`.`(+)`.apply(1)(Seq.fill(arity)(new `()`(null))*)(-)

          /**
            * modulo `null` replication output guard w/ pace
            */
          def apply(arity: Int, pace: Duration)(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            self.`(!)`.`(+)`.apply(2)(pace, Seq.fill(arity)(new `()`(null))*)(-)

          /**
            * modulo `null` replication output guard w/ code
            */
          def apply[T](arity: Int)(code: => Task[T])(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            self.`(!)`.`(+)`.apply[T](3)(Seq.fill(arity)(new `()`(null))*)(code)(-)

          /**
            * modulo `null` replication output guard w/ pace w/ code
            */
          def apply[T](arity: Int, pace: Duration)(code: => Task[T])(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            self.`(!)`.`(+)`.apply[T](4)(pace, Seq.fill(arity)(new `()`(null))*)(code)(-)

        object `(*)`:

          /**
            * modulo variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(value: => S*)(- : Semaphore[UIO])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(1)(value.map(_.asInstanceOf[`()`]*))(-)
            else
              ZStream.suspend(apply[S](1)(value.map(ZIO.succeed)*)(-))

          /**
            * modulo variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(pace: Duration, value: => S*)(- : Semaphore[UIO])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(2)(pace, value.map(_.asInstanceOf[`()`]*))(-)
            else
              ZStream.suspend(apply[S](2)(pace, value.map(ZIO.succeed)*)(-))

          /**
            * modulo variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(value: => S*)(code: => Task[T])(- : Semaphore[UIO])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(3)(value.map(_.asInstanceOf[`()`]*))(code)(-)
            else
              ZStream.suspend(apply[S, T](3)(value.map(ZIO.succeed)*)(code)(-))

          /**
            * modulo variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => S*)(code: => Task[T])(- : Semaphore[UIO])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(4)(pace, value.map(_.asInstanceOf[`()`]*))(code)(-)
            else
              ZStream.suspend(apply[S, T](4)(pace, value.map(ZIO.succeed)*)(code)(-))

          /**
            * modulo variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(value: => Task[S]*)(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              ZStream.fromZIO(ZIO.suspendSucceed(ZIO.collectAllSuccesses(value.map(_.asInstanceOf[Task[`()`]])))).flatMap(self.`(!)`.`(+)`(1)(_*)(-))
            else
              ZStream.fromZIO {
                for
                  _  <- -.acquire
                  it <- ZIO.collectAllSuccesses(value).flatMap { it => Promise.make[Nothing, Unit].map(it.map(new `()`(_)) -> _) }
                yield
                  it
              }.repeat(Schedule.forever).through1(h).tap(_ => -.release)

          /**
            * modulo variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(pace: Duration, value: => Task[S]*)(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              ZStream.fromZIO(ZIO.suspendSucceed(ZIO.collectAllSuccesses(value.map(_.asInstanceOf[Task[`()`]])))).flatMap(self.`(!)`.`(+)`(2)(pace, _*)(-))
            else
              ZStream.fromZIO {
                for
                  _  <- -.acquire
                  it <- ZIO.collectAllSuccesses(value).flatMap { it => Promise.make[Nothing, Unit].map(it.map(new `()`(_)) -> _) }
                yield
                  it
              }.repeat(Schedule.forever).through1(h).tap(_ => ZIO.sleep(pace) *> -.release)

          /**
            * modulo variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(value: => Task[S]*)(code: => Task[T])(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              ZStream.fromZIO(ZIO.suspendSucceed(ZIO.collectAllSuccesses(value.map(_.asInstanceOf[Task[`()`]])))).flatMap(self.`(!)`.`(+)`(3)(_*)(code)(-))
            else
              ZStream.fromZIO {
                for
                  _  <- -.acquire
                  it <- ZIO.collectAllSuccesses(value).flatMap { it => Promise.make[Nothing, Unit].map(it.map(new `()`(_)) -> _) }
                yield
                  it
              }.repeat(Schedule.forever).through1(h).tap(_ => exec(code) *> -.release)

          /**
            * modulo variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => Task[S]*)(code: => Task[T])(- : Semaphore[UIO]): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              ZStream.fromZIO(ZIO.suspendSucceed(ZIO.collectAllSuccesses(value.map(_.asInstanceOf[Task[`()`]])))).flatMap(self.`(!)`.`(+)`(4)(pace, _*)(code)(-))
            else
              ZStream.fromZIO {
                for
                  _  <- -.acquire
                  it <- ZIO.collectAllSuccesses(value).flatMap { it => Promise.make[Nothing, Unit].map(it.map(new `()`(_)) -> _) }
                yield
                  it
              }.repeat(Schedule.forever).through1(h).tap(_ => exec(code) *> ZIO.sleep(pace) *> -.release)

        /**
          * modulo replication input guard
          */
        def apply(_1: 1)()(- : Semaphore[UIO]): ZStream[Any, Nothing, Seq[`()`]] =
          (ZStream.fromZIO(-.acquire).repeat(Schedule.forever) zipRight s).tap(_ => -.release)

        /**
          * modulo replication input guard w/ pace
          */
        def apply(_2: 2)(pace: Duration)(- : Semaphore[UIO]): ZStream[Any, Nothing, Seq[`()`]] =
          (ZStream.fromZIO(-.acquire).repeat(Schedule.forever) zipRight s).tap(_ => ZIO.sleep(pace) *> -.release)

        /**
          * modulo replication input guard w/ code
          */
        def apply[T](_3: 3)()(code: Seq[T] => Task[Seq[T]])(- : Semaphore[UIO]): ZStream[Any, Nothing, Seq[`()`]] =
          (ZStream.fromZIO(-.acquire).repeat(Schedule.forever) zipRight s).map(_.map(_.`()`[T])).mapZIO((code andThen exec)(_).map(_.map(new `()`(_)))).tap(_ => -.release)

        /**
          * modulo replication input guard w/ pace w/ code
          */
        def apply[T](_4: 4)(pace: Duration)(code: Seq[T] => Task[Seq[T]])(- : Semaphore[UIO]): ZStream[Any, Nothing, Seq[`()`]] =
          (ZStream.fromZIO(-.acquire).repeat(Schedule.forever) zipRight s).map(_.map(_.`()`[T])).mapZIO((code andThen exec)(_).map(_.map(new `()`(_)))).tap(_ => ZIO.sleep(pace) *> -.release)

      object `(ν)`:

        /**
          * replication bound output guard
          */
        def apply(arity: Int): ZStream[Any, Nothing, Seq[`()`]] =
          ZStream.unit.repeat(Schedule.forever) *> self.`(ν)`(arity)

        /**
          * replication bound output guard w/ pace
          */
        def apply(arity: Int, pace: Duration): ZStream[Any, Nothing, Seq[`()`]] =
          ZStream.unit.repeat(Schedule.forever) *> self.`(ν)`(arity, pace)

        /**
          * replication bound output guard w/ code
          */
        def apply[T](arity: Int)(code: => Task[T]): ZStream[Any, Nothing, Seq[`()`]] =
          ZStream.unit.repeat(Schedule.forever) *> self.`(ν)`[T](arity)(code)

        /**
          * replication bound output guard w/ pace w/ code
          */
        def apply[T](arity: Int, pace: Duration)(code: => Task[T]): ZStream[Any, Nothing, Seq[`()`]] =
          ZStream.unit.repeat(Schedule.forever) *> self.`(ν)`[T](arity, pace)(code)

      /**
        * constant replication output guard
        */
      def apply(_1: 1)(value: `()`*): ZStream[Any, Nothing, Unit] =
        ZStream.fromZIO(Promise.make[Nothing, Unit].map(value -> _)).repeat(Schedule.forever).through1(h)

      /**
        * constant replication output guard w/ pace
        */
      def apply(_2: 2)(pace: Duration, value: `()`*): ZStream[Any, Nothing, Unit] =
        apply(1)(value*) <* ZStream.fromZIO(ZIO.sleep(pace))

      /**
        * constant replication output guard w/ code
        */
      def apply[T](_3: 3)(value: `()`*)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply(1)(value*).tap(_ => exec(code))

      /**
        * constant replication output guard w/ pace w/ code
        */
      def apply[T](_4: 4)(pace: Duration, value: `()`*)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply[T](3)(value*)(code) <* ZStream.fromZIO(ZIO.sleep(pace))

      object `(null)`:

        /**
          * `null` replication output guard
          */
        def apply(arity: Int): ZStream[Any, Nothing, Unit] =
          self.`(!)`.apply(1)(Seq.fill(arity)(new `()`(null))*)

        /**
          * `null` replication output guard w/ pace
          */
        def apply(arity: Int, pace: Duration): ZStream[Any, Nothing, Unit] =
          self.`(!)`.apply(2)(pace, Seq.fill(arity)(new `()`(null))*)

        /**
          * `null` replication output guard w/ code
          */
        def apply[T](arity: Int)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
          self.`(!)`.apply[T](3)(Seq.fill(arity)(new `()`(null))*)(code)

        /**
          * `null` replication output guard w/ pace w/ code
          */
        def apply[T](arity: Int, pace: Duration)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
          self.`(!)`.apply[T](4)(pace, Seq.fill(arity)(new `()`(null))*)(code)

      object `(*)`:

        /**
          * variable replication output guard
          */
        def apply[S: ClassTag](_1: 1)(value: => S*)(using DummyImplicit): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(1)(value.map(_.asInstanceOf[`()`]*))
          else
            ZStream.suspend(apply[S](1)(value.map(ZIO.succeed)*))

        /**
          * variable replication output guard w/ pace
          */
        def apply[S: ClassTag](_2: 2)(pace: Duration, value: => S*)(using DummyImplicit): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(2)(pace, value.map(_.asInstanceOf[`()`]*))
          else
            ZStream.suspend(apply[S](2)(pace, value.map(ZIO.succeed)*))

        /**
          * variable replication output guard w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(value: => S*)(code: => Task[T])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(3)(value.map(_.asInstanceOf[`()`]*))(code)
          else
            ZStream.suspend(apply[S, T](3)(value.map(ZIO.succeed)*)(code))

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => S*)(code: => Task[T])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(4)(pace, value.map(_.asInstanceOf[`()`]*))(code)
          else
            ZStream.suspend(apply[S, T](4)(pace, value.map(ZIO.succeed)*)(code))

        /**
          * variable replication output guard
          */
        def apply[S: ClassTag](_1: 1)(value: => Task[S]*): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            ZStream.fromZIO(ZIO.suspendSucceed(ZIO.collectAllSuccesses(value.map(_.asInstanceOf[Task[`()`]])))).flatMap(self.`(!)`(1)(_*))
          else
            ZStream.fromZIO(ZIO.suspendSucceed(ZIO.collectAllSuccesses(value).flatMap { it => Promise.make[Nothing, Unit].map(it.map(new `()`(_)) -> _) })).repeat(Schedule.forever).through1(h)

        /**
          * variable replication output guard w/ pace
          */
        def apply[S: ClassTag](_2: 2)(pace: Duration, value: => Task[S]*): ZStream[Any, Nothing, Unit] =
          apply[S](1)(value*) <* ZStream.fromZIO(ZIO.sleep(pace))

        /**
          * variable replication output guard w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(value: => Task[S]*)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
          apply[S](1)(value*).tap(_ => exec(code))

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => Task[S]*)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
          apply[S, T](3)(value*)(code) <* ZStream.fromZIO(ZIO.sleep(pace))

      /**
        * replication input guard
        */
      def apply(_1: 1)(): ZStream[Any, Nothing, Seq[`()`]] =
        s

      /**
        * replication input guard w/ pace
        */
      def apply(_2: 2)(pace: Duration): ZStream[Any, Nothing, Seq[`()`]] =
        apply(1)() <* ZStream.fromZIO(ZIO.sleep(pace))

      /**
        * replication input guard w/ code
        */
      def apply[T](_3: 3)()(code: Seq[T] => Task[Seq[T]]): ZStream[Any, Nothing, Seq[`()`]] =
        s.map(_.map(_.`()`[T])).mapZIO((code andThen exec)(_).map(_.map(new `()`(_))))

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](_4: 4)(pace: Duration)(code: Seq[T] => Task[Seq[T]]): ZStream[Any, Nothing, Seq[`()`]] =
        apply[T](3)()(code) <* ZStream.fromZIO(ZIO.sleep(pace))

    object `(ν)`:

      /**
        * bound output prefix
        */
      def apply(arity: Int): ZStream[Any, Nothing, Seq[`()`]] =
        for
          names   <- ZStream.fromZIO(ZIO.replicateZIO(arity)(Π.ν.make).map(_.toSeq))
          promise <- ZStream.fromZIO(Promise.make[Nothing, Unit])
          _       <- ZStream.succeed(names -> promise).through1(h)
        yield
          names

      /**
        * bound output prefix w/ pace
        */
      def apply(arity: Int, pace: Duration): ZStream[Any, Nothing, Seq[`()`]] =
        apply(arity) <* ZStream.fromZIO(ZIO.sleep(pace))

      /**
        * bound output prefix w/ code
        */
      def apply[T](arity: Int)(code: => Task[T]): ZStream[Any, Nothing, Seq[`()`]] =
        apply(arity).tap(_ => exec(code))

      /**
        * bound output prefix w/ pace w/ code
        */
      def apply[T](arity: Int, pace: Duration)(code: => Task[T]): ZStream[Any, Nothing, Seq[`()`]] =
        apply[T](arity)(code) <* ZStream.fromZIO(ZIO.sleep(pace))

    /**
      * constant output prefix
      */
    def apply(_1: 1)(value: `()`*): ZStream[Any, Nothing, Unit] =
      ZStream.fromZIO(Promise.make[Nothing, Unit].map(value -> _)).through1(h)

    /**
      * constant output prefix w/ pace
      */
    def apply(_2: 2)(pace: Duration, value: `()`*): ZStream[Any, Nothing, Unit] =
      apply(1)(value*) <* ZStream.fromZIO(ZIO.sleep(pace))

    /**
      * constant output prefix w/ code
      */
    def apply[T](_3: 3)(value: `()`*)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
      apply(1)(value*).tap(_ => exec(code))

    /**
      * constant output prefix w/ pace w/ code
      */
    def apply[T](_4: 4)(pace: Duration, value: `()`*)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
      apply[T](3)(value*)(code) <* ZStream.fromZIO(ZIO.sleep(pace))

    object `(null)`:

      /**
        * `null` output prefix
        */
      def apply(arity: Int): ZStream[Any, Nothing, Unit] =
        self.apply(1)(Seq.fill(arity)(new `()`(null))*)

      /**
        * `null` output prefix w/ pace
        */
      def apply(arity: Int, pace: Duration): ZStream[Any, Nothing, Unit] =
        self.apply(2)(pace, Seq.fill(arity)(new `()`(null))*)

      /**
        * `null` output prefix w/ code
        */
      def apply[T](arity: Int)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        self.apply[T](3)(Seq.fill(arity)(new `()`(null))*)(code)

      /**
        * `null` output prefix w/ pace w/ code
        */
      def apply[T](arity: Int, pace: Duration)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        self.apply[T](4)(pace, Seq.fill(arity)(new `()`(null))*)(code)

    object `(*)`:

      /**
        * variable output prefix
        */
      def apply[S: ClassTag](_1: 1)(value: => S*)(using DummyImplicit): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(1)(value.map(_.asInstanceOf[`()`]*))
        else
          ZStream.suspend(apply[S](1)(value.map(ZIO.succeed)*))

      /**
        * variable output prefix w/ pace
        */
      def apply[S: ClassTag](_2: 2)(pace: Duration, value: => S*)(using DummyImplicit): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(2)(pace, value.map(_.asInstanceOf[`()`]*))
        else
          ZStream.suspend(apply[S](2)(pace, value.map(ZIO.succeed)*))

      /**
        * variable output prefix w/ code
        */
      def apply[S: ClassTag, T](_3: 3)(value: => S*)(code: => Task[T])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(3)(value.map(_.asInstanceOf[`()`]*))(code)
        else
          ZStream.suspend(apply[S, T](3)(value.map(ZIO.succeed)*)(code))

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => S*)(code: => Task[T])(using DummyImplicit): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(4)(pace, value.map(_.asInstanceOf[`()`]*))(code)
        else
          ZStream.suspend(apply[S, T](4)(pace, value.map(ZIO.succeed)*)(code))

      /**
        * variable output prefix
        */
      def apply[S: ClassTag](_1: 1)(value: => Task[S]*): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          ZStream.fromZIO(ZIO.suspendSucceed(ZIO.collectAllSuccesses(value.map(_.asInstanceOf[Task[`()`]])))).flatMap(self(1)(_*))
        else
          ZStream.fromZIO(ZIO.suspendSucceed(ZIO.collectAllSuccesses(value).flatMap { it => Promise.make[Nothing, Unit].map(it.map(new `()`(_)) -> _) })).through1(h)

      /**
        * variable output prefix w/ pace
        */
      def apply[S: ClassTag](_2: 2)(pace: Duration, value: => Task[S]*): ZStream[Any, Nothing, Unit] =
        apply[S](1)(value*) <* ZStream.fromZIO(ZIO.sleep(pace))

      /**
        * variable output prefix w/ code
        */
      def apply[S: ClassTag, T](_3: 3)(value: => Task[S]*)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply[S](1)(value*).tap(_ => exec(code))

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S: ClassTag, T](_4: 4)(pace: Duration, value: => Task[S]*)(code: => Task[T]): ZStream[Any, Nothing, Unit] =
        apply[S, T](3)(value*)(code) <* ZStream.fromZIO(ZIO.sleep(pace))

    /**
      * input prefix
      */
    def apply(_1: 1)(): ZStream[Any, Nothing, Seq[`()`]] =
      `s.head`

    /**
      * input prefix w/ pace
      */
    def apply(_2: 2)(pace: Duration): ZStream[Any, Nothing, Seq[`()`]] =
      apply(1)() <* ZStream.unit.repeat(Schedule.fromDuration(pace))

    /**
      * input prefix w/ code
      */
    def apply[T](_3: 3)()(code: Seq[T] => Task[Seq[T]]): ZStream[Any, Nothing, Seq[`()`]] =
      `s.head`.map(_.map(_.`()`[T])).mapZIO((code andThen exec)(_).map(_.map(new `()`(_))))

    /**
      * input prefix w/ pace w/ code
      */
    def apply[T](_4: 4)(pace: Duration)(code: Seq[T] => Task[Seq[T]]): ZStream[Any, Nothing, Seq[`()`]] =
      apply[T](3)()(code) <* ZStream.fromZIO(ZIO.sleep(pace))

    override def toString: String = if name == null then "null" else name.toString

  }


  private object `Π-magic`:

    case class ><(hub: Hub[(Seq[`()`], Promise[Nothing, Unit])],
                  limit: Semaphore[UIO])

    extension [O](self: ZStream[Any, Nothing, O])
      def through1(hub: Hub[O])
                  (using await: UIO[Unit]): ZStream[Any, Nothing, Unit] =
        self.mapZIO(await *> hub.publish(_)).takeWhile(identity).as(())
