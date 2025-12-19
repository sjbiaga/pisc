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

  import _root_.zio.{ Duration, Hub, Promise, Ref, Queue, Schedule, Task, ZIO }
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
      * prefix w/ code
      */
    def apply[T]()(code: => Task[T]): ZStream[Any, Throwable, Unit] =
      apply().tap(_ => code)

  /**
    * events, i.e., names (topics) and values
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

      object ν:

        /**
          * replication bound output guard
          */
        def apply(): ZStream[Any, Throwable, `()`] =
          (τ.!() *> self.ν()).interruptWhen(p)

        /**
          * replication bound output guard w/ code
          */
        def apply[T]()(code: => Task[T]): ZStream[Any, Throwable, `()`] =
          (τ.!() *> self.ν()(code)).interruptWhen(p)

        /**
          * replication bound output guard w/ pace
          */
        def apply(pace: Duration): ZStream[Any, Throwable, `()`] =
          (τ.!(pace) *> self.ν()).interruptWhen(p)

        /**
          * replication bound output guard w/ pace w/ code
          */
        def apply[T](pace: Duration)(code: => Task[T]): ZStream[Any, Throwable, `()`] =
          (τ.!(pace) *> self.ν()(code)).interruptWhen(p)

      /**
        * constant replication output guard
        */
      def apply(value: `()`): ZStream[Any, Throwable, Unit] =
        ZStream.fromZIO(Promise.make[Throwable, Unit].map(value -> _)).repeat(Schedule.forever).through1(h).interruptWhen(p)

      /**
        * constant replication output guard w/ pace
        */
      def apply(pace: Duration, value: `()`): ZStream[Any, Throwable, Unit] =
        ZStream.tick(pace).mapZIO(_ => Promise.make[Throwable, Unit].map(value -> _)).through1(h).interruptWhen(p)

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
        inline def apply(): ZStream[Any, Throwable, Unit] =
          self.`null`()

        /**
          * `null` replication output guard w/ pace
          */
        inline def apply(_pace: Duration): ZStream[Any, Throwable, Unit] =
          apply()

        /**
          * `null` replication output guard w/ code
          */
        inline def apply[T]()(code: => Task[T]): ZStream[Any, Throwable, Unit] =
          self.`null`()(code)

        /**
          * `null` replication output guard w/ pace w/ code
          */
        inline def apply[T](_pace: Duration)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
          apply()(code)

      object * :

        /**
          * variable replication output guard
          */
        def apply[S](value: => Task[S]): ZStream[Any, Throwable, Unit] =
          ZStream.fromZIO(value).repeat(Schedule.forever).mapZIO { it => Promise.make[Throwable, Unit].map(new `()`(it) -> _) }.through1(h).interruptWhen(p)

        /**
          * variable replication output guard w/ pace
          */
        def apply[S](pace: Duration, value: => Task[S]): ZStream[Any, Throwable, Unit] =
          ZStream.tick(pace).mapZIO(_ => value).mapZIO { it => Promise.make[Throwable, Unit].map(new `()`(it) -> _) }.through1(h).interruptWhen(p)

        /**
          * variable replication output guard w/ code
          */
        def apply[S, T](value: => Task[S])(code: => Task[T]): ZStream[Any, Throwable, Unit] =
          apply[S](value).tap(_ => code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S, T](pace: Duration, value: => Task[S])(code: => Task[T]): ZStream[Any, Throwable, Unit] =
          apply[S](pace, value).tap(_ => code)

      /**
        * replication input guard
        */
      def apply(): ZStream[Any, Throwable, `()`] =
        s.tap { case it if it.name == null => p.succeed(()) case _ => o }.interruptWhen(p)

      /**
        * replication input guard w/ pace
        */
      def apply(pace: Duration): ZStream[Any, Throwable, `()`] =
        s.tap(_ => ZIO.sleep(pace)).tap { case it if it.name == null => p.succeed(()) case _ => o }.interruptWhen(p)

      /**
        * replication input guard w/ code
        */
      def apply[T]()(code: T => Task[T]): ZStream[Any, Throwable, `()`] =
        s.mapZIO { it => code(it.`()`[T]).map(new `()`(_)) }.tap { case it if it.name == null => p.succeed(()) case _ => o }.interruptWhen(p)

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](pace: Duration)(code: T => Task[T]): ZStream[Any, Throwable, `()`] =
        s.tap(_ => ZIO.sleep(pace)).mapZIO { it => code(it.`()`[T]).map(new `()`(_)) }.tap { case it if it.name == null => p.succeed(()) case _ => o }.interruptWhen(p)

    object ν:

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
        * bound output prefix w/ code
        */
      def apply[T]()(code: => Task[T]): ZStream[Any, Throwable, `()`] =
        apply().tap(_ => code)

    /**
      * constant output prefix
      */
    def apply(value: `()`): ZStream[Any, Throwable, Unit] =
      ZStream.fromZIO(Promise.make[Throwable, Unit].map(value -> _)).through1(h)

    /**
      * constant output prefix w/ code
      */
    def apply[T](value: `()`)(code: => Task[T]): ZStream[Any, Throwable, Unit] =
      apply(value).tap(_ => code)

    object `null`:

      /**
        * `null` output prefix
        */
      def apply(): ZStream[Any, Throwable, Unit] =
        ZStream.fromZIO(p.succeed(()).as(()))

      /**
        * `null` output prefix w/ code
        */
      def apply[T]()(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        apply().tap(_ => code)

    object * :

      /**
        * variable output prefix
        */
      def apply[S](value: => Task[S]): ZStream[Any, Throwable, Unit] =
        ZStream.fromZIO(value).mapZIO { it => Promise.make[Throwable, Unit].map(new `()`(it) -> _) }.through1(h)

      /**
        * variable output prefix w/ code
        */
      def apply[S, T](value: => Task[S])(code: => Task[T]): ZStream[Any, Throwable, Unit] =
        apply[S](value).tap(_ => code)

    /**
      * input prefix
      */
    def apply(): ZStream[Any, Throwable, `()`] =
      s.take(1).tap { case it if it.name == null => p.succeed(()) case _ => ZIO.unit }

    /**
      * input prefix w/ code
      */
    def apply[T]()(code: T => Task[T]): ZStream[Any, Throwable, `()`] =
      s.take(1).mapZIO { it => code(it.`()`[T]).map(new `()`(_)) }.tap { case it if it.name == null => p.succeed(()) case _ => ZIO.unit }

    override def toString: String = if name == null then "null" else name.toString

  }


  private object `Π-magic`:

    case class ><(hub: Hub[(`()`, Promise[Throwable, Unit])],
                  stop: Promise[Throwable, Unit],
                  queue: Queue[Unit],
                  limit: Ref[Boolean])

    extension [O](self: ZStream[Any, Throwable, O])
      inline def through1(hub: Hub[O])
                         (using await: Task[Unit]): ZStream[Any, Throwable, Unit] =
        self.tap(await *> hub.publish(_)).as(())
