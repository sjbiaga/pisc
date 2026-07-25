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

package object sΠ:

  import _root_.scala.collection.immutable.{ Map, Set }

  import _root_.cats.effect.std.Semaphore
  import _root_.zio.interop.catz.concurrentInstance
  import _root_.zio.{ Clock, Duration, Hub, Promise, Ref, Schedule, Task, UIO, ZIO }
  import _root_.zio.concurrent.CyclicBarrier
  import _root_.zio.stream.{ ZSink, ZStream }

  import `Π-loop`.{ <>, +, %, /, \ }
  import `Π-magic`.*
  export `Π-magic`.><
  import `Π-stats`.Rate


  type `Π-Map`[K, +V] = Map[K, V]

  type `Π-Set`[A] = Set[A]


  def `π-enable`(enabled: `Π-Set`[String])
                (using % : %): UIO[Unit] =
    %.update(enabled.foldLeft(_) { (m, key) =>
                                    val n = if m.contains(key)
                                            then m(key).asInstanceOf[Int]
                                            else 0
                                    m + (key -> (n + 1))
                                 }
    )

  private def enable(key: String)
                    (using %)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): UIO[Unit] =
    val (_, spell) = `π-wand`
    `π-enable`(spell(key))


  inline def `π-exclude`(enabled: String*)
                        (using % : %, \ : \): Task[Unit] =
    `π-exclude`(Set.from(enabled)) *> \()

  private def `π-exclude`(enabled: => `Π-Set`[String])
                         (using % : %): UIO[Unit] =
    %.update(enabled.foldLeft(_) { (m, key) =>
                                   val n = m(key).asInstanceOf[Int] - 1
                                   if n == 0
                                   then
                                     m - key
                                   else
                                     m + (key -> n)
                                 }
    )

  private def exclude(key: String)
                     (using %)
                     (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]]): UIO[Unit] =
    `π-exclude`(`π-elvis`(key)).unless(`π-elvis`.contains(key)).unit


  /**
    * restriction aka new name
    */
  object ν:

    def map[B](f: `()` => B): ZStream[Any, Throwable, B] = flatMap(f andThen ZStream.succeed)
    def flatMap[B](f: `()` => ZStream[Any, Throwable, B]): ZStream[Any, Throwable, B] =
      ( for
          hub   <- ZStream.fromZIO(Hub.unbounded[(`()`, Object)])
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
        def apply(rate: Rate)(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                 (using % : %, / : /, \ : \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, Unit] =
          for
            discard  <- if None eq * then ZStream.fromZIO(exclude(key)) *> ZStream.succeed(false)
                        else ZStream.fromZIO(?.await)
            _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
            if !discard
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
            _        <- if None eq * then ZStream.unit
                        else ZStream.fromZIO(promise.succeed(None))
            timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (new Object, None, rate)))))
            cb_token <- ZStream.fromZIO(promise.await)
            discard  <- if None eq * then ZStream.fromZIO(?.succeed(cb_token eq None) *> ?.await)
                        else ZStream.succeed(false)
            _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
            if !discard
            sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
            _  <- ZStream.fromZIO {
              for
                _        <- -.await.exit
                _        <- *.fold(ZIO.unit)(_.acquire)
                enabled  <- %.modify { m => m(^ + key).asInstanceOf[(Boolean, +)]._1 -> m }
                _        <- Clock.nanoTime.flatMap(timestamp.set).unless(enabled)
                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                cb_token <- continue.get.flatMap(_.await)
                _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                _        <- if cb_token eq None then sp.succeed(())
                            else
                              val (cbarrier, _) = cb_token.get
                              enable(key) *> cbarrier.await.exit
              yield
                ()
            }.repeat(Schedule.forever).interruptWhen(sp)
            _  <- ZStream.fromZIO(+.release)
          yield
            ()

        /**
          * linear replication guard w/ pace
          */
        def apply(rate: Rate, pace: Duration)(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                 (using %, /, \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate)(key)(?, -, *, +) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

        /**
          * linear replication guard w/ code
          */
        def apply[T](rate: Rate)(key: String)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate)(key)(?, -, *, +).tap(_ => code)

        /**
          * linear replication guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: Duration)(key: String)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate, pace)(key)(?, -, *, +).tap(_ => code)

      /**
        * replication guard
        */
      def apply(rate: Rate)(key: String)
               (using % : %, / : /, \ : \)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, Unit] =
        for
          _        <- ZStream.fromZIO(exclude(key))
          promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
          continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
          timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
          _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (new Object, None, rate)))))
          cb_token <- ZStream.fromZIO(promise.await)
          if cb_token ne None
          sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
          _  <- ZStream.fromZIO {
            for
              enabled  <- %.modify { m => m(^ + key).asInstanceOf[(Boolean, +)]._1 -> m }
              _        <- Clock.nanoTime.flatMap(timestamp.set).unless(enabled)
              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
              cb_token <- continue.get.flatMap(_.await)
              _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
              _        <- if cb_token eq None then sp.succeed(())
                          else
                            val (cbarrier, _) = cb_token.get
                            enable(key) *> cbarrier.await.exit
            yield
              ()
          }.repeat(Schedule.forever).interruptWhen(sp)
        yield
          ()

      /**
        * replication guard w/ pace
        */
      def apply(rate: Rate, pace: Duration)(key: String)
               (using %, /, \)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate)(key) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

      /**
        * replication guard w/ code
        */
      def apply[T](rate: Rate)(key: String)(code: => Task[T])
                  (using %, /, \)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate)(key).tap(_ => code)

      /**
        * replication guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: Duration)(key: String)(code: => Task[T])
                  (using %, /, \)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate, pace)(key).tap(_ => code)

    /**
      * prefix
      */
    def apply(rate: Rate)(key: String)
             (using % : %, / : /)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): ZStream[Any, Throwable, Unit] =
      for
        _        <- ZStream.fromZIO(exclude(key))
        promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
        timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
        _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (timestamp, (new Object, None, rate)))))
        cb_token <- ZStream.fromZIO(promise.await)
        if cb_token ne None
        (cbarrier, _) = cb_token.get
        _        <- ZStream.fromZIO(enable(key) *> cbarrier.await.exit)
      yield
        ()

    /**
      * prefix w/ pace
      */
    def apply(rate: Rate, pace: Duration)(key: String)
             (using %, /)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): ZStream[Any, Throwable, Unit] =
      apply(rate)(key) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

    /**
      * prefix w/ code
      */
    def apply[T](rate: Rate)(key: String)(code: => Task[T])
                (using %, /)
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): ZStream[Any, Throwable, Unit] =
      apply(rate)(key).tap(_ => code)

    /**
      * prefix w/ pace w/ code
      */
    def apply[T](rate: Rate, pace: Duration)(key: String)(code: => Task[T])
                (using %, /)
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): ZStream[Any, Throwable, Unit] =
      apply(rate, pace)(key).tap(_ => code)

  /**
    * events, i.e., names (hubs) and values
    */
  implicit final class `()`(private val name: Any) { self =>

    private inline def h = `()`[><].hub
    private inline def l = `()`[><].limit
    private implicit def a: Task[Unit] = l.acquire
    private def _s = ZStream.unwrapScoped(ZStream.fromHubScoped(h).tap(_ => l.release))

    extension (self: ZStream[Any, Throwable, Object])
      def `zipRight s`: ZStream[Any, Throwable, `()`] = `self zipRight s`(true, _s)
      def `zipRight s.head`: ZStream[Any, Throwable, `()`] = `self zipRight s`(false, _s)
      private def `self zipRight s`(r: Boolean,
                                    its: ZStream[Any, Throwable, (`()`, Object)]): ZStream[Any, Throwable, `()`] =
        ZStream.unwrapScoped {
          self.peel(ZSink.head).map {
            case (Some(tk), tks) =>
              tks.filter(r, tk, its)
            case _ =>
              ZStream.empty
          }
        }
      private def filter(r: Boolean,
                         tk: Object,
                         its: ZStream[Any, Throwable, (`()`, Object)]): ZStream[Any, Throwable, `()`] =
        ZStream.unwrapScoped {
          its.peel(ZSink.head).map {
            case (Some((it, tkʹ)), itsʹ) if tk eq tkʹ =>
              ZStream(it) ++ `self zipRight s`(r, ZStream.fromZIO(l.release) *> itsʹ).when(r)
            case (Some(_), itsʹ) =>
              filter(r, tk, itsʹ)
            case _ =>
              ZStream.empty
          }
        }

    def ====(that: `()`) =
      try
        this.h eq that.h
      catch _ =>
        this.name == that.name

    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

    lazy val `null` = new `()`(null)

    object `(!)`:

      object `(+)`:

        object `(ν)`:

          /**
            * linear replication bound output guard
            */
          def apply(rate: Rate)(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                   (using % : %, / : /, \ : \)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): ZStream[Any, Throwable, `()`] =
            for
              discard  <- if None eq * then ZStream.fromZIO(exclude(key)) *> ZStream.succeed(false)
                          else ZStream.fromZIO(?.await)
              _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
              if !discard
              promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
              continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
              _        <- if None eq * then ZStream.unit
                          else ZStream.fromZIO(promise.succeed(None))
              timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
              _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`()`[><], Some(false), rate)))))
              cb_token <- ZStream.fromZIO(promise.await)
              discard  <- if None eq * then ZStream.fromZIO(?.succeed(cb_token eq None) *> ?.await)
                          else ZStream.succeed(false)
              _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
              if !discard
              sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
              it <- ( for
                        _  <- ZStream.unit.repeat(Schedule.forever)
                        it <- sΠ.ν
                        it <- ZStream.fromZIO {
                          for
                            _        <- -.await.exit
                            _        <- *.fold(ZIO.unit)(_.acquire)
                            enabled  <- %.modify { m => m(^ + key).asInstanceOf[(Boolean, +)]._1 -> m }
                            _        <- Clock.nanoTime.flatMap(timestamp.set).unless(enabled)
                            _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                            cb_token <- continue.get.flatMap(_.await)
                            _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                            _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                            token    <- if cb_token eq None then sp.succeed(()).as(null)
                                        else
                                          val (cbarrier, token) = cb_token.get
                                          (enable(key) *> cbarrier.await.exit).as(token)
                          yield
                            it -> token
                         }
                       yield
                         it
                    ).interruptWhen(sp).through1(h)
              _  <- ZStream.fromZIO(+.release)
            yield
              it._1

          /**
            * linear replication bound output guard w/ pace
            */
          def apply(rate: Rate, pace: Duration)(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                   (using %, /, \)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): ZStream[Any, Throwable, `()`] =
            apply(rate)(key)(?, -, *, +) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

          /**
            * linear replication bound output guard w/ code
            */
          def apply[T](rate: Rate)(key: String)(code: Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                      (using %, /, \)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, `()`] =
            apply(rate)(key)(?, -, *, +).tap(_ => code)

          /**
            * linear replication bound output guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: Duration)(key: String)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                      (using %, /, \)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, `()`] =
            apply(rate, pace)(key)(?, -, *, +).tap(_ => code)

        /**
          * linear constant replication output guard
          */
        def apply(rate: Rate, value: `()`)(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                 (using % : %, / : /, \ : \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, Unit] =
          for
            discard  <- if None eq * then ZStream.fromZIO(exclude(key)) *> ZStream.succeed(false)
                        else ZStream.fromZIO(?.await)
            _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
            if !discard
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
            _        <- if None eq * then ZStream.unit
                        else ZStream.fromZIO(promise.succeed(None))
            timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`()`[><], Some(false), rate)))))
            cb_token <- ZStream.fromZIO(promise.await)
            discard  <- if None eq * then ZStream.fromZIO(?.succeed(cb_token eq None) *> ?.await)
                        else ZStream.succeed(false)
            _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
            if !discard
            sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
            _  <- ZStream.fromZIO {
              for
                _        <- -.await.exit
                _        <- *.fold(ZIO.unit)(_.acquire)
                enabled  <- %.modify { m => m(^ + key).asInstanceOf[(Boolean, +)]._1 -> m }
                _        <- Clock.nanoTime.flatMap(timestamp.set).unless(enabled)
                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                cb_token <- continue.get.flatMap(_.await)
                _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                token    <- if cb_token eq None then sp.succeed(()).as(null)
                            else
                              val (cbarrier, token) = cb_token.get
                              (enable(key) *> cbarrier.await.exit).as(token)
              yield
                value -> token
            }.repeat(Schedule.forever).interruptWhen(sp).through1(h)
            _  <- ZStream.fromZIO(+.release)
          yield
            ()

        /**
          * linear constant replication output guard w/ pace
          */
        def apply(rate: Rate, pace: Duration, value: `()`)(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                 (using %, /, \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate, value)(key)(?, -, *, +) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

        /**
          * linear constant replication output guard w/ code
          */
        def apply[T](rate: Rate, value: `()`)(key: String)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate, value)(key)(?, -, *, +).tap(_ => code)

        /**
          * linear constant replication output guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: Duration, value: `()`)(key: String)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate, pace, value)(key)(?, -, *, +).tap(_ => code)

        object `(*)`:

          /**
            * linear variable replication output guard
            */
          def apply[S](_1: 1)(rate: Rate, value: => S)(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                             (using DummyImplicit)
                             (using %, /, \)
                             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                       ^ : String): ZStream[Any, Throwable, Unit] =
            apply[S](1)(rate, ZIO.attempt(value))(key)(?, -, *, +)

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S](_2: 2)(rate: Rate, pace: Duration, value: => S)(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                             (using DummyImplicit)
                             (using %, /, \)
                             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                       ^ : String): ZStream[Any, Throwable, Unit] =
            apply[S](2)(rate, pace, ZIO.attempt(value))(key)(?, -, *, +)

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S, T](_3: 3)(rate: Rate, value: => S)(key: String)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                                (using DummyImplicit)
                                (using %, /, \)
                                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                          ^ : String): ZStream[Any, Throwable, Unit] =
            apply[S, T](3)(rate, ZIO.attempt(value))(key)(code)(?, -, *, +)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S, T](_4: 4)(rate: Rate, pace: Duration, value: => S)(key: String)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                                (using DummyImplicit)
                                (using %, /, \)
                                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                          ^ : String): ZStream[Any, Throwable, Unit] =
            apply[S, T](4)(rate, pace, ZIO.attempt(value))(key)(code)(?, -, *, +)

          /**
            * linear variable replication output guard
            */
          def apply[S](_1: 1)(rate: Rate, value: => Task[S])(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                             (using % : %, / : /, \ : \)
                             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                       ^ : String): ZStream[Any, Throwable, Unit] =
            for
              discard  <- if None eq * then ZStream.fromZIO(exclude(key)) *> ZStream.succeed(false)
                          else ZStream.fromZIO(?.await)
              _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
              if !discard
              promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
              continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
              _        <- if None eq * then ZStream.unit
                          else ZStream.fromZIO(promise.succeed(None))
              timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
              _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`()`[><], Some(false), rate)))))
              cb_token <- ZStream.fromZIO(promise.await)
              discard  <- if None eq * then ZStream.fromZIO(?.succeed(cb_token eq None) *> ?.await)
                          else ZStream.succeed(false)
              _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
              if !discard
              sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
              _  <- ZStream.fromZIO {
                for
                  _        <- -.await.exit
                  _        <- *.fold(ZIO.unit)(_.acquire)
                  enabled  <- %.modify { m => m(^ + key).asInstanceOf[(Boolean, +)]._1 -> m }
                  _        <- Clock.nanoTime.flatMap(timestamp.set).unless(enabled)
                  _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                  cb_token <- continue.get.flatMap(_.await)
                  _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                  _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                  it       <- if cb_token eq None then sp.succeed(()).as(`null` -> null)
                              else
                                val (cbarrier, token) = cb_token.get
                                value.map(new `()`(_) -> token).tap(_ => enable(key) *> cbarrier.await.exit)
                yield
                  it
              }.repeat(Schedule.forever).interruptWhen(sp).through1(h)
              _  <- ZStream.fromZIO(+.release)
            yield
              ()

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S](_2: 2)(rate: Rate, pace: Duration, value: => Task[S])(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                             (using %, /, \)
                             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                       ^ : String): ZStream[Any, Throwable, Unit] =
            apply[S](1)(rate, value)(key)(?, -, *, +) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S, T](_3: 3)(rate: Rate, value: => Task[S])(key: String)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                                (using %, /, \)
                                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                          ^ : String): ZStream[Any, Throwable, Unit] =
            apply[S](1)(rate, value)(key)(?, -, *, +).tap(_ => code)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S, T](_4: 4)(rate: Rate, pace: Duration, value: => Task[S])(key: String)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                                (using %, /, \)
                                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                          ^ : String): ZStream[Any, Throwable, Unit] =
            apply[S](2)(rate, pace, value)(key)(?, -, *, +).tap(_ => code)

        /**
          * linear replication input guard
          */
        def apply(rate: Rate)(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                 (using % : %, / : /, \ : \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, `()`] =
          for
            discard  <- if None eq * then ZStream.fromZIO(exclude(key)) *> ZStream.succeed(false)
                        else ZStream.fromZIO(?.await)
            _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
            if !discard
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
            _        <- if None eq * then ZStream.unit
                        else ZStream.fromZIO(promise.succeed(None))
            timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`()`[><], Some(true), rate)))))
            cb_token <- ZStream.fromZIO(promise.await)
            discard  <- if None eq * then ZStream.fromZIO(?.succeed(cb_token eq None) *> ?.await)
                        else ZStream.succeed(false)
            _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
            if !discard
            sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
            tks = ZStream.fromZIO {
              for
                _        <- -.await.exit
                _        <- *.fold(ZIO.unit)(_.acquire)
                enabled  <- %.modify { m => m(^ + key).asInstanceOf[(Boolean, +)]._1 -> m }
                _        <- Clock.nanoTime.flatMap(timestamp.set).unless(enabled)
                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                cb_token <- continue.get.flatMap(_.await)
                _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                token    <- if cb_token eq None then sp.succeed(()).as(null)
                            else
                              val (cbarrier, token) = cb_token.get
                              (enable(key) *> cbarrier.await.exit).as(token)
              yield
                token
            }.repeat(Schedule.forever).interruptWhen(sp)
            it <- tks.`zipRight s`
            _  <- ZStream.fromZIO(+.release)
          yield
            it

        /**
          * linear replication input guard w/ pace
          */
        def apply(rate: Rate, pace: Duration)(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                 (using %, /, \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, `()`] =
          apply(rate)(key)(?, -, *, +) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

        /**
          * linear replication input guard w/ code
          */
        def apply[T](rate: Rate)(key: String)(code: T => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, `()`] =
          apply(rate)(key)(?, -, *, +).map(_.`()`[T]).mapZIO(code(_).map(new `()`(_)))

        /**
          * linear replication input guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: Duration)(key: String)(code: T => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, `()`] =
          apply(rate, pace)(key)(?, -, *, +).map(_.`()`[T]).mapZIO(code(_).map(new `()`(_)))

      object `(ν)`:

        /**
          * replication bound output guard
          */
        def apply(rate: Rate)(key: String)
                 (using % : %, / : /, \ : \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, `()`] =
          for
            _        <- ZStream.fromZIO(exclude(key))
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
            timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`()`[><], Some(false), rate)))))
            cb_token <- ZStream.fromZIO(promise.await)
            if cb_token ne None
            sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
            it <- ( for
                      _  <- ZStream.unit.repeat(Schedule.forever)
                      it <- sΠ.ν
                      it <- ZStream.fromZIO {
                        for
                          enabled  <- %.modify { m => m(^ + key).asInstanceOf[(Boolean, +)]._1 -> m }
                          _        <- Clock.nanoTime.flatMap(timestamp.set).unless(enabled)
                          _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                          cb_token <- continue.get.flatMap(_.await)
                          _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                          _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                          token    <- if cb_token eq None then sp.succeed(()).as(null)
                                      else
                                        val (cbarrier, token) = cb_token.get
                                        (enable(key) *> cbarrier.await.exit).as(token)
                        yield
                          it -> token
                      }
                    yield
                      it
                  ).interruptWhen(sp).through1(h)
          yield
            it._1

        /**
          * replication bound output guard w/ pace
          */
        def apply(rate: Rate, pace: Duration)(key: String)
                 (using %, /, \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, `()`] =
          apply(rate)(key) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

        /**
          * replication bound output guard w/ code
          */
        def apply[T](rate: Rate)(key: String)(code: => Task[T])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, `()`] =
          apply(rate)(key).tap(_ => code)

        /**
          * replication bound output guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: Duration)(key: String)(code: => Task[T])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, `()`] =
          apply(rate, pace)(key).tap(_ => code)

      /**
        * constant replication output guard
        */
      def apply(rate: Rate, value: `()`)(key: String)
               (using % : %, / : /, \ : \)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, Unit] =
        for
          _        <- ZStream.fromZIO(exclude(key))
          promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
          continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
          timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
          _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`()`[><], Some(false), rate)))))
          cb_token <- ZStream.fromZIO(promise.await)
          if cb_token ne None
          sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
          _  <- ZStream.fromZIO {
            for
              enabled  <- %.modify { m => m(^ + key).asInstanceOf[(Boolean, +)]._1 -> m }
              _        <- Clock.nanoTime.flatMap(timestamp.set).unless(enabled)
              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
              cb_token <- continue.get.flatMap(_.await)
              _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
              token    <- if cb_token eq None then sp.succeed(()).as(null)
                          else
                            val (cbarrier, token) = cb_token.get
                            (enable(key) *> cbarrier.await.exit).as(token)
            yield
              value -> token
          }.repeat(Schedule.forever).interruptWhen(sp).through1(h)
        yield
          ()

      /**
        * constant replication output guard w/ pace
        */
      def apply(rate: Rate, pace: Duration, value: `()`)(key: String)
               (using %, /, \)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate, value)(key) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

      /**
        * constant replication output guard w/ code
        */
      def apply[T](rate: Rate, value: `()`)(key: String)(code: => Task[T])
               (using %, /, \)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate, value)(key).tap(_ => code)

      /**
        * constant replication output guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: Duration, value: `()`)(key: String)(code: => Task[T])
               (using %, /, \)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate, pace, value)(key).tap(_ => code)

      object `(*)`:

        /**
          * variable replication output guard
          */
        def apply[S](_1: 1)(rate: Rate, value: => S)(key: String)
                           (using DummyImplicit)
                           (using %, /, \)
                           (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                     `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                     ^ : String): ZStream[Any, Throwable, Unit] =
          apply[S](1)(rate, ZIO.attempt(value))(key)

        /**
          * variable replication output guard w/ pace
          */
        def apply[S](_2: 2)(rate: Rate, pace: Duration, value: => S)(key: String)
                           (using DummyImplicit)
                           (using %, /, \)
                           (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                     `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                     ^ : String): ZStream[Any, Throwable, Unit] =
          apply[S](2)(rate, pace, ZIO.attempt(value))(key)

        /**
          * variable replication output guard w/ code
          */
        def apply[S, T](_3: 3)(rate: Rate, value: => S)(key: String)(code: => Task[T])
                              (using DummyImplicit)
                              (using %, /, \)
                              (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                        `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                        ^ : String): ZStream[Any, Throwable, Unit] =
          apply[S, T](3)(rate, ZIO.attempt(value))(key)(code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S, T](_4: 4)(rate: Rate, pace: Duration, value: => S)(key: String)(code: => Task[T])
                              (using DummyImplicit)
                              (using %, /, \)
                              (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                        `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                        ^ : String): ZStream[Any, Throwable, Unit] =
          apply[S, T](4)(rate, pace, ZIO.attempt(value))(key)(code)

        /**
          * variable replication output guard
          */
        def apply[S](_1: 1)(rate: Rate, value: => Task[S])(key: String)
                           (using % : %, / : /, \ : \)
                           (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                     `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                     ^ : String): ZStream[Any, Throwable, Unit] =
          for
            _        <- ZStream.fromZIO(exclude(key))
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
            timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`()`[><], Some(false), rate)))))
            cb_token <- ZStream.fromZIO(promise.await)
            if cb_token ne None
            sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
            _  <- ZStream.fromZIO {
              for
                enabled  <- %.modify { m => m(^ + key).asInstanceOf[(Boolean, +)]._1 -> m }
                _        <- Clock.nanoTime.flatMap(timestamp.set).unless(enabled)
                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                cb_token <- continue.get.flatMap(_.await)
                _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                it       <- if cb_token eq None then sp.succeed(()).as(`null` -> null)
                            else
                              val (cbarrier, token) = cb_token.get
                              value.map(new `()`(_) -> token).tap(_ => enable(key) *> cbarrier.await.exit)
              yield
                it
            }.repeat(Schedule.forever).interruptWhen(sp).through1(h)
          yield
            ()

        /**
          * variable replication output guard w/ pace
          */
        def apply[S](_2: 2)(rate: Rate, pace: Duration, value: => Task[S])(key: String)
                           (using %, /, \)
                           (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                     `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                     ^ : String): ZStream[Any, Throwable, Unit] =
          apply[S](1)(rate, value)(key) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

        /**
          * variable replication output guard w/ code
          */
        def apply[S, T](_3: 3)(rate: Rate, value: => Task[S])(key: String)(code: => Task[T])
                              (using %, /, \)
                              (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                        `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                        ^ : String): ZStream[Any, Throwable, Unit] =
          apply[S](1)(rate, value)(key).tap(_ => code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S, T](_4: 4)(rate: Rate, pace: Duration, value: => Task[S])(key: String)(code: => Task[T])
                              (using %, /, \)
                              (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                        `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                        ^ : String): ZStream[Any, Throwable, Unit] =
          apply[S](2)(rate, pace, value)(key).tap(_ => code)

      /**
        * replication input guard
        */
      def apply(rate: Rate)(key: String)
               (using % : %, / : /, \ : \)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, `()`] =
        for
          _        <- ZStream.fromZIO(exclude(key))
          promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
          continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
          timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
          _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`()`[><], Some(true), rate)))))
          cb_token <- ZStream.fromZIO(promise.await)
          if cb_token ne None
          sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
          tks = ZStream.fromZIO {
            for
              enabled  <- %.modify { m => m(^ + key).asInstanceOf[(Boolean, +)]._1 -> m }
              _        <- Clock.nanoTime.flatMap(timestamp.set).unless(enabled)
              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
              cb_token <- continue.get.flatMap(_.await)
              _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
              token    <- if cb_token eq None then sp.succeed(()).as(null)
                          else
                            val (cbarrier, token) = cb_token.get
                            (enable(key) *> cbarrier.await.exit).as(token)
            yield
              token
          }.repeat(Schedule.forever).interruptWhen(sp)
          it <- tks.`zipRight s`
        yield
          it

      /**
        * replication input guard w/ pace
        */
      def apply(rate: Rate, pace: Duration)(key: String)
               (using %, /, \)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, `()`] =
        apply(rate)(key) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

      /**
        * replication input guard w/ code
        */
      def apply[T](rate: Rate)(key: String)(code: T => Task[T])
                  (using %, /, \)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, `()`] =
        apply(rate)(key).map(_.`()`[T]).mapZIO(code(_).map(new `()`(_)))

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: Duration)(key: String)(code: T => Task[T])
                  (using %, /, \)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, `()`] =
        apply(rate, pace)(key).map(_.`()`[T]).mapZIO(code(_).map(new `()`(_)))

    object `(ν)`:

      /**
        * bound output prefix
        */
      def apply(rate: Rate)(key: String)
               (using % : %, / : /)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, `()`] =
        for
          _        <- ZStream.fromZIO(exclude(key))
          promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
          timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
          _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (timestamp, (`()`[><], Some(false), rate)))))
          cb_token <- ZStream.fromZIO(promise.await)
          if cb_token ne None
          (cbarrier, token) = cb_token.get
          it <- sΠ.ν
          _  <- ZStream.succeed(it -> token).tap(_ => enable(key) *> cbarrier.await.exit).through1(h)
        yield
          it

      /**
        * bound output prefix w/ pace
        */
      def apply(rate: Rate, pace: Duration)(key: String)
               (using %, /)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, `()`] =
        apply(rate)(key) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * bound output prefix w/ code
        */
      def apply[T](rate: Rate)(key: String)(code: => Task[T])
                  (using %, /)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, `()`] =
        apply(rate)(key).tap(_ => code)

      /**
        * bound output prefix w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: Duration)(key: String)(code: => Task[T])
                  (using %, /)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, `()`] =
        apply(rate, pace)(key).tap(_ => code)

    /**
      * constant output prefix
      */
    def apply(rate: Rate, value: `()`)(key: String)
             (using % : %, / : /)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): ZStream[Any, Throwable, Unit] =
      for
        _        <- ZStream.fromZIO(exclude(key))
        promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
        timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
        _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (timestamp, (`()`[><], Some(false), rate)))))
        cb_token <- ZStream.fromZIO(promise.await)
        if cb_token ne None
        (cbarrier, token) = cb_token.get
        _        <- ZStream.succeed(value -> token).tap(_ => enable(key) *> cbarrier.await.exit).through1(h)
      yield
        ()

    /**
      * constant output prefix w/ pace
      */
    def apply(rate: Rate, pace: Duration, value: `()`)(key: String)
             (using %, /)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate, value)(key) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

    /**
      * constant output prefix w/ code
      */
    def apply[T](rate: Rate, value: `()`)(key: String)(code: => Task[T])
                (using %, /)
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): ZStream[Any, Throwable, Unit] =
      apply(rate, value)(key).tap(_ => code)

    /**
      * constant output prefix w/ pace w/ code
      */
    def apply[T](rate: Rate, pace: Duration, value: `()`)(key: String)(code: => Task[T])
                (using %, /)
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): ZStream[Any, Throwable, Unit] =
      apply(rate, pace, value)(key).tap(_ => code)

    object `(*)`:

      /**
        * variable output prefix
        */
      def apply[S](_1: 1)(rate: Rate, value: => S)(key: String)
                         (using DummyImplicit)
                         (using %, /)
                         (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                   `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                   ^ : String): ZStream[Any, Throwable, Unit] =
        apply[S](1)(rate, ZIO.attempt(value))(key)

      /**
        * variable output prefix w/ pace
        */
      def apply[S](_2: 2)(rate: Rate, pace: Duration, value: => S)(key: String)
                         (using DummyImplicit)
                         (using %, /)
                         (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                   `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                   ^ : String): ZStream[Any, Throwable, Unit] =
        apply[S](1)(rate, value)(key) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * variable output prefix w/ code
        */
      def apply[S, T](_3: 3)(rate: Rate, value: => S)(key: String)(code: => Task[T])
                            (using DummyImplicit)
                            (using %, /)
                            (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                      `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                      ^ : String): ZStream[Any, Throwable, Unit] =
        apply[S](1)(rate, value)(key).tap(_ => code)

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S, T](_4: 4)(rate: Rate, pace: Duration, value: => S)(key: String)(code: => Task[T])
                            (using DummyImplicit)
                            (using %, /)
                            (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                      `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                      ^ : String): ZStream[Any, Throwable, Unit] =
        apply[S](2)(rate, pace, value)(key).tap(_ => code)

      /**
        * variable output prefix
        */
      def apply[S](_1: 1)(rate: Rate, value: => Task[S])(key: String)
                         (using % : %, / : /)
                         (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                   `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                   ^ : String): ZStream[Any, Throwable, Unit] =
        for
          _        <- ZStream.fromZIO(exclude(key))
          promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
          timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
          _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (timestamp, (`()`[><], Some(false), rate)))))
          cb_token <- ZStream.fromZIO(promise.await)
          if cb_token ne None
          (cbarrier, token) = cb_token.get
          _        <- ZStream.fromZIO(value).map(new `()`(_) -> token).tap(_ => enable(key) *> cbarrier.await.exit).through1(h)
        yield
          ()

      /**
        * variable output prefix w/ pace
        */
      def apply[S](_2: 2)(rate: Rate, pace: Duration, value: => Task[S])(key: String)
                         (using %, /)
                         (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                   `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                   ^ : String): ZStream[Any, Throwable, Unit] =
        apply[S](1)(rate, value)(key) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * variable output prefix w/ code
        */
      def apply[S, T](_3: 3)(rate: Rate, value: => Task[S])(key: String)(code: => Task[T])
                            (using %, /)
                            (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                      `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                      ^ : String): ZStream[Any, Throwable, Unit] =
        apply[S](1)(rate, value)(key).tap(_ => code)

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S, T](_4: 4)(rate: Rate, pace: Duration, value: => Task[S])(key: String)(code: => Task[T])
                            (using %, /)
                            (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                      `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                      ^ : String): ZStream[Any, Throwable, Unit] =
        apply[S](2)(rate, pace, value)(key).tap(_ => code)

    /**
      * input prefix
      */
    def apply(rate: Rate)(key: String)
             (using % : %, / : /)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): ZStream[Any, Throwable, `()`] =
      for
        _        <- ZStream.fromZIO(exclude(key))
        promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
        timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
        _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (timestamp, (`()`[><], Some(true), rate)))))
        cb_token <- ZStream.fromZIO(promise.await)
        if cb_token ne None
        (cbarrier, token) = cb_token.get
        tks = ZStream.fromZIO(enable(key) *> cbarrier.await.exit).as(token)
        it <- tks.`zipRight s.head`
      yield
        it

    /**
      * input prefix w/ pace
      */
    def apply(rate: Rate, pace: Duration)(key: String)
             (using %, /)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): ZStream[Any, Throwable, `()`] =
      apply(rate)(key) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

    /**
      * input prefix w/ code
      */
    def apply[T](rate: Rate)(key: String)(code: T => Task[T])
                (using %, /)
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): ZStream[Any, Throwable, `()`] =
      apply(rate)(key).map(_.`()`[T]).mapZIO(code(_).map(new `()`(_)))

    /**
      * input prefix w/ pace w/ code
      */
    def apply[T](rate: Rate, pace: Duration)(key: String)(code: T => Task[T])
                (using %, /)
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): ZStream[Any, Throwable, `()`] =
      apply(rate, pace)(key).map(_.`()`[T]).mapZIO(code(_).map(new `()`(_)))

    override def toString: String = if name == null then "null" else name.toString

  }


  private object `Π-magic`:

    case class ><(hub: Hub[(`()`, Object)],
                  limit: Semaphore[Task])

    extension [O](self: ZStream[Any, Throwable, O])
      def through1(hub: Hub[O])
                  (using await: Task[Unit]): ZStream[Any, Throwable, O] =
        self.mapZIO { it => await *> hub.publish(it).map(it -> _) }.takeWhile(_._2).map(_._1)
