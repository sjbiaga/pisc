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

package object sΠ:

  import _root_.scala.collection.immutable.{ Map, Set }

  import _root_.zio.{ Clock, Duration, Hub, Promise, Ref, Queue, Schedule, Task, UIO, ZIO }
  import _root_.zio.concurrent.CyclicBarrier
  import _root_.zio.stream.ZStream

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
                        (using % : %, \ : \): UIO[Unit] =
    `π-exclude`(Set.from(enabled)) *> \()

  private def `π-exclude`(enabled: `Π-Set`[String])
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
    if `π-elvis`.contains(key)
    then
      `π-exclude`(`π-elvis`(key))
    else
      ZIO.unit


  /**
    * restriction aka new name
    */
  object ν:

    def map[B](f: `()` => B): ZStream[Any, Throwable, B] = flatMap(f andThen ZStream.succeed)
    def flatMap[B](f: `()` => ZStream[Any, Throwable, B]): ZStream[Any, Throwable, B] =
      ( for
          hub   <- ZStream.fromZIO(Hub.unbounded[(`()`, Object)])
          queue <- ZStream.fromZIO(Queue.unbounded[Unit])
          limit <- ZStream.fromZIO(Ref.make(false))
        yield
          f(><(hub, queue, limit))
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
        def apply(rate: Rate)(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                 (using % : %, / : /, \ : \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, Unit] =
          for
            _        <- ( for
                            discard <- if None eq + then ZStream.fromZIO(exclude(key)) *> ZStream.succeed(false)
                                       else ZStream.fromZIO(?.await)
                            _       <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
                          yield
                            ()
                        )
            discard  <- if None eq + then ZStream.succeed(false)
                        else ZStream.fromZIO(?.await)
            if !discard
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            continue <- ZStream.fromZIO(Ref.make(promise))
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            _        <- if None eq + then ZStream.unit else ZStream.fromZIO(promise.succeed(None))
            now      <- ZStream.fromZIO(Clock.nanoTime)
            timestamp <- ZStream.fromZIO(Ref.make(now))
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (new Object, None, rate)))))
            cb_token <- ZStream.fromZIO(promise.await)
            _        <- if None eq + then ZStream.fromZIO(?.succeed(cb_token eq None)) else ZStream.unit
            discard  <- for
                          discard <- ZStream.fromZIO(?.await)
                          _       <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
                        yield
                          discard
            if !discard
            sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
            _  <- ZStream.fromZIO {
              for
                _        <- -.await.exit
                _        <- +.fold(ZIO.unit)(_.take)
                now      <- Clock.nanoTime
                enabled  <- %.modify { m => m(^ + key).asInstanceOf[(Boolean, +)]._1 -> m }
                _        <- if enabled then ZIO.unit else timestamp.set(now)
                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                promise  <- continue.get
                cb_token <- promise.await
                promise  <- Promise.make[Throwable, Option[<>]]
                _        <- continue.set(promise)
                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                _        <- if cb_token eq None then sp.succeed(())
                            else
                              val (cbarrier, _) = cb_token.get
                              enable(key) *> cbarrier.await.exit
              yield
                ()
            }.repeat(Schedule.forever).interruptWhen(sp)
            _  <- ZStream.fromZIO(*.fold(ZIO.unit)(_.offer(())))
          yield
            ()

        /**
          * linear replication guard w/ pace
          */
        def apply(rate: Rate, pace: Duration)(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                 (using %, /, \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate)(key)(?, -, +, *) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

        /**
          * linear replication guard w/ code
          */
        def apply[T](rate: Rate)(key: String)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate)(key)(?, -, +, *).tap(_ => code)

        /**
          * linear replication guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: Duration)(key: String)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate, pace)(key)(?, -, +, *).tap(_ => code)

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
          continue <- ZStream.fromZIO(Ref.make(promise))
          promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
          now      <- ZStream.fromZIO(Clock.nanoTime)
          timestamp <- ZStream.fromZIO(Ref.make(now))
          _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (new Object, None, rate)))))
          cb_token <- ZStream.fromZIO(promise.await)
          if cb_token ne None
          sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
          _  <- ZStream.fromZIO {
            for
              now      <- Clock.nanoTime
              enabled  <- %.modify { m => m(^ + key).asInstanceOf[(Boolean, +)]._1 -> m }
              _        <- if enabled then ZIO.unit else timestamp.set(now)
              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
              promise  <- continue.get
              cb_token <- promise.await
              promise  <- Promise.make[Throwable, Option[<>]]
              _        <- continue.set(promise)
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
        now      <- ZStream.fromZIO(Clock.nanoTime)
        timestamp <- ZStream.fromZIO(Ref.make(now))
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
    private inline def q = `()`[><].queue
    private inline def r = `()`[><].limit
    private implicit def a: UIO[Unit] = q.take *> r.set(false)
    private def o =
      for
        b <- r.get
        s <- q.size
        _ <- if !b || s == 0 then q.offer(()) *> r.set(true) else ZIO.unit
      yield
        ()
    private def s(tk: Object) = ZStream.unwrapScoped(ZStream.fromHubScoped(h).tap(_ => o)).filter(_._2 eq tk).map(_._1)

    def ====(that: `()`) =
      try
        this.h eq that.h
      catch
        case _ =>
          this.name == that.name

    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

    lazy val `null` = new `()`(null)

    object ! :

      object + :

        object ν:

          /**
            * linear replication bound output guard
            */
          def apply(rate: Rate)(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                   (using % : %, / : /, \ : \)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): ZStream[Any, Throwable, `()`] =
            for
              _        <- ( for
                              discard <- if None eq + then ZStream.fromZIO(exclude(key)) *> ZStream.succeed(false)
                                         else ZStream.fromZIO(?.await)
                              _       <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
                            yield
                              ()
                          )
              discard  <- if None eq + then ZStream.succeed(false)
                          else ZStream.fromZIO(?.await)
              if !discard
              promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
              continue <- ZStream.fromZIO(Ref.make(promise))
              promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
              _        <- if None eq + then ZStream.unit else ZStream.fromZIO(promise.succeed(None))
              now      <- ZStream.fromZIO(Clock.nanoTime)
              timestamp <- ZStream.fromZIO(Ref.make(now))
              _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`()`[><], Some(false), rate)))))
              cb_token <- ZStream.fromZIO(promise.await)
              _        <- if None eq + then ZStream.fromZIO(?.succeed(cb_token eq None)) else ZStream.unit
              discard  <- for
                            discard <- ZStream.fromZIO(?.await)
                            _       <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
                          yield
                            discard
              if !discard
              sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
              it <- ( for
                        _  <- ZStream.unit.repeat(Schedule.forever)
                        it <- sΠ.ν
                        it <- ZStream.fromZIO {
                          for
                            _        <- -.await.exit
                            _        <- +.fold(ZIO.unit)(_.take)
                            now      <- Clock.nanoTime
                            enabled  <- %.modify { m => m(^ + key).asInstanceOf[(Boolean, +)]._1 -> m }
                            _        <- if enabled then ZIO.unit else timestamp.set(now)
                            _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                            promise  <- continue.get
                            cb_token <- promise.await
                            promise  <- Promise.make[Throwable, Option[<>]]
                            _        <- continue.set(promise)
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
              _  <- ZStream.fromZIO(*.fold(ZIO.unit)(_.offer(())))
            yield
              it._1

          /**
            * linear replication bound output guard w/ pace
            */
          def apply(rate: Rate, pace: Duration)(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                   (using %, /, \)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): ZStream[Any, Throwable, `()`] =
            apply(rate)(key)(?, -, +, *) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

          /**
            * linear replication bound output guard w/ code
            */
          def apply[T](rate: Rate)(key: String)(code: Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                      (using %, /, \)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, `()`] =
            apply(rate)(key)(?, -, +, *).tap(_ => code)

          /**
            * linear replication bound output guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: Duration)(key: String)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                      (using %, /, \)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, `()`] =
            apply(rate, pace)(key)(?, -, +, *).tap(_ => code)

        /**
          * linear constant replication output guard
          */
        def apply(rate: Rate, value: `()`)(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                 (using % : %, / : /, \ : \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, Unit] =
          for
            _        <- ZStream.fromZIO(ZIO.debug(0->"lin const out"))
            _        <- ( for
                            discard <- if None eq + then ZStream.fromZIO(exclude(key)) *> ZStream.succeed(false)
                                       else ZStream.fromZIO(?.await)
                            _       <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
                          yield
                            ()
                        )
            _        <- ZStream.fromZIO(ZIO.debug(1->"lin const out"))
            discard  <- if None eq + then ZStream.succeed(false)
                        else ZStream.fromZIO(?.await)
            if !discard
            _        <- ZStream.fromZIO(ZIO.debug(2->"lin const out"))
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            continue <- ZStream.fromZIO(Ref.make(promise))
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            _        <- if None eq + then ZStream.unit else ZStream.fromZIO(promise.succeed(None))
            now      <- ZStream.fromZIO(Clock.nanoTime)
            timestamp <- ZStream.fromZIO(Ref.make(now))
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`()`[><], Some(false), rate)))))
            cb_token <- ZStream.fromZIO(promise.await)
            _        <- if None eq + then ZStream.fromZIO(?.succeed(cb_token eq None)) else ZStream.unit
            discard  <- for
                          discard <- ZStream.fromZIO(?.await)
                          _       <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
                        yield
                          discard
            if !discard
            sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
            _  <- ZStream.fromZIO {
              for
                _        <- -.await.exit
                _        <- +.fold(ZIO.unit)(_.take)
                now      <- Clock.nanoTime
                enabled  <- %.modify { m => m(^ + key).asInstanceOf[(Boolean, +)]._1 -> m }
                _        <- if enabled then ZIO.unit else timestamp.set(now)
                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                promise  <- continue.get
                cb_token <- promise.await
                promise  <- Promise.make[Throwable, Option[<>]]
                _        <- continue.set(promise)
                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                token    <- if cb_token eq None then sp.succeed(()).as(null)
                            else
                              val (cbarrier, token) = cb_token.get
                              (enable(key) *> cbarrier.await.exit).as(token)
              yield
                value -> token
            }.repeat(Schedule.forever).interruptWhen(sp).through1(h)
            _  <- ZStream.fromZIO(*.fold(ZIO.unit)(_.offer(())))
          yield
            ()

        /**
          * linear constant replication output guard w/ pace
          */
        def apply(rate: Rate, pace: Duration, value: `()`)(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                 (using %, /, \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate, value)(key)(?, -, +, *) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

        /**
          * linear constant replication output guard w/ code
          */
        def apply[T](rate: Rate, value: `()`)(key: String)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate, value)(key)(?, -, +, *).tap(_ => code)

        /**
          * linear constant replication output guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: Duration, value: `()`)(key: String)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate, pace, value)(key)(?, -, +, *).tap(_ => code)

        object * :

          /**
            * linear variable replication output guard
            */
          def apply[S](rate: Rate, value: => S)(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                      (using %, /, \)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, Unit] =
           apply[S](rate, ZIO.attempt(value))(key)(?, -, +, *)

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S](rate: Rate, pace: Duration, value: => S)(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                      (using %, /, \)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, Unit] =
           apply[S](rate, pace, ZIO.attempt(value))(key)(?, -, +, *)

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S, T](rate: Rate, value: => S)(key: String)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                         (using %, /, \)
                         (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                   `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                   ^ : String): ZStream[Any, Throwable, Unit] =
           apply[S, T](rate, ZIO.attempt(value))(key)(code)(?, -, +, *)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S, T](rate: Rate, pace: Duration, value: => S)(key: String)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                         (using %, /, \)
                         (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                   `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                   ^ : String): ZStream[Any, Throwable, Unit] =
           apply[S, T](rate, pace, ZIO.attempt(value))(key)(code)(?, -, +, *)

          /**
            * linear variable replication output guard
            */
          @annotation.targetName("applyF")
          def apply[S](rate: Rate, value: => Task[S])(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                      (using % : %, / : /, \ : \)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, Unit] =
            for
              _        <- ( for
                              discard <- if None eq + then ZStream.fromZIO(exclude(key)) *> ZStream.succeed(false)
                                         else ZStream.fromZIO(?.await)
                              _       <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
                            yield
                              ()
                          )
              discard  <- if None eq + then ZStream.succeed(false)
                          else ZStream.fromZIO(?.await)
              if !discard
              promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
              continue <- ZStream.fromZIO(Ref.make(promise))
              promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
              _        <- if None eq + then ZStream.unit else ZStream.fromZIO(promise.succeed(None))
              now      <- ZStream.fromZIO(Clock.nanoTime)
              timestamp <- ZStream.fromZIO(Ref.make(now))
              _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`()`[><], Some(false), rate)))))
              cb_token <- ZStream.fromZIO(promise.await)
              _        <- if None eq + then ZStream.fromZIO(?.succeed(cb_token eq None)) else ZStream.unit
              discard  <- for
                            discard <- ZStream.fromZIO(?.await)
                            _       <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
                          yield
                            discard
              if !discard
              sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
              _  <- ZStream.fromZIO {
                for
                  _        <- -.await.exit
                  _        <- +.fold(ZIO.unit)(_.take)
                  now      <- Clock.nanoTime
                  enabled  <- %.modify { m => m(^ + key).asInstanceOf[(Boolean, +)]._1 -> m }
                  _        <- if enabled then ZIO.unit else timestamp.set(now)
                  _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                  promise  <- continue.get
                  cb_token <- promise.await
                  promise  <- Promise.make[Throwable, Option[<>]]
                  _        <- continue.set(promise)
                  _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                  it       <- if cb_token eq None then sp.succeed(()).as(`null` -> null)
                              else
                                val (cbarrier, token) = cb_token.get
                                value.map(new `()`(_) -> token).tap(_ => enable(key) *> cbarrier.await.exit)
                yield
                  it
              }.repeat(Schedule.forever).interruptWhen(sp).through1(h)
              _  <- ZStream.fromZIO(*.fold(ZIO.unit)(_.offer(())))
            yield
              ()

          /**
            * linear variable replication output guard w/ pace
            */
          @annotation.targetName("applyF")
          def apply[S](rate: Rate, pace: Duration, value: => Task[S])(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                      (using %, /, \)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, Unit] =
            apply[S](rate, value)(key)(?, -, +, *) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

          /**
            * linear variable replication output guard w/ code
            */
          @annotation.targetName("applyF")
          def apply[S, T](rate: Rate, value: => Task[S])(key: String)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                         (using %, /, \)
                         (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                   `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                   ^ : String): ZStream[Any, Throwable, Unit] =
            apply[S](rate, value)(key)(?, -, +, *).tap(_ => code)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          @annotation.targetName("applyF")
          def apply[S, T](rate: Rate, pace: Duration, value: => Task[S])(key: String)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                         (using %, /, \)
                         (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                   `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                   ^ : String): ZStream[Any, Throwable, Unit] =
            apply[S](rate, pace, value)(key)(?, -, +, *).tap(_ => code)

        /**
          * linear replication input guard
          */
        def apply(rate: Rate)(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                 (using % : %, / : /, \ : \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, `()`] =
          for
            _        <- ( for
                            discard <- if None eq + then ZStream.fromZIO(exclude(key)) *> ZStream.succeed(false)
                                       else ZStream.fromZIO(?.await)
                            _       <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
                          yield
                            ()
                        )
            discard  <- if None eq + then ZStream.succeed(false)
                        else ZStream.fromZIO(?.await)
            if !discard
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            continue <- ZStream.fromZIO(Ref.make(promise))
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            _        <- if None eq + then ZStream.unit else ZStream.fromZIO(promise.succeed(None))
            now      <- ZStream.fromZIO(Clock.nanoTime)
            timestamp <- ZStream.fromZIO(Ref.make(now))
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`()`[><], Some(true), rate)))))
            cb_token <- ZStream.fromZIO(promise.await)
            _        <- if None eq + then ZStream.fromZIO(?.succeed(cb_token eq None)) else ZStream.unit
            discard  <- for
                          discard <- ZStream.fromZIO(?.await)
                          _       <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
                        yield
                          discard
            if !discard
            sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
            tk <- ZStream.fromZIO {
              for
                _        <- -.await.exit
                _        <- +.fold(ZIO.unit)(_.take)
                now      <- Clock.nanoTime
                enabled  <- %.modify { m => m(^ + key).asInstanceOf[(Boolean, +)]._1 -> m }
                _        <- if enabled then ZIO.unit else timestamp.set(now)
                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                promise  <- continue.get
                cb_token <- promise.await
                promise  <- Promise.make[Throwable, Option[<>]]
                _        <- continue.set(promise)
                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                token    <- if cb_token eq None then sp.succeed(()).as(null)
                            else
                              val (cbarrier, token) = cb_token.get
                              (enable(key) *> cbarrier.await.exit).as(token)
              yield
                token
            }.repeat(Schedule.forever).interruptWhen(sp)
            it <- s(tk).take(1)
            _  <- ZStream.fromZIO(*.fold(ZIO.unit)(_.offer(())))
          yield
            it

        /**
          * linear replication input guard w/ pace
          */
        def apply(rate: Rate, pace: Duration)(key: String)(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                 (using %, /, \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, `()`] =
          apply(rate)(key)(?, -, +, *) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

        /**
          * linear replication input guard w/ code
          */
        def apply[T](rate: Rate)(key: String)(code: T => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, `()`] =
          apply(rate)(key)(?, -, +, *).mapZIO { it => code(it.`()`[T]).map(new `()`(_)) }

        /**
          * linear replication input guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: Duration)(key: String)(code: T => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, + : Option[Queue[Unit]], * : Option[Queue[Unit]])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, `()`] =
          apply(rate, pace)(key)(?, -, +, *).mapZIO { it => code(it.`()`[T]).map(new `()`(_)) }

      object ν:

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
            continue <- ZStream.fromZIO(Ref.make(promise))
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            now      <- ZStream.fromZIO(Clock.nanoTime)
            timestamp <- ZStream.fromZIO(Ref.make(now))
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`()`[><], Some(false), rate)))))
            cb_token <- ZStream.fromZIO(promise.await)
            if cb_token ne None
            sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
            it <- ( for
                      _  <- ZStream.unit.repeat(Schedule.forever)
                      it <- sΠ.ν
                      it <- ZStream.fromZIO {
                        for
                          now      <- Clock.nanoTime
                          enabled  <- %.modify { m => m(^ + key).asInstanceOf[(Boolean, +)]._1 -> m }
                          _        <- if enabled then ZIO.unit else timestamp.set(now)
                          _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                          promise  <- continue.get
                          cb_token <- promise.await
                          promise  <- Promise.make[Throwable, Option[<>]]
                          _        <- continue.set(promise)
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
          continue <- ZStream.fromZIO(Ref.make(promise))
          promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
          now      <- ZStream.fromZIO(Clock.nanoTime)
          timestamp <- ZStream.fromZIO(Ref.make(now))
          _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`()`[><], Some(false), rate)))))
          cb_token <- ZStream.fromZIO(promise.await)
          if cb_token ne None
          sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
          _  <- ZStream.fromZIO {
            for
              now      <- Clock.nanoTime
              enabled  <- %.modify { m => m(^ + key).asInstanceOf[(Boolean, +)]._1 -> m }
              _        <- if enabled then ZIO.unit else timestamp.set(now)
              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
              promise  <- continue.get
              cb_token <- promise.await
              promise  <- Promise.make[Throwable, Option[<>]]
              _        <- continue.set(promise)
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

      object * :

        /**
          * variable replication output guard
          */
        def apply[S](rate: Rate, value: => S)(key: String)
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, Unit] =
         apply[S](rate, ZIO.attempt(value))(key)

        /**
          * variable replication output guard w/ pace
          */
        def apply[S](rate: Rate, pace: Duration, value: => S)(key: String)
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, Unit] =
         apply[S](rate, pace, ZIO.attempt(value))(key)

        /**
          * variable replication output guard w/ code
          */
        def apply[S, T](rate: Rate, value: => S)(key: String)(code: => Task[T])
                       (using %, /, \)
                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                 ^ : String): ZStream[Any, Throwable, Unit] =
         apply[S, T](rate, ZIO.attempt(value))(key)(code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S, T](rate: Rate, pace: Duration, value: => S)(key: String)(code: => Task[T])
                       (using %, /, \)
                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                 ^ : String): ZStream[Any, Throwable, Unit] =
         apply[S, T](rate, pace, ZIO.attempt(value))(key)(code)

        /**
          * variable replication output guard
          */
        @annotation.targetName("applyF")
        def apply[S](rate: Rate, value: => Task[S])(key: String)
                    (using % : %, / : /, \ : \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, Unit] =
          for
            _        <- ZStream.fromZIO(exclude(key))
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            continue <- ZStream.fromZIO(Ref.make(promise))
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            now      <- ZStream.fromZIO(Clock.nanoTime)
            timestamp <- ZStream.fromZIO(Ref.make(now))
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`()`[><], Some(false), rate)))))
            cb_token <- ZStream.fromZIO(promise.await)
            if cb_token ne None
            sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
            _  <- ZStream.fromZIO {
              for
                now      <- Clock.nanoTime
                enabled  <- %.modify { m => m(^ + key).asInstanceOf[(Boolean, +)]._1 -> m }
                _        <- if enabled then ZIO.unit else timestamp.set(now)
                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                promise  <- continue.get
                cb_token <- promise.await
                promise  <- Promise.make[Throwable, Option[<>]]
                _        <- continue.set(promise)
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
        @annotation.targetName("applyF")
        def apply[S](rate: Rate, pace: Duration, value: => Task[S])(key: String)
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, Unit] =
          apply[S](rate, value)(key) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

        /**
          * variable replication output guard w/ code
          */
        @annotation.targetName("applyF")
        def apply[S, T](rate: Rate, value: => Task[S])(key: String)(code: => Task[T])
                       (using %, /, \)
                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                 ^ : String): ZStream[Any, Throwable, Unit] =
          apply[S](rate, value)(key).tap(_ => code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        @annotation.targetName("applyF")
        def apply[S, T](rate: Rate, pace: Duration, value: => Task[S])(key: String)(code: => Task[T])
                       (using %, /, \)
                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                 ^ : String): ZStream[Any, Throwable, Unit] =
          apply[S](rate, pace, value)(key).tap(_ => code)

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
          continue <- ZStream.fromZIO(Ref.make(promise))
          promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
          now      <- ZStream.fromZIO(Clock.nanoTime)
          timestamp <- ZStream.fromZIO(Ref.make(now))
          _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`()`[><], Some(true), rate)))))
          cb_token <- ZStream.fromZIO(promise.await)
          if cb_token ne None
          sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
          tk <- ZStream.fromZIO {
            for
              now      <- Clock.nanoTime
              enabled  <- %.modify { m => m(^ + key).asInstanceOf[(Boolean, +)]._1 -> m }
              _        <- if enabled then ZIO.unit else timestamp.set(now)
              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
              promise  <- continue.get
              cb_token <- promise.await
              promise  <- Promise.make[Throwable, Option[<>]]
              _        <- continue.set(promise)
              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
              token    <- if cb_token eq None then sp.succeed(()).as(null)
                          else
                            val (cbarrier, token) = cb_token.get
                            (enable(key) *> cbarrier.await.exit).as(token)
            yield
              token
          }.repeat(Schedule.forever).interruptWhen(sp)
          it <- s(tk).take(1)
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
        apply(rate)(key).mapZIO { it => code(it.`()`[T]).map(new `()`(_)) }

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: Duration)(key: String)(code: T => Task[T])
                  (using %, /, \)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, `()`] =
        apply(rate, pace)(key).mapZIO { it => code(it.`()`[T]).map(new `()`(_)) }

    object ν:

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
          now      <- ZStream.fromZIO(Clock.nanoTime)
          timestamp <- ZStream.fromZIO(Ref.make(now))
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
        _        <- ZStream.fromZIO(ZIO.debug(-300))
        now      <- ZStream.fromZIO(Clock.nanoTime)
        timestamp <- ZStream.fromZIO(Ref.make(now))
        _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (timestamp, (`()`[><], Some(false), rate)))))
        cb_token <- ZStream.fromZIO(promise.await)
        _        <- ZStream.fromZIO(ZIO.debug(300))
        if cb_token ne None
        (cbarrier, token) = cb_token.get
        _        <- ZStream.fromZIO(ZIO.debug(-600))
        _        <- ZStream.succeed(value -> token).tap(_ => enable(key) *> cbarrier.await.exit).through1(h)
        _        <- ZStream.fromZIO(ZIO.debug(600))
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

    object * :

      /**
        * variable output prefix
        */
      def apply[S](rate: Rate, value: => S)(key: String)
                  (using %, /)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, Unit] =
        apply[S](rate, ZIO.attempt(value))(key)

      /**
        * variable output prefix w/ pace
        */
      def apply[S](rate: Rate, pace: Duration, value: => S)(key: String)
                  (using %, /)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, Unit] =
        apply[S](rate, value)(key) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * variable output prefix w/ code
        */
      def apply[S, T](rate: Rate, value: => S)(key: String)(code: => Task[T])
                     (using %, /)
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                               ^ : String): ZStream[Any, Throwable, Unit] =
        apply[S](rate, value)(key).tap(_ => code)

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S, T](rate: Rate, pace: Duration, value: => S)(key: String)(code: => Task[T])
                     (using %, /)
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                               ^ : String): ZStream[Any, Throwable, Unit] =
        apply[S](rate, pace, value)(key).tap(_ => code)

      /**
        * variable output prefix
        */
      @annotation.targetName("applyF")
      def apply[S](rate: Rate, value: => Task[S])(key: String)
                  (using % : %, / : /)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, Unit] =
        for
          _        <- ZStream.fromZIO(exclude(key))
          promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
          now      <- ZStream.fromZIO(Clock.nanoTime)
          timestamp <- ZStream.fromZIO(Ref.make(now))
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
      @annotation.targetName("applyF")
      def apply[S](rate: Rate, pace: Duration, value: => Task[S])(key: String)
                  (using %, /)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, Unit] =
        apply[S](rate, value)(key) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * variable output prefix w/ code
        */
      @annotation.targetName("applyF")
      def apply[S, T](rate: Rate, value: => Task[S])(key: String)(code: => Task[T])
                     (using %, /)
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                               ^ : String): ZStream[Any, Throwable, Unit] =
        apply[S](rate, value)(key).tap(_ => code)

      /**
        * variable output prefix w/ pace w/ code
        */
      @annotation.targetName("applyF")
      def apply[S, T](rate: Rate, pace: Duration, value: => Task[S])(key: String)(code: => Task[T])
                     (using %, /)
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                               ^ : String): ZStream[Any, Throwable, Unit] =
        apply[S](rate, pace, value)(key).tap(_ => code)

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
        _        <- ZStream.fromZIO(ZIO.debug(-400))
        now      <- ZStream.fromZIO(Clock.nanoTime)
        timestamp <- ZStream.fromZIO(Ref.make(now))
        _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (timestamp, (`()`[><], Some(true), rate)))))
        cb_token <- ZStream.fromZIO(promise.await)
        _        <- ZStream.fromZIO(ZIO.debug(400))
        if cb_token ne None
        (cbarrier, token) = cb_token.get
        _  <- ZStream.fromZIO(ZIO.debug(-800))
        _  <- ZStream.fromZIO(enable(key) *> cbarrier.await.exit)
        it <- s(token).take(1)
        _  <- ZStream.fromZIO(ZIO.debug(800))
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
      apply(rate)(key).mapZIO { it => code(it.`()`[T]).map(new `()`(_)) }

    /**
      * input prefix w/ pace w/ code
      */
    def apply[T](rate: Rate, pace: Duration)(key: String)(code: T => Task[T])
                (using %, /)
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): ZStream[Any, Throwable, `()`] =
      apply(rate, pace)(key).mapZIO { it => code(it.`()`[T]).map(new `()`(_)) }

    override def toString: String = if name == null then "null" else name.toString

  }


  private object `Π-magic`:

    case class ><(hub: Hub[(`()`, Object)],
                  queue: Queue[Unit],
                  limit: Ref[Boolean])

    extension [O](self: ZStream[Any, Throwable, O])
      def through1(hub: Hub[O])
                  (using await: Task[Unit]): ZStream[Any, Throwable, O] =
        self.mapZIO { it => await *> hub.publish(it).map(it -> _) }.takeWhile(_._2).map(_._1)
