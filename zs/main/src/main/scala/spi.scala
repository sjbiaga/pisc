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

  import _root_.scala.reflect.{ ClassTag, classTag }

  import _root_.cats.effect.std.Semaphore

  import _root_.zio.{ Duration, Exit, Promise, Ref, Schedule, Task, UIO, ZIO }
  import _root_.zio.concurrent.CyclicBarrier
  import _root_.zio.stream.ZStream

  import `Π-loop`.{ <>, +, %, /, \ }
  import `Π-stats`.Rate


  type `Π-Map`[K, +V] = Map[K, V]

  type `Π-Set`[A] = Set[A]

  type `Π-Function0` = () => String ?=> ZStream[Any, Nothing, Unit]
  type `Π-Function1` = `()` => String ?=> ZStream[Any, Nothing, Unit]


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


  inline def `π-exclude`(enabled: String*)
                        (using % : %, \ : \): UIO[Unit] =
    \(`π-exclude`(Set.from(enabled)))

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
    ZIO.when(`π-elvis`.contains(key))(`π-exclude`(`π-elvis`(key))).unit


  /**
    * restriction aka new name
    */
  object ν:

    def map[B](f: `()` => B): ZStream[Any, Nothing, B] = flatMap(f andThen ZStream.succeed)
    def flatMap[B](f: `()` => ZStream[Any, Nothing, B]): ZStream[Any, Nothing, B] = f(new {})


  /**
    * silent transition
    */
  object τ:

    private val `new {}` = new {}

    object `(!)`:

      object `(+)`:

        /**
          * linear replication guard
          */
        def apply(rate: Rate)(key: String)(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                 (using %, /, \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Nothing, Unit] =
        apply(rate, Duration.Zero)(key)(?, -, *, +)

        /**
          * linear replication guard w/ pace
          */
        def apply(rate: Rate, pace: Duration)(key: String)(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                 (using % : %, / : /, \ : \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Nothing, Unit] =
          for
            discard  <- if None eq * then ZStream.fromZIO(exclude(key)) *> ZStream.succeed(false)
                        else ZStream.fromZIO(?.await)
            _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
            if !discard
            promise  <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]])
            continue <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]].flatMap(Ref.make))
            _        <- if None eq * then ZStream.unit
                        else ZStream.fromZIO(promise.succeed(None))
            enabled  <- ZStream.fromZIO(promise.isDone.negate.flatMap(Ref.make))
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`new {}`, None, rate))))
            cb_fb_in <- ZStream.fromZIO(promise.await)
            discard  <- if None eq * then ZStream.fromZIO(?.succeed(cb_fb_in eq None) *> ?.await)
                        else ZStream.succeed(false)
            _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
            if !discard
            sp <- ZStream.fromZIO(Promise.make[Nothing, Unit])
            _  <- ZStream.fromZIO {
              for
                _        <- -.await.exit
                _        <- *.fold(ZIO.unit)(_.acquire)
                _        <- \(%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }).unlessZIO(enabled.get)
                cb_fb_in <- continue.get.flatMap(_.await)
                _        <- Promise.make[Nothing, Option[<>]].flatMap(continue.set)
                _        <- enabled.set(false)
                _        <- if cb_fb_in eq None then sp.succeed(())
                            else
                              val (cbarrier, fiber, _) = cb_fb_in.get
                              cbarrier.await.exit *> fiber.join
              yield
                ()
            }.repeat(Schedule.forever).interruptWhen(sp)
            _  <- ZStream.fromZIO(ZIO.sleep(pace))
            _  <- ZStream.fromZIO(+.release)
            _  <- ZStream.unit.whenZIO(sp.isDone.negate)
          yield
            ()

        /**
          * linear replication guard w/ code
          */
        def apply[T](rate: Rate)(key: String)(code: => Task[T])(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Nothing, Unit] =
          apply(rate)(key)(?, -, *, +).tap(_ => code)

        /**
          * linear replication guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: Duration)(key: String)(code: => Task[T])(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Nothing, Unit] =
          apply(rate, pace)(key)(?, -, *, +).tap(_ => code)

      /**
        * replication guard
        */
      def apply(rate: Rate)(key: String)
               (using % : %, / : /, \ : \)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Nothing, Unit] =
        for
          _        <- ZStream.fromZIO(exclude(key))
          promise  <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]])
          continue <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]].flatMap(Ref.make))
          enabled  <- ZStream.fromZIO(Ref.make(true))
          _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`new {}`, None, rate))))
          cb_fb_in <- ZStream.fromZIO(promise.await)
          if cb_fb_in ne None
          sp <- ZStream.fromZIO(Promise.make[Nothing, Unit])
          _  <- ZStream.fromZIO {
            for
              _        <- \(%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }).unlessZIO(enabled.get)
              cb_fb_in <- continue.get.flatMap(_.await)
              _        <- Promise.make[Nothing, Option[<>]].flatMap(continue.set)
              _        <- enabled.set(false)
              _        <- if cb_fb_in eq None then sp.succeed(())
                          else
                            val (cbarrier, fiber, _) = cb_fb_in.get
                            cbarrier.await.exit *> fiber.join
            yield
              ()
          }.repeat(Schedule.forever).interruptWhen(sp)
          _  <- ZStream.unit.whenZIO(sp.isDone.negate)
        yield
          ()

      /**
        * replication guard w/ pace
        */
      def apply(rate: Rate, pace: Duration)(key: String)
               (using %, /, \)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Nothing, Unit] =
        apply(rate)(key) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

      /**
        * replication guard w/ code
        */
      def apply[T](rate: Rate)(key: String)(code: => Task[T])
                  (using %, /, \)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Nothing, Unit] =
        apply(rate)(key).tap(_ => code)

      /**
        * replication guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: Duration)(key: String)(code: => Task[T])
                  (using %, /, \)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Nothing, Unit] =
        apply(rate, pace)(key).tap(_ => code)

    /**
      * prefix
      */
    def apply(rate: Rate)(key: String)
             (using % : %, / : /)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): ZStream[Any, Nothing, Unit] =
      for
        _        <- ZStream.fromZIO(exclude(key))
        promise  <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]])
        _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (`new {}`, None, rate))))
        cb_fb_in <- ZStream.fromZIO(promise.await)
        if cb_fb_in ne None
        (cbarrier, fiber, _) = cb_fb_in.get
        _  <- ZStream.fromZIO(cbarrier.await.exit *> fiber.join)
      yield
        ()

    /**
      * prefix w/ pace
      */
    def apply(rate: Rate, pace: Duration)(key: String)
             (using %, /)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): ZStream[Any, Nothing, Unit] =
      apply(rate)(key) <* ZStream.fromZIO(ZIO.sleep(pace))

    /**
      * prefix w/ code
      */
    def apply[T](rate: Rate)(key: String)(code: => Task[T])
                (using %, /)
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): ZStream[Any, Nothing, Unit] =
      apply(rate)(key).tap(_ => code)

    /**
      * prefix w/ pace w/ code
      */
    def apply[T](rate: Rate, pace: Duration)(key: String)(code: => Task[T])
                (using %, /)
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): ZStream[Any, Nothing, Unit] =
      apply(rate, pace)(key).tap(_ => code)


  /**
    * names and values
    */
  implicit final class `()`(private val name: Any) { self =>

    def ====(that: `()`) = this.name == that.name

    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

    object `(!)`:

      object `(+)`:

        object `(ν)`:

          /**
            * linear replication bound output guard
            */
          def apply(rate: Rate)(key: String)(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                   (using %, /, \)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): ZStream[Any, Nothing, `()`] =
            apply(rate, Duration.Zero)(key)(?, -, *, +)

          /**
            * linear replication bound output guard w/ pace
            */
          def apply(rate: Rate, pace: Duration)(key: String)(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                   (using % : %, / : /, \ : \)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): ZStream[Any, Nothing, `()`] =
            for
              discard  <- if None eq * then ZStream.fromZIO(exclude(key)) *> ZStream.succeed(false)
                          else ZStream.fromZIO(?.await)
              _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
              if !discard
              promise  <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]])
              continue <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]].flatMap(Ref.make))
              _        <- if None eq * then ZStream.unit
                          else ZStream.fromZIO(promise.succeed(None))
              enabled  <- ZStream.fromZIO(promise.isDone.negate.flatMap(Ref.make))
              _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`()`[{}], Some(Left(())), rate))))
              cb_fb_in <- ZStream.fromZIO(promise.await)
              discard  <- if None eq * then ZStream.fromZIO(?.succeed(cb_fb_in eq None) *> ?.await)
                          else ZStream.succeed(false)
              _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
              if !discard
              sp <- ZStream.fromZIO(Promise.make[Nothing, Unit])
              it <- ( for
                        _  <- ZStream.unit.repeat(Schedule.forever)
                        it <- sΠ.ν
                        _  <- ZStream.fromZIO {
                          for
                            _        <- -.await.exit
                            _        <- *.fold(ZIO.unit)(_.acquire)
                            _        <- \(%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }).unlessZIO(enabled.get)
                            cb_fb_in <- continue.get.flatMap(_.await)
                            _        <- Promise.make[Nothing, Option[<>]].flatMap(continue.set)
                            _        <- enabled.set(false)
                            _        <- if cb_fb_in eq None then sp.succeed(())
                                        else
                                          val (cbarrier, fiber, input) = cb_fb_in.get
                                          input.set(it) *> cbarrier.await.exit *> fiber.join
                          yield
                            ()
                         }
                       yield
                         it
                    ).interruptWhen(sp)
              _  <- ZStream.fromZIO(ZIO.sleep(pace))
              _  <- ZStream.fromZIO(+.release)
              _  <- ZStream.unit.whenZIO(sp.isDone.negate)
            yield
              it

          /**
            * linear replication bound output guard w/ code
            */
          def apply[T](rate: Rate)(key: String)(code: Task[T])(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                      (using %, /, \)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Nothing, `()`] =
            apply(rate)(key)(?, -, *, +).tap(_ => code)

          /**
            * linear replication bound output guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: Duration)(key: String)(code: => Task[T])(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                      (using %, /, \)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Nothing, `()`] =
            apply(rate, pace)(key)(?, -, *, +).tap(_ => code)

        /**
          * linear constant replication output guard
          */
        def apply(rate: Rate, value: `()`)(key: String)(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                 (using %, /, \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Nothing, Unit] =
          apply(rate, Duration.Zero, value)(key)(?, -, *, +)

        /**
          * linear constant replication output guard w/ pace
          */
        def apply(rate: Rate, pace: Duration, value: `()`)(key: String)(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                 (using % : %, / : /, \ : \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Nothing, Unit] =
          for
            discard  <- if None eq * then ZStream.fromZIO(exclude(key)) *> ZStream.succeed(false)
                        else ZStream.fromZIO(?.await)
            _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
            if !discard
            promise  <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]])
            continue <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]].flatMap(Ref.make))
            _        <- if None eq * then ZStream.unit
                        else ZStream.fromZIO(promise.succeed(None))
            enabled  <- ZStream.fromZIO(promise.isDone.negate.flatMap(Ref.make))
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`()`[{}], Some(Left(())), rate))))
            cb_fb_in <- ZStream.fromZIO(promise.await)
            discard  <- if None eq * then ZStream.fromZIO(?.succeed(cb_fb_in eq None) *> ?.await)
                        else ZStream.succeed(false)
            _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
            if !discard
            sp <- ZStream.fromZIO(Promise.make[Nothing, Unit])
            _  <- ZStream.fromZIO {
              for
                _        <- -.await.exit
                _        <- *.fold(ZIO.unit)(_.acquire)
                _        <- \(%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }).unlessZIO(enabled.get)
                cb_fb_in <- continue.get.flatMap(_.await)
                _        <- Promise.make[Nothing, Option[<>]].flatMap(continue.set)
                _        <- enabled.set(false)
                _        <- if cb_fb_in eq None then sp.succeed(())
                            else
                              val (cbarrier, fiber, input) = cb_fb_in.get
                              input.set(value) *> cbarrier.await.exit *> fiber.join
              yield
                ()
            }.repeat(Schedule.forever).interruptWhen(sp)
            _  <- ZStream.fromZIO(ZIO.sleep(pace))
            _  <- ZStream.fromZIO(+.release)
            _  <- ZStream.unit.whenZIO(sp.isDone.negate)
          yield
            ()

        /**
          * linear constant replication output guard w/ code
          */
        def apply[T](rate: Rate, value: `()`)(key: String)(code: => Task[T])(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Nothing, Unit] =
          apply(rate, value)(key)(?, -, *, +).tap(_ => code)

        /**
          * linear constant replication output guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: Duration, value: `()`)(key: String)(code: => Task[T])(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Nothing, Unit] =
          apply(rate, pace, value)(key)(?, -, *, +).tap(_ => code)

        object `(*)`:

          /**
            * linear variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(rate: Rate, value: => S)(key: String)(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                                       (using DummyImplicit)
                                       (using %, /, \)
                                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                 ^ : String): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(rate, value.asInstanceOf[`()`])(key)(?, -, *, +)
            else
              apply[S](1)(rate, ZIO.attempt(value))(key)(?, -, *, +)

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(rate: Rate, pace: Duration, value: => S)(key: String)(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                                       (using DummyImplicit)
                                       (using %, /, \)
                                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                 ^ : String): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(rate, pace, value.asInstanceOf[`()`])(key)(?, -, *, +)
            else
              apply[S](2)(rate, pace, ZIO.attempt(value))(key)(?, -, *, +)

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => S)(key: String)(code: => Task[T])(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                                          (using DummyImplicit)
                                          (using %, /, \)
                                          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                    `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                    ^ : String): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(rate, value.asInstanceOf[`()`])(key)(code)(?, -, *, +)
            else
              apply[S, T](3)(rate, ZIO.attempt(value))(key)(code)(?, -, *, +)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: Duration, value: => S)(key: String)(code: => Task[T])(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                                          (using DummyImplicit)
                                          (using %, /, \)
                                          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                    `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                    ^ : String): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(rate, pace, value.asInstanceOf[`()`])(key)(code)(?, -, *, +)
            else
              apply[S, T](4)(rate, pace, ZIO.attempt(value))(key)(code)(?, -, *, +)

          /**
            * linear variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(rate: Rate, value: => Task[S])(key: String)(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                                       (using %, /, \)
                                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                 ^ : String): ZStream[Any, Nothing, Unit] =
            apply[S](2)(rate, Duration.Zero, value)(key)(?, -, *, +)

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(rate: Rate, pace: Duration, value: => Task[S])(key: String)(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                                       (using % : %, / : /, \ : \)
                                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                 ^ : String): ZStream[Any, Nothing, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              ZStream.fromZIO(ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]]: UIO[`()`])).flatMap(self.`(!)`.`(+)`(rate, pace, _)(key)(?, -, *, +))
            else
              for
                discard  <- if None eq * then ZStream.fromZIO(exclude(key)) *> ZStream.succeed(false)
                            else ZStream.fromZIO(?.await)
                _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
                if !discard
                promise  <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]])
                continue <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]].flatMap(Ref.make))
                _        <- if None eq * then ZStream.unit
                            else ZStream.fromZIO(promise.succeed(None))
                enabled  <- ZStream.fromZIO(promise.isDone.negate.flatMap(Ref.make))
                _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`()`[{}], Some(Left(())), rate))))
                cb_fb_in <- ZStream.fromZIO(promise.await)
                discard  <- if None eq * then ZStream.fromZIO(?.succeed(cb_fb_in eq None) *> ?.await)
                            else ZStream.succeed(false)
                _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
                if !discard
                sp <- ZStream.fromZIO(Promise.make[Nothing, Unit])
                _  <- ZStream.fromZIO {
                  for
                    _        <- -.await.exit
                    _        <- *.fold(ZIO.unit)(_.acquire)
                    _        <- \(%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }).unlessZIO(enabled.get)
                    cb_fb_in <- continue.get.flatMap(_.await)
                    _        <- Promise.make[Nothing, Option[<>]].flatMap(continue.set)
                    _        <- enabled.set(false)
                    _        <- if cb_fb_in eq None then sp.succeed(())
                                else
                                  val (cbarrier, fiber, input) = cb_fb_in.get
                                  (value: UIO[S]).map(new `()`(_)).flatMap(input.set(_) *> cbarrier.await.exit *> fiber.join)
                  yield
                    ()
                }.repeat(Schedule.forever).interruptWhen(sp)
                _  <- ZStream.fromZIO(ZIO.sleep(pace))
                _  <- ZStream.fromZIO(+.release)
                _  <- ZStream.unit.whenZIO(sp.isDone.negate)
              yield
                ()

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => Task[S])(key: String)(code: => Task[T])(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                                          (using %, /, \)
                                          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                    `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                    ^ : String): ZStream[Any, Nothing, Unit] =
            apply[S](1)(rate, value)(key)(?, -, *, +).tap(_ => code)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: Duration, value: => Task[S])(key: String)(code: => Task[T])(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                                          (using %, /, \)
                                          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                    `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                    ^ : String): ZStream[Any, Nothing, Unit] =
            apply[S](2)(rate, pace, value)(key)(?, -, *, +).tap(_ => code)

        /**
          * linear replication input guard
          */
        def apply(rate: Rate)(key: String)(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                 (using %, /, \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Nothing, `()`] =
          apply(rate, Duration.Zero)(key)(?, -, *, +)

        /**
          * linear replication input guard w/ pace
          */
        def apply(rate: Rate, pace: Duration)(key: String)(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                 (using % : %, / : /, \ : \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Nothing, `()`] =
          for
            discard  <- if None eq * then ZStream.fromZIO(exclude(key)) *> ZStream.succeed(false)
                        else ZStream.fromZIO(?.await)
            _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
            if !discard
            promise  <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]])
            continue <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]].flatMap(Ref.make))
            _        <- if None eq * then ZStream.unit
                        else ZStream.fromZIO(promise.succeed(None))
            enabled  <- ZStream.fromZIO(promise.isDone.negate.flatMap(Ref.make))
            result   <- ZStream.fromZIO(Ref.make[`()`](null))
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`()`[{}], Some(Right(result)), rate))))
            cb_fb_in <- ZStream.fromZIO(promise.await)
            discard  <- if None eq * then ZStream.fromZIO(?.succeed(cb_fb_in eq None) *> ?.await)
                        else ZStream.succeed(false)
            _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
            if !discard
            sp <- ZStream.fromZIO(Promise.make[Nothing, Unit])
            _  <- ZStream.fromZIO {
              for
                _        <- -.await.exit
                _        <- *.fold(ZIO.unit)(_.acquire)
                _        <- \(%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }).unlessZIO(enabled.get)
                cb_fb_in <- continue.get.flatMap(_.await)
                _        <- Promise.make[Nothing, Option[<>]].flatMap(continue.set)
                _        <- enabled.set(false)
                _        <- if cb_fb_in eq None then sp.succeed(())
                            else
                              val (cbarrier, fiber, _) = cb_fb_in.get
                              cbarrier.await.exit *> fiber.join
              yield
                ()
            }.repeat(Schedule.forever).interruptWhen(sp)
            _  <- ZStream.fromZIO(ZIO.sleep(pace))
            _  <- ZStream.fromZIO(+.release)
            it <- ZStream.fromZIO(result.get)
            _  <- ZStream.unit.whenZIO(sp.isDone.negate)
          yield
            it

        /**
          * linear replication input guard w/ code
          */
        def apply[T](rate: Rate)(key: String)(code: T => Task[T])(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Nothing, `()`] =
          apply(rate)(key)(?, -, *, +).map(_.`()`[T]).mapZIO(code(_).map(new `()`(_)))

        /**
          * linear replication input guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: Duration)(key: String)(code: T => Task[T])(? : Promise[Nothing, Boolean], - : CyclicBarrier, * : Option[Semaphore[UIO]], + : Semaphore[UIO])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Nothing, `()`] =
          apply(rate, pace)(key)(?, -, *, +).map(_.`()`[T]).mapZIO(code(_).map(new `()`(_)))

      object `(ν)`:

        /**
          * replication bound output guard
          */
        def apply(rate: Rate)(key: String)
                 (using % : %, / : /, \ : \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Nothing, `()`] =
          for
            _        <- ZStream.fromZIO(exclude(key))
            promise  <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]])
            continue <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]].flatMap(Ref.make))
            enabled  <- ZStream.fromZIO(Ref.make(true))
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`()`[{}], Some(Left(())), rate))))
            cb_fb_in <- ZStream.fromZIO(promise.await)
            if cb_fb_in ne None
            sp <- ZStream.fromZIO(Promise.make[Nothing, Unit])
            it <- ( for
                      _  <- ZStream.unit.repeat(Schedule.forever)
                      it <- sΠ.ν
                      _ <- ZStream.fromZIO {
                        for
                          _        <- \(%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }).unlessZIO(enabled.get)
                          cb_fb_in <- continue.get.flatMap(_.await)
                          _        <- Promise.make[Nothing, Option[<>]].flatMap(continue.set)
                          _        <- enabled.set(false)
                          _        <- if cb_fb_in eq None then sp.succeed(())
                                      else
                                        val (cbarrier, fiber, input) = cb_fb_in.get
                                        input.set(it) *> cbarrier.await.exit *> fiber.join
                        yield
                          ()
                      }
                    yield
                      it
                  ).interruptWhen(sp)
            _  <- ZStream.unit.whenZIO(sp.isDone.negate)
          yield
            it

        /**
          * replication bound output guard w/ pace
          */
        def apply(rate: Rate, pace: Duration)(key: String)
                 (using %, /, \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Nothing, `()`] =
          apply(rate)(key) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

        /**
          * replication bound output guard w/ code
          */
        def apply[T](rate: Rate)(key: String)(code: => Task[T])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Nothing, `()`] =
          apply(rate)(key).tap(_ => code)

        /**
          * replication bound output guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: Duration)(key: String)(code: => Task[T])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Nothing, `()`] =
          apply(rate, pace)(key).tap(_ => code)

      /**
        * constant replication output guard
        */
      def apply(rate: Rate, value: `()`)(key: String)
               (using % : %, / : /, \ : \)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Nothing, Unit] =
        for
          _        <- ZStream.fromZIO(exclude(key))
          promise  <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]])
          continue <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]].flatMap(Ref.make))
          enabled  <- ZStream.fromZIO(Ref.make(true))
          _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`()`[{}], Some(Left(())), rate))))
          cb_fb_in <- ZStream.fromZIO(promise.await)
          if cb_fb_in ne None
          sp <- ZStream.fromZIO(Promise.make[Nothing, Unit])
          _  <- ZStream.fromZIO {
            for
              _        <- \(%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }).unlessZIO(enabled.get)
              cb_fb_in <- continue.get.flatMap(_.await)
              _        <- Promise.make[Nothing, Option[<>]].flatMap(continue.set)
              _        <- enabled.set(false)
              _        <- if cb_fb_in eq None then sp.succeed(())
                          else
                            val (cbarrier, fiber, input) = cb_fb_in.get
                            input.set(value) *> cbarrier.await.exit *> fiber.join
            yield
              ()
          }.repeat(Schedule.forever).interruptWhen(sp)
          _  <- ZStream.unit.whenZIO(sp.isDone.negate)
        yield
          ()

      /**
        * constant replication output guard w/ pace
        */
      def apply(rate: Rate, pace: Duration, value: `()`)(key: String)
               (using %, /, \)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Nothing, Unit] =
        apply(rate, value)(key) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

      /**
        * constant replication output guard w/ code
        */
      def apply[T](rate: Rate, value: `()`)(key: String)(code: => Task[T])
               (using %, /, \)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Nothing, Unit] =
        apply(rate, value)(key).tap(_ => code)

      /**
        * constant replication output guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: Duration, value: `()`)(key: String)(code: => Task[T])
               (using %, /, \)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Nothing, Unit] =
        apply(rate, pace, value)(key).tap(_ => code)

      object `(*)`:

        /**
          * variable replication output guard
          */
        def apply[S: ClassTag](_1: 1)(rate: Rate, value: => S)(key: String)
                                     (using DummyImplicit)
                                     (using %, /, \)
                                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                               ^ : String): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(rate, value.asInstanceOf[`()`])(key)
          else
            apply[S](1)(rate, ZIO.attempt(value))(key)

        /**
          * variable replication output guard w/ pace
          */
        def apply[S: ClassTag](_2: 2)(rate: Rate, pace: Duration, value: => S)(key: String)
                                     (using DummyImplicit)
                                     (using %, /, \)
                                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                               ^ : String): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(rate, pace, value.asInstanceOf[`()`])(key)
          else
            apply[S](2)(rate, pace, ZIO.attempt(value))(key)

        /**
          * variable replication output guard w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => S)(key: String)(code: => Task[T])
                                        (using DummyImplicit)
                                        (using %, /, \)
                                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                  ^ : String): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(rate, value.asInstanceOf[`()`])(key)(code)
          else
            apply[S, T](3)(rate, ZIO.attempt(value))(key)(code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: Duration, value: => S)(key: String)(code: => Task[T])
                                        (using DummyImplicit)
                                        (using %, /, \)
                                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                  ^ : String): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(rate, pace, value.asInstanceOf[`()`])(key)(code)
          else
            apply[S, T](4)(rate, pace, ZIO.attempt(value))(key)(code)

        /**
          * variable replication output guard
          */
        def apply[S: ClassTag](_1: 1)(rate: Rate, value: => Task[S])(key: String)
                                     (using % : %, / : /, \ : \)
                                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                               ^ : String): ZStream[Any, Nothing, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            ZStream.fromZIO(ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]]: UIO[`()`])).flatMap(self.`(!)`(rate, _)(key))
          else
            for
              _        <- ZStream.fromZIO(exclude(key))
              promise  <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]])
              continue <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]].flatMap(Ref.make))
              enabled  <- ZStream.fromZIO(Ref.make(true))
              _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`()`[{}], Some(Left(())), rate))))
              cb_fb_in <- ZStream.fromZIO(promise.await)
              if cb_fb_in ne None
              sp <- ZStream.fromZIO(Promise.make[Nothing, Unit])
              _  <- ZStream.fromZIO {
                for
                  _        <- \(%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }).unlessZIO(enabled.get)
                  cb_fb_in <- continue.get.flatMap(_.await)
                  _        <- Promise.make[Nothing, Option[<>]].flatMap(continue.set)
                  _        <- enabled.set(false)
                  _        <- if cb_fb_in eq None then sp.succeed(())
                              else
                                val (cbarrier, fiber, input) = cb_fb_in.get
                                (value: UIO[S]).map(new `()`(_)).flatMap(input.set(_) *> cbarrier.await.exit *> fiber.join)
                yield
                  ()
              }.repeat(Schedule.forever).interruptWhen(sp)
              _  <- ZStream.unit.whenZIO(sp.isDone.negate)
            yield
              ()

        /**
          * variable replication output guard w/ pace
          */
        def apply[S: ClassTag](_2: 2)(rate: Rate, pace: Duration, value: => Task[S])(key: String)
                                     (using %, /, \)
                                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                               ^ : String): ZStream[Any, Nothing, Unit] =
          apply[S](1)(rate, value)(key) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

        /**
          * variable replication output guard w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => Task[S])(key: String)(code: => Task[T])
                                        (using %, /, \)
                                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                  ^ : String): ZStream[Any, Nothing, Unit] =
          apply[S](1)(rate, value)(key).tap(_ => code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: Duration, value: => Task[S])(key: String)(code: => Task[T])
                                        (using %, /, \)
                                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                  ^ : String): ZStream[Any, Nothing, Unit] =
          apply[S](2)(rate, pace, value)(key).tap(_ => code)

      /**
        * replication input guard
        */
      def apply(rate: Rate)(key: String)
               (using % : %, / : /, \ : \)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Nothing, `()`] =
        for
          _        <- ZStream.fromZIO(exclude(key))
          promise  <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]])
          continue <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]].flatMap(Ref.make))
          enabled  <- ZStream.fromZIO(Ref.make(true))
          result   <- ZStream.fromZIO(Ref.make[`()`](null))
          _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`()`[{}], Some(Right(result)), rate))))
          cb_fb_in <- ZStream.fromZIO(promise.await)
          if cb_fb_in ne None
          sp <- ZStream.fromZIO(Promise.make[Nothing, Unit])
          _  <- ZStream.fromZIO {
            for
              _        <- \(%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }).unlessZIO(enabled.get)
              cb_fb_in <- continue.get.flatMap(_.await)
              _        <- Promise.make[Nothing, Option[<>]].flatMap(continue.set)
              _        <- enabled.set(false)
              _        <- if cb_fb_in eq None then sp.succeed(())
                          else
                            val (cbarrier, fiber, _) = cb_fb_in.get
                            cbarrier.await.exit *> fiber.join
            yield
              ()
          }.repeat(Schedule.forever).interruptWhen(sp)
          it <- ZStream.fromZIO(result.get)
          _  <- ZStream.unit.whenZIO(sp.isDone.negate)
        yield
          it

      /**
        * replication input guard w/ pace
        */
      def apply(rate: Rate, pace: Duration)(key: String)
               (using %, /, \)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Nothing, `()`] =
        apply(rate)(key) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

      /**
        * replication input guard w/ code
        */
      def apply[T](rate: Rate)(key: String)(code: T => Task[T])
                  (using %, /, \)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Nothing, `()`] =
        apply(rate)(key).map(_.`()`[T]).mapZIO(code(_).map(new `()`(_)))

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: Duration)(key: String)(code: T => Task[T])
                  (using %, /, \)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Nothing, `()`] =
        apply(rate, pace)(key).map(_.`()`[T]).mapZIO(code(_).map(new `()`(_)))

    object `(ν)`:

      /**
        * bound output prefix
        */
      def apply(rate: Rate)(key: String)
               (using % : %, / : /)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Nothing, `()`] =
        for
          _        <- ZStream.fromZIO(exclude(key))
          promise  <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]])
          _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (`()`[{}], Some(Left(())), rate))))
          cb_fb_in <- ZStream.fromZIO(promise.await)
          if cb_fb_in ne None
          (cbarrier, fiber, input) = cb_fb_in.get
          it <- sΠ.ν
          _  <- ZStream.fromZIO(input.set(it) *> cbarrier.await.exit *> fiber.join)
        yield
          it

      /**
        * bound output prefix w/ pace
        */
      def apply(rate: Rate, pace: Duration)(key: String)
               (using %, /)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Nothing, `()`] =
        apply(rate)(key) <* ZStream.fromZIO(ZIO.sleep(pace))

      /**
        * bound output prefix w/ code
        */
      def apply[T](rate: Rate)(key: String)(code: => Task[T])
                  (using %, /)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Nothing, `()`] =
        apply(rate)(key).tap(_ => code)

      /**
        * bound output prefix w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: Duration)(key: String)(code: => Task[T])
                  (using %, /)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Nothing, `()`] =
        apply(rate, pace)(key).tap(_ => code)

    /**
      * constant output prefix
      */
    def apply(rate: Rate, value: `()`)(key: String)
             (using % : %, / : /)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): ZStream[Any, Nothing, Unit] =
      for
        _        <- ZStream.fromZIO(exclude(key))
        promise  <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]])
        _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (`()`[{}], Some(Left(())), rate))))
        cb_fb_in <- ZStream.fromZIO(promise.await)
        if cb_fb_in ne None
        (cbarrier, fiber, input) = cb_fb_in.get
        _  <- ZStream.fromZIO(input.set(value) *> cbarrier.await.exit *> fiber.join)
      yield
        ()

    /**
      * constant output prefix w/ pace
      */
    def apply(rate: Rate, pace: Duration, value: `()`)(key: String)
             (using %, /)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): ZStream[Any, Nothing, Unit] =
        apply(rate, value)(key) <* ZStream.fromZIO(ZIO.sleep(pace))

    /**
      * constant output prefix w/ code
      */
    def apply[T](rate: Rate, value: `()`)(key: String)(code: => Task[T])
                (using %, /)
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): ZStream[Any, Nothing, Unit] =
      apply(rate, value)(key).tap(_ => code)

    /**
      * constant output prefix w/ pace w/ code
      */
    def apply[T](rate: Rate, pace: Duration, value: `()`)(key: String)(code: => Task[T])
                (using %, /)
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): ZStream[Any, Nothing, Unit] =
      apply(rate, pace, value)(key).tap(_ => code)

    object `(*)`:

      /**
        * variable output prefix
        */
      def apply[S: ClassTag](_1: 1)(rate: Rate, value: => S)(key: String)
                                   (using DummyImplicit)
                                   (using %, /)
                                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                             ^ : String): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(rate, value.asInstanceOf[`()`])(key)
        else
          apply[S](1)(rate, ZIO.attempt(value))(key)

      /**
        * variable output prefix w/ pace
        */
      def apply[S: ClassTag](_2: 2)(rate: Rate, pace: Duration, value: => S)(key: String)
                                   (using DummyImplicit)
                                   (using %, /)
                                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                             ^ : String): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(rate, pace, value.asInstanceOf[`()`])(key)
        else
          apply[S](1)(rate, value)(key) <* ZStream.fromZIO(ZIO.sleep(pace))

      /**
        * variable output prefix w/ code
        */
      def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => S)(key: String)(code: => Task[T])
                                      (using DummyImplicit)
                                      (using %, /)
                                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                ^ : String): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(rate, value.asInstanceOf[`()`])(key)(code)
        else
          apply[S](1)(rate, value)(key).tap(_ => code)

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: Duration, value: => S)(key: String)(code: => Task[T])
                                      (using DummyImplicit)
                                      (using %, /)
                                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                ^ : String): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(rate, pace, value.asInstanceOf[`()`])(key)(code)
        else
          apply[S](2)(rate, pace, value)(key).tap(_ => code)

      /**
        * variable output prefix
        */
      def apply[S: ClassTag](_1: 1)(rate: Rate, value: => Task[S])(key: String)
                                   (using % : %, / : /)
                                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                             ^ : String): ZStream[Any, Nothing, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          ZStream.fromZIO(ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]]: UIO[`()`])).flatMap(self(rate, _)(key))
        else
          for
            _        <- ZStream.fromZIO(exclude(key))
            promise  <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]])
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (`()`[{}], Some(Left(())), rate))))
            cb_fb_in <- ZStream.fromZIO(promise.await)
            if cb_fb_in ne None
            (cbarrier, fiber, input) = cb_fb_in.get
            _  <- ZStream.fromZIO((value: UIO[S]).map(new `()`(_)).flatMap(input.set(_) *> cbarrier.await.exit *> fiber.join))
          yield
            ()

      /**
        * variable output prefix w/ pace
        */
      def apply[S: ClassTag](_2: 2)(rate: Rate, pace: Duration, value: => Task[S])(key: String)
                                   (using %, /)
                                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                             ^ : String): ZStream[Any, Nothing, Unit] =
        apply[S](1)(rate, value)(key) <* ZStream.fromZIO(ZIO.sleep(pace))

      /**
        * variable output prefix w/ code
        */
      def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => Task[S])(key: String)(code: => Task[T])
                                      (using %, /)
                                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                ^ : String): ZStream[Any, Nothing, Unit] =
        apply[S](1)(rate, value)(key).tap(_ => code)

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: Duration, value: => Task[S])(key: String)(code: => Task[T])
                                      (using %, /)
                                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                ^ : String): ZStream[Any, Nothing, Unit] =
        apply[S](2)(rate, pace, value)(key).tap(_ => code)

    /**
      * input prefix
      */
    def apply(rate: Rate)(key: String)
             (using % : %, / : /)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): ZStream[Any, Nothing, `()`] =
      for
        _        <- ZStream.fromZIO(exclude(key))
        promise  <- ZStream.fromZIO(Promise.make[Nothing, Option[<>]])
        result   <- ZStream.fromZIO(Ref.make[`()`](null))
        _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (`()`[{}], Some(Right(result)), rate))))
        cb_fb_in <- ZStream.fromZIO(promise.await)
        if cb_fb_in ne None
        (cbarrier, fiber, _) = cb_fb_in.get
        _  <- ZStream.fromZIO(cbarrier.await.exit *> fiber.join)
        it <- ZStream.fromZIO(result.get)
      yield
        it

    /**
      * input prefix w/ pace
      */
    def apply(rate: Rate, pace: Duration)(key: String)
             (using %, /)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): ZStream[Any, Nothing, `()`] =
      apply(rate)(key) <* ZStream.fromZIO(ZIO.sleep(pace))

    /**
      * input prefix w/ code
      */
    def apply[T](rate: Rate)(key: String)(code: T => Task[T])
                (using %, /)
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): ZStream[Any, Nothing, `()`] =
      apply(rate)(key).map(_.`()`[T]).mapZIO(code(_).map(new `()`(_)))

    /**
      * input prefix w/ pace w/ code
      */
    def apply[T](rate: Rate, pace: Duration)(key: String)(code: T => Task[T])
                (using %, /)
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): ZStream[Any, Nothing, `()`] =
      apply(rate, pace)(key).map(_.`()`[T]).mapZIO(code(_).map(new `()`(_)))

    override def toString: String = if name == null then "null" else name.toString

  }
