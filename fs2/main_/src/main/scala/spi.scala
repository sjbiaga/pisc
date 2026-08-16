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

  import _root_.scala.concurrent.duration.FiniteDuration

  import _root_.scala.reflect.{ ClassTag, classTag }

  import _root_.cats.syntax.applicative.*
  import _root_.cats.syntax.apply.*
  import _root_.cats.syntax.functor.*
  import _root_.cats.syntax.flatMap.*

  import _root_.cats.effect.{ Async, Deferred, Ref }
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.{ CyclicBarrier, Semaphore, Supervisor }

  import _root_.fs2.concurrent.SignallingRef
  import _root_.fs2.Stream

  import `Π-loop`.{ <>, +, %, /, \ }
  import `Π-stats`.Rate


  type `Π-Map`[K, +V] = Map[K, V]

  type `Π-Set`[A] = Set[A]

  type `Π-Function0`[F[_]] = () => String ?=> Stream[F, Unit]
  type `Π-Function1`[F[_]] = `()`[F] => String ?=> Stream[F, Unit]


  /**
    * Supervised [[code]].
    * @param code
    */
  private def exec[F[_]: Async, T](code: => F[T]): F[T] =
    Supervisor[F](await = true)
      .use(_.supervise(code))
      .flatMap(_.join)
      .flatMap {
        case Succeeded(it) => it
        case _             => Async[F].pure(null.asInstanceOf[T])
      }


  inline def `π-exclude`[F[_]: Async](enabled: String*)
                                     (using % : %[F], \ : \[F]): F[Unit] =
    \(`π-exclude`[F](Set.from(enabled)))

  private def `π-exclude`[F[_]](enabled: `Π-Set`[String])
                               (using % : %[F]): F[Unit] =
    %.update(enabled.foldLeft(_) { (m, key) =>
                                   val n = m(key).asInstanceOf[Int] - 1
                                   if n == 0
                                   then
                                     m - key
                                   else
                                     m + (key -> n)
                                 }
    )

  private def exclude[F[_]: Async](key: String)
                                  (using %[F])
                                  (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]]): F[Unit] =
    `π-exclude`[F](`π-elvis`(key)).whenA(`π-elvis`.contains(key))


  /**
    * restriction aka new name
    */
  final class ν[F[_]: Async]:

    def map[B](f: `()`[F] => B): Stream[F, B] = flatMap(f andThen Stream.emit[F, B])
    def flatMap[B](f: `()`[F] => Stream[F, B]): Stream[F, B] = f(new {})


  /**
    * silent transition
    */
  final class τ[F[_]: Async]:

    object `(!)`:

      object `(+)`:

        /**
          * linear replication guard
          */
        def apply(rate: Rate)(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                 (using % : %[F], / : /[F], \ : \[F])
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, Unit] =
          for
            _        <- if None eq * then Stream.eval(exclude(key))
                        else Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
            _        <- if None eq * then Stream.unit
                        else Stream.eval(deferred.complete(None))
            enabled  <- Stream.eval(deferred.tryGet.map(_ eq None) >>= Ref[F].of)
            timestamp <- Stream.eval(Async[F].monotonic.map(_.toNanos) >>= Ref[F].of)
            _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, timestamp), (new {}, None, rate))))
            cb_fb_in <- Stream.eval(deferred.get)
            _        <- if None eq * then Stream.eval(?.complete(cb_fb_in eq None) >> ?.get)
                                                .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                        else Stream.unit
            timeset   =  Async[F].monotonic.map(_.toNanos) >>= timestamp.set
            sr <- Stream.eval(SignallingRef[F].of(false))
            _  <- Stream.repeatEval {
              for
                _        <- -.await
                _        <- *.fold(Async[F].unit)(_.acquire)
                _        <- enabled.get >>= timeset.unlessA
                _        <- enabled.get >>= \(%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }).unlessA
                cb_fb_in <- continue.get.flatMap(_.get)
                _        <- Deferred[F, Option[<>[F]]] >>= continue.set
                _        <- enabled.set(false)
                _        <- if cb_fb_in eq None then sr.set(true)
                            else
                              val (cbarrier, fiber, _) = cb_fb_in.get
                              cbarrier.await >> fiber.join.void
              yield
                ()
            }.interruptWhen(sr)
            _  <- Stream.eval(+.release)
            _  <- Stream.eval(sr.get) >>= Stream.empty.whenA
          yield
            ()

        /**
          * linear replication guard w/ pace
          */
        def apply(rate: Rate, pace: FiniteDuration)(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                 (using %[F], /[F], \[F])
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, Unit] =
        apply(rate)(key)(?, -, *, +).spaced(pace)

        /**
          * linear replication guard w/ code
          */
        def apply[T](rate: Rate)(key: String)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                    (using %[F], /[F], \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          apply(rate)(key)(?, -, *, +).evalTap(_ => exec(code))

        /**
          * linear replication guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                    (using %[F], /[F], \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          apply(rate, pace)(key)(?, -, *, +).evalTap(_ => exec(code))

      /**
        * replication guard
        */
      def apply(rate: Rate)(key: String)
               (using % : %[F], / : /[F], \ : \[F])
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, Unit] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
          enabled  <- Stream.eval(Ref[F].of(true))
          timestamp <- Stream.eval(Async[F].monotonic.map(_.toNanos) >>= Ref[F].of)
          _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, timestamp), (new {}, None, rate))))
          cb_fb_in <- Stream.eval(deferred.get)
          if cb_fb_in ne None
          timeset   =  Async[F].monotonic.map(_.toNanos) >>= timestamp.set
          sr <- Stream.eval(SignallingRef[F].of(false))
          _  <- Stream.repeatEval {
            for
              _        <- enabled.get >>= timeset.unlessA
              _        <- enabled.get >>= \(%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }).unlessA
              cb_fb_in <- continue.get.flatMap(_.get)
              _        <- Deferred[F, Option[<>[F]]] >>= continue.set
              _        <- enabled.set(false)
              _        <- if cb_fb_in eq None then sr.set(true)
                          else
                            val (cbarrier, fiber, _) = cb_fb_in.get
                            cbarrier.await >> fiber.join.void
            yield
              ()
          }.interruptWhen(sr)
          _  <- Stream.eval(sr.get) >>= Stream.empty.whenA
        yield
          ()

      /**
        * replication guard w/ pace
        */
      def apply(rate: Rate, pace: FiniteDuration)(key: String)
               (using %[F], /[F], \[F])
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, Unit] =
        apply(rate)(key).spaced(pace)

      /**
        * replication guard w/ code
        */
      def apply[T](rate: Rate)(key: String)(code: => F[T])
                  (using %[F], /[F], \[F])
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, Unit] =
        apply(rate)(key).evalTap(_ => exec(code))

      /**
        * replication guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: => F[T])
                  (using %[F], /[F], \[F])
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, Unit] =
        apply(rate, pace)(key).evalTap(_ => exec(code))

    /**
      * prefix
      */
    def apply(rate: Rate)(key: String)
             (using % : %[F], / : /[F])
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): Stream[F, Unit] =
      for
        _        <- Stream.eval(exclude(key))
        deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
        timestamp <- Stream.eval(Async[F].monotonic.map(_.toNanos) >>= Ref[F].of)
        _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> null, timestamp), (new {}, None, rate))))
        cb_fb_in <- Stream.eval(deferred.get)
        if cb_fb_in ne None
        (cbarrier, fiber, _) = cb_fb_in.get
        _  <- Stream.eval(cbarrier.await >> fiber.join)
      yield
        ()

    /**
      * prefix w/ pace
      */
    def apply(rate: Rate, pace: FiniteDuration)(key: String)
             (using %[F], /[F])
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): Stream[F, Unit] =
      apply(rate)(key) <* Stream.sleep(pace)

    /**
      * prefix w/ code
      */
    def apply[T](rate: Rate)(key: String)(code: => F[T])
                (using %[F], /[F])
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): Stream[F, Unit] =
      apply(rate)(key).evalTap(_ => exec(code))

    /**
      * prefix w/ pace w/ code
      */
    def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: => F[T])
                (using %[F], /[F])
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): Stream[F, Unit] =
      apply(rate, pace)(key).evalTap(_ => exec(code))


  /**
    * names and values
    */
  implicit final class `()`[F[_]: Async](private val name: Any) { self =>

    def ====(that: `()`[F]) = this.name == that.name

    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()`[F] = this

    object `(!)`:

      object `(+)`:

        object `(ν)`:

          /**
            * linear replication bound output guard
            */
          def apply(rate: Rate)(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                   (using % : %[F], / : /[F], \ : \[F])
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): Stream[F, `()`[F]] =
            for
              _        <- if None eq * then Stream.eval(exclude(key))
                          else Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
              deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
              continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
              _        <- if None eq * then Stream.unit
                          else Stream.eval(deferred.complete(None))
              enabled  <- Stream.eval(deferred.tryGet.map(_ eq None) >>= Ref[F].of)
              timestamp <- Stream.eval(Async[F].monotonic.map(_.toNanos) >>= Ref[F].of)
              _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, timestamp), (`()`[{}], Some(Left(())), rate))))
              cb_fb_in <- Stream.eval(deferred.get)
              _        <- if None eq * then Stream.eval(?.complete(cb_fb_in eq None) >> ?.get)
                                                  .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                          else Stream.unit
              timeset   =  Async[F].monotonic.map(_.toNanos) >>= timestamp.set
              sr <- Stream.eval(SignallingRef[F].of(false))
              it <- ( for
                        _  <- Stream.unit.repeat
                        it <- sΠ.ν[F]
                        _  <- Stream.eval {
                          for
                            _        <- -.await
                            _        <- *.fold(Async[F].unit)(_.acquire)
                            _        <- enabled.get >>= timeset.unlessA
                            _        <- enabled.get >>= \(%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }).unlessA
                            cb_fb_in <- continue.get.flatMap(_.get)
                            _        <- Deferred[F, Option[<>[F]]] >>= continue.set
                            _        <- enabled.set(false)
                            _        <- if cb_fb_in eq None then sr.set(true)
                                        else
                                          val (cbarrier, fiber, input) = cb_fb_in.get
                                          input.set(it) >> cbarrier.await >> fiber.join.void
                          yield
                            ()
                        }
                      yield
                        it
                    ).interruptWhen(sr)
              _  <- Stream.eval(+.release)
              _  <- Stream.eval(sr.get) >>= Stream.empty.whenA
            yield
              it

          /**
            * linear replication bound output guard w/ pace
            */
          def apply(rate: Rate, pace: FiniteDuration)(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                   (using %[F], /[F], \[F])
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): Stream[F, `()`[F]] =
            apply(rate)(key)(?, -, *, +).spaced(pace)

          /**
            * linear replication bound output guard w/ code
            */
          def apply[T](rate: Rate)(key: String)(code: F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                      (using %[F], /[F], \[F])
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): Stream[F, `()`[F]] =
            apply(rate)(key)(?, -, *, +).evalTap(_ => exec(code))

          /**
            * linear replication bound output guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                      (using %[F], /[F], \[F])
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): Stream[F, `()`[F]] =
            apply(rate, pace)(key)(?, -, *, +).evalTap(_ => exec(code))

        /**
          * linear constant replication output guard
          */
        def apply(rate: Rate, value: `()`[F])(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                 (using % : %[F], / : /[F], \ : \[F])
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, Unit] =
          for
            _        <- if None eq * then Stream.eval(exclude(key))
                        else Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
            _        <- if None eq * then Stream.unit
                        else Stream.eval(deferred.complete(None))
            enabled  <- Stream.eval(deferred.tryGet.map(_ eq None) >>= Ref[F].of)
            timestamp <- Stream.eval(Async[F].monotonic.map(_.toNanos) >>= Ref[F].of)
            _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, timestamp), (`()`[{}], Some(Left(())), rate))))
            cb_fb_in <- Stream.eval(deferred.get)
            _        <- if None eq * then Stream.eval(?.complete(cb_fb_in eq None) >> ?.get)
                                                .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                        else Stream.unit
            timeset   =  Async[F].monotonic.map(_.toNanos) >>= timestamp.set
            sr <- Stream.eval(SignallingRef[F].of(false))
            _  <- Stream.repeatEval {
              for
                _        <- -.await
                _        <- *.fold(Async[F].unit)(_.acquire)
                _        <- enabled.get >>= timeset.unlessA
                _        <- enabled.get >>= \(%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }).unlessA
                cb_fb_in <- continue.get.flatMap(_.get)
                _        <- Deferred[F, Option[<>[F]]] >>= continue.set
                _        <- enabled.set(false)
                _        <- if cb_fb_in eq None then sr.set(true)
                            else
                              val (cbarrier, fiber, input) = cb_fb_in.get
                              input.set(value) >> cbarrier.await >> fiber.join.void
              yield
                ()
            }.interruptWhen(sr)
            _  <- Stream.eval(+.release)
            _  <- Stream.eval(sr.get) >>= Stream.empty.whenA
          yield
            ()

        /**
          * linear constant replication output guard w/ pace
          */
        def apply(rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                 (using %[F], /[F], \[F])
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, Unit] =
          apply(rate, value)(key)(?, -, *, +).spaced(pace)

        /**
          * linear constant replication output guard w/ code
          */
        def apply[T](rate: Rate, value: `()`[F])(key: String)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                    (using %[F], /[F], \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          apply(rate, value)(key)(?, -, *, +).evalTap(_ => exec(code))

        /**
          * linear constant replication output guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                    (using %[F], /[F], \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          apply(rate, pace, value)(key)(?, -, *, +).evalTap(_ => exec(code))

        object `(*)`:

          /**
            * linear variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(rate: Rate, value: => S)(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                                       (using DummyImplicit)
                                       (using %[F], /[F], \[F])
                                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                 ^ : String): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(rate, value.asInstanceOf[`()`[F]])(key)(?, -, *, +)
            else
              apply[S](1)(rate, Async[F].delay(value))(key)(?, -, *, +)

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(rate: Rate, pace: FiniteDuration, value: => S)(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                                       (using DummyImplicit)
                                       (using %[F], /[F], \[F])
                                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                 ^ : String): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(rate, pace, value.asInstanceOf[`()`[F]])(key)(?, -, *, +)
            else
              apply[S](2)(rate, pace, Async[F].delay(value))(key)(?, -, *, +)

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => S)(key: String)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                                          (using DummyImplicit)
                                          (using %[F], /[F], \[F])
                                          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                    `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                    ^ : String): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(rate, value.asInstanceOf[`()`[F]])(key)(code)(?, -, *, +)
            else
              apply[S, T](3)(rate, Async[F].delay(value))(key)(code)(?, -, *, +)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: FiniteDuration, value: => S)(key: String)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                                          (using DummyImplicit)
                                          (using %[F], /[F], \[F])
                                          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                    `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                    ^ : String): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(rate, pace, value.asInstanceOf[`()`[F]])(key)(code)(?, -, *, +)
            else
              apply[S, T](4)(rate, pace, Async[F].delay(value))(key)(code)(?, -, *, +)

          /**
            * linear variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(rate: Rate, value: => F[S])(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                                       (using % : %[F], / : /[F], \ : \[F])
                                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                 ^ : String): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              Stream.eval(Async[F].defer(value.asInstanceOf[F[`()`[F]]])).flatMap(self.`(!)`.`(+)`(rate, _)(key)(?, -, *, +))
            else
              for
                _        <- if None eq * then Stream.eval(exclude(key))
                            else Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
                continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
                _        <- if None eq * then Stream.unit
                            else Stream.eval(deferred.complete(None))
                enabled  <- Stream.eval(deferred.tryGet.map(_ eq None) >>= Ref[F].of)
                timestamp <- Stream.eval(Async[F].monotonic.map(_.toNanos) >>= Ref[F].of)
                _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, timestamp), (`()`[{}], Some(Left(())), rate))))
                cb_fb_in <- Stream.eval(deferred.get)
                _        <- if None eq * then Stream.eval(?.complete(cb_fb_in eq None) >> ?.get)
                                                    .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                            else Stream.unit
                timeset   =  Async[F].monotonic.map(_.toNanos) >>= timestamp.set
                sr <- Stream.eval(SignallingRef[F].of(false))
                _  <- Stream.repeatEval {
                  for
                    _        <- -.await
                    _        <- *.fold(Async[F].unit)(_.acquire)
                    _        <- enabled.get >>= timeset.unlessA
                    _        <- enabled.get >>= \(%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }).unlessA
                    cb_fb_in <- continue.get.flatMap(_.get)
                    _        <- Deferred[F, Option[<>[F]]] >>= continue.set
                    _        <- enabled.set(false)
                    _        <- if cb_fb_in eq None then sr.set(true)
                                else
                                  val (cbarrier, fiber, input) = cb_fb_in.get
                                  value.map(new `()`[F](_)).flatMap(input.set(_) >> cbarrier.await >> fiber.join.void)
                  yield
                    ()
                }.interruptWhen(sr)
                _  <- Stream.eval(+.release)
                _  <- Stream.eval(sr.get) >>= Stream.empty.whenA
              yield
                ()

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(rate: Rate, pace: FiniteDuration, value: => F[S])(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                                       (using %[F], /[F], \[F])
                                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                 ^ : String): Stream[F, Unit] =
            apply[S](1)(rate, value)(key)(?, -, *, +).spaced(pace)

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => F[S])(key: String)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                                          (using %[F], /[F], \[F])
                                          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                    `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                    ^ : String): Stream[F, Unit] =
            apply[S](1)(rate, value)(key)(?, -, *, +).evalTap(_ => exec(code))

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: FiniteDuration, value: => F[S])(key: String)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                                          (using %[F], /[F], \[F])
                                          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                    `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                    ^ : String): Stream[F, Unit] =
            apply[S](2)(rate, pace, value)(key)(?, -, *, +).evalTap(_ => exec(code))

        /**
          * linear replication input guard
          */
        def apply(rate: Rate)(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                 (using % : %[F], / : /[F], \ : \[F])
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, `()`[F]] =
          for
            _        <- if None eq * then Stream.eval(exclude(key))
                        else Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
            _        <- if None eq * then Stream.unit
                        else Stream.eval(deferred.complete(None))
            enabled  <- Stream.eval(deferred.tryGet.map(_ eq None) >>= Ref[F].of)
            result   <- Stream.eval(Ref[F].of[`()`[F]](null))
            timestamp <- Stream.eval(Async[F].monotonic.map(_.toNanos) >>= Ref[F].of)
            _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, timestamp), (`()`[{}], Some(Right(result)), rate))))
            cb_fb_in <- Stream.eval(deferred.get)
            _        <- if None eq * then Stream.eval(?.complete(cb_fb_in eq None) >> ?.get)
                                                .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                        else Stream.unit
            timeset   =  Async[F].monotonic.map(_.toNanos) >>= timestamp.set
            sr <- Stream.eval(SignallingRef[F].of(false))
            _  <- Stream.repeatEval {
              for
                _        <- -.await
                _        <- *.fold(Async[F].unit)(_.acquire)
                _        <- enabled.get >>= timeset.unlessA
                _        <- enabled.get >>= \(%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }).unlessA
                cb_fb_in <- continue.get.flatMap(_.get)
                _        <- Deferred[F, Option[<>[F]]] >>= continue.set
                _        <- enabled.set(false)
                _        <- if cb_fb_in eq None then sr.set(true)
                            else
                              val (cbarrier, fiber, _) = cb_fb_in.get
                              cbarrier.await >> fiber.join.void
              yield
                ()
            }.interruptWhen(sr)
            _  <- Stream.eval(+.release)
            it <- Stream.eval(result.get)
            _  <- Stream.eval(sr.get) >>= Stream.empty.whenA
          yield
            it

        /**
          * linear replication input guard w/ pace
          */
        def apply(rate: Rate, pace: FiniteDuration)(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                 (using %[F], /[F], \[F])
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, `()`[F]] =
          apply(rate)(key)(?, -, *, +).spaced(pace)

        /**
          * linear replication input guard w/ code
          */
        def apply[T](rate: Rate)(key: String)(code: T => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                    (using %[F], /[F], \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, `()`[F]] =
          apply(rate)(key)(?, -, *, +).map(_.`()`[T]).evalMap((code andThen exec)(_).map(new `()`[F](_)))

        /**
          * linear replication input guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: T => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                    (using %[F], /[F], \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, `()`[F]] =
          apply(rate, pace)(key)(?, -, *, +).map(_.`()`[T]).evalMap((code andThen exec)(_).map(new `()`[F](_)))

      object `(ν)`:

        /**
          * replication bound output guard
          */
        def apply(rate: Rate)(key: String)
                 (using % : %[F], / : /[F], \ : \[F])
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, `()`[F]] =
          for
            _        <- Stream.eval(exclude(key))
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
            enabled  <- Stream.eval(Ref[F].of(true))
            timestamp <- Stream.eval(Async[F].monotonic.map(_.toNanos) >>= Ref[F].of)
            _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, timestamp), (`()`[{}], Some(Left(())), rate))))
            cb_fb_in <- Stream.eval(deferred.get)
            if cb_fb_in ne None
            timeset   =  Async[F].monotonic.map(_.toNanos) >>= timestamp.set
            sr <- Stream.eval(SignallingRef[F].of(false))
            it <- ( for
                      _  <- Stream.unit.repeat
                      it <- sΠ.ν[F]
                      _  <- Stream.eval {
                        for
                          _        <- enabled.get >>= timeset.unlessA
                          _        <- enabled.get >>= \(%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }).unlessA
                          cb_fb_in <- continue.get.flatMap(_.get)
                          _        <- Deferred[F, Option[<>[F]]] >>= continue.set
                          _        <- enabled.set(false)
                          _        <- if cb_fb_in eq None then sr.set(true)
                                      else
                                        val (cbarrier, fiber, input) = cb_fb_in.get
                                        input.set(it) >> cbarrier.await >> fiber.join.void
                        yield
                          ()
                      }
                    yield
                      it
                  ).interruptWhen(sr)
            _  <- Stream.eval(sr.get) >>= Stream.empty.whenA
          yield
            it

        /**
          * replication bound output guard w/ pace
          */
        def apply(rate: Rate, pace: FiniteDuration)(key: String)
                 (using %[F], /[F], \[F])
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, `()`[F]] =
          apply(rate)(key).spaced(pace)

        /**
          * replication bound output guard w/ code
          */
        def apply[T](rate: Rate)(key: String)(code: => F[T])
                    (using %[F], /[F], \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, `()`[F]] =
          apply(rate)(key).evalTap(_ => exec(code))

        /**
          * replication bound output guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: => F[T])
                    (using %[F], /[F], \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, `()`[F]] =
          apply(rate, pace)(key).evalTap(_ => exec(code))

      /**
        * constant replication output guard
        */
      def apply(rate: Rate, value: `()`[F])(key: String)
               (using % : %[F], / : /[F], \ : \[F])
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, Unit] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
          enabled  <- Stream.eval(Ref[F].of(true))
          timestamp <- Stream.eval(Async[F].monotonic.map(_.toNanos) >>= Ref[F].of)
          _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, timestamp), (`()`[{}], Some(Left(())), rate))))
          cb_fb_in <- Stream.eval(deferred.get)
          if cb_fb_in ne None
          timeset   =  Async[F].monotonic.map(_.toNanos) >>= timestamp.set
          sr <- Stream.eval(SignallingRef[F].of(false))
          _  <- Stream.repeatEval {
            for
              _        <- enabled.get >>= timeset.unlessA
              _        <- enabled.get >>= \(%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }).unlessA
              cb_fb_in <- continue.get.flatMap(_.get)
              _        <- Deferred[F, Option[<>[F]]] >>= continue.set
              _        <- enabled.set(false)
              _        <- if cb_fb_in eq None then sr.set(true)
                          else
                            val (cbarrier, fiber, input) = cb_fb_in.get
                            input.set(value) >> cbarrier.await >> fiber.join.void
            yield
              ()
          }.interruptWhen(sr)
          _  <- Stream.eval(sr.get) >>= Stream.empty.whenA
        yield
          ()

      /**
        * constant replication output guard w/ pace
        */
      def apply(rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String)
               (using %[F], /[F], \[F])
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, Unit] =
        apply(rate, value)(key).spaced(pace)

      /**
        * constant replication output guard w/ code
        */
      def apply[T](rate: Rate, value: `()`[F])(key: String)(code: => F[T])
               (using %[F], /[F], \[F])
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, Unit] =
        apply(rate, value)(key).evalTap(_ => exec(code))

      /**
        * constant replication output guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String)(code: => F[T])
               (using %[F], /[F], \[F])
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, Unit] =
        apply(rate, pace, value)(key).evalTap(_ => exec(code))

      object `(*)`:

        /**
          * variable replication output guard
          */
        def apply[S: ClassTag](_1: 1)(rate: Rate, value: => S)(key: String)
                                     (using DummyImplicit)
                                     (using %[F], /[F], \[F])
                                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                               ^ : String): Stream[F, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(rate, value.asInstanceOf[`()`[F]])(key)
          else
            apply[S](1)(rate, Async[F].delay(value))(key)

        /**
          * variable replication output guard w/ pace
          */
        def apply[S: ClassTag](_2: 2)(rate: Rate, pace: FiniteDuration, value: => S)(key: String)
                                     (using DummyImplicit)
                                     (using %[F], /[F], \[F])
                                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                               ^ : String): Stream[F, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(rate, pace, value.asInstanceOf[`()`[F]])(key)
          else
            apply[S](2)(rate, pace, Async[F].delay(value))(key)

        /**
          * variable replication output guard w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => S)(key: String)(code: => F[T])
                                        (using DummyImplicit)
                                        (using %[F], /[F], \[F])
                                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                  ^ : String): Stream[F, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(rate, value.asInstanceOf[`()`[F]])(key)(code)
          else
            apply[S, T](3)(rate, Async[F].delay(value))(key)(code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: FiniteDuration, value: => S)(key: String)(code: => F[T])
                                        (using DummyImplicit)
                                        (using %[F], /[F], \[F])
                                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                  ^ : String): Stream[F, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.`(!)`(rate, pace, value.asInstanceOf[`()`[F]])(key)(code)
          else
            apply[S, T](4)(rate, pace, Async[F].delay(value))(key)(code)

        /**
          * variable replication output guard
          */
        def apply[S: ClassTag](_1: 1)(rate: Rate, value: => F[S])(key: String)
                                     (using % : %[F], / : /[F], \ : \[F])
                                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                               ^ : String): Stream[F, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            Stream.eval(Async[F].defer(value.asInstanceOf[F[`()`[F]]])).flatMap(self.`(!)`(rate, _)(key))
          else
            for
              _        <- Stream.eval(exclude(key))
              deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
              continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
              enabled  <- Stream.eval(Ref[F].of(true))
              timestamp <- Stream.eval(Async[F].monotonic.map(_.toNanos) >>= Ref[F].of)
              _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, timestamp), (`()`[{}], Some(Left(())), rate))))
              cb_fb_in <- Stream.eval(deferred.get)
              if cb_fb_in ne None
              timeset   =  Async[F].monotonic.map(_.toNanos) >>= timestamp.set
              sr <- Stream.eval(SignallingRef[F].of(false))
              _  <- Stream.repeatEval {
                for
                  _        <- enabled.get >>= timeset.unlessA
                  _        <- enabled.get >>= \(%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }).unlessA
                  cb_fb_in <- continue.get.flatMap(_.get)
                  _        <- Deferred[F, Option[<>[F]]] >>= continue.set
                  _        <- enabled.set(false)
                  _        <- if cb_fb_in eq None then sr.set(true)
                              else
                                val (cbarrier, fiber, input) = cb_fb_in.get
                                value.map(new `()`[F](_)).flatMap(input.set(_) >> cbarrier.await >> fiber.join.void)
                yield
                  ()
              }.interruptWhen(sr)
              _  <- Stream.eval(sr.get) >>= Stream.empty.whenA
            yield
              ()

        /**
          * variable replication output guard w/ pace
          */
        def apply[S: ClassTag](_2: 2)(rate: Rate, pace: FiniteDuration, value: => F[S])(key: String)
                                     (using %[F], /[F], \[F])
                                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                               ^ : String): Stream[F, Unit] =
          apply[S](1)(rate, value)(key).spaced(pace)

        /**
          * variable replication output guard w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => F[S])(key: String)(code: => F[T])
                                        (using %[F], /[F], \[F])
                                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                  ^ : String): Stream[F, Unit] =
          apply[S](1)(rate, value)(key).evalTap(_ => exec(code))

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: FiniteDuration, value: => F[S])(key: String)(code: => F[T])
                                        (using %[F], /[F], \[F])
                                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                  ^ : String): Stream[F, Unit] =
          apply[S](2)(rate, pace, value)(key).evalTap(_ => exec(code))

      /**
        * replication input guard
        */
      def apply(rate: Rate)(key: String)
               (using % : %[F], / : /[F], \ : \[F])
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, `()`[F]] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
          enabled  <- Stream.eval(Ref[F].of(true))
          result   <- Stream.eval(Ref[F].of[`()`[F]](null))
          timestamp <- Stream.eval(Async[F].monotonic.map(_.toNanos) >>= Ref[F].of)
          _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, timestamp), (`()`[{}], Some(Right(result)), rate))))
          cb_fb_in <- Stream.eval(deferred.get)
          if cb_fb_in ne None
          timeset   =  Async[F].monotonic.map(_.toNanos) >>= timestamp.set
          sr <- Stream.eval(SignallingRef[F].of(false))
          _  <- Stream.repeatEval {
            for
              _        <- enabled.get >>= timeset.unlessA
              _        <- enabled.get >>= \(%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }).unlessA
              cb_fb_in <- continue.get.flatMap(_.get)
              _        <- Deferred[F, Option[<>[F]]] >>= continue.set
              _        <- enabled.set(false)
              _        <- if cb_fb_in eq None then sr.set(true)
                          else
                            val (cbarrier, fiber, _) = cb_fb_in.get
                            cbarrier.await >> fiber.join.void
            yield
              ()
          }.interruptWhen(sr)
          it <- Stream.eval(result.get)
          _  <- Stream.eval(sr.get) >>= Stream.empty.whenA
        yield
          it

      /**
        * replication input guard w/ pace
        */
      def apply(rate: Rate, pace: FiniteDuration)(key: String)
               (using %[F], /[F], \[F])
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, `()`[F]] =
        apply(rate)(key).spaced(pace)

      /**
        * replication input guard w/ code
        */
      def apply[T](rate: Rate)(key: String)(code: T => F[T])
                  (using %[F], /[F], \[F])
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, `()`[F]] =
        apply(rate)(key).map(_.`()`[T]).evalMap((code andThen exec)(_).map(new `()`[F](_)))

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: T => F[T])
                  (using %[F], /[F], \[F])
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, `()`[F]] =
        apply(rate, pace)(key).map(_.`()`[T]).evalMap((code andThen exec)(_).map(new `()`[F](_)))

    object `(ν)`:

      /**
        * bound output prefix
        */
      def apply(rate: Rate)(key: String)
               (using % : %[F], / : /[F])
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, `()`[F]] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          timestamp <- Stream.eval(Async[F].monotonic.map(_.toNanos) >>= Ref[F].of)
          _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> null, timestamp), (`()`[{}], Some(Left(())), rate))))
          cb_fb_in <- Stream.eval(deferred.get)
          if cb_fb_in ne None
          (cbarrier, fiber, input) = cb_fb_in.get
          it <- sΠ.ν[F]
          _  <- Stream.eval(input.set(it) >> cbarrier.await >> fiber.join)
        yield
          it

      /**
        * bound output prefix w/ pace
        */
      def apply(rate: Rate, pace: FiniteDuration)(key: String)
               (using %[F], /[F])
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, `()`[F]] =
        apply(rate)(key) <* Stream.sleep(pace)

      /**
        * bound output prefix w/ code
        */
      def apply[T](rate: Rate)(key: String)(code: => F[T])
                  (using %[F], /[F])
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, `()`[F]] =
        apply(rate)(key).evalTap(_ => exec(code))

      /**
        * bound output prefix w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: => F[T])
                  (using %[F], /[F])
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, `()`[F]] =
        apply(rate, pace)(key).evalTap(_ => exec(code))

    /**
      * constant output prefix
      */
    def apply(rate: Rate, value: `()`[F])(key: String)
             (using % : %[F], / : /[F])
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): Stream[F, Unit] =
      for
        _        <- Stream.eval(exclude(key))
        deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
        timestamp <- Stream.eval(Async[F].monotonic.map(_.toNanos) >>= Ref[F].of)
        _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> null, timestamp), (`()`[{}], Some(Left(())), rate))))
        cb_fb_in <- Stream.eval(deferred.get)
        if cb_fb_in ne None
        (cbarrier, fiber, input) = cb_fb_in.get
        _  <- Stream.eval(input.set(value) >> cbarrier.await >> fiber.join)
      yield
        ()

    /**
      * constant output prefix w/ pace
      */
    def apply(rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String)
             (using %[F], /[F])
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): Stream[F, Unit] =
        apply(rate, value)(key) <* Stream.sleep(pace)

    /**
      * constant output prefix w/ code
      */
    def apply[T](rate: Rate, value: `()`[F])(key: String)(code: => F[T])
                (using %[F], /[F])
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): Stream[F, Unit] =
      apply(rate, value)(key).evalTap(_ => exec(code))

    /**
      * constant output prefix w/ pace w/ code
      */
    def apply[T](rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String)(code: => F[T])
                (using %[F], /[F])
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): Stream[F, Unit] =
      apply(rate, pace, value)(key).evalTap(_ => exec(code))

    object `(*)`:

      /**
        * variable output prefix
        */
      def apply[S: ClassTag](_1: 1)(rate: Rate, value: => S)(key: String)
                                   (using DummyImplicit)
                                   (using %[F], /[F])
                                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                             ^ : String): Stream[F, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(rate, value.asInstanceOf[`()`[F]])(key)
        else
          apply[S](1)(rate, Async[F].delay(value))(key)

      /**
        * variable output prefix w/ pace
        */
      def apply[S: ClassTag](_2: 2)(rate: Rate, pace: FiniteDuration, value: => S)(key: String)
                                   (using DummyImplicit)
                                   (using %[F], /[F])
                                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                             ^ : String): Stream[F, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(rate, pace, value.asInstanceOf[`()`[F]])(key)
        else
          apply[S](1)(rate, value)(key) <* Stream.sleep(pace)

      /**
        * variable output prefix w/ code
        */
      def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => S)(key: String)(code: => F[T])
                                      (using DummyImplicit)
                                      (using %[F], /[F])
                                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                ^ : String): Stream[F, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(rate, value.asInstanceOf[`()`[F]])(key)(code)
        else
          apply[S](1)(rate, value)(key).evalTap(_ => exec(code))

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: FiniteDuration, value: => S)(key: String)(code: => F[T])
                                      (using DummyImplicit)
                                      (using %[F], /[F])
                                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                ^ : String): Stream[F, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(rate, pace, value.asInstanceOf[`()`[F]])(key)(code)
        else
          apply[S](2)(rate, pace, value)(key).evalTap(_ => exec(code))

      /**
        * variable output prefix
        */
      def apply[S: ClassTag](_1: 1)(rate: Rate, value: => F[S])(key: String)
                                   (using % : %[F], / : /[F])
                                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                             ^ : String): Stream[F, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          Stream.eval(Async[F].defer(value.asInstanceOf[F[`()`[F]]])).flatMap(self(rate, _)(key))
        else
          for
            _        <- Stream.eval(exclude(key))
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            timestamp <- Stream.eval(Async[F].monotonic.map(_.toNanos) >>= Ref[F].of)
            _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> null, timestamp), (`()`[{}], Some(Left(())), rate))))
            cb_fb_in <- Stream.eval(deferred.get)
            if cb_fb_in ne None
            (cbarrier, fiber, input) = cb_fb_in.get
            _  <- Stream.eval(value.map(new `()`[F](_)).flatMap(input.set(_) >> cbarrier.await >> fiber.join))
          yield
            ()

      /**
        * variable output prefix w/ pace
        */
      def apply[S: ClassTag](_2: 2)(rate: Rate, pace: FiniteDuration, value: => F[S])(key: String)
                            (using %[F], /[F])
                            (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                      `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                      ^ : String): Stream[F, Unit] =
        apply[S](1)(rate, value)(key) <* Stream.sleep(pace)

      /**
        * variable output prefix w/ code
        */
      def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => F[S])(key: String)(code: => F[T])
                               (using %[F], /[F])
                               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                         ^ : String): Stream[F, Unit] =
        apply[S](1)(rate, value)(key).evalTap(_ => exec(code))

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: FiniteDuration, value: => F[S])(key: String)(code: => F[T])
                               (using %[F], /[F])
                               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                         ^ : String): Stream[F, Unit] =
        apply[S](2)(rate, pace, value)(key).evalTap(_ => exec(code))

    /**
      * input prefix
      */
    def apply(rate: Rate)(key: String)
             (using % : %[F], / : /[F])
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): Stream[F, `()`[F]] =
      for
        _        <- Stream.eval(exclude(key))
        deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
        result   <- Stream.eval(Ref[F].of[`()`[F]](null))
        timestamp <- Stream.eval(Async[F].monotonic.map(_.toNanos) >>= Ref[F].of)
        _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> null, timestamp), (`()`[{}], Some(Right(result)), rate))))
        cb_fb_in <- Stream.eval(deferred.get)
        if cb_fb_in ne None
        (cbarrier, fiber, _) = cb_fb_in.get
        _  <- Stream.eval(cbarrier.await >> fiber.join)
        it <- Stream.eval(result.get)
      yield
        it

    /**
      * input prefix w/ pace
      */
    def apply(rate: Rate, pace: FiniteDuration)(key: String)
             (using %[F], /[F])
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): Stream[F, `()`[F]] =
      apply(rate)(key) <* Stream.sleep(pace)

    /**
      * input prefix w/ code
      */
    def apply[T](rate: Rate)(key: String)(code: T => F[T])
                (using %[F], /[F])
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): Stream[F, `()`[F]] =
      apply(rate)(key).map(_.`()`[T]).evalMap((code andThen exec)(_).map(new `()`[F](_)))

    /**
      * input prefix w/ pace w/ code
      */
    def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: T => F[T])
                (using %[F], /[F])
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): Stream[F, `()`[F]] =
      apply(rate, pace)(key).map(_.`()`[T]).evalMap((code andThen exec)(_).map(new `()`[F](_)))

    override def toString: String = if name == null then "null" else name.toString

  }
