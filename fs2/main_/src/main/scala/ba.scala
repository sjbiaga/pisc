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
  import _root_.cats.effect.std.{ CyclicBarrier, Semaphore, UUIDGen }

  import _root_.fs2.concurrent.SignallingRef
  import _root_.fs2.Stream

  import _root_.io.github.timwspence.cats.stm.STM

  import `Π-loop`.{ <>, +, %, /, \ }
  import `Π-stats`.Rate

  import `π-$`.*, `π-ζ`.*


  type `Π-Map`[K, +V] = Map[K, V]

  type `Π-Set`[A] = Set[A]

  type `Π-Function0`[F[_]] = () => String ?=> Stream[F, Unit]
  type `Π-Function1`[F[_]] = `()`[F] => String ?=> Stream[F, Unit]


  private val `0.seconds` = FiniteDuration(0, java.util.concurrent.TimeUnit.SECONDS)


  /**
    * Wraps ambient keys.
    *
    * @param value
    */
  final class `)(`(private val value: Any) extends AnyVal:
    override def toString: String = value.toString

  object `)(`:
    /**
      * Initial ambient unique key.
      */
    def apply[F[_]: Async: UUIDGen](): F[`)(`] =
      UUIDGen.randomUUID[F].map(new `)(`(_))

  /**
    * Type of keys in [[`][`]].
    */
  type `)*(` = Set[`)(`]


  sealed abstract trait Ordʹ { val ord: Int }
  sealed trait Ord(val ord: Int) extends Ordʹ

  val `π-τ` = new Ord(-1) {}

  /**
    * Type of directions.
    */
  enum `π-$` extends Ordʹ {
    case `π-local` extends `π-$` with Ord(0)
    case `π-s2s`   extends `π-$` with Ord(1)
    case `π-p2c`   extends `π-$` with Ord(2)
    case `π-c2p`   extends `π-$` with Ord(2)
  }

  /**
    * Type of capabilities.
    */
  enum `π-ζ` extends Ordʹ {
    case `π-enter`  extends `π-ζ` with Ord(3)
    case `π-accept` extends `π-ζ` with Ord(3)
    case `π-exit`   extends `π-ζ` with Ord(4)
    case `π-expel`  extends `π-ζ` with Ord(4)
    case `π-merge+` extends `π-ζ` with Ord(5)
    case `π-merge-` extends `π-ζ` with Ord(5)
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
                                  (using % : %[F])
                                  (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]]): F[Unit] =
    `π-exclude`[F](`π-elvis`(key)).whenA(`π-elvis`.contains(key))


  /**
    * restriction aka new name
    */
  final class ν[F[_]: Async]:

    def map[B](f: `()`[F] => B): Stream[F, B] = flatMap(f andThen Stream.emit[F, B])
    def flatMap[B](f: `()`[F] => Stream[F, B]): Stream[F, B] =
      f {
        Map(
          `π-local`.ord  -> new {},
          `π-s2s`.ord    -> new {},
          `π-p2c`.ord    -> new {},
          `π-accept`.ord -> new {},
          `π-expel`.ord  -> new {},
          `π-merge+`.ord -> new {}
        )
      }


  /**
    * silent transition
    */
  final class τ[F[_]: Async]:

    object `(!)`:

      object `(+)`:

        /**
          * linear replication guard
          */
        def apply(rate: Rate)(key: String, `)(`: `)(`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                 (using %[F], /[F], \[F])
                 (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
          apply(rate, `0.seconds`)(key, `)(`)(?, -, *, +)

        /**
          * linear replication guard w/ pace
          */
        def apply(rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                 (using % : %[F], / : /[F], \ : \[F])
                 (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, Unit] =
          for
            _        <- if None eq * then Stream.eval(exclude(key))
                        else Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
            _        <- if None eq * then Stream.unit
                        else Stream.eval(deferred.complete(None))
            enabled  <- Stream.eval(deferred.tryGet.map(_ eq None) >>= Ref[F].of)
            timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
            _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, `)(` -> `π-τ`, timestamp), (τ.`new {}`, None, rate))))
            cb_fb_in <- Stream.eval(deferred.get)
            _        <- if None eq * then Stream.eval(?.complete(cb_fb_in eq None) >> ?.get)
                                                .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                        else Stream.unit
            timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
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
            _  <- Stream.sleep(pace)
            _  <- Stream.eval(+.release)
            _  <- Stream.eval(sr.get) >>= Stream.empty.whenA
          yield
            ()

        /**
          * linear replication guard w/ code
          */
        def apply[T](rate: Rate)(key: String, `)(`: `)(`)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                    (using %[F], /[F], \[F])
                    (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
          apply(rate)(key, `)(`)(?, -, *, +).evalTap(_ => code)

        /**
          * linear replication guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                    (using %[F], /[F], \[F])
                    (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
          apply(rate, pace)(key, `)(`)(?, -, *, +).evalTap(_ => code)

      /**
        * replication guard
        */
      def apply(rate: Rate)(key: String, `)(`: `)(`)
               (using % : %[F], / : /[F], \ : \[F])
               (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, Unit] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
          enabled  <- Stream.eval(Ref[F].of(true))
          timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
          _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, `)(` -> `π-τ`, timestamp), (τ.`new {}`, None, rate))))
          cb_fb_in <- Stream.eval(deferred.get)
          if cb_fb_in ne None
          timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
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
      def apply(rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)
               (using %[F], /[F], \[F])
               (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
        apply(rate)(key, `)(`).spaced(pace)

      /**
        * replication guard w/ code
        */
      def apply[T](rate: Rate)(key: String, `)(`: `)(`)(code: => F[T])
                  (using %[F], /[F], \[F])
                  (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
        apply(rate)(key, `)(`).evalTap(_ => code)

      /**
        * replication guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(code: => F[T])
                  (using %[F], /[F], \[F])
                  (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
        apply(rate, pace)(key, `)(`).evalTap(_ => code)

    /**
      * prefix
      */
    def apply(rate: Rate)(key: String, `)(`: `)(`)
             (using % : %[F], / : /[F])
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): Stream[F, Unit] =
      for
        _        <- Stream.eval(exclude(key))
        deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
        timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
        _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> null, `)(` -> `π-τ`, timestamp), (τ.`new {}`, None, rate))))
        cb_fb_in <- Stream.eval(deferred.get)
        if cb_fb_in ne None
        (cbarrier, fiber, _) = cb_fb_in.get
        _  <- Stream.eval(cbarrier.await >> fiber.join)
      yield
        ()

    /**
      * prefix w/ pace
      */
    def apply(rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)
             (using %[F], /[F])
             (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
      apply(rate)(key, `)(`) <* Stream.sleep(pace)

    /**
      * prefix w/ code
      */
    def apply[T](rate: Rate)(key: String, `)(`: `)(`)(code: => F[T])
                (using %[F], /[F])
                (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
      apply(rate)(key, `)(`).evalTap(_ => code)

    /**
      * prefix w/ pace w/ code
      */
    def apply[T](rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(code: => F[T])
                (using %[F], /[F])
                (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
      apply(rate, pace)(key, `)(`).evalTap(_ => code)


  object τ:

    private val `new {}` = new {}


  /**
    * names and values
    */
  implicit final class `()`[F[_]: Async](private val name: Any) { self =>

    private def map = `()`[Map[Int, {}]]

    def ====(that: `()`[F]) =
      try
        this.map eq that.map
      catch _ =>
        this.name == that.name

    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()`[F] = this

    object π:

      object `(!)`:

        object `(+)`:

          object `(ν)`:

            /**
              * linear replication bound output guard
              */
            def apply(rate: Rate)(key: String, `)(`: `)(`)(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                     (using %[F], /[F], \[F])
                     (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, `()`[F]] =
              apply(rate, `0.seconds`)(key, `)(`)(dir)(?, -, *, +)

            /**
              * linear replication bound output guard w/ pace
              */
            def apply(rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                     (using % : %[F], / : /[F], \ : \[F])
                     (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                               ^ : String): Stream[F, `()`[F]] =
              for
                _        <- if None eq * then Stream.eval(exclude(key))
                            else Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
                continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
                _        <- if None eq * then Stream.unit
                            else Stream.eval(deferred.complete(None))
                enabled  <- Stream.eval(deferred.tryGet.map(_ eq None) >>= Ref[F].of)
                timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
                _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, `)(` -> dir, timestamp), (map(dir.ord), Some(Left(())), rate))))
                cb_fb_in <- Stream.eval(deferred.get)
                _        <- if None eq * then Stream.eval(?.complete(cb_fb_in eq None) >> ?.get)
                                                    .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                            else Stream.unit
                timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
                sr <- Stream.eval(SignallingRef[F].of(false))
                it <- ( for
                          _  <- Stream.unit.repeat
                          it <- sΠ.ν[F]
                          _ <- Stream.eval {
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
                _  <- Stream.sleep(pace)
                _  <- Stream.eval(+.release)
                _  <- Stream.eval(sr.get) >>= Stream.empty.whenA
              yield
                it

            /**
              * linear replication bound output guard w/ code
              */
            def apply[T](rate: Rate)(key: String, `)(`: `)(`)(code: F[T])(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                        (using %[F], /[F], \[F])
                        (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, `()`[F]] =
              apply(rate)(key, `)(`)(dir)(?, -, *, +).evalTap(_ => code)

            /**
              * linear replication bound output guard w/ pace w/ code
              */
            def apply[T](rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                        (using %[F], /[F], \[F])
                        (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, `()`[F]] =
              apply(rate, pace)(key, `)(`)(dir)(?, -, *, +).evalTap(_ => code)

          /**
            * linear constant replication output guard
            */
          def apply(rate: Rate, value: `()`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                   (using %[F], /[F], \[F])
                   (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
            apply(rate, `0.seconds`, value)(key, `)(`)(dir)(?, -, *, +)

          /**
            * linear constant replication output guard w/ pace
            */
          def apply(rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                   (using % : %[F], / : /[F], \ : \[F])
                   (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): Stream[F, Unit] =
            for
              _        <- if None eq * then Stream.eval(exclude(key))
                          else Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
              deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
              continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
              _        <- if None eq * then Stream.unit
                          else Stream.eval(deferred.complete(None))
              enabled  <- Stream.eval(deferred.tryGet.map(_ eq None) >>= Ref[F].of)
              timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
              _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, `)(` -> dir, timestamp), (map(dir.ord), Some(Left(())), rate))))
              cb_fb_in <- Stream.eval(deferred.get)
              _        <- if None eq * then Stream.eval(?.complete(cb_fb_in eq None) >> ?.get)
                                                  .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                          else Stream.unit
              timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
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
              _  <- Stream.sleep(pace)
              _  <- Stream.eval(+.release)
              _  <- Stream.eval(sr.get) >>= Stream.empty.whenA
            yield
              ()

          /**
            * linear constant replication output guard w/ code
            */
          def apply[T](rate: Rate, value: `()`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                      (using %[F], /[F], \[F])
                      (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
            apply(rate, value)(key, `)(`)(dir)(?, -, *, +).evalTap(_ => code)

          /**
            * linear constant replication output guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                      (using %[F], /[F], \[F])
                      (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
            apply(rate, pace, value)(key, `)(`)(dir)(?, -, *, +).evalTap(_ => code)

          object `(*)`:

            /**
              * linear variable replication output guard
              */
            def apply[S: ClassTag](_1: 1)(rate: Rate, value: => S)(key: String, `)(`: `)(`)(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                                         (using DummyImplicit)
                                         (using %[F], /[F], \[F])
                                         (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
              if classTag[S].runtimeClass eq self.getClass
              then
                self.π.`(!)`.`(+)`(rate, value.asInstanceOf[`()`[F]])(key, `)(`)(dir)(?, -, *, +)
              else
                apply[S](1)(rate, Async[F].delay(value))(key, `)(`)(dir)(?, -, *, +)

            /**
              * linear variable replication output guard w/ pace
              */
            def apply[S: ClassTag](_2: 2)(rate: Rate, pace: FiniteDuration, value: => S)(key: String, `)(`: `)(`)(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                                         (using DummyImplicit)
                                         (using %[F], /[F], \[F])
                                         (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
              if classTag[S].runtimeClass eq self.getClass
              then
                self.π.`(!)`.`(+)`(rate, pace, value.asInstanceOf[`()`[F]])(key, `)(`)(dir)(?, -, *, +)
              else
                apply[S](2)(rate, pace, Async[F].delay(value))(key, `)(`)(dir)(?, -, *, +)

            /**
              * linear variable replication output guard w/ code
              */
            def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => S)(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                                            (using DummyImplicit)
                                            (using %[F], /[F], \[F])
                                            (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
              if classTag[S].runtimeClass eq self.getClass
              then
                self.π.`(!)`.`(+)`(rate, value.asInstanceOf[`()`[F]])(key, `)(`)(dir)(code)(?, -, *, +)
              else
                apply[S, T](3)(rate, Async[F].delay(value))(key, `)(`)(dir)(code)(?, -, *, +)

            /**
              * linear variable replication output guard w/ pace w/ code
              */
            def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: FiniteDuration, value: => S)(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                                            (using DummyImplicit)
                                            (using %[F], /[F], \[F])
                                            (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
              if classTag[S].runtimeClass eq self.getClass
              then
                self.π.`(!)`.`(+)`(rate, pace, value.asInstanceOf[`()`[F]])(key, `)(`)(dir)(code)(?, -, *, +)
              else
                apply[S, T](4)(rate, pace, Async[F].delay(value))(key, `)(`)(dir)(code)(?, -, *, +)

            /**
              * linear variable replication output guard
              */
            def apply[S: ClassTag](_1: 1)(rate: Rate, value: => F[S])(key: String, `)(`: `)(`)(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                                         (using %[F], /[F], \[F])
                                         (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
              apply[S](2)(rate, `0.seconds`, value)(key, `)(`)(dir)(?, -, *, +)

            /**
              * linear variable replication output guard w/ pace
              */
            def apply[S: ClassTag](_2: 2)(rate: Rate, pace: FiniteDuration, value: => F[S])(key: String, `)(`: `)(`)(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                                         (using % : %[F], / : /[F], \ : \[F])
                                         (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                   ^ : String): Stream[F, Unit] =
              if classTag[S].runtimeClass eq self.getClass
              then
                Stream.eval(Async[F].defer(value.asInstanceOf[F[`()`[F]]])).flatMap(self.π.`(!)`.`(+)`(rate, _)(key, `)(`)(dir)(?, -, *, +))
              else
                for
                  _        <- if None eq * then Stream.eval(exclude(key))
                              else Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                  deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
                  continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
                  _        <- if None eq * then Stream.unit
                              else Stream.eval(deferred.complete(None))
                  enabled  <- Stream.eval(deferred.tryGet.map(_ eq None) >>= Ref[F].of)
                  timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
                  _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, `)(` -> dir, timestamp), (map(dir.ord), Some(Left(())), rate))))
                  cb_fb_in <- Stream.eval(deferred.get)
                  _        <- if None eq * then Stream.eval(?.complete(cb_fb_in eq None) >> ?.get)
                                                      .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                              else Stream.unit
                  timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
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
                  _  <- Stream.sleep(pace)
                  _  <- Stream.eval(+.release)
                  _  <- Stream.eval(sr.get) >>= Stream.empty.whenA
                yield
                  ()

            /**
              * linear variable replication output guard w/ code
              */
            def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => F[S])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                                            (using %[F], /[F], \[F])
                                            (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
              apply[S](1)(rate, value)(key, `)(`)(dir)(?, -, *, +).evalTap(_ => code)

            /**
              * linear variable replication output guard w/ pace w/ code
              */
            def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: FiniteDuration, value: => F[S])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                                            (using %[F], /[F], \[F])
                                            (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
              apply[S](2)(rate, pace, value)(key, `)(`)(dir)(?, -, *, +).evalTap(_ => code)

          /**
            * linear replication input guard
            */
          def apply(rate: Rate)(key: String, `)(`: `)(`)(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                   (using %[F], /[F], \[F])
                   (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, `()`[F]] =
            apply(rate, `0.seconds`)(key, `)(`)(dir)(?, -, *, +)

          /**
            * linear replication input guard w/ pace
            */
          def apply(rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                   (using % : %[F], / : /[F], \ : \[F])
                   (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
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
              timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
              _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, `)(` -> dir, timestamp), (map(dir.ord), Some(Right(result)), rate))))
              cb_fb_in <- Stream.eval(deferred.get)
              _        <- if None eq * then Stream.eval(?.complete(cb_fb_in eq None) >> ?.get)
                                                  .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                          else Stream.unit
              timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
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
              _  <- Stream.sleep(pace)
              _  <- Stream.eval(+.release)
              it <- Stream.eval(result.get)
              _  <- Stream.eval(sr.get) >>= Stream.empty.whenA
            yield
              it

          /**
            * linear replication input guard w/ code
            */
          def apply[T](rate: Rate)(key: String, `)(`: `)(`)(code: T => F[T])(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                      (using %[F], /[F], \[F])
                      (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, `()`[F]] =
            apply(rate)(key, `)(`)(dir)(?, -, *, +).evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) }

          /**
            * linear replication input guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(code: T => F[T])(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                      (using %[F], /[F], \[F])
                      (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, `()`[F]] =
            apply(rate, pace)(key, `)(`)(dir)(?, -, *, +).evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) }

        object `(ν)`:

          /**
            * replication bound output guard
            */
          def apply(rate: Rate)(key: String, `)(`: `)(`)(dir: `π-$`)
                   (using % : %[F], / : /[F], \ : \[F])
                   (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): Stream[F, `()`[F]] =
            for
              _        <- Stream.eval(exclude(key))
              deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
              continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
              enabled  <- Stream.eval(Ref[F].of(true))
              timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
              _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, `)(` -> dir, timestamp), (map(dir.ord), Some(Left(())), rate))))
              cb_fb_in <- Stream.eval(deferred.get)
              if cb_fb_in ne None
              timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
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
          def apply(rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(dir: `π-$`)
                   (using %[F], /[F], \[F])
                   (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, `()`[F]] =
            apply(rate)(key, `)(`)(dir).spaced(pace)

          /**
            * replication bound output guard w/ code
            */
          def apply[T](rate: Rate)(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                      (using %[F], /[F], \[F])
                      (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, `()`[F]] =
            apply(rate)(key, `)(`)(dir).evalTap(_ => code)

          /**
            * replication bound output guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                      (using %[F], /[F], \[F])
                      (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, `()`[F]] =
            apply(rate, pace)(key, `)(`)(dir).evalTap(_ => code)

        /**
          * constant replication output guard
          */
        def apply(rate: Rate, value: `()`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
                 (using % : %[F], / : /[F], \ : \[F])
                 (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, Unit] =
          for
            _        <- Stream.eval(exclude(key))
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
            enabled  <- Stream.eval(Ref[F].of(true))
            timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
            _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, `)(` -> dir, timestamp), (map(dir.ord), Some(Left(())), rate))))
            cb_fb_in <- Stream.eval(deferred.get)
            if cb_fb_in ne None
            timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
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
        def apply(rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
                 (using %[F], /[F], \[F])
                 (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
          apply(rate, value)(key, `)(`)(dir).spaced(pace)

        /**
          * constant replication output guard w/ code
          */
        def apply[T](rate: Rate, value: `()`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                    (using %[F], /[F], \[F])
                    (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
          apply(rate, value)(key, `)(`)(dir).evalTap(_ => code)

        /**
          * constant replication output guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                    (using %[F], /[F], \[F])
                    (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
          apply(rate, pace, value)(key, `)(`)(dir).evalTap(_ => code)

        object `(*)`:

          /**
            * variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(rate: Rate, value: => S)(key: String, `)(`: `)(`)(dir: `π-$`)
                                       (using DummyImplicit)
                                       (using %[F], /[F], \[F])
                                       (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.π.`(!)`(rate, value.asInstanceOf[`()`[F]])(key, `)(`)(dir)
            else
              apply[S](1)(rate, Async[F].delay(value))(key, `)(`)(dir)

          /**
            * variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(rate: Rate, pace: FiniteDuration, value: => S)(key: String, `)(`: `)(`)(dir: `π-$`)
                                       (using DummyImplicit)
                                       (using %[F], /[F], \[F])
                                       (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.π.`(!)`(rate, pace, value.asInstanceOf[`()`[F]])(key, `)(`)(dir)
            else
                apply[S](2)(rate, pace, Async[F].delay(value))(key, `)(`)(dir)

          /**
            * variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => S)(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                                          (using DummyImplicit)
                                          (using %[F], /[F], \[F])
                                          (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.π.`(!)`(rate, value.asInstanceOf[`()`[F]])(key, `)(`)(dir)(code)
            else
              apply[S, T](3)(rate, Async[F].delay(value))(key, `)(`)(dir)(code)

          /**
            * variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: FiniteDuration, value: => S)(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                                          (using DummyImplicit)
                                          (using %[F], /[F], \[F])
                                          (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.π.`(!)`(rate, pace, value.asInstanceOf[`()`[F]])(key, `)(`)(dir)(code)
            else
              apply[S, T](4)(rate, pace, Async[F].delay(value))(key, `)(`)(dir)(code)

          /**
            * variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(rate: Rate, value: => F[S])(key: String, `)(`: `)(`)(dir: `π-$`)
                                       (using % : %[F], / : /[F], \ : \[F])
                                       (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                 ^ : String): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              Stream.eval(Async[F].defer(value.asInstanceOf[F[`()`[F]]])).flatMap(self.π.`(!)`(rate, _)(key, `)(`)(dir))
            else
              for
                _        <- Stream.eval(exclude(key))
                deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
                continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
                enabled  <- Stream.eval(Ref[F].of(true))
                timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
                _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, `)(` -> dir, timestamp), (map(dir.ord), Some(Left(())), rate))))
                cb_fb_in <- Stream.eval(deferred.get)
                if cb_fb_in ne None
                timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
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
          def apply[S: ClassTag](_2: 2)(rate: Rate, pace: FiniteDuration, value: => F[S])(key: String, `)(`: `)(`)(dir: `π-$`)
                                       (using %[F], /[F], \[F])
                                       (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
            apply[S](1)(rate, value)(key, `)(`)(dir).spaced(pace)

          /**
            * variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => F[S])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                                          (using %[F], /[F], \[F])
                                          (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
            apply[S](1)(rate, value)(key, `)(`)(dir).evalTap(_ => code)

          /**
            * variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: FiniteDuration, value: => F[S])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                                          (using %[F], /[F], \[F])
                                          (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
            apply[S](2)(rate, pace, value)(key, `)(`)(dir).evalTap(_ => code)

        /**
          * replication input guard
          */
        def apply(rate: Rate)(key: String, `)(`: `)(`)(dir: `π-$`)
                 (using % : %[F], / : /[F], \ : \[F])
                 (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, `()`[F]] =
          for
            _        <- Stream.eval(exclude(key))
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
            enabled  <- Stream.eval(Ref[F].of(true))
            result   <- Stream.eval(Ref[F].of[`()`[F]](null))
            timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
            _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, `)(` -> dir, timestamp), (map(dir.ord), Some(Right(result)), rate))))
            cb_fb_in <- Stream.eval(deferred.get)
            if cb_fb_in ne None
            timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
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
        def apply(rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(dir: `π-$`)
                 (using %[F], /[F], \[F])
                 (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, `()`[F]] =
          apply(rate)(key, `)(`)(dir).spaced(pace)

        /**
          * replication input guard w/ code
          */
        def apply[T](rate: Rate)(key: String, `)(`: `)(`)(dir: `π-$`)(code: T => F[T])
                    (using %[F], /[F], \[F])
                    (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, `()`[F]] =
          apply(rate)(key, `)(`)(dir).evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) }

        /**
          * replication input guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(dir: `π-$`)(code: T => F[T])
                    (using %[F], /[F], \[F])
                    (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, `()`[F]] =
          apply(rate, pace)(key, `)(`)(dir).evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) }

      object `(ν)`:

        /**
          * bound output prefix
          */
        def apply(rate: Rate)(key: String, `)(`: `)(`)(dir: `π-$`)
                 (using % : %[F], / : /[F])
                 (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, `()`[F]] =
          for
            _        <- Stream.eval(exclude(key))
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
            _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> null, `)(` -> dir, timestamp), (map(dir.ord), Some(Left(())), rate))))
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
        def apply(rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(dir: `π-$`)
                 (using %[F], /[F])
                 (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, `()`[F]] =
          apply(rate)(key, `)(`)(dir) <* Stream.sleep(pace)

        /**
          * bound output prefix w/ code
          */
        def apply[T](rate: Rate)(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                    (using %[F], /[F])
                    (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, `()`[F]] =
          apply(rate)(key, `)(`)(dir).evalTap(_ => code)

        /**
          * bound output prefix w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                    (using %[F], /[F])
                    (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, `()`[F]] =
          apply(rate, pace)(key, `)(`)(dir).evalTap(_ => code)

      /**
        * constant output prefix
        */
      def apply(rate: Rate, value: `()`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
               (using % : %[F], / : /[F])
               (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, Unit] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
          _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> null, `)(` -> dir, timestamp), (map(dir.ord), Some(Left(())), rate))))
          cb_fb_in <- Stream.eval(deferred.get)
          if cb_fb_in ne None
          (cbarrier, fiber, input) = cb_fb_in.get
          _  <- Stream.eval(input.set(value) >> cbarrier.await >> fiber.join)
        yield
          ()

      /**
        * constant output prefix w/ pace
        */
      def apply(rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
               (using %[F], /[F])
               (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
        apply(rate, value)(key, `)(`)(dir) <* Stream.sleep(pace)

      /**
        * constant output prefix w/ code
        */
      def apply[T](rate: Rate, value: `()`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                  (using %[F], /[F])
                  (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
        apply(rate, value)(key, `)(`)(dir).evalTap(_ => code)

      /**
        * constant output prefix w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                  (using %[F], /[F])
                  (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
        apply(rate, pace, value)(key, `)(`)(dir).evalTap(_ => code)

      object `(*)`:

        /**
          * variable output prefix
          */
        def apply[S: ClassTag](_1: 1)(rate: Rate, value: => S)(key: String, `)(`: `)(`)(dir: `π-$`)
                                     (using DummyImplicit)
                                     (using %[F], /[F])
                                     (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.π(rate, value.asInstanceOf[`()`[F]])(key, `)(`)(dir)
          else
            apply[S](1)(rate, Async[F].delay(value))(key, `)(`)(dir)

        /**
          * variable output prefix w/ pace
          */
        def apply[S: ClassTag](_2: 2)(rate: Rate, pace: FiniteDuration, value: => S)(key: String, `)(`: `)(`)(dir: `π-$`)
                                     (using DummyImplicit)
                                     (using %[F], /[F])
                                     (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.π(rate, pace, value.asInstanceOf[`()`[F]])(key, `)(`)(dir)
          else
            apply[S](2)(rate, pace, Async[F].delay(value))(key, `)(`)(dir)

        /**
          * variable output prefix w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => S)(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                                        (using DummyImplicit)
                                        (using %[F], /[F])
                                        (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.π(rate, value.asInstanceOf[`()`[F]])(key, `)(`)(dir)(code)
          else
            apply[S, T](3)(rate, Async[F].delay(value))(key, `)(`)(dir)(code)

        /**
          * variable output prefix w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: FiniteDuration, value: => S)(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                                        (using DummyImplicit)
                                        (using %[F], /[F])
                                        (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.π(rate, pace, value.asInstanceOf[`()`[F]])(key, `)(`)(dir)(code)
          else
            apply[S, T](4)(rate, pace, Async[F].delay(value))(key, `)(`)(dir)(code)

        /**
          * variable output prefix
          */
        def apply[S: ClassTag](_1: 1)(rate: Rate, value: => F[S])(key: String, `)(`: `)(`)(dir: `π-$`)
                                     (using % : %[F], / : /[F])
                                     (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                               ^ : String): Stream[F, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            Stream.eval(Async[F].defer(value.asInstanceOf[F[`()`[F]]])).flatMap(self.π(rate, _)(key, `)(`)(dir))
          else
            for
              _        <- Stream.eval(exclude(key))
              deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
              timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
              _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> null, `)(` -> dir, timestamp), (map(dir.ord), Some(Left(())), rate))))
              cb_fb_in <- Stream.eval(deferred.get)
              if cb_fb_in ne None
              (cbarrier, fiber, input) = cb_fb_in.get
              _  <- Stream.eval(value.map(new `()`[F](_)).flatMap(input.set(_) >> cbarrier.await >> fiber.join))
            yield
              ()

        /**
          * variable output prefix w/ pace
          */
        def apply[S: ClassTag](_2: 2)(rate: Rate, pace: FiniteDuration, value: => F[S])(key: String, `)(`: `)(`)(dir: `π-$`)
                                     (using %[F], /[F])
                                     (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
          apply[S](1)(rate, value)(key, `)(`)(dir) <* Stream.sleep(pace)

        /**
          * variable output prefix w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => F[S])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                                        (using %[F], /[F])
                                        (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
          apply[S](1)(rate, value)(key, `)(`)(dir).evalTap(_ => code)

        /**
          * variable output prefix w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: FiniteDuration, value: => F[S])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                                        (using %[F], /[F])
                                        (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
          apply[S](2)(rate, pace, value)(key, `)(`)(dir).evalTap(_ => code)

      /**
        * input prefix
        */
      def apply(rate: Rate)(key: String, `)(`: `)(`)(dir: `π-$`)
               (using % : %[F], / : /[F])
               (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, `()`[F]] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          result   <- Stream.eval(Ref[F].of[`()`[F]](null))
          timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
          _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> null, `)(` -> dir, timestamp), (map(dir.ord), Some(Right(result)), rate))))
          cb_fb_in <- Stream.eval(deferred.get)
          if cb_fb_in ne None
          (cbarrier, fiber, input) = cb_fb_in.get
          _  <- Stream.eval(cbarrier.await >> fiber.join)
          it <- Stream.eval(result.get)
        yield
          it

      /**
        * input prefix w/ pace
        */
      def apply(rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(dir: `π-$`)
               (using %[F], /[F])
               (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, `()`[F]] =
        apply(rate)(key, `)(`)(dir) <* Stream.sleep(pace)

      /**
        * input prefix w/ code
        */
      def apply[T](rate: Rate)(key: String, `)(`: `)(`)(dir: `π-$`)(code: T => F[T])
                  (using %[F], /[F])
                  (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, `()`[F]] =
        apply(rate)(key, `)(`)(dir).evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) }

      /**
        * input prefix w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(dir: `π-$`)(code: T => F[T])
                  (using %[F], /[F])
                  (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, `()`[F]] =
        apply(rate, pace)(key, `)(`)(dir).evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) }

    object ζ:

      object `(!)`:

        object `(+)`:

          /**
            * linear replication capability guard
            */
          def apply(rate: Rate)(key: String, `)(`: `)(`)(cap: `π-ζ`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                   (using %[F], /[F], \[F])
                   (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
            apply(rate, `0.seconds`)(key, `)(`)(cap)(?, -, *, +)

          /**
            * linear replication capability guard w/ pace
            */
          def apply(rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(cap: `π-ζ`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                   (using % : %[F], / : /[F], \ : \[F])
                   (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): Stream[F, Unit] =
            for
              _        <- Stream.eval(exclude(key))
              deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
              continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
              enabled  <- Stream.eval(Ref[F].of(true))
              polarity  = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
              timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
              _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, `)(` -> cap, timestamp), (map(cap.ord), Some(if polarity then Right(null) else Left(())), rate))))
              cb_fb_in <- Stream.eval(deferred.get)
              if cb_fb_in ne None
              timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
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
              _  <- Stream.sleep(pace)
              _  <- Stream.eval(+.release)
              _  <- Stream.eval(sr.get) >>= Stream.empty.whenA
            yield
              ()

          /**
            * linear replication capability guard w/ code
            */
          def apply[T](rate: Rate)(key: String, `)(`: `)(`)(cap: `π-ζ`)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                      (using %[F], /[F], \[F])
                      (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
            apply(rate)(key, `)(`)(cap)(?, -, *, +).evalTap(_ => code)

          /**
            * linear replication capability guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(cap: `π-ζ`)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], * : Option[Semaphore[F]], + : Semaphore[F])
                      (using %[F], /[F], \[F])
                      (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
            apply(rate, pace)(key, `)(`)(cap)(?, -, *, +).evalTap(_ => code)

        /**
          * replication capability guard
          */
        def apply(rate: Rate)(key: String, `)(`: `)(`)(cap: `π-ζ`)
                 (using % : %[F], / : /[F], \ : \[F])
                 (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, Unit] =
          for
            _        <- Stream.eval(exclude(key))
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
            enabled  <- Stream.eval(Ref[F].of(true))
            polarity  = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
            timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
            _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, `)(` -> cap, timestamp), (map(cap.ord), Some(if polarity then Right(null) else Left(())), rate))))
            cb_fb_in <- Stream.eval(deferred.get)
            if cb_fb_in ne None
            timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
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
          * replication capability guard w/ pace
          */
        def apply(rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(cap: `π-ζ`)
                 (using %[F], /[F], \[F])
                 (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
          apply(rate)(key, `)(`)(cap).spaced(pace)

        /**
          * replication capability guard w/ code
          */
        def apply[T](rate: Rate)(key: String, `)(`: `)(`)(cap: `π-ζ`)(code: => F[T])
                    (using %[F], /[F], \[F])
                    (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
          apply(rate)(key, `)(`)(cap).evalTap(_ => code)

        /**
          * replication capability guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(cap: `π-ζ`)(code: => F[T])
                    (using %[F], /[F], \[F])
                    (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
          apply(rate, pace)(key, `)(`)(cap).evalTap(_ => code)

      /**
        * capability prefix
        */
      def apply(rate: Rate)(key: String, `)(`: `)(`)(cap: `π-ζ`)
               (using % : %[F], / : /[F])
               (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, Unit] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          polarity  = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
          timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
          _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> null, `)(` -> cap, timestamp), (map(cap.ord), Some(if polarity then Right(null) else Left(())), rate))))
          cb_fb_in <- Stream.eval(deferred.get)
          if cb_fb_in ne None
          (cbarrier, fiber, _) = cb_fb_in.get
          _  <- Stream.eval(cbarrier.await >> fiber.join)
        yield
          ()

      /**
        * capability prefix w/ pace
        */
      def apply(rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(cap: `π-ζ`)
               (using %[F], /[F])
               (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
        apply(rate)(key, `)(`)(cap) <* Stream.sleep(pace)

      /**
        * capability prefix w/ code
        */
      def apply[T](rate: Rate)(key: String, `)(`: `)(`)(cap: `π-ζ`)(code: => F[T])
                  (using %[F], /[F])
                  (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
        apply(rate)(key, `)(`)(cap).evalTap(_ => code)

      /**
        * capability prefix w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration)(key: String, `)(`: `)(`)(cap: `π-ζ`)(code: => F[T])
                  (using %[F], /[F])
                  (using `Π-Map`[String, `Π-Set`[String]], String): Stream[F, Unit] =
        apply(rate, pace)(key, `)(`)(cap).evalTap(_ => code)

    override def toString: String = if name == null then "null" else name.toString

  }


  final class `}{`[F[_]: Async: UUIDGen](val stm: STM[F]):

    import stm.*

    /**
      * Ambients' trees' nodes.
      */
    final case class `}{`(label: Option[String],
                          root: `)*(`,
                          children: Set[`)*(`],
                          siblings: Set[`)*(`])

    object `}{`:
      def apply(key: `)(`, label: Option[String])
               (using `][`: `][`, `1`: TSemaphore): F[`)(`] =
        for
          uuid <- sΠ.`)(`()
          node  = Set(uuid)
          _    <- stm.commit {
            for
              _ <- `1`.acquire
              _ <- `][`.modify { m =>
                                 val root = m.keys.find(_.contains(key)).get
                                 val tree @ `}{`(_, _, children, _) = m(root)
                                 children.foldLeft {
                                   m + (node -> `}{`(label, root, Set.empty, children))
                                     + (root -> tree.copy(children = children + node))
                                 } { (m, child) =>
                                   val tree @ `}{`(_, _, _, siblings) = m(child)
                                   m + (child -> tree.copy(siblings = siblings + node))
                                 }
                               }
              _ <- `1`.release
            yield
              ()
          }
        yield
          uuid

      /**
        * The label and the snapshot.
        */
      def apply(key: `)(`, snapshot: Boolean = false)
               (using `][`: `][`): Txn[(String, String)] =
        `][`.get.map { m =>
                       var root = m.keys.find(_.contains(key)).get
                       def label(node: `)*(`): String = m(node).label.getOrElse("")
                       label(root) -> (
                         if !snapshot
                         then ""
                         else
                           while m(root).root ne null do root = m(root).root
                           var id = 0
                           var tree = Map[`)*(`, Int](root -> id)
                           def make(root: `)*(`): Unit =
                             for
                               node <- m(root).children
                             do
                               id += 1
                               tree += node -> id
                               make(node)
                           make(root)
                           def xml(root: `)*(`, count: Int, indent: String): StringBuilder =
                             val pid = tree(root)
                             def siblings(node: `)*(`, count: Int): StringBuilder =
                               val sid = tree(node)
                               val sb = StringBuilder()
                               sb.append(s"$indent\t\t<siblings count=$count sibling=$sid>\n")
                                 .append {
                                   ( for
                                       nodeʹ <- m(node).siblings
                                       sidʹ   = tree(nodeʹ)
                                     yield
                                       StringBuilder(s"""$indent\t\t\t<node id=$sidʹ label="${label(nodeʹ)}" parent=$pid sibling=$sid/>""")
                                   ).reduce(_.append("\n").append(_)).append("\n")
                                 }
                                 .append(s"$indent\t\t</siblings>\n")
                             def children: StringBuilder =
                               val sb = StringBuilder()
                               sb.append(s"$indent<children count=$count parent=$pid>\n")
                                 .append {
                                   ( for
                                       node <- m(root).children
                                       cid   = tree(node)
                                     yield
                                       val sbʹ = StringBuilder()
                                       val count = m(node).children.size
                                       if count == 0
                                       then
                                         val count = m(node).siblings.size
                                         if count == 0
                                         then
                                           sbʹ.append(s"""$indent\t<node id=$cid label="${label(node)}" parent=$pid/>""")
                                         else
                                           sbʹ.append(s"""$indent\t<node id=$cid label="${label(node)}" parent=$pid>\n""")
                                              .append(siblings(node, count))
                                              .append(s"$indent\t</node>")
                                       else
                                         sbʹ.append(s"""$indent\t<node id=$cid label="${label(node)}" parent=$pid>\n""")
                                            .append(xml(node, count, indent + "\t\t"))
                                            .append("\n")
                                            .append {
                                              val count = m(node).siblings.size
                                              if count == 0
                                              then
                                                StringBuilder()
                                              else
                                                siblings(node, count)
                                            }
                                            .append(s"$indent\t</node>")
                                   ).reduce(_.append("\n").append(_)).append("\n")
                                 }
                                 .append(s"$indent</children>")
                             children
                           val count = m(root).children.size
                           val sb = StringBuilder()
                           if count == 0
                           then
                             sb.append(s"""<root id=${tree(root)} label="${label(root)}"/>\n""")
                               .toString
                           else
                             sb.append(s"""<root id=${tree(root)} label="${label(root)}">\n""")
                               .append(xml(root, count, "\t"))
                               .append("\n</root>")
                               .toString
                       )
                     }

    /**
      * Type of ambients' trees.
      */
    type `][` = TVar[Map[`)*(`, `}{`]]

    object `][`:
      def apply(): F[(`)(`, `][`, TSemaphore)] =
        for
          uuid <- `)(`()
          root  = Set(uuid)
          map   = Map(root -> `}{`(None, null, Set.empty, Set.empty))
          tree <- stm.commit { TVar.of[Map[`)*(`, `}{`]](map) }
          sem  <- stm.commit { TSemaphore.make(1) }
        yield
          (uuid, tree, sem)

    object >< :

      @annotation.tailrec
      private def check(node: `)*(`,
                        nodeʹ: `)*(`,
                        dir_cap: `π-$` | `π-ζ`,
                        dir_capʹ: `π-$` | `π-ζ`)
                       (using `][`: `][`): Txn[Boolean] =
        (dir_cap, dir_capʹ) match
          case (`π-local`, `π-local`)   =>
            stm.pure(node == nodeʹ)
          case (`π-s2s`, `π-s2s`)
             | (`π-enter`, `π-accept`)
             | (`π-merge+`, `π-merge-`) =>
            `][`.get.map(_(node).siblings.contains(nodeʹ))
          case (`π-p2c`, `π-c2p`)
             | (`π-expel`, `π-exit`)    =>
            `][`.get.map(_(nodeʹ).root == node)
          case (`π-c2p`, `π-p2c`)       => check(nodeʹ, node, dir_capʹ, dir_cap)
          case (`π-accept`, `π-enter`)  => check(nodeʹ, node, dir_capʹ, dir_cap)
          case (`π-exit`, `π-expel`)    => check(nodeʹ, node, dir_capʹ, dir_cap)
          case (`π-merge-`, `π-merge+`) => check(nodeʹ, node, dir_capʹ, dir_cap)

      object π:

        def apply(key: `)(`, dir: `π-$`, keyʹ: `)(`, dirʹ: `π-$`)
                 (using `][`: `][`, `1`: TSemaphore): F[Unit] =
          stm.commit {
            for
              _     <- `1`.acquire
              node  <- `][`.get.map(_.keys.find(_.contains(key)).get)
              nodeʹ <- `][`.get.map(_.keys.find(_.contains(keyʹ)).get)
              _     <- check(node, nodeʹ, dir, dirʹ).flatMap(stm.check(_))
            yield
              ()
          }

      object ζ:

        private def remove(node: `)*(`, tree: `}{`)
                          (using `][`: `][`): Txn[Unit] =
          val `}{`(_, root, _, siblings) = tree
          `][`.modify { m =>
                        val rtree = m(root)
                        siblings.foldLeft {
                          m + (root -> rtree.copy(children = siblings))
                        } { (m, sibling) =>
                          val tree @ `}{`(_, _, _, siblings) = m(sibling)
                          m + (sibling -> tree.copy(siblings = siblings - node))
                        }
                      }

        private def insert(node: `)*(`, root: `)*(`)
                          (using `][`: `][`): Txn[Unit] =
          for
            _ <- `][`.modify { m =>
                               val tree = m(root)
                               tree.children.foldLeft(m) { (m, child) =>
                                 val tree @ `}{`(_, _, _, siblings) = m(child)
                                 m + (child -> tree.copy(siblings = siblings + node))
                               }
                             }
            _ <- `][`.modify { m =>
                               val ntree = m(node)
                               val rtree @ `}{`(_, _, children, _) = m(root)
                               m + (root -> rtree.copy(children = children + node))
                                 + (node -> ntree.copy(root = root, siblings = children))
                             }
          yield
            ()

        private def update(temp: `}{`, root: `)*(`, join: `)*(`)
                          (using `][`: `][`): Txn[Unit] =
          `][`.modify { m =>
                        val tree @ `}{`(_, _, children, _) = m(temp.root)
                        temp.siblings.foldLeft {
                          m + (temp.root -> tree.copy(children = children - root + join))
                        } { (m, sibling) =>
                          val tree @ `}{`(_, _, _, siblings) = m(sibling)
                          m + (sibling -> tree.copy(siblings = siblings - root + join))
                        }
                      }

        private def merge(tree: `}{`, join: `)*(`)
                         (using `][`: `][`): Txn[Unit] =
          for
            _ <- `][`.modify { tree.children.foldLeft(_) { (m, node) =>
                                val tree = m(node)
                                m + (node -> tree.copy(root = join))
                               }
                             }
            _ <- `][`.modify { m =>
                               val temp @ `}{`(_, _, children, _) = m(join)
                               tree.children.foldLeft {
                                 m + (join -> temp.copy(children = children ++ tree.children))
                               } { (m, node) =>
                                 val tree = m(node)
                                 m + (node -> tree.copy(siblings = tree.siblings ++ children))
                               }
                             }
          yield
            ()

        @annotation.tailrec
        private def apply(node: `)*(`, nodeʹ: `)*(`, cap: `π-ζ`, capʹ: `π-ζ`)
                         (using `][`: `][`): Txn[Unit] =
          cap match
            case `π-enter` | `π-exit` =>
              for
                m            <- `][`.get
                (root, tree)  = cap match
                                  case `π-enter` =>
                                    (nodeʹ, m(node))
                                  case `π-exit` =>
                                    (m(nodeʹ).root, m(node))
                _            <- remove(node, tree)
                _            <- insert(node, root)
              yield
                ()

            case `π-merge+` =>
              for
                m    <- `][`.get
                tree  = m(nodeʹ)
                _    <- remove(nodeʹ, tree)
                m    <- `][`.get
                temp  =  m(node)
                join  = node ++ nodeʹ
                _    <- `][`.modify { _ - node - nodeʹ + (join -> temp) }
                _    <- update(temp, node, join)
                _    <- merge(tree, join)
              yield
                ()

            case _ =>
              apply(nodeʹ, node, capʹ, cap)

        def apply(key: `)(`, cap: `π-ζ`, keyʹ: `)(`, capʹ: `π-ζ`)
                 (using `][`: `][`, `1`: TSemaphore): F[Unit] =
          stm.commit {
            for
              _     <- `1`.acquire
              node  <- `][`.get.map(_.keys.find(_.contains(key)).get)
              nodeʹ <- `][`.get.map(_.keys.find(_.contains(keyʹ)).get)
              _     <- check(node, nodeʹ, cap, capʹ).flatMap(stm.check(_))
              _     <- this(node, nodeʹ, cap, capʹ)
            yield
              ()
          }
