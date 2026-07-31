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

  import _root_.zio.{ Clock, Duration, FiberRef, Promise, Random, Ref, Schedule, Task, UIO, ZIO }
  import _root_.zio.concurrent.CyclicBarrier
  import _root_.zio.stm.{ TRef, TSemaphore }
  import _root_.zio.stm.{ USTM, ZSTM }
  import _root_.zio.stream.ZStream

  import `Π-loop`.{ <>, +, %, /, \ }
  import `Π-stats`.Rate

  import `π-$`.*, `π-ζ`.*


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
    def apply(): UIO[`)(`] =
      Random.nextUUID.map(new `)(`(_))

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


  type `Π-Map`[K, +V] = Map[K, V]

  type `Π-Set`[A] = Set[A]


  inline def `π-exclude`(enabled: String*)
                        (using % : %, \ : \): Task[Unit] =
    `π-exclude`(Set.from(enabled)) *> \

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
                     (using % : %)
                     (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]]): UIO[Unit] =
    ZIO.when(`π-elvis`.contains(key))(`π-exclude`(`π-elvis`(key))).unit


  /**
    * restriction aka new name
    */
  object ν:

    def map[B](f: `()` => B): ZStream[Any, Throwable, B] = flatMap(f andThen ZStream.succeed)
    def flatMap[B](f: `()` => ZStream[Any, Throwable, B]): ZStream[Any, Throwable, B] =
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
  object τ:

    object `(!)`:

      object `(+)`:

        /**
          * linear replication guard
          */
        def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
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
            enabled  <- ZStream.fromZIO(promise.isDone.negate.flatMap(Ref.make))
            `)(`     <- ZStream.fromZIO(`)(`.get)
            timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`)(` -> `π-τ`, (new {}, None, rate))))))
            cb_fb_in <- ZStream.fromZIO(promise.await)
            _        <- if None eq * then ZStream.fromZIO(?.succeed(cb_fb_in eq None) *> ?.await)
                        else ZStream.succeed(false)
            _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
            if !discard
            sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
            _  <- ZStream.fromZIO {
              for
                _        <- -.await.exit
                _        <- *.fold(ZIO.unit)(_.acquire)
                _        <- Clock.nanoTime.flatMap(timestamp.set)
                _        <- (%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \).unlessZIO(enabled.get)
                cb_fb_in <- continue.get.flatMap(_.await)
                _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                _        <- enabled.set(false)
                _        <- if cb_fb_in eq None then sp.succeed(())
                            else
                              val (cbarrier, fiber, _) = cb_fb_in.get
                              cbarrier.await.exit *> fiber.join
              yield
                ()
            }.repeat(Schedule.forever).interruptWhen(sp)
            _  <- ZStream.fromZIO(+.release)
          yield
            ()

        /**
          * linear replication guard w/ pace
          */
        def apply(rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                 (using %, /, \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate)(key, `)(`)(?, -, *, +) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

        /**
          * linear replication guard w/ code
          */
        def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`])(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate)(key, `)(`)(?, -, *, +).tap(_ => code)

        /**
          * linear replication guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate, pace)(key, `)(`)(?, -, *, +).tap(_ => code)

      /**
        * replication guard
        */
      def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`])
               (using % : %, / : /, \ : \)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, Unit] =
        for
          _        <- ZStream.fromZIO(exclude(key))
          promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
          continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
          enabled  <- ZStream.fromZIO(Ref.make(true))
          `)(`     <- ZStream.fromZIO(`)(`.get)
          timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
          _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`)(` -> `π-τ`, (new {}, None, rate))))))
          cb_fb_in <- ZStream.fromZIO(promise.await)
          if cb_fb_in ne None
          sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
          _  <- ZStream.fromZIO {
            for
              _        <- Clock.nanoTime.flatMap(timestamp.set)
              _        <- (%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \).unlessZIO(enabled.get)
              cb_fb_in <- continue.get.flatMap(_.await)
              _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
              _        <- enabled.set(false)
              _        <- if cb_fb_in eq None then sp.succeed(())
                          else
                            val (cbarrier, fiber, _) = cb_fb_in.get
                            cbarrier.await.exit *> fiber.join
            yield
              ()
          }.repeat(Schedule.forever).interruptWhen(sp)
        yield
          ()

      /**
        * replication guard w/ pace
        */
      def apply(rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])
               (using %, /, \)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate)(key, `)(`) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

      /**
        * replication guard w/ code
        */
      def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`])(code: => Task[T])
                  (using %, /, \)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate)(key, `)(`).tap(_ => code)

      /**
        * replication guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(code: => Task[T])
                  (using %, /, \)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate, pace)(key, `)(`).tap(_ => code)

    /**
      * prefix
      */
    def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`])
             (using % : %, / : /)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): ZStream[Any, Throwable, Unit] =
      for
        _        <- ZStream.fromZIO(exclude(key))
        promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
        `)(`     <- ZStream.fromZIO(`)(`.get)
        timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
        _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (timestamp, (`)(` -> `π-τ`, (new {}, None, rate))))))
        cb_fb_in <- ZStream.fromZIO(promise.await)
        if cb_fb_in ne None
        (cbarrier, fiber, _) = cb_fb_in.get
        _        <- ZStream.fromZIO(cbarrier.await.exit *> fiber.join)
      yield
        ()

    /**
      * prefix w/ pace
      */
    def apply(rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])
             (using %, /)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): ZStream[Any, Throwable, Unit] =
      apply(rate)(key, `)(`) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

    /**
      * prefix w/ code
      */
    def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`])(code: => Task[T])
                (using %, /)
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): ZStream[Any, Throwable, Unit] =
      apply(rate)(key, `)(`).tap(_ => code)

    /**
      * prefix w/ pace w/ code
      */
    def apply[T](rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(code: => Task[T])
                (using %, /)
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): ZStream[Any, Throwable, Unit] =
      apply(rate, pace)(key, `)(`).tap(_ => code)

  /**
    * names and values
    */
  implicit final class `()`(private val name: Any) { self =>

    private def map = `()`[Map[Int, {}]]

    def ====(that: `()`) =
      try
        this.map eq that.map
      catch _ =>
        this.name == that.name

    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

    object π:

      object `(!)`:

        object `(+)`:

          object `(ν)`:

            /**
              * linear replication bound output guard
              */
            def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
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
                enabled  <- ZStream.fromZIO(promise.isDone.negate.flatMap(Ref.make))
                `)(`     <- ZStream.fromZIO(`)(`.get)
                timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
                _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`)(` -> dir, (map(dir.ord), Some(Left(())), rate))))))
                cb_fb_in <- ZStream.fromZIO(promise.await)
                _        <- if None eq * then ZStream.fromZIO(?.succeed(cb_fb_in eq None) *> ?.await)
                            else ZStream.succeed(false)
                _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
                if !discard
                sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
                it <- ( for
                          _  <- ZStream.unit.repeat(Schedule.forever)
                          it <- sΠ.ν
                          _  <- ZStream.fromZIO {
                            for
                              _        <- -.await.exit
                              _        <- *.fold(ZIO.unit)(_.acquire)
                              _        <- Clock.nanoTime.flatMap(timestamp.set)
                              _        <- (%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \).unlessZIO(enabled.get)
                              cb_fb_in <- continue.get.flatMap(_.await)
                              _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
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
                _  <- ZStream.fromZIO(+.release)
              yield
                it

            /**
              * linear replication bound output guard w/ pace
              */
            def apply(rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                     (using %, /, \)
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                               ^ : String): ZStream[Any, Throwable, `()`] =
              apply(rate)(key, `)(`)(dir)(?, -, *, +) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

            /**
              * linear replication bound output guard w/ code
              */
            def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                        (using %, /, \)
                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                  ^ : String): ZStream[Any, Throwable, `()`] =
              apply(rate)(key, `)(`)(dir)(?, -, *, +).tap(_ => code)

            /**
              * linear replication bound output guard w/ pace w/ code
              */
            def apply[T](rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                        (using %, /, \)
                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                  ^ : String): ZStream[Any, Throwable, `()`] =
              apply(rate, pace)(key, `)(`)(dir)(?, -, *, +).tap(_ => code)

          /**
            * linear constant replication output guard
            */
          def apply(rate: Rate, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
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
              enabled  <- ZStream.fromZIO(promise.isDone.negate.flatMap(Ref.make))
              `)(`     <- ZStream.fromZIO(`)(`.get)
              timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
              _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`)(` -> dir, (map(dir.ord), Some(Left(())), rate))))))
              cb_fb_in <- ZStream.fromZIO(promise.await)
              _        <- if None eq * then ZStream.fromZIO(?.succeed(cb_fb_in eq None) *> ?.await)
                          else ZStream.succeed(false)
              _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
              if !discard
              sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
              _  <- ZStream.fromZIO {
                for
                  _        <- -.await.exit
                  _        <- *.fold(ZIO.unit)(_.acquire)
                  _        <- Clock.nanoTime.flatMap(timestamp.set)
                  _        <- (%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \).unlessZIO(enabled.get)
                  cb_fb_in <- continue.get.flatMap(_.await)
                  _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                  _        <- enabled.set(false)
                  _        <- if cb_fb_in eq None then sp.succeed(())
                              else
                                val (cbarrier, fiber, input) = cb_fb_in.get
                                input.set(value) *> cbarrier.await.exit *> fiber.join
                yield
                  ()
              }.repeat(Schedule.forever).interruptWhen(sp)
              _  <- ZStream.fromZIO(+.release)
            yield
              ()

          /**
            * linear constant replication output guard w/ pace
            */
          def apply(rate: Rate, pace: Duration, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                   (using %, /, \)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): ZStream[Any, Throwable, Unit] =
            apply(rate, value)(key, `)(`)(dir)(?, -, *, +) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

          /**
            * linear constant replication output guard w/ code
            */
          def apply[T](rate: Rate, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                      (using %, /, \)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, Unit] =
            apply(rate, value)(key, `)(`)(dir)(?, -, *, +).tap(_ => code)

          /**
            * linear constant replication output guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: Duration, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                      (using %, /, \)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, Unit] =
            apply(rate, pace, value)(key, `)(`)(dir)(?, -, *, +).tap(_ => code)

          object `(*)`:

            /**
              * linear variable replication output guard
              */
            def apply[S: ClassTag](_1: 1)(rate: Rate, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                                         (using DummyImplicit)
                                         (using %, /, \)
                                         (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                   `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                   ^ : String): ZStream[Any, Throwable, Unit] =
              if classTag[S].runtimeClass eq self.getClass
              then
                self.π.`(!)`.`(+)`(rate, value.asInstanceOf[`()`])(key, `)(`)(dir)(?, -, *, +)
              else
                apply[S](1)(rate, ZIO.attempt(value))(key, `)(`)(dir)(?, -, *, +)

            /**
              * linear variable replication output guard w/ pace
              */
            def apply[S: ClassTag](_2: 2)(rate: Rate, pace: Duration, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                                         (using DummyImplicit)
                                         (using %, /, \)
                                         (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                   `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                   ^ : String): ZStream[Any, Throwable, Unit] =
              if classTag[S].runtimeClass eq self.getClass
              then
                self.π.`(!)`.`(+)`(rate, pace, value.asInstanceOf[`()`])(key, `)(`)(dir)(?, -, *, +)
              else
                apply[S](2)(rate, pace, ZIO.attempt(value))(key, `)(`)(dir)(?, -, *, +)

            /**
              * linear variable replication output guard w/ code
              */
            def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                                            (using DummyImplicit)
                                            (using %, /, \)
                                            (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                      `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                      ^ : String): ZStream[Any, Throwable, Unit] =
              if classTag[S].runtimeClass eq self.getClass
              then
                self.π.`(!)`.`(+)`(rate, value.asInstanceOf[`()`])(key, `)(`)(dir)(code)(?, -, *, +)
              else
                apply[S, T](3)(rate, ZIO.attempt(value))(key, `)(`)(dir)(code)(?, -, *, +)

            /**
              * linear variable replication output guard w/ pace w/ code
              */
            def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: Duration, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                                            (using DummyImplicit)
                                            (using %, /, \)
                                            (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                      `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                      ^ : String): ZStream[Any, Throwable, Unit] =
              if classTag[S].runtimeClass eq self.getClass
              then
                self.π.`(!)`.`(+)`(rate, pace, value.asInstanceOf[`()`])(key, `)(`)(dir)(code)(?, -, *, +)
              else
                apply[S, T](4)(rate, pace, ZIO.attempt(value))(key, `)(`)(dir)(code)(?, -, *, +)

            /**
              * linear variable replication output guard
              */
            def apply[S: ClassTag](_1: 1)(rate: Rate, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                                         (using % : %, / : /, \ : \)
                                         (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                   `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                   ^ : String): ZStream[Any, Throwable, Unit] =
              if classTag[S].runtimeClass eq self.getClass
              then
                ZStream.fromZIO(ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]])).flatMap(self.π.`(!)`.`(+)`(rate, _)(key, `)(`)(dir)(?, -, *, +))
              else
                for
                  discard  <- if None eq * then ZStream.fromZIO(exclude(key)) *> ZStream.succeed(false)
                              else ZStream.fromZIO(?.await)
                  _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
                  if !discard
                  promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
                  continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
                  _        <- if None eq * then ZStream.unit
                              else ZStream.fromZIO(promise.succeed(None))
                  enabled  <- ZStream.fromZIO(promise.isDone.negate.flatMap(Ref.make))
                  `)(`     <- ZStream.fromZIO(`)(`.get)
                  timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
                  _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`)(` -> dir, (map(dir.ord), Some(Left(())), rate))))))
                  cb_fb_in <- ZStream.fromZIO(promise.await)
                  _        <- if None eq * then ZStream.fromZIO(?.succeed(cb_fb_in eq None) *> ?.await)
                              else ZStream.succeed(false)
                  _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
                  if !discard
                  sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
                  _  <- ZStream.fromZIO {
                    for
                      _        <- -.await.exit
                      _        <- *.fold(ZIO.unit)(_.acquire)
                      _        <- Clock.nanoTime.flatMap(timestamp.set)
                      _        <- (%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \).unlessZIO(enabled.get)
                      cb_fb_in <- continue.get.flatMap(_.await)
                      _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                      _        <- enabled.set(false)
                      _        <- if cb_fb_in eq None then sp.succeed(())
                                  else
                                    val (cbarrier, fiber, input) = cb_fb_in.get
                                    value.map(new `()`(_)).flatMap(input.set(_) *> cbarrier.await.exit *> fiber.join)
                    yield
                      ()
                  }.repeat(Schedule.forever).interruptWhen(sp)
                  _  <- ZStream.fromZIO(+.release)
                yield
                  ()

            /**
              * linear variable replication output guard w/ pace
              */
            def apply[S: ClassTag](_2: 2)(rate: Rate, pace: Duration, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                                         (using %, /, \)
                                         (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                   `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                   ^ : String): ZStream[Any, Throwable, Unit] =
              apply[S](1)(rate, value)(key, `)(`)(dir)(?, -, *, +) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

            /**
              * linear variable replication output guard w/ code
              */
            def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                                            (using %, /, \)
                                            (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                      `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                      ^ : String): ZStream[Any, Throwable, Unit] =
              apply[S](1)(rate, value)(key, `)(`)(dir)(?, -, *, +).tap(_ => code)

            /**
              * linear variable replication output guard w/ pace w/ code
              */
            def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: Duration, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                                            (using %, /, \)
                                            (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                      `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                      ^ : String): ZStream[Any, Throwable, Unit] =
              apply[S](2)(rate, pace, value)(key, `)(`)(dir)(?, -, *, +).tap(_ => code)

          /**
            * linear replication input guard
            */
          def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
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
              enabled  <- ZStream.fromZIO(promise.isDone.negate.flatMap(Ref.make))
              `)(`     <- ZStream.fromZIO(`)(`.get)
              result   <- ZStream.fromZIO(Ref.make[`()`](null))
              timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
              _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`)(` -> dir, (map(dir.ord), Some(Right(result)), rate))))))
              cb_fb_in <- ZStream.fromZIO(promise.await)
              _        <- if None eq * then ZStream.fromZIO(?.succeed(cb_fb_in eq None) *> ?.await)
                          else ZStream.succeed(false)
              _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
              if !discard
              sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
              _  <- ZStream.fromZIO {
                for
                  _        <- -.await.exit
                  _        <- *.fold(ZIO.unit)(_.acquire)
                  _        <- Clock.nanoTime.flatMap(timestamp.set)
                  _        <- (%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \).unlessZIO(enabled.get)
                  cb_fb_in <- continue.get.flatMap(_.await)
                  _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                  _        <- enabled.set(false)
                  _        <- if cb_fb_in eq None then sp.succeed(())
                              else
                                val (cbarrier, fiber, _) = cb_fb_in.get
                                cbarrier.await.exit *> fiber.join
                yield
                  ()
              }.repeat(Schedule.forever).interruptWhen(sp)
              _  <- ZStream.fromZIO(+.release)
              it <- ZStream.fromZIO(result.get)
            yield
              it

          /**
            * linear replication input guard w/ pace
            */
          def apply(rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                   (using %, /, \)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): ZStream[Any, Throwable, `()`] =
            apply(rate)(key, `)(`)(dir)(?, -, *, +) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

          /**
            * linear replication input guard w/ code
            */
          def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: T => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                      (using %, /, \)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, `()`] =
            apply(rate)(key, `)(`)(dir)(?, -, *, +).mapZIO { it => code(it.`()`[T]).map(new `()`(_)) }

          /**
            * linear replication input guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: T => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                      (using %, /, \)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, `()`] =
            apply(rate, pace)(key, `)(`)(dir)(?, -, *, +).mapZIO { it => code(it.`()`[T]).map(new `()`(_)) }

        object `(ν)`:

          /**
            * replication bound output guard
            */
          def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                   (using % : %, / : /, \ : \)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): ZStream[Any, Throwable, `()`] =
            for
              _        <- ZStream.fromZIO(exclude(key))
              promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
              continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
              enabled  <- ZStream.fromZIO(Ref.make(true))
              `)(`     <- ZStream.fromZIO(`)(`.get)
              timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
              _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`)(` -> dir, (map(dir.ord), Some(Left(())), rate))))))
              cb_fb_in <- ZStream.fromZIO(promise.await)
              if cb_fb_in ne None
              sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
              it <- ( for
                        _  <- ZStream.unit.repeat(Schedule.forever)
                        it <- sΠ.ν
                        _  <- ZStream.fromZIO {
                          for
                            _        <- Clock.nanoTime.flatMap(timestamp.set)
                            _        <- (%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \).unlessZIO(enabled.get)
                            cb_fb_in <- continue.get.flatMap(_.await)
                            _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
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
            yield
              it

          /**
            * replication bound output guard w/ pace
            */
          def apply(rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                   (using %, /, \)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): ZStream[Any, Throwable, `()`] =
            apply(rate)(key, `)(`)(dir) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

          /**
            * replication bound output guard w/ code
            */
          def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                      (using %, /, \)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, `()`] =
            apply(rate)(key, `)(`)(dir).tap(_ => code)

          /**
            * replication bound output guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                      (using %, /, \)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, `()`] =
            apply(rate, pace)(key, `)(`)(dir).tap(_ => code)

        /**
          * constant replication output guard
          */
        def apply(rate: Rate, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                 (using % : %, / : /, \ : \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, Unit] =
          for
            _        <- ZStream.fromZIO(exclude(key))
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
            enabled  <- ZStream.fromZIO(Ref.make(true))
            `)(`     <- ZStream.fromZIO(`)(`.get)
            timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`)(` -> dir, (map(dir.ord), Some(Left(())), rate))))))
            cb_fb_in <- ZStream.fromZIO(promise.await)
            if cb_fb_in ne None
            sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
            _  <- ZStream.fromZIO {
              for
                _        <- Clock.nanoTime.flatMap(timestamp.set)
                _        <- (%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \).unlessZIO(enabled.get)
                cb_fb_in <- continue.get.flatMap(_.await)
                _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                _        <- enabled.set(false)
                _        <- if cb_fb_in eq None then sp.succeed(())
                            else
                              val (cbarrier, fiber, input) = cb_fb_in.get
                              input.set(value) *> cbarrier.await.exit *> fiber.join
              yield
                ()
            }.repeat(Schedule.forever).interruptWhen(sp)
          yield
            ()

        /**
          * constant replication output guard w/ pace
          */
        def apply(rate: Rate, pace: Duration, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                 (using %, /, \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate, value)(key, `)(`)(dir) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

        /**
          * constant replication output guard w/ code
          */
        def apply[T](rate: Rate, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                 (using %, /, \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate, value)(key, `)(`)(dir).tap(_ => code)

        /**
          * constant replication output guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: Duration, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                 (using %, /, \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate, pace, value)(key, `)(`)(dir).tap(_ => code)

        object `(*)`:

          /**
            * variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(rate: Rate, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                                       (using DummyImplicit)
                                       (using %, /, \)
                                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                 ^ : String): ZStream[Any, Throwable, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.π.`(!)`(rate, value.asInstanceOf[`()`])(key, `)(`)(dir)
            else
              apply[S](1)(rate, ZIO.attempt(value))(key, `)(`)(dir)

          /**
            * variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(rate: Rate, pace: Duration, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                                       (using DummyImplicit)
                                       (using %, /, \)
                                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                 ^ : String): ZStream[Any, Throwable, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.π.`(!)`(rate, pace, value.asInstanceOf[`()`])(key, `)(`)(dir)
            else
              apply[S](2)(rate, pace, ZIO.attempt(value))(key, `)(`)(dir)

          /**
            * variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                                          (using DummyImplicit)
                                          (using %, /, \)
                                          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                    `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                    ^ : String): ZStream[Any, Throwable, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.π.`(!)`(rate, value.asInstanceOf[`()`])(key, `)(`)(dir)(code)
            else
              apply[S, T](3)(rate, ZIO.attempt(value))(key, `)(`)(dir)(code)

          /**
            * variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: Duration, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                                          (using DummyImplicit)
                                          (using %, /, \)
                                          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                    `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                    ^ : String): ZStream[Any, Throwable, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.π.`(!)`(rate, pace, value.asInstanceOf[`()`])(key, `)(`)(dir)(code)
            else
              apply[S, T](4)(rate, pace, ZIO.attempt(value))(key, `)(`)(dir)(code)

          /**
            * variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(rate: Rate, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                                       (using % : %, / : /, \ : \)
                                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                 ^ : String): ZStream[Any, Throwable, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              ZStream.fromZIO(ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]])).flatMap(self.π.`(!)`(rate, _)(key, `)(`)(dir))
            else
              for
                _        <- ZStream.fromZIO(exclude(key))
                promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
                continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
                enabled  <- ZStream.fromZIO(Ref.make(true))
                `)(`     <- ZStream.fromZIO(`)(`.get)
                timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
                _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`)(` -> dir, (map(dir.ord), Some(Left(())), rate))))))
                cb_fb_in <- ZStream.fromZIO(promise.await)
                if cb_fb_in ne None
                sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
                _  <- ZStream.fromZIO {
                  for
                    _        <- Clock.nanoTime.flatMap(timestamp.set)
                    _        <- (%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \).unlessZIO(enabled.get)
                    cb_fb_in <- continue.get.flatMap(_.await)
                    _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                    _        <- enabled.set(false)
                    _        <- if cb_fb_in eq None then sp.succeed(())
                                else
                                  val (cbarrier, fiber, input) = cb_fb_in.get
                                  value.map(new `()`(_)).flatMap(input.set(_) *> cbarrier.await.exit *> fiber.join)
                  yield
                    ()
                }.repeat(Schedule.forever).interruptWhen(sp)
              yield
                ()

          /**
            * variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(rate: Rate, pace: Duration, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                                       (using %, /, \)
                                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                 ^ : String): ZStream[Any, Throwable, Unit] =
            apply[S](1)(rate, value)(key, `)(`)(dir) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

          /**
            * variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                                          (using %, /, \)
                                          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                    `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                    ^ : String): ZStream[Any, Throwable, Unit] =
            apply[S](1)(rate, value)(key, `)(`)(dir).tap(_ => code)

          /**
            * variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: Duration, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                                          (using %, /, \)
                                          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                    `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                    ^ : String): ZStream[Any, Throwable, Unit] =
            apply[S](2)(rate, pace, value)(key, `)(`)(dir).tap(_ => code)

        /**
          * replication input guard
          */
        def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                 (using % : %, / : /, \ : \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, `()`] =
          for
            _        <- ZStream.fromZIO(exclude(key))
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
            enabled  <- ZStream.fromZIO(Ref.make(true))
            `)(`     <- ZStream.fromZIO(`)(`.get)
            result   <- ZStream.fromZIO(Ref.make[`()`](null))
            timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`)(` -> dir, (map(dir.ord), Some(Right(result)), rate))))))
            cb_fb_in <- ZStream.fromZIO(promise.await)
            if cb_fb_in ne None
            sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
            _  <- ZStream.fromZIO {
              for
                _        <- Clock.nanoTime.flatMap(timestamp.set)
                _        <- (%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \).unlessZIO(enabled.get)
                cb_fb_in <- continue.get.flatMap(_.await)
                _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                _        <- enabled.set(false)
                _        <- if cb_fb_in eq None then sp.succeed(())
                            else
                              val (cbarrier, fiber, _) = cb_fb_in.get
                              cbarrier.await.exit *> fiber.join
              yield
                ()
            }.repeat(Schedule.forever).interruptWhen(sp)
            it <- ZStream.fromZIO(result.get)
          yield
            it

        /**
          * replication input guard w/ pace
          */
        def apply(rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                 (using %, /, \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, `()`] =
          apply(rate)(key, `)(`)(dir) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

        /**
          * replication input guard w/ code
          */
        def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: T => Task[T])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, `()`] =
          apply(rate)(key, `)(`)(dir).mapZIO { it => code(it.`()`[T]).map(new `()`(_)) }

        /**
          * replication input guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: T => Task[T])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, `()`] =
          apply(rate, pace)(key, `)(`)(dir).mapZIO { it => code(it.`()`[T]).map(new `()`(_)) }

      object `(ν)`:

        /**
          * bound output prefix
          */
        def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                 (using % : %, / : /)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, `()`] =
          for
            _        <- ZStream.fromZIO(exclude(key))
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            `)(`     <- ZStream.fromZIO(`)(`.get)
            timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (timestamp, (`)(` -> dir, (map(dir.ord), Some(Left(())), rate))))))
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
        def apply(rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                 (using %, /)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, `()`] =
          apply(rate)(key, `)(`)(dir) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

        /**
          * bound output prefix w/ code
          */
        def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                    (using %, /)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, `()`] =
          apply(rate)(key, `)(`)(dir).tap(_ => code)

        /**
          * bound output prefix w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                    (using %, /)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, `()`] =
          apply(rate, pace)(key, `)(`)(dir).tap(_ => code)

      /**
        * constant output prefix
        */
      def apply(rate: Rate, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
               (using % : %, / : /)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, Unit] =
        for
          _        <- ZStream.fromZIO(exclude(key))
          promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
          `)(`     <- ZStream.fromZIO(`)(`.get)
          timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
          _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (timestamp, (`)(` -> dir, (map(dir.ord), Some(Left(())), rate))))))
          cb_fb_in <- ZStream.fromZIO(promise.await)
          if cb_fb_in ne None
          (cbarrier, fiber, input) = cb_fb_in.get
          _  <- ZStream.fromZIO(input.set(value) *> cbarrier.await.exit *> fiber.join)
        yield
          ()

      /**
        * constant output prefix w/ pace
        */
      def apply(rate: Rate, pace: Duration, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
               (using %, /)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate, value)(key, `)(`)(dir) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * constant output prefix w/ code
        */
      def apply[T](rate: Rate, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                  (using %, /)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate, value)(key, `)(`)(dir).tap(_ => code)

      /**
        * constant output prefix w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: Duration, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                  (using %, /)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate, pace, value)(key, `)(`)(dir).tap(_ => code)

      object `(*)`:

        /**
          * variable output prefix
          */
        def apply[S: ClassTag](_1: 1)(rate: Rate, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                                     (using DummyImplicit)
                                     (using %, /)
                                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                               ^ : String): ZStream[Any, Throwable, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.π(rate, value.asInstanceOf[`()`])(key, `)(`)(dir)
          else
            apply[S](1)(rate, ZIO.attempt(value))(key, `)(`)(dir)

        /**
          * variable output prefix w/ pace
          */
        def apply[S: ClassTag](_2: 2)(rate: Rate, pace: Duration, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                                     (using DummyImplicit)
                                     (using %, /)
                                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                               ^ : String): ZStream[Any, Throwable, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.π(rate, pace, value.asInstanceOf[`()`])(key, `)(`)(dir)
          else
            apply[S](1)(rate, value)(key, `)(`)(dir) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

        /**
          * variable output prefix w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                                        (using DummyImplicit)
                                        (using %, /)
                                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                  ^ : String): ZStream[Any, Throwable, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.π(rate, value.asInstanceOf[`()`])(key, `)(`)(dir)(code)
          else
            apply[S](1)(rate, value)(key, `)(`)(dir).tap(_ => code)

        /**
          * variable output prefix w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: Duration, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                                        (using DummyImplicit)
                                        (using %, /)
                                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                  ^ : String): ZStream[Any, Throwable, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            self.π(rate, pace, value.asInstanceOf[`()`])(key, `)(`)(dir)(code)
          else
            apply[S](2)(rate, pace, value)(key, `)(`)(dir).tap(_ => code)

        /**
          * variable output prefix
          */
        def apply[S: ClassTag](_1: 1)(rate: Rate, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                                     (using % : %, / : /)
                                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                               ^ : String): ZStream[Any, Throwable, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            ZStream.fromZIO(ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]])).flatMap(self.π(rate, _)(key, `)(`)(dir))
          else
            for
              _        <- ZStream.fromZIO(exclude(key))
              promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
              `)(`     <- ZStream.fromZIO(`)(`.get)
              timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
              _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (timestamp, (`)(` -> dir, (map(dir.ord), Some(Left(())), rate))))))
              cb_fb_in <- ZStream.fromZIO(promise.await)
              if cb_fb_in ne None
              (cbarrier, fiber, input) = cb_fb_in.get
              _  <- ZStream.fromZIO(value.map(new `()`(_)).flatMap(input.set(_) *> cbarrier.await.exit *> fiber.join))
            yield
              ()

        /**
          * variable output prefix w/ pace
          */
        def apply[S: ClassTag](_2: 2)(rate: Rate, pace: Duration, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                                     (using %, /)
                                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                               ^ : String): ZStream[Any, Throwable, Unit] =
          apply[S](1)(rate, value)(key, `)(`)(dir) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

        /**
          * variable output prefix w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                                        (using %, /)
                                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                  ^ : String): ZStream[Any, Throwable, Unit] =
          apply[S](1)(rate, value)(key, `)(`)(dir).tap(_ => code)

        /**
          * variable output prefix w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: Duration, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                                        (using %, /)
                                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                                  ^ : String): ZStream[Any, Throwable, Unit] =
          apply[S](2)(rate, pace, value)(key, `)(`)(dir).tap(_ => code)

      /**
        * input prefix
        */
      def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
               (using % : %, / : /)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, `()`] =
        for
          _        <- ZStream.fromZIO(exclude(key))
          promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
          `)(`     <- ZStream.fromZIO(`)(`.get)
          result   <- ZStream.fromZIO(Ref.make[`()`](null))
          timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
          _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (timestamp, (`)(` -> dir, (map(dir.ord), Some(Right(result)), rate))))))
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
      def apply(rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
               (using %, /)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, `()`] =
        apply(rate)(key, `)(`)(dir) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * input prefix w/ code
        */
      def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: T => Task[T])
                  (using %, /)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, `()`] =
        apply(rate)(key, `)(`)(dir).mapZIO { it => code(it.`()`[T]).map(new `()`(_)) }

      /**
        * input prefix w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: T => Task[T])
                  (using %, /)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, `()`] =
        apply(rate, pace)(key, `)(`)(dir).mapZIO { it => code(it.`()`[T]).map(new `()`(_)) }

    object ζ:

      object `(!)`:

        object `(+)`:

          /**
            * linear replication capability guard
            */
          def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                   (using % : %, / : /, \ : \)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): ZStream[Any, Throwable, Unit] =
            for
              _        <- ZStream.fromZIO(exclude(key))
              promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
              continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
              enabled  <- ZStream.fromZIO(Ref.make(true))
              polarity  = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
              `)(`     <- ZStream.fromZIO(`)(`.get)
              timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
              _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`)(` -> cap, (map(cap.ord), Some(if polarity then Right(null) else Left(())), rate))))))
              cb_fb_in <- ZStream.fromZIO(promise.await)
              if cb_fb_in ne None
              sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
              _  <- ZStream.fromZIO {
                for
                  _        <- -.await.exit
                  _        <- *.fold(ZIO.unit)(_.acquire)
                  _        <- Clock.nanoTime.flatMap(timestamp.set)
                  _        <- (%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \).unlessZIO(enabled.get)
                  cb_fb_in <- continue.get.flatMap(_.await)
                  _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                  _        <- enabled.set(false)
                  _        <- if cb_fb_in eq None then sp.succeed(())
                              else
                                val (cbarrier, fiber, _) = cb_fb_in.get
                                cbarrier.await.exit *> fiber.join
                 yield
                   ()
              }.repeat(Schedule.forever).interruptWhen(sp)
              _  <- ZStream.fromZIO(+.release)
            yield
              ()

          /**
            * linear replication capability guard w/ pace
            */
          def apply(rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                   (using %, /, \)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): ZStream[Any, Throwable, Unit] =
            apply(rate)(key, `)(`)(cap)(?, -, *, +) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

          /**
            * linear replication capability guard w/ code
            */
          def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                      (using %, /, \)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, Unit] =
            apply(rate)(key, `)(`)(cap)(?, -, *, +).tap(_ => code)

          /**
            * linear replication capability guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                      (using %, /, \)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, Unit] =
            apply(rate, pace)(key, `)(`)(cap)(?, -, *, +).tap(_ => code)

        /**
          * replication capability guard
          */
        def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)
                 (using % : %, / : /, \ : \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, Unit] =
          for
            _        <- ZStream.fromZIO(exclude(key))
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
            enabled  <- ZStream.fromZIO(Ref.make(true))
            polarity  = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
            `)(`     <- ZStream.fromZIO(`)(`.get)
            timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (timestamp, (`)(` -> cap, (map(cap.ord), Some(if polarity then Right(null) else Left(())), rate))))))
            cb_fb_in <- ZStream.fromZIO(promise.await)
            if cb_fb_in ne None
            sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
            _  <- ZStream.fromZIO {
              for
                _        <- Clock.nanoTime.flatMap(timestamp.set)
                _        <- (%.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \).unlessZIO(enabled.get)
                cb_fb_in <- continue.get.flatMap(_.await)
                _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                _        <- enabled.set(false)
                _        <- if cb_fb_in eq None then sp.succeed(())
                            else
                              val (cbarrier, fiber, _) = cb_fb_in.get
                              cbarrier.await.exit *> fiber.join
               yield
                 ()
            }.interruptWhen(sp)
          yield
            ()

        /**
          * replication capability guard w/ pace
          */
        def apply(rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)
                 (using %, /, \)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate)(key, `)(`)(cap) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

        /**
          * replication capability guard w/ code
          */
        def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)(code: => Task[T])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate)(key, `)(`)(cap).tap(_ => code)

        /**
          * replication capability guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)(code: => Task[T])
                    (using %, /, \)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate, pace)(key, `)(`)(cap).tap(_ => code)

      /**
        * capability prefix
        */
      def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)
               (using % : %, / : /)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, Unit] =
        for
          _        <- ZStream.fromZIO(exclude(key))
          promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
          polarity  = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
          `)(`     <- ZStream.fromZIO(`)(`.get)
          timestamp <- ZStream.fromZIO(Clock.nanoTime.flatMap(Ref.make))
          _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (timestamp, (`)(` -> cap, (map(cap.ord), Some(if polarity then Right(null) else Left(())), rate))))))
          cb_fb_in <- ZStream.fromZIO(promise.await)
          if cb_fb_in ne None
          (cbarrier, fiber, _) = cb_fb_in.get
          _  <- ZStream.fromZIO(cbarrier.await.exit *> fiber.join)
        yield
          ()

      /**
        * capability prefix w/ pace
        */
      def apply(rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)
               (using %, /)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate)(key, `)(`)(cap) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * capability prefix w/ code
        */
      def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)(code: => Task[T])
                  (using %, /)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate)(key, `)(`)(cap).tap(_ => code)

      /**
        * capability prefix w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)(code: => Task[T])
                  (using %, /)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate, pace)(key, `)(`)(cap).tap(_ => code)

    override def toString: String = if name == null then "null" else name.toString

  }


  object `}{`:

    /**
      * Ambients' trees' nodes.
      */
    final case class `}{`(label: Option[String],
                          root: `)*(`,
                          children: Set[`)*(`],
                          siblings: Set[`)*(`])

    object `}{`:
      def apply(`)(`: FiberRef[`)(`], label: Option[String])
               (using `][`: `][`, `1`: TSemaphore): UIO[Unit] =
        for
          key  <- `)(`.get
          uuid <- sΠ.`)(`()
          node  = Set(uuid)
          _    <- `)(`.set(uuid)
          _    <- ( for
                      _ <- `1`.acquire
                      _ <- `][`.update { m =>
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
                  ).commit
        yield
          ()

      /**
        * The label and the snapshot.
        */
      def apply(key: `)(`, snapshot: Boolean = false)
               (using `][`: `][`): USTM[(String, String)] =
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
    type `][` = TRef[Map[`)*(`, `}{`]]

    object `][`:
      def apply(): UIO[(`)(`, `][`, TSemaphore)] =
        for
          uuid <- sΠ.`)(`()
          root  = Set(uuid)
          map   = Map(root -> `}{`(None, null, Set.empty, Set.empty))
          tree <- TRef.make[Map[`)*(`, `}{`]](map).commit
          sem  <- TSemaphore.make(1).commit
        yield
          (uuid, tree, sem)

    object >< :

      @annotation.tailrec
      private def check(node: `)*(`,
                        nodeʹ: `)*(`,
                        dir_cap: `π-$` | `π-ζ`,
                        dir_capʹ: `π-$` | `π-ζ`)
                       (using `][`: `][`): USTM[Boolean] =
        (dir_cap, dir_capʹ) match
          case (`π-local`, `π-local`)   =>
            ZSTM.succeed(node == nodeʹ)
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
                 (using `][`: `][`, `1`: TSemaphore): UIO[Unit] =
          ( for
              _     <- `1`.acquire
              node  <- `][`.get.map(_.keys.find(_.contains(key)).get)
              nodeʹ <- `][`.get.map(_.keys.find(_.contains(keyʹ)).get)
              _     <- check(node, nodeʹ, dir, dirʹ).flatMap(ZSTM.check(_))
            yield
              ()
          ).commit

      object ζ:

        private def remove(node: `)*(`, tree: `}{`)
                          (using `][`: `][`): USTM[Unit] =
          val `}{`(_, root, _, siblings) = tree
          `][`.update { m =>
                        val rtree = m(root)
                        siblings.foldLeft {
                          m + (root -> rtree.copy(children = siblings))
                        } { (m, sibling) =>
                          val tree @ `}{`(_, _, _, siblings) = m(sibling)
                          m + (sibling -> tree.copy(siblings = siblings - node))
                        }
                      }

        private def insert(node: `)*(`, root: `)*(`)
                          (using `][`: `][`): USTM[Unit] =
          for
            _ <- `][`.update { m =>
                               val tree = m(root)
                               tree.children.foldLeft(m) { (m, child) =>
                                 val tree @ `}{`(_, _, _, siblings) = m(child)
                                 m + (child -> tree.copy(siblings = siblings + node))
                               }
                             }
            _ <- `][`.update { m =>
                               val ntree = m(node)
                               val rtree @ `}{`(_, _, children, _) = m(root)
                               m + (root -> rtree.copy(children = children + node))
                                 + (node -> ntree.copy(root = root, siblings = children))
                             }
          yield
            ()

        private def update(temp: `}{`, root: `)*(`, join: `)*(`)
                          (using `][`: `][`): USTM[Unit] =
          `][`.update { m =>
                        val tree @ `}{`(_, _, children, _) = m(temp.root)
                        temp.siblings.foldLeft {
                          m + (temp.root -> tree.copy(children = children - root + join))
                        } { (m, sibling) =>
                          val tree @ `}{`(_, _, _, siblings) = m(sibling)
                          m + (sibling -> tree.copy(siblings = siblings - root + join))
                        }
                      }

        private def merge(tree: `}{`, join: `)*(`)
                         (using `][`: `][`): USTM[Unit] =
          for
            _ <- `][`.update { tree.children.foldLeft(_) { (m, node) =>
                                val tree = m(node)
                                m + (node -> tree.copy(root = join))
                               }
                             }
            _ <- `][`.update { m =>
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
                         (using `][`: `][`): USTM[Unit] =
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
                _    <- `][`.update { _ - node - nodeʹ + (join -> temp) }
                _    <- update(temp, node, join)
                _    <- merge(tree, join)
              yield
                ()

            case _ =>
              apply(nodeʹ, node, capʹ, cap)

        def apply(key: `)(`, cap: `π-ζ`, keyʹ: `)(`, capʹ: `π-ζ`)
                 (using `][`: `][`, `1`: TSemaphore): UIO[Unit] =
          ( for
              _     <- `1`.acquire
              node  <- `][`.get.map(_.keys.find(_.contains(key)).get)
              nodeʹ <- `][`.get.map(_.keys.find(_.contains(keyʹ)).get)
              _     <- check(node, nodeʹ, cap, capʹ).flatMap(ZSTM.check(_))
              _     <- this(node, nodeʹ, cap, capʹ)
            yield
              ()
          ).commit
