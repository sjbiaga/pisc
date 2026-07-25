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
  import _root_.zio.{ Duration, FiberRef, Hub, Promise, Random, Ref, Schedule, Task, UIO, ZIO }
  import _root_.zio.concurrent.CyclicBarrier
  import _root_.zio.stm.{ TRef, TSemaphore }
  import _root_.zio.stm.{ USTM, ZSTM }
  import _root_.zio.stream.{ ZSink, ZStream }

  import `Π-loop`.{ <>, +, %, /, \ }
  import `Π-magic`.*
  export `Π-magic`.>*<
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
                    (using % : %)
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
                     (using % : %)
                     (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]]): UIO[Unit] =
    `π-exclude`(`π-elvis`(key)).when(`π-elvis`.contains(key)).unit


  /**
    * restriction aka new name
    */
  object ν:

    def map[B](f: `()` => B): ZStream[Any, Throwable, B] = flatMap(f andThen ZStream.succeed)
    def flatMap[B](f: `()` => ZStream[Any, Throwable, B]): ZStream[Any, Throwable, B] =
      ( for
          map <- ZStream.fromZIO {
            for
              local_hub   <- Hub.unbounded[(`()`, Object)]
              local_limit <- Semaphore[Task](0)
              s2s_hub   <- Hub.unbounded[(`()`, Object)]
              s2s_limit <- Semaphore[Task](0)
              p2c_hub   <- Hub.unbounded[(`()`, Object)]
              p2c_limit <- Semaphore[Task](0)
              accept_hub   <- Hub.unbounded[(`()`, Object)]
              accept_limit <- Semaphore[Task](0)
              expel_hub   <- Hub.unbounded[(`()`, Object)]
              expel_limit <- Semaphore[Task](0)
              merge_hub   <- Hub.unbounded[(`()`, Object)]
              merge_limit <- Semaphore[Task](0)
            yield
              Map(
                `π-local`.ord  -> ><(local_hub, local_limit),
                `π-s2s`.ord    -> ><(s2s_hub, s2s_limit),
                `π-p2c`.ord    -> ><(p2c_hub, p2c_limit),
                `π-accept`.ord -> ><(accept_hub, accept_limit),
                `π-expel`.ord  -> ><(expel_hub, expel_limit),
                `π-merge+`.ord -> ><(merge_hub, merge_limit)
              )
          }
        yield
          f(map)
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
            `)(`     <- ZStream.fromZIO(`)(`.get)
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`)(` -> `π-τ`, (new Object -> -1, None, rate)))))
            cb_fb_tk <- ZStream.fromZIO(promise.await)
            _        <- if None eq * then ZStream.fromZIO(?.succeed(cb_fb_tk eq None) *> ?.await)
                        else ZStream.succeed(false)
            _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
            if !discard
            sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
            _  <- ZStream.fromZIO {
              for
                _        <- -.await.exit
                _        <- *.fold(ZIO.unit)(_.acquire)
                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                cb_fb_tk <- continue.get.flatMap(_.await)
                _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                _        <- if cb_fb_tk eq None then sp.succeed(())
                            else
                              val (cbarrier, fiber, _) = cb_fb_tk.get
                              fiber.join *> enable(key) *> cbarrier.await.exit
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
          `)(`     <- ZStream.fromZIO(`)(`.get)
          _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`)(` -> `π-τ`, (new Object -> -1, None, rate)))))
          cb_fb_tk <- ZStream.fromZIO(promise.await)
          if cb_fb_tk ne None
          sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
          _  <- ZStream.fromZIO {
            for
              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
              cb_fb_tk <- continue.get.flatMap(_.await)
              _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
              _        <- if cb_fb_tk eq None then sp.succeed(())
                          else
                            val (cbarrier, fiber, _) = cb_fb_tk.get
                            fiber.join *> enable(key) *> cbarrier.await.exit
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
        _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (`)(` -> `π-τ`, (new Object -> -1, None, rate)))))
        cb_fb_tk <- ZStream.fromZIO(promise.await)
        if cb_fb_tk ne None
        (cbarrier, fiber, _) = cb_fb_tk.get
        _        <- ZStream.fromZIO(fiber.join *> enable(key) *> cbarrier.await.exit)
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
    * events, i.e., names (hubs) and values
    */
  implicit final class `()`(private val name: Any) { self =>

    private def map = `()`[>*<]

    private inline def h(implicit ord: Int) = map(ord).hub
    private inline def l(implicit ord: Int) = map(ord).limit
    private implicit def a(using Int): Task[Unit] = l.acquire
    private def _s(using Int) = ZStream.unwrapScoped(ZStream.fromHubScoped(h).tap(_ => l.release))

    extension (self: ZStream[Any, Throwable, Object])(using Int)
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
        this.map eq that.map
      catch _ =>
        this.name == that.name

    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

    lazy val `null` = new `()`(null)
    lazy val unit = new `()`(())

    object π:

      object `(!)`:

        object `(+)`:

          object `(ν)`:

            /**
              * linear replication bound output guard
              */
            def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                     (using % : %, / : /, \ : \)
                     (using TSemaphore)
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                               ^ : String): ZStream[Any, Throwable, `()`] =
              implicit val ord = dir.ord
              for
                discard  <- if None eq * then ZStream.fromZIO(exclude(key)) *> ZStream.succeed(false)
                            else ZStream.fromZIO(?.await)
                _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
                if !discard
                promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
                continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
                _        <- if None eq * then ZStream.unit
                            else ZStream.fromZIO(promise.succeed(None))
                `)(`     <- ZStream.fromZIO(`)(`.get)
                _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`)(` -> dir, (map -> ord, Some(false), rate)))))
                cb_fb_tk <- ZStream.fromZIO(promise.await)
                _        <- if None eq * then ZStream.fromZIO(?.succeed(cb_fb_tk eq None) *> ?.await)
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
                              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                              cb_fb_tk <- continue.get.flatMap(_.await)
                              _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                              token    <- if cb_fb_tk eq None then sp.succeed(()).as(null)
                                          else
                                            val (cbarrier, fiber, token) = cb_fb_tk.get
                                            (fiber.join *> `}{`.><.release1 *> enable(key) *> cbarrier.await.exit).as(token)
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
            def apply(rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                     (using %, /, \)
                     (using TSemaphore)
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                               ^ : String): ZStream[Any, Throwable, `()`] =
              apply(rate)(key, `)(`)(dir)(?, -, *, +) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

            /**
              * linear replication bound output guard w/ code
              */
            def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                        (using %, /, \)
                        (using TSemaphore)
                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                  ^ : String): ZStream[Any, Throwable, `()`] =
              apply(rate)(key, `)(`)(dir)(?, -, *, +).tap(_ => code)

            /**
              * linear replication bound output guard w/ pace w/ code
              */
            def apply[T](rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                        (using %, /, \)
                        (using TSemaphore)
                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                  ^ : String): ZStream[Any, Throwable, `()`] =
              apply(rate, pace)(key, `)(`)(dir)(?, -, *, +).tap(_ => code)

          /**
            * linear constant replication output guard
            */
          def apply(rate: Rate, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                   (using % : %, / : /, \ : \)
                   (using TSemaphore)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): ZStream[Any, Throwable, Unit] =
            implicit val ord = dir.ord
            for
              discard  <- if None eq * then ZStream.fromZIO(exclude(key)) *> ZStream.succeed(false)
                          else ZStream.fromZIO(?.await)
              _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
              if !discard
              promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
              continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
              _        <- if None eq * then ZStream.unit
                          else ZStream.fromZIO(promise.succeed(None))
              `)(`     <- ZStream.fromZIO(`)(`.get)
              _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`)(` -> dir, (map -> ord, Some(false), rate)))))
              cb_fb_tk <- ZStream.fromZIO(promise.await)
              _        <- if None eq * then ZStream.fromZIO(?.succeed(cb_fb_tk eq None) *> ?.await)
                          else ZStream.succeed(false)
              _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
              if !discard
              sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
              _  <- ZStream.fromZIO {
                for
                  _        <- -.await.exit
                  _        <- *.fold(ZIO.unit)(_.acquire)
                  _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                  cb_fb_tk <- continue.get.flatMap(_.await)
                  _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                  _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                  token    <- if cb_fb_tk eq None then sp.succeed(()).as(null)
                              else
                                val (cbarrier, fiber, token) = cb_fb_tk.get
                                (fiber.join *> `}{`.><.release1 *> enable(key) *> cbarrier.await.exit).as(token)
                yield
                  value -> token
              }.repeat(Schedule.forever).interruptWhen(sp).through1(h)
              _  <- ZStream.fromZIO(+.release)
            yield
              ()

          /**
            * linear constant replication output guard w/ pace
            */
          def apply(rate: Rate, pace: Duration, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                   (using %, /, \)
                   (using TSemaphore)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): ZStream[Any, Throwable, Unit] =
            apply(rate, value)(key, `)(`)(dir)(?, -, *, +) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

          /**
            * linear constant replication output guard w/ code
            */
          def apply[T](rate: Rate, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                      (using %, /, \)
                      (using TSemaphore)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, Unit] =
            apply(rate, value)(key, `)(`)(dir)(?, -, *, +).tap(_ => code)

          /**
            * linear constant replication output guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: Duration, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                      (using %, /, \)
                      (using TSemaphore)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, Unit] =
            apply(rate, pace, value)(key, `)(`)(dir)(?, -, *, +).tap(_ => code)

          object `(*)`:

            /**
              * linear variable replication output guard
              */
            def apply[S](_1: 1)(rate: Rate, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                               (using DummyImplicit)
                               (using %, /, \)
                               (using TSemaphore)
                               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                         ^ : String): ZStream[Any, Throwable, Unit] =
             apply[S](1)(rate, ZIO.attempt(value))(key, `)(`)(dir)(?, -, *, +)

            /**
              * linear variable replication output guard w/ pace
              */
            def apply[S](_2: 2)(rate: Rate, pace: Duration, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                               (using DummyImplicit)
                               (using %, /, \)
                               (using TSemaphore)
                               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                         ^ : String): ZStream[Any, Throwable, Unit] =
             apply[S](2)(rate, pace, ZIO.attempt(value))(key, `)(`)(dir)(?, -, *, +)

            /**
              * linear variable replication output guard w/ code
              */
            def apply[S, T](_3: 3)(rate: Rate, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                                  (using DummyImplicit)
                                  (using %, /, \)
                                  (using TSemaphore)
                                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                            ^ : String): ZStream[Any, Throwable, Unit] =
             apply[S, T](3)(rate, ZIO.attempt(value))(key, `)(`)(dir)(code)(?, -, *, +)

            /**
              * linear variable replication output guard w/ pace w/ code
              */
            def apply[S, T](_4: 4)(rate: Rate, pace: Duration, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                                  (using DummyImplicit)
                                  (using %, /, \)
                                  (using TSemaphore)
                                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                            ^ : String): ZStream[Any, Throwable, Unit] =
             apply[S, T](4)(rate, pace, ZIO.attempt(value))(key, `)(`)(dir)(code)(?, -, *, +)

            /**
              * linear variable replication output guard
              */
            def apply[S](_1: 1)(rate: Rate, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                               (using % : %, / : /, \ : \)
                               (using TSemaphore)
                               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                         ^ : String): ZStream[Any, Throwable, Unit] =
              implicit val ord = dir.ord
              for
                discard  <- if None eq * then ZStream.fromZIO(exclude(key)) *> ZStream.succeed(false)
                            else ZStream.fromZIO(?.await)
                _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
                if !discard
                promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
                continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
                _        <- if None eq * then ZStream.unit
                            else ZStream.fromZIO(promise.succeed(None))
                `)(`     <- ZStream.fromZIO(`)(`.get)
                _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`)(` -> dir, (map -> ord, Some(false), rate)))))
                cb_fb_tk <- ZStream.fromZIO(promise.await)
                _        <- if None eq * then ZStream.fromZIO(?.succeed(cb_fb_tk eq None) *> ?.await)
                            else ZStream.succeed(false)
                _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
                if !discard
                sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
                _  <- ZStream.fromZIO {
                  for
                    _        <- -.await.exit
                    _        <- *.fold(ZIO.unit)(_.acquire)
                    _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                    cb_fb_tk <- continue.get.flatMap(_.await)
                    _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                    _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                    it       <- if cb_fb_tk eq None then sp.succeed(()).as(`null` -> null)
                                else
                                  val (cbarrier, fiber, token) = cb_fb_tk.get
                                  value.map(new `()`(_) -> token).tap(_ => fiber.join *> `}{`.><.release1 *> enable(key) *> cbarrier.await.exit)
                  yield
                    it
                }.repeat(Schedule.forever).interruptWhen(sp).through1(h)
                _  <- ZStream.fromZIO(+.release)
              yield
                ()

            /**
              * linear variable replication output guard w/ pace
              */
            def apply[S](_2: 2)(rate: Rate, pace: Duration, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                               (using %, /, \)
                               (using TSemaphore)
                               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                         ^ : String): ZStream[Any, Throwable, Unit] =
              apply[S](1)(rate, value)(key, `)(`)(dir)(?, -, *, +) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

            /**
              * linear variable replication output guard w/ code
              */
            def apply[S, T](_3: 3)(rate: Rate, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                                  (using %, /, \)
                                  (using TSemaphore)
                                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                            ^ : String): ZStream[Any, Throwable, Unit] =
              apply[S](1)(rate, value)(key, `)(`)(dir)(?, -, *, +).tap(_ => code)

            /**
              * linear variable replication output guard w/ pace w/ code
              */
            def apply[S, T](_4: 4)(rate: Rate, pace: Duration, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                                  (using %, /, \)
                                  (using TSemaphore)
                                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                            ^ : String): ZStream[Any, Throwable, Unit] =
              apply[S](2)(rate, pace, value)(key, `)(`)(dir)(?, -, *, +).tap(_ => code)

          /**
            * linear replication input guard
            */
          def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                   (using % : %, / : /, \ : \)
                   (using TSemaphore)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): ZStream[Any, Throwable, `()`] =
            implicit val ord = dir.ord
            for
              discard  <- if None eq * then ZStream.fromZIO(exclude(key)) *> ZStream.succeed(false)
                          else ZStream.fromZIO(?.await)
              _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
              if !discard
              promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
              continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
              _        <- if None eq * then ZStream.unit
                          else ZStream.fromZIO(promise.succeed(None))
              `)(`     <- ZStream.fromZIO(`)(`.get)
              _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`)(` -> dir, (map -> ord, Some(true), rate)))))
              cb_fb_tk <- ZStream.fromZIO(promise.await)
              _        <- if None eq * then ZStream.fromZIO(?.succeed(cb_fb_tk eq None) *> ?.await)
                          else ZStream.succeed(false)
              _        <- if discard then ZStream.fromZIO(-.await.exit) else ZStream.unit
              if !discard
              sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
              it <- ZStream.fromZIO {
                for
                  _        <- -.await.exit
                  _        <- *.fold(ZIO.unit)(_.acquire)
                  _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                  cb_fb_tk <- continue.get.flatMap(_.await)
                  _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                  _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                  token    <- if cb_fb_tk eq None then sp.succeed(()).as(null)
                              else
                                val (cbarrier, fiber, token) = cb_fb_tk.get
                                (fiber.join *> `}{`.><.release1 *> enable(key) *> cbarrier.await.exit).as(token)
                yield
                  token
              }.repeat(Schedule.forever).interruptWhen(sp).`zipRight s`
              _  <- ZStream.fromZIO(+.release)
            yield
              it

          /**
            * linear replication input guard w/ pace
            */
          def apply(rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                   (using %, /, \)
                   (using TSemaphore)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): ZStream[Any, Throwable, `()`] =
            apply(rate)(key, `)(`)(dir)(?, -, *, +) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

          /**
            * linear replication input guard w/ code
            */
          def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: T => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                      (using %, /, \)
                      (using TSemaphore)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, `()`] =
            apply(rate)(key, `)(`)(dir)(?, -, *, +).mapZIO { it => code(it.`()`[T]).map(new `()`(_)) }

          /**
            * linear replication input guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: T => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                      (using %, /, \)
                      (using TSemaphore)
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
                   (using TSemaphore)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): ZStream[Any, Throwable, `()`] =
            implicit val ord = dir.ord
            for
              _        <- ZStream.fromZIO(exclude(key))
              promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
              continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
              `)(`     <- ZStream.fromZIO(`)(`.get)
              _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`)(` -> dir, (map -> ord, Some(false), rate)))))
              cb_fb_tk <- ZStream.fromZIO(promise.await)
              if cb_fb_tk ne None
              sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
              it <- ( for
                        _  <- ZStream.unit.repeat(Schedule.forever)
                        it <- sΠ.ν
                        it <- ZStream.fromZIO {
                          for
                            _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                            cb_fb_tk <- continue.get.flatMap(_.await)
                            _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                            _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                            token    <- if cb_fb_tk eq None then sp.succeed(()).as(null)
                                        else
                                          val (cbarrier, fiber, token) = cb_fb_tk.get
                                          (fiber.join *> `}{`.><.release1 *> enable(key) *> cbarrier.await.exit).as(token)
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
          def apply(rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                   (using %, /, \)
                   (using TSemaphore)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): ZStream[Any, Throwable, `()`] =
            apply(rate)(key, `)(`)(dir) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

          /**
            * replication bound output guard w/ code
            */
          def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                      (using %, /, \)
                      (using TSemaphore)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, `()`] =
            apply(rate)(key, `)(`)(dir).tap(_ => code)

          /**
            * replication bound output guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                      (using %, /, \)
                      (using TSemaphore)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, `()`] =
            apply(rate, pace)(key, `)(`)(dir).tap(_ => code)

        /**
          * constant replication output guard
          */
        def apply(rate: Rate, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                 (using % : %, / : /, \ : \)
                 (using TSemaphore)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, Unit] =
          implicit val ord = dir.ord
          for
            _        <- ZStream.fromZIO(exclude(key))
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
            `)(`     <- ZStream.fromZIO(`)(`.get)
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`)(` -> dir, (map -> ord, Some(false), rate)))))
            cb_fb_tk <- ZStream.fromZIO(promise.await)
            if cb_fb_tk ne None
            sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
            _  <- ZStream.fromZIO {
              for
                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                cb_fb_tk <- continue.get.flatMap(_.await)
                _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                token    <- if cb_fb_tk eq None then sp.succeed(()).as(null)
                            else
                              val (cbarrier, fiber, token) = cb_fb_tk.get
                              (fiber.join *> `}{`.><.release1 *> enable(key) *> cbarrier.await.exit).as(token)
              yield
                value -> token
            }.repeat(Schedule.forever).interruptWhen(sp).through1(h)
          yield
            ()

        /**
          * constant replication output guard w/ pace
          */
        def apply(rate: Rate, pace: Duration, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                 (using %, /, \)
                 (using TSemaphore)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate, value)(key, `)(`)(dir) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

        /**
          * constant replication output guard w/ code
          */
        def apply[T](rate: Rate, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                 (using %, /, \)
                 (using TSemaphore)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate, value)(key, `)(`)(dir).tap(_ => code)

        /**
          * constant replication output guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: Duration, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                 (using %, /, \)
                 (using TSemaphore)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate, pace, value)(key, `)(`)(dir).tap(_ => code)

        object `(*)`:

          /**
            * variable replication output guard
            */
          def apply[S](_1: 1)(rate: Rate, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                             (using DummyImplicit)
                             (using %, /, \)
                             (using TSemaphore)
                             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                       ^ : String): ZStream[Any, Throwable, Unit] =
           apply[S](1)(rate, ZIO.attempt(value))(key, `)(`)(dir)

          /**
            * variable replication output guard w/ pace
            */
          def apply[S](_2: 2)(rate: Rate, pace: Duration, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                             (using DummyImplicit)
                             (using %, /, \)
                             (using TSemaphore)
                             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                       ^ : String): ZStream[Any, Throwable, Unit] =
           apply[S](2)(rate, pace, ZIO.attempt(value))(key, `)(`)(dir)

          /**
            * variable replication output guard w/ code
            */
          def apply[S, T](_3: 3)(rate: Rate, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                                (using DummyImplicit)
                                (using %, /, \)
                                (using TSemaphore)
                                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                          ^ : String): ZStream[Any, Throwable, Unit] =
           apply[S, T](3)(rate, ZIO.attempt(value))(key, `)(`)(dir)(code)

          /**
            * variable replication output guard w/ pace w/ code
            */
          def apply[S, T](_4: 4)(rate: Rate, pace: Duration, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                                (using DummyImplicit)
                                (using %, /, \)
                                (using TSemaphore)
                                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                          ^ : String): ZStream[Any, Throwable, Unit] =
           apply[S, T](4)(rate, pace, ZIO.attempt(value))(key, `)(`)(dir)(code)

          /**
            * variable replication output guard
            */
          def apply[S](_1: 1)(rate: Rate, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                             (using % : %, / : /, \ : \)
                             (using TSemaphore)
                             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                       ^ : String): ZStream[Any, Throwable, Unit] =
            implicit val ord = dir.ord
            for
              _        <- ZStream.fromZIO(exclude(key))
              promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
              continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
              `)(`     <- ZStream.fromZIO(`)(`.get)
              _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`)(` -> dir, (map -> ord, Some(false), rate)))))
              cb_fb_tk <- ZStream.fromZIO(promise.await)
              if cb_fb_tk ne None
              sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
              _  <- ZStream.fromZIO {
                for
                  _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                  cb_fb_tk <- continue.get.flatMap(_.await)
                  _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                  _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                  it       <- if cb_fb_tk eq None then sp.succeed(()).as(`null` -> null)
                              else
                                val (cbarrier, fiber, token) = cb_fb_tk.get
                                value.map(new `()`(_) -> token).tap(_ => fiber.join *> `}{`.><.release1 *> enable(key) *> cbarrier.await.exit)
                yield
                  it
              }.repeat(Schedule.forever).interruptWhen(sp).through1(h)
            yield
              ()

          /**
            * variable replication output guard w/ pace
            */
          def apply[S](_2: 2)(rate: Rate, pace: Duration, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                             (using %, /, \)
                             (using TSemaphore)
                             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                       ^ : String): ZStream[Any, Throwable, Unit] =
            apply[S](1)(rate, value)(key, `)(`)(dir) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

          /**
            * variable replication output guard w/ code
            */
          def apply[S, T](_3: 3)(rate: Rate, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                                (using %, /, \)
                                (using TSemaphore)
                                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                          ^ : String): ZStream[Any, Throwable, Unit] =
            apply[S](1)(rate, value)(key, `)(`)(dir).tap(_ => code)

          /**
            * variable replication output guard w/ pace w/ code
            */
          def apply[S, T](_4: 4)(rate: Rate, pace: Duration, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                                (using %, /, \)
                                (using TSemaphore)
                                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                          ^ : String): ZStream[Any, Throwable, Unit] =
            apply[S](2)(rate, pace, value)(key, `)(`)(dir).tap(_ => code)

        /**
          * replication input guard
          */
        def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                 (using % : %, / : /, \ : \)
                 (using TSemaphore)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, `()`] =
          implicit val ord = dir.ord
          for
            _        <- ZStream.fromZIO(exclude(key))
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
            `)(`     <- ZStream.fromZIO(`)(`.get)
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`)(` -> dir, (map -> ord, Some(true), rate)))))
            cb_fb_tk <- ZStream.fromZIO(promise.await)
            if cb_fb_tk ne None
            sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
            it <- ZStream.fromZIO {
              for
                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                cb_fb_tk <- continue.get.flatMap(_.await)
                _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                token    <- if cb_fb_tk eq None then sp.succeed(()).as(null)
                            else
                              val (cbarrier, fiber, token) = cb_fb_tk.get
                              (fiber.join *> `}{`.><.release1 *> enable(key) *> cbarrier.await.exit).as(token)
              yield
                token
            }.repeat(Schedule.forever).interruptWhen(sp).`zipRight s`
          yield
            it

        /**
          * replication input guard w/ pace
          */
        def apply(rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                 (using %, /, \)
                 (using TSemaphore)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, `()`] =
          apply(rate)(key, `)(`)(dir) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

        /**
          * replication input guard w/ code
          */
        def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: T => Task[T])
                    (using %, /, \)
                    (using TSemaphore)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, `()`] =
          apply(rate)(key, `)(`)(dir).mapZIO { it => code(it.`()`[T]).map(new `()`(_)) }

        /**
          * replication input guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: T => Task[T])
                    (using %, /, \)
                    (using TSemaphore)
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
                 (using TSemaphore)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, `()`] =
          implicit val ord = dir.ord
          for
            _        <- ZStream.fromZIO(exclude(key))
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            `)(`     <- ZStream.fromZIO(`)(`.get)
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (`)(` -> dir, (map -> ord, Some(false), rate)))))
            cb_fb_tk <- ZStream.fromZIO(promise.await)
            if cb_fb_tk ne None
            (cbarrier, fiber, token) = cb_fb_tk.get
            it <- sΠ.ν
            _  <- ZStream.succeed(it -> token).tap(_ => fiber.join *> `}{`.><.release1 *> enable(key) *> cbarrier.await.exit).through1(h)
          yield
            it

        /**
          * bound output prefix w/ pace
          */
        def apply(rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                 (using %, /)
                 (using TSemaphore)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, `()`] =
          apply(rate)(key, `)(`)(dir) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

        /**
          * bound output prefix w/ code
          */
        def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                    (using %, /)
                    (using TSemaphore)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, `()`] =
          apply(rate)(key, `)(`)(dir).tap(_ => code)

        /**
          * bound output prefix w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                    (using %, /)
                    (using TSemaphore)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, `()`] =
          apply(rate, pace)(key, `)(`)(dir).tap(_ => code)

      /**
        * constant output prefix
        */
      def apply(rate: Rate, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
               (using % : %, / : /)
               (using TSemaphore)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, Unit] =
        implicit val ord = dir.ord
        for
          _        <- ZStream.fromZIO(exclude(key))
          promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
          `)(`     <- ZStream.fromZIO(`)(`.get)
          _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (`)(` -> dir, (map -> ord, Some(false), rate)))))
          cb_fb_tk <- ZStream.fromZIO(promise.await)
          if cb_fb_tk ne None
          (cbarrier, fiber, token) = cb_fb_tk.get
          _        <- ZStream.succeed(value -> token).tap(_ => fiber.join *> `}{`.><.release1 *> enable(key) *> cbarrier.await.exit).through1(h)
        yield
          ()

      /**
        * constant output prefix w/ pace
        */
      def apply(rate: Rate, pace: Duration, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
               (using %, /)
               (using TSemaphore)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate, value)(key, `)(`)(dir) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * constant output prefix w/ code
        */
      def apply[T](rate: Rate, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                  (using %, /)
                  (using TSemaphore)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate, value)(key, `)(`)(dir).tap(_ => code)

      /**
        * constant output prefix w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: Duration, value: `()`)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                  (using %, /)
                  (using TSemaphore)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate, pace, value)(key, `)(`)(dir).tap(_ => code)

      object `(*)`:

        /**
          * variable output prefix
          */
        def apply[S](_1: 1)(rate: Rate, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                           (using DummyImplicit)
                           (using %, /)
                           (using TSemaphore)
                           (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                     `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                     ^ : String): ZStream[Any, Throwable, Unit] =
          apply[S](1)(rate, ZIO.attempt(value))(key, `)(`)(dir)

        /**
          * variable output prefix w/ pace
          */
        def apply[S](_2: 2)(rate: Rate, pace: Duration, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                           (using DummyImplicit)
                           (using %, /)
                           (using TSemaphore)
                           (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                     `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                     ^ : String): ZStream[Any, Throwable, Unit] =
          apply[S](1)(rate, value)(key, `)(`)(dir) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

        /**
          * variable output prefix w/ code
          */
        def apply[S, T](_3: 3)(rate: Rate, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                              (using DummyImplicit)
                              (using %, /)
                              (using TSemaphore)
                              (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                        `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                        ^ : String): ZStream[Any, Throwable, Unit] =
          apply[S](1)(rate, value)(key, `)(`)(dir).tap(_ => code)

        /**
          * variable output prefix w/ pace w/ code
          */
        def apply[S, T](_4: 4)(rate: Rate, pace: Duration, value: => S)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                              (using DummyImplicit)
                              (using %, /)
                              (using TSemaphore)
                              (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                        `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                        ^ : String): ZStream[Any, Throwable, Unit] =
          apply[S](2)(rate, pace, value)(key, `)(`)(dir).tap(_ => code)

        /**
          * variable output prefix
          */
        def apply[S](_1: 1)(rate: Rate, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                           (using % : %, / : /)
                           (using TSemaphore)
                           (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                     `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                     ^ : String): ZStream[Any, Throwable, Unit] =
          implicit val ord = dir.ord
          for
            _        <- ZStream.fromZIO(exclude(key))
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            `)(`     <- ZStream.fromZIO(`)(`.get)
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (`)(` -> dir, (map -> ord, Some(false), rate)))))
            cb_fb_tk <- ZStream.fromZIO(promise.await)
            if cb_fb_tk ne None
            (cbarrier, fiber, token) = cb_fb_tk.get
            _        <- ZStream.fromZIO(value).map(new `()`(_) -> token).tap(_ => fiber.join *> `}{`.><.release1 *> enable(key) *> cbarrier.await.exit).through1(h)
          yield
            ()

        /**
          * variable output prefix w/ pace
          */
        def apply[S](_2: 2)(rate: Rate, pace: Duration, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
                           (using %, /)
                           (using TSemaphore)
                           (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                     `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                     ^ : String): ZStream[Any, Throwable, Unit] =
          apply[S](1)(rate, value)(key, `)(`)(dir) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

        /**
          * variable output prefix w/ code
          */
        def apply[S, T](_3: 3)(rate: Rate, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                              (using %, /)
                              (using TSemaphore)
                              (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                        `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                        ^ : String): ZStream[Any, Throwable, Unit] =
          apply[S](1)(rate, value)(key, `)(`)(dir).tap(_ => code)

        /**
          * variable output prefix w/ pace w/ code
          */
        def apply[S, T](_4: 4)(rate: Rate, pace: Duration, value: => Task[S])(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: => Task[T])
                              (using %, /)
                              (using TSemaphore)
                              (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                        `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                        ^ : String): ZStream[Any, Throwable, Unit] =
          apply[S](2)(rate, pace, value)(key, `)(`)(dir).tap(_ => code)

      /**
        * input prefix
        */
      def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
               (using % : %, / : /)
               (using TSemaphore)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, `()`] =
        implicit val ord = dir.ord
        for
          _        <- ZStream.fromZIO(exclude(key))
          promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
          `)(`     <- ZStream.fromZIO(`)(`.get)
          _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (`)(` -> dir, (map -> ord, Some(true), rate)))))
          cb_fb_tk <- ZStream.fromZIO(promise.await)
          if cb_fb_tk ne None
          (cbarrier, fiber, token) = cb_fb_tk.get
          it <- ZStream.fromZIO(fiber.join *> `}{`.><.release1 *> enable(key) *> cbarrier.await.exit).as(token).`zipRight s.head`
        yield
          it

      /**
        * input prefix w/ pace
        */
      def apply(rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)
               (using %, /)
               (using TSemaphore)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, `()`] =
        apply(rate)(key, `)(`)(dir) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * input prefix w/ code
        */
      def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: T => Task[T])
                  (using %, /)
                  (using TSemaphore)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, `()`] =
        apply(rate)(key, `)(`)(dir).mapZIO { it => code(it.`()`[T]).map(new `()`(_)) }

      /**
        * input prefix w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(dir: `π-$`)(code: T => Task[T])
                  (using %, /)
                  (using TSemaphore)
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
                   (using TSemaphore)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): ZStream[Any, Throwable, Unit] =
            implicit val ord = cap.ord
            for
              _        <- ZStream.fromZIO(exclude(key))
              promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
              continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
              polarity  = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
              `)(`     <- ZStream.fromZIO(`)(`.get)
              _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`)(` -> cap, (map -> ord, Some(polarity), rate)))))
              cb_fb_tk <- ZStream.fromZIO(promise.await)
              if cb_fb_tk ne None
              sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
              tks = ZStream.fromZIO {
                for
                  _        <- -.await.exit
                  _        <- *.fold(ZIO.unit)(_.acquire)
                  _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                  cb_fb_tk <- continue.get.flatMap(_.await)
                  _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                  _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                  token    <- if cb_fb_tk eq None then sp.succeed(()).as(null)
                              else
                                val (cbarrier, fiber, token) = cb_fb_tk.get
                                (fiber.join *> `}{`.><.release1 *> enable(key) *> cbarrier.await.exit).as(token)
                 yield
                   token
              }.repeat(Schedule.forever).interruptWhen(sp)
              _  <- if polarity then tks.`zipRight s` else tks.map(unit -> _).through1(h)
              _  <- ZStream.fromZIO(+.release)
            yield
              ()

          /**
            * linear replication capability guard w/ pace
            */
          def apply(rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                   (using %, /, \)
                   (using TSemaphore)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): ZStream[Any, Throwable, Unit] =
            apply(rate)(key, `)(`)(cap)(?, -, *, +) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

          /**
            * linear replication capability guard w/ code
            */
          def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                      (using %, /, \)
                      (using TSemaphore)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, Unit] =
            apply(rate)(key, `)(`)(cap)(?, -, *, +).tap(_ => code)

          /**
            * linear replication capability guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)(code: => Task[T])(? : Promise[Throwable, Boolean], - : CyclicBarrier, * : Option[Semaphore[Task]], + : Semaphore[Task])
                      (using %, /, \)
                      (using TSemaphore)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): ZStream[Any, Throwable, Unit] =
            apply(rate, pace)(key, `)(`)(cap)(?, -, *, +).tap(_ => code)

        /**
          * replication capability guard
          */
        def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)
                 (using % : %, / : /, \ : \)
                 (using TSemaphore)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, Unit] =
          implicit val ord = cap.ord
          for
            _        <- ZStream.fromZIO(exclude(key))
            promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
            continue <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]].flatMap(Ref.make))
            polarity  = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
            `)(`     <- ZStream.fromZIO(`)(`.get)
            _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> continue -> (`)(` -> cap, (map -> ord, Some(polarity), rate)))))
            cb_fb_tk <- ZStream.fromZIO(promise.await)
            if cb_fb_tk ne None
            sp <- ZStream.fromZIO(Promise.make[Throwable, Unit])
            tks = ZStream.fromZIO {
              for
                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +)]._2)) } *> \()
                cb_fb_tk <- continue.get.flatMap(_.await)
                _        <- Promise.make[Throwable, Option[<>]].flatMap(continue.set)
                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +)]._2)) }
                token    <- if cb_fb_tk eq None then sp.succeed(()).as(null)
                            else
                              val (cbarrier, fiber, token) = cb_fb_tk.get
                              (fiber.join *> `}{`.><.release1 *> enable(key) *> cbarrier.await.exit).as(token)
               yield
                 token
            }.interruptWhen(sp)
            _  <- if polarity then tks.`zipRight s` else tks.map(unit -> _).through1(h)
          yield
            ()

        /**
          * replication capability guard w/ pace
          */
        def apply(rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)
                 (using %, /, \)
                 (using TSemaphore)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate)(key, `)(`)(cap) zipLeft ZStream.unit.repeat(Schedule.spaced(pace))

        /**
          * replication capability guard w/ code
          */
        def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)(code: => Task[T])
                    (using %, /, \)
                    (using TSemaphore)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate)(key, `)(`)(cap).tap(_ => code)

        /**
          * replication capability guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)(code: => Task[T])
                    (using %, /, \)
                    (using TSemaphore)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): ZStream[Any, Throwable, Unit] =
          apply(rate, pace)(key, `)(`)(cap).tap(_ => code)

      /**
        * capability prefix
        */
      def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)
               (using % : %, / : /)
               (using TSemaphore)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, Unit] =
        implicit val ord = cap.ord
        for
          _        <- ZStream.fromZIO(exclude(key))
          promise  <- ZStream.fromZIO(Promise.make[Throwable, Option[<>]])
          polarity  = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
          `)(`     <- ZStream.fromZIO(`)(`.get)
          _        <- ZStream.fromZIO(/.offer(^ -> key -> (promise -> null -> (`)(` -> cap, (map -> ord, Some(polarity), rate)))))
          cb_fb_tk <- ZStream.fromZIO(promise.await)
          if cb_fb_tk ne None
          (cbarrier, fiber, token) = cb_fb_tk.get
          tks = ZStream.fromZIO(fiber.join *> `}{`.><.release1 *> enable(key) *> cbarrier.await.exit).as(token)
          _  <- if polarity then tks.`zipRight s.head` else tks.map(unit -> _).through1(h)
        yield
          ()

      /**
        * capability prefix w/ pace
        */
      def apply(rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)
               (using %, /)
               (using TSemaphore)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate)(key, `)(`)(cap) <* ZStream.unit.repeat(Schedule.fromDuration(pace))

      /**
        * capability prefix w/ code
        */
      def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)(code: => Task[T])
                  (using %, /)
                  (using TSemaphore)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): ZStream[Any, Throwable, Unit] =
        apply(rate)(key, `)(`)(cap).tap(_ => code)

      /**
        * capability prefix w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: Duration)(key: String, `)(`: FiberRef[`)(`])(cap: `π-ζ`)(code: => Task[T])
                  (using %, /)
                  (using TSemaphore)
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
               (using `][`: `][`, `2`: TSemaphore): UIO[Unit] =
        for
          key  <- `)(`.get
          uuid <- sΠ.`)(`()
          node  = Set(uuid)
          _    <- `)(`.set(uuid)
          _    <- ( for
                      _ <- `2`.acquireN(2)
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
                      _ <- `2`.releaseN(2)
                    yield
                      ()
                  ).commit
        yield
          ()

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
          sem  <- TSemaphore.make(2).commit
        yield
          (uuid, tree, sem)

    object >< :

      def release1(using `2`: TSemaphore): UIO[Unit] =
        `2`.release.commit

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
                 (using `][`: `][`, `2`: TSemaphore): UIO[Unit] =
          ( for
              _     <- `2`.acquireN(2)
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
                 (using `][`: `][`, `2`: TSemaphore): UIO[Unit] =
          ( for
              _     <- `2`.acquireN(2)
              node  <- `][`.get.map(_.keys.find(_.contains(key)).get)
              nodeʹ <- `][`.get.map(_.keys.find(_.contains(keyʹ)).get)
              _     <- check(node, nodeʹ, cap, capʹ).flatMap(ZSTM.check(_))
              _     <- this(node, nodeʹ, cap, capʹ)
            yield
              ()
          ).commit


  private object `Π-magic`:

    case class ><(hub: Hub[(`()`, Object)],
                  limit: Semaphore[Task])

    type >*< = Map[Int, ><]

    extension [O](self: ZStream[Any, Throwable, O])
      def through1(hub: Hub[O])
                  (using await: Task[Unit]): ZStream[Any, Throwable, O] =
        self.mapZIO { it => await *> hub.publish(it).map(it -> _) }.takeWhile(_._2).map(_._1)
