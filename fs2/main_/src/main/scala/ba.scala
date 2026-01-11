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

  import _root_.scala.concurrent.duration.FiniteDuration

  import _root_.cats.syntax.apply.*
  import _root_.cats.syntax.functor.*
  import _root_.cats.syntax.flatMap.*

  import _root_.cats.effect.{ Async, Deferred, Ref, Resource, Unique }
  import _root_.cats.effect.std.{ CyclicBarrier, Queue, UUIDGen }

  import _root_.fs2.concurrent.{ SignallingRef, Topic }
  import _root_.fs2.{ Pull, Stream }

  import _root_.io.github.timwspence.cats.stm.STM

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


  type `Π-Map`[K, +V] = Map[K, V]

  type `Π-Set`[A] = Set[A]


  def `π-enable`[F[_]](enabled: `Π-Set`[String])
                      (using % : %[F]): F[Unit] =
    %.update(enabled.foldLeft(_) { (m, key) =>
                                    val n = if m.contains(key)
                                            then m(key).asInstanceOf[Int]
                                            else 0
                                    m + (key -> (n + 1))
                                 }
    )

  private def enable[F[_]](key: String)
                          (using % : %[F])
                          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): F[Unit] =
    val (_, spell) = `π-wand`
    `π-enable`[F](spell(key))


  inline def `π-exclude`[F[_]: Async](enabled: String*)
                                        (using % : %[F], \ : \[F]): F[Unit] =
    `π-exclude`[F](Set.from(enabled)) >> \()

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
    if `π-elvis`.contains(key)
    then
      `π-exclude`[F](`π-elvis`(key))
    else
      Async[F].unit


  /**
    * restriction aka new name
    */
  final class ν[F[_]: Async]:

    def map[B](f: `()`[F] => B): Stream[F, B] = flatMap(f andThen Stream.emit[F, B])
    def flatMap[B](f: `()`[F] => Stream[F, B]): Stream[F, B] =
      ( for
          map <- Stream.eval {
            for
              local_topic <- Topic[F, (`()`[F], Unique.Token)]
              local_queue <- Queue.unbounded[F, Unit]
              local_limit <- Ref[F].of(false)
              s2s_topic <- Topic[F, (`()`[F], Unique.Token)]
              s2s_queue <- Queue.unbounded[F, Unit]
              s2s_limit <- Ref[F].of(false)
              p2c_topic <- Topic[F, (`()`[F], Unique.Token)]
              p2c_queue <- Queue.unbounded[F, Unit]
              p2c_limit <- Ref[F].of(false)
              accept_topic <- Topic[F, (`()`[F], Unique.Token)]
              accept_queue <- Queue.unbounded[F, Unit]
              accept_limit <- Ref[F].of(false)
              expel_topic <- Topic[F, (`()`[F], Unique.Token)]
              expel_queue <- Queue.unbounded[F, Unit]
              expel_limit <- Ref[F].of(false)
              merge_topic <- Topic[F, (`()`[F], Unique.Token)]
              merge_queue <- Queue.unbounded[F, Unit]
              merge_limit <- Ref[F].of(false)
            yield
              Map(
                `π-local`.ord  -> ><[F](local_topic, local_queue, local_limit),
                `π-s2s`.ord    -> ><[F](s2s_topic, s2s_queue, s2s_limit),
                `π-p2c`.ord    -> ><[F](p2c_topic, p2c_queue, p2c_limit),
                `π-accept`.ord -> ><[F](accept_topic, accept_queue, accept_limit),
                `π-expel`.ord  -> ><[F](expel_topic, expel_queue, expel_limit),
                `π-merge+`.ord -> ><[F](merge_topic, merge_queue, merge_limit)
              )
          }
        yield
          f(map)
      ).flatten


  /**
    * silent transition
    */
  final class τ[F[_]: Async]:

    object ! :

      object + :

        /**
          * linear replication guard
          */
        def apply(rate: Rate, `_}{`: `}{`[F])(key: String, `)(`: `)(`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                 (using % : %[F], / : /[F], \ : \[F])
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, Unit] =
          for
            _        <- if None eq + then Stream.eval(exclude(key))
                        else Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            continue <- Stream.eval(Ref[F].of(deferred))
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            _        <- if None eq + then Stream.unit else Stream.eval(deferred.complete(None))
            now      <- Stream.eval(Async[F].monotonic.map(_.toNanos))
            timestamp <- Stream.eval(Ref[F].of(now))
            _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp -> (`)(` -> `π-τ`, (new Object -> -1, None, rate))))))
            cb_fb_tk <- Stream.eval(deferred.get)
            _        <- if None eq + then Stream.eval(?.complete(cb_fb_tk eq None)) else Stream.unit
            _        <- Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
            sr <- Stream.eval(SignallingRef[F].of(false))
            _  <- Stream.repeatEval {
              for
                _        <- -.await
                _        <- +.fold(Async[F].unit)(_.take)
                now      <- Async[F].monotonic.map(_.toNanos)
                enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                _        <- if enabled then Async[F].unit else timestamp.set(now)
                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                deferred <- continue.get
                cb_fb_tk <- deferred.get
                deferred <- Deferred[F, Option[<>[F]]]
                _        <- continue.set(deferred)
                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                _        <- if cb_fb_tk eq None then sr.set(true)
                            else
                              val (cbarrier, fiber, _) = cb_fb_tk.get
                              fiber.join >> enable[F](key) >> cbarrier.await
              yield
                ()
            }.interruptWhen(sr)
            _  <- Stream.eval(*.fold(Async[F].unit)(_.offer(())))
          yield
            ()

        /**
          * linear replication guard w/ pace
          */
        def apply(rate: Rate, pace: FiniteDuration, `_}{`: `}{`[F])(key: String, `)(`: `)(`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                 (using %[F], /[F], \[F])
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, Unit] =
        apply(rate, `_}{`)(key, `)(`)(?, -, +, *).spaced(pace)

        /**
          * linear replication guard w/ code
          */
        def apply[T](rate: Rate, `_}{`: `}{`[F])(key: String, `)(`: `)(`)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                    (using %[F], /[F], \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          apply(rate, `_}{`)(key, `)(`)(?, -, +, *).evalTap(_ => code)

        /**
          * linear replication guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration, `_}{`: `}{`[F])(key: String, `)(`: `)(`)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                    (using %[F], /[F], \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          apply(rate, pace, `_}{`)(key, `)(`)(?, -, +, *).evalTap(_ => code)

      /**
        * replication guard
        */
      def apply(rate: Rate, `_}{`: `}{`[F])(key: String, `)(`: `)(`)
               (using % : %[F], / : /[F], \ : \[F])
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, Unit] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          continue <- Stream.eval(Ref[F].of(deferred))
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          now      <- Stream.eval(Async[F].monotonic.map(_.toNanos))
          timestamp <- Stream.eval(Ref[F].of(now))
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp -> (`)(` -> `π-τ`, (new Object -> -1, None, rate))))))
          cb_fb_tk <- Stream.eval(deferred.get)
          if cb_fb_tk ne None
          sr <- Stream.eval(SignallingRef[F].of(false))
          _  <- Stream.repeatEval {
            for
              now      <- Async[F].monotonic.map(_.toNanos)
              enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
              _        <- if enabled then Async[F].unit else timestamp.set(now)
              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
              deferred <- continue.get
              cb_fb_tk <- deferred.get
              deferred <- Deferred[F, Option[<>[F]]]
              _        <- continue.set(deferred)
              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
              _        <- if cb_fb_tk eq None then sr.set(true)
                          else
                            val (cbarrier, fiber, _) = cb_fb_tk.get
                            fiber.join >> enable[F](key) >> cbarrier.await
            yield
              ()
          }.interruptWhen(sr)
        yield
          ()

      /**
        * replication guard w/ pace
        */
      def apply(rate: Rate, pace: FiniteDuration, `_}{`: `}{`[F])(key: String, `)(`: `)(`)
               (using %[F], /[F], \[F])
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, Unit] =
        apply(rate, `_}{`)(key, `)(`).spaced(pace)

      /**
        * replication guard w/ code
        */
      def apply[T](rate: Rate, `_}{`: `}{`[F])(key: String, `)(`: `)(`)(code: => F[T])
                  (using %[F], /[F], \[F])
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, Unit] =
        apply(rate, `_}{`)(key, `)(`).evalTap(_ => code)

      /**
        * replication guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration, `_}{`: `}{`[F])(key: String, `)(`: `)(`)(code: => F[T])
                  (using %[F], /[F], \[F])
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, Unit] =
        apply(rate, pace, `_}{`)(key, `)(`).evalTap(_ => code)

    /**
      * prefix
      */
    def apply(rate: Rate, `_}{`: `}{`[F])(key: String, `)(`: `)(`)
             (using % : %[F], / : /[F])
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): Stream[F, Unit] =
      for
        _        <- Stream.eval(exclude(key))
        deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
        now      <- Stream.eval(Async[F].monotonic.map(_.toNanos))
        timestamp <- Stream.eval(Ref[F].of(now))
        _        <- Stream.eval(/.offer(^ -> key -> (deferred -> null -> (timestamp -> (`)(` -> `π-τ`, (new Object -> -1, None, rate))))))
        cb_fb_tk <- Stream.eval(deferred.get)
        if cb_fb_tk ne None
        (cbarrier, fiber, _) = cb_fb_tk.get
        _        <- Stream.eval(fiber.join >> enable[F](key) >> cbarrier.await)
      yield
        ()

    /**
      * prefix w/ pace
      */
    def apply(rate: Rate, pace: FiniteDuration, `_}{`: `}{`[F])(key: String, `)(`: `)(`)
             (using %[F], /[F])
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): Stream[F, Unit] =
      apply(rate, `_}{`)(key, `)(`) <* Stream.sleep(pace)

    /**
      * prefix w/ code
      */
    def apply[T](rate: Rate, `_}{`: `}{`[F])(key: String, `)(`: `)(`)(code: => F[T])
                (using %[F], /[F])
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): Stream[F, Unit] =
      apply(rate, `_}{`)(key, `)(`).evalTap(_ => code)

    /**
      * prefix w/ pace w/ code
      */
    def apply[T](rate: Rate, pace: FiniteDuration, `_}{`: `}{`[F])(key: String, `)(`: `)(`)(code: => F[T])
                (using %[F], /[F])
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): Stream[F, Unit] =
      apply(rate, pace, `_}{`)(key, `)(`).evalTap(_ => code)

  /**
    * events, i.e., names (topics) and values
    */
  implicit final class `()`[F[_]: Async](private val name: Any) { self =>

    private def map = `()`[>*<[F]]

    private inline def t(implicit ord: Int) = map(ord).topic
    private inline def q(implicit ord: Int) = map(ord).queue
    private inline def r(implicit ord: Int) = map(ord).limit
    private implicit def a(using Int): F[Unit] = q.take >> r.set(false)
    private def o(using Int) =
      for
        b <- r.get
        s <- q.size
        _ <- if !b || s == 0 then q.offer(()) >> r.set(true) else Async[F].unit
      yield
        ()
    private def s(using Int) = Stream.resource(t.subscribeAwaitUnbounded <* Resource.eval(o)).flatten

    extension (self: Stream[F, Unique.Token])
      private def `zipRight s`(using Int) =
        def zip(tks: Pull[F, Nothing, Option[(Unique.Token, Stream[F, Unique.Token])]],
                its: Pull[F, Nothing, Option[((`()`[F], Unique.Token), Stream[F, (`()`[F], Unique.Token)])]]): Pull[F, `()`[F], Unit] =
          tks.flatMap {
            case Some((tk, tksʹ)) =>
              its.flatMap {
                case Some(((it, tkʹ), itsʹ)) if tk eq tkʹ =>
                  Pull.output1(it) >> zip(tksʹ.pull.uncons1, itsʹ.pull.uncons1)
                case Some((_, itsʹ)) =>
                  zip(tks, itsʹ.pull.uncons1)
                case _ =>
                  Pull.done
              }
            case _ =>
              Pull.done
          }
        zip(self.pull.uncons1, s.pull.uncons1).stream

    def ====(that: `()`[F]) =
      try
        this.map eq that.map
      catch
        case _ =>
          this.name == that.name

    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()`[F] = this

    lazy val `null` = new `()`[F](null)
    lazy val unit = new `()`[F](())

    object π:

      object ! :

        object + :

          object ν:

            /**
              * linear replication bound output guard
              */
            def apply(rate: Rate, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                     (using % : %[F], / : /[F], \ : \[F])
                     (using `}{`.stm.TSemaphore)
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                               ^ : String): Stream[F, `()`[F]] =
              implicit val ord = dir.ord
              for
                _        <- if None eq + then Stream.eval(exclude(key))
                            else Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
                continue <- Stream.eval(Ref[F].of(deferred))
                deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
                _        <- if None eq + then Stream.unit else Stream.eval(deferred.complete(None))
                now      <- Stream.eval(Async[F].monotonic.map(_.toNanos))
                timestamp <- Stream.eval(Ref[F].of(now))
                _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp -> (`)(` -> dir, (map -> ord, Some(false), rate))))))
                cb_fb_tk <- Stream.eval(deferred.get)
                _        <- if None eq + then Stream.eval(?.complete(cb_fb_tk eq None)) else Stream.unit
                _        <- Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                sr <- Stream.eval(SignallingRef[F].of(false))
                it <- ( for
                          _  <- Stream.unit.repeat
                          it <- sΠ.ν[F]
                          it <- Stream.eval {
                            for
                              _        <- -.await
                              _        <- +.fold(Async[F].unit)(_.take)
                              now      <- Async[F].monotonic.map(_.toNanos)
                              enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                              _        <- if enabled then Async[F].unit else timestamp.set(now)
                              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                              deferred <- continue.get
                              cb_fb_tk <- deferred.get
                              deferred <- Deferred[F, Option[<>[F]]]
                              _        <- continue.set(deferred)
                              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                              token    <- if cb_fb_tk eq None then sr.set(true).as(null)
                                          else
                                            val (cbarrier, fiber, token) = cb_fb_tk.get
                                            (fiber.join >> `}{`.><.release1 >> enable[F](key) >> cbarrier.await).as(token)
                            yield
                              it -> token
                          }
                        yield
                          it
                      ).interruptWhen(sr).through1(t)
                _  <- Stream.eval(*.fold(Async[F].unit)(_.offer(())))
              yield
                it._1

            /**
              * linear replication bound output guard w/ pace
              */
            def apply(rate: Rate, pace: FiniteDuration, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                     (using %[F], /[F], \[F])
                     (using `}{`.stm.TSemaphore)
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                               ^ : String): Stream[F, `()`[F]] =
              apply(rate, `}{`)(key, `)(`)(dir)(?, -, +, *).spaced(pace)

            /**
              * linear replication bound output guard w/ code
              */
            def apply[T](rate: Rate, `}{`: `}{`[F])(key: String, `)(`: `)(`)(code: F[T])(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                        (using %[F], /[F], \[F])
                        (using `}{`.stm.TSemaphore)
                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                  ^ : String): Stream[F, `()`[F]] =
              apply(rate, `}{`)(key, `)(`)(dir)(?, -, +, *).evalTap(_ => code)

            /**
              * linear replication bound output guard w/ pace w/ code
              */
            def apply[T](rate: Rate, pace: FiniteDuration, `}{`: `}{`[F])(key: String, `)(`: `)(`)(code: => F[T])(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                        (using %[F], /[F], \[F])
                        (using `}{`.stm.TSemaphore)
                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                  ^ : String): Stream[F, `()`[F]] =
              apply(rate, pace, `}{`)(key, `)(`)(dir)(?, -, +, *).evalTap(_ => code)

          /**
            * linear constant replication output guard
            */
          def apply(rate: Rate, value: `()`[F], `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                   (using % : %[F], / : /[F], \ : \[F])
                   (using `}{`.stm.TSemaphore)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): Stream[F, Unit] =
            implicit val ord = dir.ord
            for
              _        <- if None eq + then Stream.eval(exclude(key))
                          else Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
              deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
              continue <- Stream.eval(Ref[F].of(deferred))
              deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
              _        <- if None eq + then Stream.unit else Stream.eval(deferred.complete(None))
              now      <- Stream.eval(Async[F].monotonic.map(_.toNanos))
              timestamp <- Stream.eval(Ref[F].of(now))
              _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp -> (`)(` -> dir, (map -> ord, Some(false), rate))))))
              cb_fb_tk <- Stream.eval(deferred.get)
              _        <- if None eq + then Stream.eval(?.complete(cb_fb_tk eq None)) else Stream.unit
              _        <- Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
              sr <- Stream.eval(SignallingRef[F].of(false))
              _  <- Stream.repeatEval {
                for
                  _        <- -.await
                  _        <- +.fold(Async[F].unit)(_.take)
                  now      <- Async[F].monotonic.map(_.toNanos)
                  enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                  _        <- if enabled then Async[F].unit else timestamp.set(now)
                  _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                  deferred <- continue.get
                  cb_fb_tk <- deferred.get
                  deferred <- Deferred[F, Option[<>[F]]]
                  _        <- continue.set(deferred)
                  _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                  token    <- if cb_fb_tk eq None then sr.set(true).as(null)
                              else
                                val (cbarrier, fiber, token) = cb_fb_tk.get
                                (fiber.join >> `}{`.><.release1 >> enable[F](key) >> cbarrier.await).as(token)
                yield
                  value -> token
              }.interruptWhen(sr).through1(t)
              _  <- Stream.eval(*.fold(Async[F].unit)(_.offer(())))
            yield
              ()

          /**
            * linear constant replication output guard w/ pace
            */
          def apply(rate: Rate, pace: FiniteDuration, value: `()`[F], `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                   (using % : %[F], / : /[F], \ : \[F])
                   (using `}{`.stm.TSemaphore)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): Stream[F, Unit] =
            apply(rate, value, `}{`)(key, `)(`)(dir)(?, -, +, *).spaced(pace)

          /**
            * linear constant replication output guard w/ code
            */
          def apply[T](rate: Rate, value: `()`[F], `}{`: `}{`[F])(key: String, `)(`: `)(`)(code: => F[T])(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                      (using % : %[F], / : /[F], \ : \[F])
                      (using `}{`.stm.TSemaphore)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): Stream[F, Unit] =
            apply(rate, value, `}{`)(key, `)(`)(dir)(?, -, +, *).evalTap(_ => code)

          /**
            * linear constant replication output guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: FiniteDuration, value: `()`[F], `}{`: `}{`[F])(key: String, `)(`: `)(`)(code: => F[T])(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                      (using % : %[F], / : /[F], \ : \[F])
                      (using `}{`.stm.TSemaphore)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): Stream[F, Unit] =
            apply(rate, pace, value, `}{`)(key, `)(`)(dir)(?, -, +, *).evalTap(_ => code)

          object * :

            /**
              * linear variable replication output guard
              */
            def apply[S](rate: Rate, value: => S, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                        (using %[F], /[F], \[F])
                        (using `}{`.stm.TSemaphore)
                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                  ^ : String): Stream[F, Unit] =
             apply[S](rate, Async[F].delay(value), `}{`)(key, `)(`)(dir)(?, -, +, *)

            /**
              * linear variable replication output guard w/ pace
              */
            def apply[S](rate: Rate, pace: FiniteDuration, value: => S, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                        (using %[F], /[F], \[F])
                        (using `}{`.stm.TSemaphore)
                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                  ^ : String): Stream[F, Unit] =
             apply[S](rate, pace, Async[F].delay(value), `}{`)(key, `)(`)(dir)(?, -, +, *)

            /**
              * linear variable replication output guard w/ code
              */
            def apply[S, T](rate: Rate, value: => S, `}{`: `}{`[F])(key: String, `)(`: `)(`)(code: => F[T])(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                           (using %[F], /[F], \[F])
                           (using `}{`.stm.TSemaphore)
                           (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                     `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                     ^ : String): Stream[F, Unit] =
             apply[S, T](rate, Async[F].delay(value), `}{`)(key, `)(`)(code)(dir)(?, -, +, *)

            /**
              * linear variable replication output guard w/ pace w/ code
              */
            def apply[S, T](rate: Rate, pace: FiniteDuration, value: => S, `}{`: `}{`[F])(key: String, `)(`: `)(`)(code: => F[T])(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                           (using %[F], /[F], \[F])
                           (using `}{`.stm.TSemaphore)
                           (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                     `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                     ^ : String): Stream[F, Unit] =
             apply[S, T](rate, pace, Async[F].delay(value), `}{`)(key, `)(`)(code)(dir)(?, -, +, *)

            /**
              * linear variable replication output guard
              */
            @annotation.targetName("applyF")
            def apply[S](rate: Rate, value: => F[S], `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                        (using % : %[F], / : /[F], \ : \[F])
                        (using `}{`.stm.TSemaphore)
                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                  ^ : String): Stream[F, Unit] =
              implicit val ord = dir.ord
              for
                _        <- if None eq + then Stream.eval(exclude(key))
                            else Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
                continue <- Stream.eval(Ref[F].of(deferred))
                deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
                _        <- if None eq + then Stream.unit else Stream.eval(deferred.complete(None))
                now      <- Stream.eval(Async[F].monotonic.map(_.toNanos))
                timestamp <- Stream.eval(Ref[F].of(now))
                _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp -> (`)(` -> dir, (map -> ord, Some(false), rate))))))
                cb_fb_tk <- Stream.eval(deferred.get)
                _        <- if None eq + then Stream.eval(?.complete(cb_fb_tk eq None)) else Stream.unit
                _        <- Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                sr <- Stream.eval(SignallingRef[F].of(false))
                _  <- Stream.repeatEval {
                  for
                    _        <- -.await
                    _        <- +.fold(Async[F].unit)(_.take)
                    now      <- Async[F].monotonic.map(_.toNanos)
                    enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                    _        <- if enabled then Async[F].unit else timestamp.set(now)
                    _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                    deferred <- continue.get
                    cb_fb_tk <- deferred.get
                    deferred <- Deferred[F, Option[<>[F]]]
                    _        <- continue.set(deferred)
                    _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                    it       <- if cb_fb_tk eq None then sr.set(true).as(`null` -> null)
                                else
                                  val (cbarrier, fiber, token) = cb_fb_tk.get
                                  value.map(new `()`[F](_) -> token).flatTap(_ => fiber.join *> `}{`.><.release1 *> enable[F](key) >> cbarrier.await)
                  yield
                    it
                }.interruptWhen(sr).through1(t)
                _  <- Stream.eval(*.fold(Async[F].unit)(_.offer(())))
              yield
                ()

            /**
              * linear variable replication output guard w/ pace
              */
            @annotation.targetName("applyF")
            def apply[S](rate: Rate, pace: FiniteDuration, value: => F[S], `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                        (using %[F], /[F], \[F])
                        (using `}{`.stm.TSemaphore)
                        (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                  `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                  ^ : String): Stream[F, Unit] =
              apply[S](rate, value, `}{`)(key, `)(`)(dir)(?, -, +, *).spaced(pace)

            /**
              * linear variable replication output guard w/ code
              */
            @annotation.targetName("applyF")
            def apply[S, T](rate: Rate, value: => F[S], `}{`: `}{`[F])(key: String, `)(`: `)(`)(code: => F[T])(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                           (using %[F], /[F], \[F])
                           (using `}{`.stm.TSemaphore)
                           (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                     `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                     ^ : String): Stream[F, Unit] =
              apply[S](rate, value, `}{`)(key, `)(`)(dir)(?, -, +, *).evalTap(_ => code)

            /**
              * linear variable replication output guard w/ pace w/ code
              */
            @annotation.targetName("applyF")
            def apply[S, T](rate: Rate, pace: FiniteDuration, value: => F[S], `}{`: `}{`[F])(key: String, `)(`: `)(`)(code: => F[T])(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                           (using %[F], /[F], \[F])
                           (using `}{`.stm.TSemaphore)
                           (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                     `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                     ^ : String): Stream[F, Unit] =
              apply[S](rate, pace, value, `}{`)(key, `)(`)(dir)(?, -, +, *).evalTap(_ => code)

          /**
            * linear replication input guard
            */
          def apply(rate: Rate, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                   (using % : %[F], / : /[F], \ : \[F])
                   (using `}{`.stm.TSemaphore)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): Stream[F, `()`[F]] =
            implicit val ord = dir.ord
            for
              _        <- if None eq + then Stream.eval(exclude(key))
                          else Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
              deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
              continue <- Stream.eval(Ref[F].of(deferred))
              deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
              _        <- if None eq + then Stream.unit else Stream.eval(deferred.complete(None))
              now      <- Stream.eval(Async[F].monotonic.map(_.toNanos))
              timestamp <- Stream.eval(Ref[F].of(now))
              _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp -> (`)(` -> dir, (map -> ord, Some(true), rate))))))
              cb_fb_tk <- Stream.eval(deferred.get)
              _        <- if None eq + then Stream.eval(?.complete(cb_fb_tk eq None)) else Stream.unit
              _        <- Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
              sr <- Stream.eval(SignallingRef[F].of(false))
              it <- Stream.repeatEval {
                for
                  _        <- -.await
                  _        <- +.fold(Async[F].unit)(_.take)
                  now      <- Async[F].monotonic.map(_.toNanos)
                  enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                  _        <- if enabled then Async[F].unit else timestamp.set(now)
                  _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                  deferred <- continue.get
                  cb_fb_tk <- deferred.get
                  deferred <- Deferred[F, Option[<>[F]]]
                  _        <- continue.set(deferred)
                  _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                  token    <- if cb_fb_tk eq None then sr.set(true).as(null)
                              else
                                val (cbarrier, fiber, token) = cb_fb_tk.get
                                (fiber.join >> `}{`.><.release1 >> enable[F](key) >> cbarrier.await).as(token)
                yield
                  token
              }.interruptWhen(sr).`zipRight s`
              _  <- Stream.eval(*.fold(Async[F].unit)(_.offer(())))
            yield
              it

          /**
            * linear replication input guard w/ pace
            */
          def apply(rate: Rate, pace: FiniteDuration, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                   (using %[F], /[F], \[F])
                   (using `}{`.stm.TSemaphore)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): Stream[F, `()`[F]] =
            apply(rate, `}{`)(key, `)(`)(dir)(?, -, +, *).spaced(pace)

          /**
            * linear replication input guard w/ code
            */
          def apply[T](rate: Rate, `}{`: `}{`[F])(key: String, `)(`: `)(`)(code: T => F[T])(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                      (using %[F], /[F], \[F])
                      (using `}{`.stm.TSemaphore)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): Stream[F, `()`[F]] =
            apply(rate, `}{`)(key, `)(`)(dir)(?, -, +, *).evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) }

          /**
            * linear replication input guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: FiniteDuration, `}{`: `}{`[F])(key: String, `)(`: `)(`)(code: T => F[T])(dir: `π-$`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                      (using %[F], /[F], \[F])
                      (using `}{`.stm.TSemaphore)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): Stream[F, `()`[F]] =
            apply(rate, pace, `}{`)(key, `)(`)(dir)(?, -, +, *).evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) }

        object ν:

          /**
            * replication bound output guard
            */
          def apply(rate: Rate, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
                   (using % : %[F], / : /[F], \ : \[F])
                   (using `}{`.stm.TSemaphore)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): Stream[F, `()`[F]] =
            implicit val ord = dir.ord
            for
              _        <- Stream.eval(exclude(key))
              deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
              continue <- Stream.eval(Ref[F].of(deferred))
              deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
              now      <- Stream.eval(Async[F].monotonic.map(_.toNanos))
              timestamp <- Stream.eval(Ref[F].of(now))
              _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp -> (`)(` -> dir, (map -> ord, Some(false), rate))))))
              cb_fb_tk <- Stream.eval(deferred.get)
              if cb_fb_tk ne None
              sr <- Stream.eval(SignallingRef[F].of(false))
              it <- ( for
                        _  <- Stream.unit.repeat
                        it <- sΠ.ν[F]
                        it <- Stream.eval {
                          for
                            now      <- Async[F].monotonic.map(_.toNanos)
                            enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                            _        <- if enabled then Async[F].unit else timestamp.set(now)
                            _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                            deferred <- continue.get
                            cb_fb_tk <- deferred.get
                            deferred <- Deferred[F, Option[<>[F]]]
                            _        <- continue.set(deferred)
                            _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                            token    <- if cb_fb_tk eq None then sr.set(true).as(null)
                                        else
                                          val (cbarrier, fiber, token) = cb_fb_tk.get
                                          (fiber.join >> `}{`.><.release1 >> enable[F](key) >> cbarrier.await).as(token)
                          yield
                            it -> token
                        }
                      yield
                        it
                    ).interruptWhen(sr).through1(t)
            yield
              it._1

          /**
            * replication bound output guard w/ pace
            */
          def apply(rate: Rate, pace: FiniteDuration, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
                   (using %[F], /[F], \[F])
                   (using `}{`.stm.TSemaphore)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): Stream[F, `()`[F]] =
            apply(rate, `}{`)(key, `)(`)(dir).spaced(pace)

          /**
            * replication bound output guard w/ code
            */
          def apply[T](rate: Rate, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                      (using %[F], /[F], \[F])
                      (using `}{`.stm.TSemaphore)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): Stream[F, `()`[F]] =
            apply(rate, `}{`)(key, `)(`)(dir).evalTap(_ => code)

          /**
            * replication bound output guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: FiniteDuration, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                      (using %[F], /[F], \[F])
                      (using `}{`.stm.TSemaphore)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): Stream[F, `()`[F]] =
            apply(rate, pace, `}{`)(key, `)(`)(dir).evalTap(_ => code)

        /**
          * constant replication output guard
          */
        def apply(rate: Rate, value: `()`[F], `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
                 (using % : %[F], / : /[F], \ : \[F])
                 (using `}{`.stm.TSemaphore)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, Unit] =
          implicit val ord = dir.ord
          for
            _        <- Stream.eval(exclude(key))
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            continue <- Stream.eval(Ref[F].of(deferred))
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            now      <- Stream.eval(Async[F].monotonic.map(_.toNanos))
            timestamp <- Stream.eval(Ref[F].of(now))
            _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp -> (`)(` -> dir, (map -> ord, Some(false), rate))))))
            cb_fb_tk <- Stream.eval(deferred.get)
            if cb_fb_tk ne None
            sr <- Stream.eval(SignallingRef[F].of(false))
            _  <- Stream.repeatEval {
              for
                now      <- Async[F].monotonic.map(_.toNanos)
                enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                _        <- if enabled then Async[F].unit else timestamp.set(now)
                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                deferred <- continue.get
                cb_fb_tk <- deferred.get
                deferred <- Deferred[F, Option[<>[F]]]
                _        <- continue.set(deferred)
                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                token    <- if cb_fb_tk eq None then sr.set(true).as(null)
                            else
                              val (cbarrier, fiber, token) = cb_fb_tk.get
                              (fiber.join >> `}{`.><.release1 >> enable[F](key) >> cbarrier.await).as(token)
              yield
                value -> token
            }.interruptWhen(sr).through1(t)
          yield
            ()

        /**
          * constant replication output guard w/ pace
          */
        def apply(rate: Rate, pace: FiniteDuration, value: `()`[F], `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
                 (using %[F], /[F], \[F])
                 (using `}{`.stm.TSemaphore)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, Unit] =
          apply(rate, value, `}{`)(key, `)(`)(dir).spaced(pace)

        /**
          * constant replication output guard w/ code
          */
        def apply[T](rate: Rate, value: `()`[F], `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                    (using %[F], /[F], \[F])
                    (using `}{`.stm.TSemaphore)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          apply(rate, value, `}{`)(key, `)(`)(dir).evalTap(_ => code)

        /**
          * constant replication output guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration, value: `()`[F], `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                    (using %[F], /[F], \[F])
                    (using `}{`.stm.TSemaphore)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          apply(rate, pace, value, `}{`)(key, `)(`)(dir).evalTap(_ => code)

        object * :

          /**
            * variable replication output guard
            */
          def apply[S](rate: Rate, value: => S, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
                      (using %[F], /[F], \[F])
                      (using `}{`.stm.TSemaphore)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): Stream[F, Unit] =
            apply[S](rate, Async[F].delay(value), `}{`)(key, `)(`)(dir)

          /**
            * variable replication output guard w/ pace
            */
          def apply[S](rate: Rate, pace: FiniteDuration, value: => S, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
                      (using %[F], /[F], \[F])
                      (using `}{`.stm.TSemaphore)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): Stream[F, Unit] =
            apply[S](rate, pace, Async[F].delay(value), `}{`)(key, `)(`)(dir)

          /**
            * variable replication output guard w/ code
            */
          def apply[S, T](rate: Rate, value: => S, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                         (using %[F], /[F], \[F])
                         (using `}{`.stm.TSemaphore)
                         (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                   `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                   ^ : String): Stream[F, Unit] =
            apply[S, T](rate, Async[F].delay(value), `}{`)(key, `)(`)(dir)(code)

          /**
            * variable replication output guard w/ pace w/ code
            */
          def apply[S, T](rate: Rate, pace: FiniteDuration, value: => S, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                         (using %[F], /[F], \[F])
                         (using `}{`.stm.TSemaphore)
                         (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                   `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                   ^ : String): Stream[F, Unit] =
            apply[S, T](rate, pace, Async[F].delay(value), `}{`)(key, `)(`)(dir)(code)

          /**
            * variable replication output guard
            */
          @annotation.targetName("applyF")
          def apply[S](rate: Rate, value: => F[S], `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
                      (using % : %[F], / : /[F], \ : \[F])
                      (using `}{`.stm.TSemaphore)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): Stream[F, Unit] =
            implicit val ord = dir.ord
            for
              _        <- Stream.eval(exclude(key))
              deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
              continue <- Stream.eval(Ref[F].of(deferred))
              deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
              now      <- Stream.eval(Async[F].monotonic.map(_.toNanos))
              timestamp <- Stream.eval(Ref[F].of(now))
              _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp -> (`)(` -> dir, (map -> ord, Some(false), rate))))))
              cb_fb_tk <- Stream.eval(deferred.get)
              if cb_fb_tk ne None
              sr <- Stream.eval(SignallingRef[F].of(false))
              _  <- Stream.repeatEval {
                for
                  now      <- Async[F].monotonic.map(_.toNanos)
                  enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                  _        <- if enabled then Async[F].unit else timestamp.set(now)
                  _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                  deferred <- continue.get
                  cb_fb_tk <- deferred.get
                  deferred <- Deferred[F, Option[<>[F]]]
                  _        <- continue.set(deferred)
                  _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                  deferred <- Deferred[F, Unit]
                  it       <- if cb_fb_tk eq None then sr.set(true).as(`null` -> null)
                              else
                                val (cbarrier, fiber, token) = cb_fb_tk.get
                                value.map(new `()`[F](_) -> token).flatTap(_ => fiber.join >> `}{`.><.release1 >> enable[F](key) >> cbarrier.await)
                yield
                  it
              }.interruptWhen(sr).through1(t)
            yield
              ()

          /**
            * variable replication output guard w/ pace
            */
          @annotation.targetName("applyF")
          def apply[S](rate: Rate, pace: FiniteDuration, value: => F[S], `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
                      (using %[F], /[F], \[F])
                      (using `}{`.stm.TSemaphore)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): Stream[F, Unit] =
            apply[S](rate, value, `}{`)(key, `)(`)(dir).spaced(pace)

          /**
            * variable replication output guard w/ code
            */
          @annotation.targetName("applyF")
          def apply[S, T](rate: Rate, value: => F[S], `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                         (using %[F], /[F], \[F])
                         (using `}{`.stm.TSemaphore)
                         (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                   `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                   ^ : String): Stream[F, Unit] =
            apply[S](rate, value, `}{`)(key, `)(`)(dir).evalTap(_ => code)

          /**
            * variable replication output guard w/ pace w/ code
            */
          @annotation.targetName("applyF")
          def apply[S, T](rate: Rate, pace: FiniteDuration, value: => F[S], `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                         (using %[F], /[F], \[F])
                         (using `}{`.stm.TSemaphore)
                         (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                   `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                   ^ : String): Stream[F, Unit] =
            apply[S](rate, pace, value, `}{`)(key, `)(`)(dir).evalTap(_ => code)

        /**
          * replication input guard
          */
        def apply(rate: Rate, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
                 (using % : %[F], / : /[F], \ : \[F])
                 (using `}{`.stm.TSemaphore)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, `()`[F]] =
          implicit val ord = dir.ord
          for
            _        <- Stream.eval(exclude(key))
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            continue <- Stream.eval(Ref[F].of(deferred))
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            now      <- Stream.eval(Async[F].monotonic.map(_.toNanos))
            timestamp <- Stream.eval(Ref[F].of(now))
            _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp -> (`)(` -> dir, (map -> ord, Some(true), rate))))))
            cb_fb_tk <- Stream.eval(deferred.get)
            if cb_fb_tk ne None
            sr <- Stream.eval(SignallingRef[F].of(false))
            it <- Stream.repeatEval {
                for
                  now      <- Async[F].monotonic.map(_.toNanos)
                  enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                  _        <- if enabled then Async[F].unit else timestamp.set(now)
                  _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                  deferred <- continue.get
                  cb_fb_tk <- deferred.get
                  deferred <- Deferred[F, Option[<>[F]]]
                  _        <- continue.set(deferred)
                  _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                  token    <- if cb_fb_tk eq None then sr.set(true).as(null)
                              else
                                val (cbarrier, fiber, token) = cb_fb_tk.get
                                (fiber.join >> `}{`.><.release1 >> enable[F](key) >> cbarrier.await).as(token)
                 yield
                   token
            }.interruptWhen(sr).`zipRight s`
          yield
            it

        /**
          * replication input guard w/ pace
          */
        def apply(rate: Rate, pace: FiniteDuration, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
                 (using %[F], /[F], \[F])
                 (using `}{`.stm.TSemaphore)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, `()`[F]] =
          apply(rate, `}{`)(key, `)(`)(dir).spaced(pace)

        /**
          * replication input guard w/ code
          */
        def apply[T](rate: Rate, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: T => F[T])
                    (using %[F], /[F], \[F])
                    (using `}{`.stm.TSemaphore)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, `()`[F]] =
          apply(rate, `}{`)(key, `)(`)(dir).evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) }

        /**
          * replication input guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: T => F[T])
                    (using %[F], /[F], \[F])
                    (using `}{`.stm.TSemaphore)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, `()`[F]] =
          apply(rate, pace, `}{`)(key, `)(`)(dir).evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) }

      object ν:

        /**
          * bound output prefix
          */
        def apply(rate: Rate, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
                 (using % : %[F], / : /[F])
                 (using `}{`.stm.TSemaphore)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, `()`[F]] =
          implicit val ord = dir.ord
          for
            _        <- Stream.eval(exclude(key))
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            now      <- Stream.eval(Async[F].monotonic.map(_.toNanos))
            timestamp <- Stream.eval(Ref[F].of(now))
            _        <- Stream.eval(/.offer(^ -> key -> (deferred -> null -> (timestamp -> (`)(` -> dir, (map -> ord, Some(false), rate))))))
            cb_fb_tk <- Stream.eval(deferred.get)
            if cb_fb_tk ne None
            (cbarrier, fiber, token) = cb_fb_tk.get
            it <- sΠ.ν[F]
            _  <- Stream.emit(it -> token).evalTap(_ => fiber.join >> `}{`.><.release1 >> enable[F](key) >> cbarrier.await).through1(t)
          yield
            it

        /**
          * bound output prefix w/ pace
          */
        def apply(rate: Rate, pace: FiniteDuration, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
                 (using %[F], /[F])
                 (using `}{`.stm.TSemaphore)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, `()`[F]] =
          apply(rate, `}{`)(key, `)(`)(dir) <* Stream.sleep(pace)

        /**
          * bound output prefix w/ code
          */
        def apply[T](rate: Rate, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                    (using %[F], /[F])
                    (using `}{`.stm.TSemaphore)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, `()`[F]] =
          apply(rate, `}{`)(key, `)(`)(dir).evalTap(_ => code)

        /**
          * bound output prefix w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                    (using %[F], /[F])
                    (using `}{`.stm.TSemaphore)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, `()`[F]] =
          apply(rate, pace, `}{`)(key, `)(`)(dir).evalTap(_ => code)

      /**
        * constant output prefix
        */
      def apply(rate: Rate, value: `()`[F], `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
               (using % : %[F], / : /[F])
               (using `}{`.stm.TSemaphore)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, Unit] =
        implicit val ord = dir.ord
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          now      <- Stream.eval(Async[F].monotonic.map(_.toNanos))
          timestamp <- Stream.eval(Ref[F].of(now))
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> null -> (timestamp -> (`)(` -> dir, (map -> ord, Some(false), rate))))))
          cb_fb_tk <- Stream.eval(deferred.get)
          if cb_fb_tk ne None
          (cbarrier, fiber, token) = cb_fb_tk.get
          _ <- Stream.emit(value -> token).evalTap(_ => fiber.join >> `}{`.><.release1 >> enable[F](key) >> cbarrier.await).through1(t)
        yield
          ()

      /**
        * constant output prefix w/ pace
        */
      def apply(rate: Rate, pace: FiniteDuration, value: `()`[F], `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
               (using %[F], /[F])
               (using `}{`.stm.TSemaphore)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, Unit] =
        apply(rate, value, `}{`)(key, `)(`)(dir) <* Stream.sleep(pace)

      /**
        * constant output prefix w/ code
        */
      def apply[T](rate: Rate, value: `()`[F], `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                  (using %[F], /[F])
                  (using `}{`.stm.TSemaphore)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, Unit] =
        apply(rate, value, `}{`)(key, `)(`)(dir).evalTap(_ => code)

      /**
        * constant output prefix w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration, value: `()`[F], `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                  (using %[F], /[F])
                  (using `}{`.stm.TSemaphore)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, Unit] =
        apply(rate, pace, value, `}{`)(key, `)(`)(dir).evalTap(_ => code)

      object * :

        /**
          * variable output prefix
          */
        def apply[S](rate: Rate, value: => S, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
                    (using %[F], /[F])
                    (using `}{`.stm.TSemaphore)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          apply[S](rate, Async[F].delay(value), `}{`)(key, `)(`)(dir)

        /**
          * variable output prefix w/ pace
          */
        def apply[S](rate: Rate, pace: FiniteDuration, value: => S, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
                    (using %[F], /[F])
                    (using `}{`.stm.TSemaphore)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          apply[S](rate, pace, Async[F].delay(value), `}{`)(key, `)(`)(dir)

        /**
          * variable output prefix w/ code
          */
        def apply[S, T](rate: Rate, value: => S, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                       (using %[F], /[F])
                       (using `}{`.stm.TSemaphore)
                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                 ^ : String): Stream[F, Unit] =
          apply[S, T](rate, Async[F].delay(value), `}{`)(key, `)(`)(dir)(code)

        /**
          * variable output prefix w/ pace w/ code
          */
        def apply[S, T](rate: Rate, pace: FiniteDuration, value: => S, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                       (using %[F], /[F])
                       (using `}{`.stm.TSemaphore)
                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                 ^ : String): Stream[F, Unit] =
          apply[S, T](rate, pace, Async[F].delay(value), `}{`)(key, `)(`)(dir)(code)

        /**
          * variable output prefix
          */
        @annotation.targetName("applyF")
        def apply[S](rate: Rate, value: => F[S], `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
                    (using % : %[F], / : /[F])
                    (using `}{`.stm.TSemaphore)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          implicit val ord = dir.ord
          for
            _        <- Stream.eval(exclude(key))
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            now      <- Stream.eval(Async[F].monotonic.map(_.toNanos))
            timestamp <- Stream.eval(Ref[F].of(now))
            _        <- Stream.eval(/.offer(^ -> key -> (deferred -> null -> (timestamp -> (`)(` -> dir, (map -> ord, Some(false), rate))))))
            cb_fb_tk <- Stream.eval(deferred.get)
            if cb_fb_tk ne None
            (cbarrier, fiber, token) = cb_fb_tk.get
            _        <- Stream.eval(value).map(new `()`[F](_) -> token).evalTap(_ => fiber.join >> `}{`.><.release1 >> enable[F](key) >> cbarrier.await).through1(t)
          yield
            ()

        /**
          * variable output prefix w/ pace
          */
        @annotation.targetName("applyF")
        def apply[S](rate: Rate, pace: FiniteDuration, value: => F[S], `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
                    (using %[F], /[F])
                    (using `}{`.stm.TSemaphore)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          apply[S](rate, value, `}{`)(key, `)(`)(dir) <* Stream.sleep(pace)

        /**
          * variable output prefix w/ code
          */
        @annotation.targetName("applyF")
        def apply[S, T](rate: Rate, value: => F[S], `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                       (using %[F], /[F])
                       (using `}{`.stm.TSemaphore)
                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                 ^ : String): Stream[F, Unit] =
          apply[S](rate, value, `}{`)(key, `)(`)(dir).evalTap(_ => code)

        /**
          * variable output prefix w/ pace w/ code
          */
        @annotation.targetName("applyF")
        def apply[S, T](rate: Rate, pace: FiniteDuration, value: => F[S], `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: => F[T])
                       (using %[F], /[F])
                       (using `}{`.stm.TSemaphore)
                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                 ^ : String): Stream[F, Unit] =
          apply[S](rate, pace, value, `}{`)(key, `)(`)(dir).evalTap(_ => code)

      /**
        * input prefix
        */
      def apply(rate: Rate, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
               (using % : %[F], / : /[F])
               (using `}{`.stm.TSemaphore)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, `()`[F]] =
        implicit val ord = dir.ord
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          now      <- Stream.eval(Async[F].monotonic.map(_.toNanos))
          timestamp <- Stream.eval(Ref[F].of(now))
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> null -> (timestamp -> (`)(` -> dir, (map -> ord, Some(true), rate))))))
          cb_fb_tk <- Stream.eval(deferred.get)
          if cb_fb_tk ne None
          (cbarrier, fiber, token) = cb_fb_tk.get
          it <- Stream.eval(fiber.join >> `}{`.><.release1 >> enable[F](key) >> cbarrier.await).as(token).`zipRight s`.head
        yield
          it

      /**
        * input prefix w/ pace
        */
      def apply(rate: Rate, pace: FiniteDuration, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)
               (using %[F], /[F])
               (using `}{`.stm.TSemaphore)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, `()`[F]] =
        apply(rate, `}{`)(key, `)(`)(dir).spaced(pace)

      /**
        * input prefix w/ code
        */
      def apply[T](rate: Rate, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: T => F[T])
                  (using %[F], /[F])
                  (using `}{`.stm.TSemaphore)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, `()`[F]] =
        apply(rate, `}{`)(key, `)(`)(dir).evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) }

      /**
        * input prefix w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration, `}{`: `}{`[F])(key: String, `)(`: `)(`)(dir: `π-$`)(code: T => F[T])
                  (using %[F], /[F])
                  (using `}{`.stm.TSemaphore)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, `()`[F]] =
        apply(rate, pace, `}{`)(key, `)(`)(dir).evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) }

    object ζ:

      object ! :

        object + :

          /**
            * linear replication capability guard
            */
          def apply(rate: Rate, `}{`: `}{`[F])(key: String, `)(`: `)(`)(cap: `π-ζ`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                   (using % : %[F], / : /[F], \ : \[F])
                   (using `}{`.stm.TSemaphore)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): Stream[F, Unit] =
            implicit val ord = cap.ord
            for
              _        <- Stream.eval(exclude(key))
              deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
              continue <- Stream.eval(Ref[F].of(deferred))
              deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
              polarity  = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
              now      <- Stream.eval(Async[F].monotonic.map(_.toNanos))
              timestamp <- Stream.eval(Ref[F].of(now))
              _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp -> (`)(` -> cap, (map -> ord, Some(polarity), rate))))))
              cb_fb_tk <- Stream.eval(deferred.get)
              if cb_fb_tk ne None
              sr <- Stream.eval(SignallingRef[F].of(false))
              tks = Stream.repeatEval {
                for
                  _        <- -.await
                  _        <- +.fold(Async[F].unit)(_.take)
                  now      <- Async[F].monotonic.map(_.toNanos)
                  enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                  _        <- if enabled then Async[F].unit else timestamp.set(now)
                  _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                  deferred <- continue.get
                  cb_fb_tk <- deferred.get
                  deferred <- Deferred[F, Option[<>[F]]]
                  _        <- continue.set(deferred)
                  _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                  token    <- if cb_fb_tk eq None then sr.set(true).as(null)
                              else
                                val (cbarrier, fiber, token) = cb_fb_tk.get
                                (fiber.join >> `}{`.><.release1 >> enable[F](key) >> cbarrier.await).as(token)
                 yield
                   token
              }.interruptWhen(sr)
              _  <- if polarity then tks.`zipRight s` else tks.map(unit -> _).through1(t)
              _  <- Stream.eval(*.fold(Async[F].unit)(_.offer(())))
            yield
              ()

          /**
            * linear replication capability guard w/ pace
            */
          def apply(rate: Rate, pace: FiniteDuration, `}{`: `}{`[F])(key: String, `)(`: `)(`)(cap: `π-ζ`)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                   (using %[F], /[F], \[F])
                   (using `}{`.stm.TSemaphore)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): Stream[F, Unit] =
            apply(rate, `}{`)(key, `)(`)(cap)(?, -, +, *).spaced(pace)

          /**
            * linear replication capability guard w/ code
            */
          def apply[T](rate: Rate, `}{`: `}{`[F])(key: String, `)(`: `)(`)(cap: `π-ζ`)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                      (using %[F], /[F], \[F])
                      (using `}{`.stm.TSemaphore)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): Stream[F, Unit] =
            apply(rate, `}{`)(key, `)(`)(cap)(?, -, +, *).evalTap(_ => code)

          /**
            * linear replication capability guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: FiniteDuration, `}{`: `}{`[F])(key: String, `)(`: `)(`)(cap: `π-ζ`)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                      (using %[F], /[F], \[F])
                      (using `}{`.stm.TSemaphore)
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): Stream[F, Unit] =
            apply(rate, pace, `}{`)(key, `)(`)(cap)(?, -, +, *).evalTap(_ => code)

        /**
          * replication capability guard
          */
        def apply(rate: Rate, `}{`: `}{`[F])(key: String, `)(`: `)(`)(cap: `π-ζ`)
                 (using % : %[F], / : /[F], \ : \[F])
                 (using `}{`.stm.TSemaphore)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, Unit] =
          implicit val ord = cap.ord
          for
            _        <- Stream.eval(exclude(key))
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            continue <- Stream.eval(Ref[F].of(deferred))
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            polarity  = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
            now      <- Stream.eval(Async[F].monotonic.map(_.toNanos))
            timestamp <- Stream.eval(Ref[F].of(now))
            _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp -> (`)(` -> cap, (map -> ord, Some(polarity), rate))))))
            cb_fb_tk <- Stream.eval(deferred.get)
            if cb_fb_tk ne None
            sr <- Stream.eval(SignallingRef[F].of(false))
            tks = Stream.repeatEval {
              for
                now      <- Async[F].monotonic.map(_.toNanos)
                enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                _        <- if enabled then Async[F].unit else timestamp.set(now)
                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                deferred <- continue.get
                cb_fb_tk <- deferred.get
                deferred <- Deferred[F, Option[<>[F]]]
                _        <- continue.set(deferred)
                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                token    <- if cb_fb_tk eq None then sr.set(true).as(null)
                            else
                              val (cbarrier, fiber, token) = cb_fb_tk.get
                              (fiber.join >> `}{`.><.release1 >> enable[F](key) >> cbarrier.await).as(token)
               yield
                 token
            }.interruptWhen(sr)
            _  <- if polarity then tks.`zipRight s` else tks.map(unit -> _).through1(t)
          yield
            ()

        /**
          * replication capability guard w/ pace
          */
        def apply(rate: Rate, pace: FiniteDuration, `}{`: `}{`[F])(key: String, `)(`: `)(`)(cap: `π-ζ`)
                 (using %[F], /[F], \[F])
                 (using `}{`.stm.TSemaphore)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, Unit] =
          apply(rate, `}{`)(key, `)(`)(cap).spaced(pace)

        /**
          * replication capability guard w/ code
          */
        def apply[T](rate: Rate, `}{`: `}{`[F])(key: String, `)(`: `)(`)(cap: `π-ζ`)(code: => F[T])
                    (using %[F], /[F], \[F])
                    (using `}{`.stm.TSemaphore)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          apply(rate, `}{`)(key, `)(`)(cap).evalTap(_ => code)

        /**
          * replication capability guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration, `}{`: `}{`[F])(key: String, `)(`: `)(`)(cap: `π-ζ`)(code: => F[T])
                    (using %[F], /[F], \[F])
                    (using `}{`.stm.TSemaphore)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          apply(rate, pace, `}{`)(key, `)(`)(cap).evalTap(_ => code)

      /**
        * capability prefix
        */
      def apply(rate: Rate, `}{`: `}{`[F])(key: String, `)(`: `)(`)(cap: `π-ζ`)
               (using % : %[F], / : /[F])
               (using `}{`.stm.TSemaphore)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, Unit] =
        implicit val ord = cap.ord
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          polarity  = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
          now      <- Stream.eval(Async[F].monotonic.map(_.toNanos))
          timestamp <- Stream.eval(Ref[F].of(now))
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> null -> (timestamp -> (`)(` -> cap, (map -> ord, Some(polarity), rate))))))
          cb_fb_tk <- Stream.eval(deferred.get)
          if cb_fb_tk ne None
          (cbarrier, fiber, token) = cb_fb_tk.get
          tks = Stream.eval(fiber.join >> `}{`.><.release1 >> enable[F](key) >> cbarrier.await).as(token)
          _  <- if polarity then tks.`zipRight s`.head else tks.map(unit -> _).through1(t)
        yield
          ()

      /**
        * capability prefix w/ pace
        */
      def apply(rate: Rate, pace: FiniteDuration, `}{`: `}{`[F])(key: String, `)(`: `)(`)(cap: `π-ζ`)
               (using %[F], /[F])
               (using `}{`.stm.TSemaphore)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, Unit] =
        apply(rate, `}{`)(key, `)(`)(cap) <* Stream.sleep(pace)

      /**
        * capability prefix w/ code
        */
      def apply[T](rate: Rate, `}{`: `}{`[F])(key: String, `)(`: `)(`)(cap: `π-ζ`)(code: => F[T])
                  (using %[F], /[F])
                  (using `}{`.stm.TSemaphore)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, Unit] =
        apply(rate, `}{`)(key, `)(`)(cap).evalTap(_ => code)

      /**
        * capability prefix w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration, `}{`: `}{`[F])(key: String, `)(`: `)(`)(cap: `π-ζ`)(code: => F[T])
                  (using %[F], /[F])
                  (using `}{`.stm.TSemaphore)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, Unit] =
        apply(rate, pace, `}{`)(key, `)(`)(cap).evalTap(_ => code)

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
               (using `][`: `][`, `2`: TSemaphore): F[`)(`] =
        for
          uuid <- sΠ.`)(`()
          node  = Set(uuid)
          _    <- stm.commit {
            for
              _ <- `2`.acquire
              _ <- `2`.acquire
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
              _ <- `2`.release
              _ <- `2`.release
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
          sem  <- stm.commit { TSemaphore.make(2) }
        yield
          (uuid, tree, sem)

    object >< :

      def release1(using `2`: TSemaphore): F[Unit] =
        stm.commit { `2`.release }

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
                 (using `][`: `][`, `2`: TSemaphore): F[Unit] =
          stm.commit {
            for
              _     <- `2`.acquire
              _     <- `2`.acquire
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
                 (using `][`: `][`, `2`: TSemaphore): F[Unit] =
          stm.commit {
            for
              _     <- `2`.acquire
              _     <- `2`.acquire
              node  <- `][`.get.map(_.keys.find(_.contains(key)).get)
              nodeʹ <- `][`.get.map(_.keys.find(_.contains(keyʹ)).get)
              _     <- check(node, nodeʹ, cap, capʹ).flatMap(stm.check(_))
              _     <- this(node, nodeʹ, cap, capʹ)
            yield
              ()
          }

  private object `Π-magic`:

    case class ><[F[_]](topic: Topic[F, (`()`[F], Unique.Token)],
                        queue: Queue[F, Unit],
                        limit: Ref[F, Boolean])

    type >*<[F[_]] = Map[Int, ><[F]]

    extension [F[_]: Async, O](self: Stream[F, O])
      inline def through1(topic: Topic[F, O])
                         (using await: F[Unit]): Stream[F, O] =
        self.evalMap { it => await >> topic.publish1(it).map(it -> _) }.takeWhile(_._2.isRight).map(_._1)
