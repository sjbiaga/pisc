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

  import _root_.cats.effect.{ Clock, Deferred, Ref, Resource, Temporal }
  import _root_.cats.effect.std.{ CyclicBarrier, Queue }

  import _root_.fs2.Stream
  import _root_.fs2.concurrent.{ SignallingRef, Topic }

  import `Π-loop`.{ +, %, /, \ }
  import `Π-magic`.*
  export `Π-magic`.><
  import `Π-stats`.Rate


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


  inline def `π-exclude`[F[_]: Temporal](enabled: String*)
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

  private def exclude[F[_]: Temporal](key: String)
                                     (using % : %[F])
                                     (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]]): F[Unit] =
    if `π-elvis`.contains(key)
    then
      `π-exclude`[F](`π-elvis`(key))
    else
      Temporal[F].unit


  /**
    * restriction aka new name
    */
  final class ν[F[_]: Temporal]:

    def map[B](f: `()`[F] => B): Stream[F, B] = flatMap(f andThen Stream.emit[F, B])
    def flatMap[B](f: `()`[F] => Stream[F, B]): Stream[F, B] =
      ( for
          topic <- Stream.eval(Topic[F, (`()`[F], Deferred[F, Unit])])
          queue <- Stream.eval(Queue.unbounded[F, Unit])
          limit <- Stream.eval(Ref[F].of(false))
        yield
          f(><(topic, queue, limit))
      ).flatten


  /**
    * silent transition
    */
  final class τ[F[_]: Clock: Temporal]:

    object ! :

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
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          continue <- Stream.eval(Ref[F].of(deferred))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
          timestamp <- Stream.eval(Ref[F].of(now))
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp, (new Object, None, rate)))))
          cbarrier <- Stream.eval(deferred.get)
          _        <- if cbarrier eq None then Stream.empty
                      else
                        for
                          sr <- Stream.eval(SignallingRef[F].of(false))
                          _  <- Stream.repeatEval {
                            for
                              now      <- Clock[F].monotonic.map(_.toNanos)
                              enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                              _        <- if enabled then Temporal[F].unit else timestamp.set(now)
                              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                              deferred <- continue.get
                              cbarrier <- deferred.get
                              deferred <- Deferred[F, Option[CyclicBarrier[F]]]
                              _        <- continue.set(deferred)
                              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                              _        <- if cbarrier eq None then sr.set(true) else enable[F](key) >> cbarrier.get.await
                            yield
                              ()
                          }.interruptWhen(sr)
                        yield
                          ()
        yield
          ()

      /**
        * replication guard w/ pace
        */
      def apply(rate: Rate, pace: FiniteDuration)(key: String)
               (using % : %[F], / : /[F], \ : \[F])
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, Unit] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          continue <- Stream.eval(Ref[F].of(deferred))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
          timestamp <- Stream.eval(Ref[F].of(now))
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp, (new Object, None, rate)))))
          cbarrier <- Stream.eval(deferred.get)
          _        <- if cbarrier eq None then Stream.empty
                      else
                        for
                          sr <- Stream.eval(SignallingRef[F].of(false))
                          _  <- Stream.repeatEval {
                            for
                              now      <- Clock[F].monotonic.map(_.toNanos)
                              enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                              _        <- if enabled then Temporal[F].unit else timestamp.set(now)
                              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                              deferred <- continue.get
                              cbarrier <- deferred.get
                              deferred <- Deferred[F, Option[CyclicBarrier[F]]]
                              _        <- continue.set(deferred)
                              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                              _        <- if cbarrier eq None then sr.set(true) else enable[F](key) >> cbarrier.get.await
                            yield
                              ()
                          }.interruptWhen(sr).spaced(pace)
                        yield
                          ()
        yield
          ()

      /**
        * replication guard w/ code
        */
      def apply[T](rate: Rate)(key: String)(code: => F[T])
                  (using % : %[F], / : /[F], \ : \[F])
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, Unit] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          continue <- Stream.eval(Ref[F].of(deferred))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
          timestamp <- Stream.eval(Ref[F].of(now))
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp, (new Object, None, rate)))))
          cbarrier <- Stream.eval(deferred.get)
          _        <- if cbarrier eq None then Stream.empty
                      else
                        for
                          sr <- Stream.eval(SignallingRef[F].of(false))
                          _  <- Stream.repeatEval {
                            for
                              now      <- Clock[F].monotonic.map(_.toNanos)
                              enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                              _        <- if enabled then Temporal[F].unit else timestamp.set(now)
                              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                              deferred <- continue.get
                              cbarrier <- deferred.get
                              deferred <- Deferred[F, Option[CyclicBarrier[F]]]
                              _        <- continue.set(deferred)
                              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                              _        <- if cbarrier eq None then sr.set(true) else enable[F](key) >> cbarrier.get.await
                            yield
                              ()
                          }.interruptWhen(sr).evalTap(_ => code)
                        yield
                          ()
        yield
          ()

      /**
        * replication guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: => F[T])
                  (using % : %[F], / : /[F], \ : \[F])
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, Unit] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          continue <- Stream.eval(Ref[F].of(deferred))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
          timestamp <- Stream.eval(Ref[F].of(now))
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp, (new Object, None, rate)))))
          cbarrier <- Stream.eval(deferred.get)
          _        <- if cbarrier eq None then Stream.empty
                      else
                        for
                          sr <- Stream.eval(SignallingRef[F].of(false))
                          _  <- Stream.repeatEval {
                            for
                              now      <- Clock[F].monotonic.map(_.toNanos)
                              enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                              _        <- if enabled then Temporal[F].unit else timestamp.set(now)
                              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                              deferred <- continue.get
                              cbarrier <- deferred.get
                              deferred <- Deferred[F, Option[CyclicBarrier[F]]]
                              _        <- continue.set(deferred)
                              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                              _        <- if cbarrier eq None then sr.set(true) else enable[F](key) >> cbarrier.get.await
                            yield
                              ()
                          }.interruptWhen(sr).spaced(pace).evalTap(_ => code)
                        yield
                          ()
        yield
          ()

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
        deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
        now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
        timestamp <- Stream.eval(Ref[F].of(now))
        _        <- Stream.eval(/.offer(^ -> key -> (deferred -> null -> (timestamp, (new Object, None, rate)))))
        cbarrier <- Stream.eval(deferred.get)
        _        <- if cbarrier eq None then Stream.empty
                    else Stream.eval(enable[F](key) >> cbarrier.get.await)
      yield
        ()

    /**
      * prefix w/ code
      */
    def apply[T](rate: Rate)(key: String)(code: => F[T])
                (using % : %[F], / : /[F])
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): Stream[F, Unit] =
      for
        _        <- Stream.eval(exclude(key))
        deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
        now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
        timestamp <- Stream.eval(Ref[F].of(now))
        _        <- Stream.eval(/.offer(^ -> key -> (deferred -> null -> (timestamp, (new Object, None, rate)))))
        cbarrier <- Stream.eval(deferred.get)
        _        <- if cbarrier eq None then Stream.empty
                    else Stream.eval(enable[F](key) >> cbarrier.get.await).evalTap(_ => code)
      yield
        ()

  /**
    * events, i.e., names (topics) and values
    */
  implicit final class `()`[F[_]: Clock: Temporal](private val name: Any) { self =>

    private inline def t = `()`[><[F]].topic
    private inline def q = `()`[><[F]].queue
    private inline def r = `()`[><[F]].limit
    private implicit def a: F[Unit] = q.take >> r.set(false)
    private def o =
      for
        b <- r.get
        s <- q.size
        _ <- if !b || s == 0 then q.offer(()) >> r.set(true) else Temporal[F].unit
      yield
        ()
    private def s = Stream.resource(t.subscribeAwaitUnbounded <* Resource.eval(o)).flatten.evalFilter(_._2.complete(())).map(_._1)

    def ====(that: `()`[F]) =
      try
        this.t eq that.t
      catch
        case _ =>
          this.name == that.name

    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()`[F] = this

    lazy val `null` = new `()`[F](null)

    object ! :

      object ν:

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
            deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
            continue <- Stream.eval(Ref[F].of(deferred))
            deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
            now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
            timestamp <- Stream.eval(Ref[F].of(now))
            _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp, (`()`[><[F]], Some(false), rate)))))
            cbarrier <- Stream.eval(deferred.get)
            name     <- if cbarrier eq None then Stream.empty
                        else
                          for
                            sr <- Stream.eval(SignallingRef[F].of(false))
                            _  <- Stream.unit.repeat
                            it <- sΠ.ν[F]
                            _  <- Stream.eval {
                              for
                                now      <- Clock[F].monotonic.map(_.toNanos)
                                enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                                _        <- if enabled then Temporal[F].unit else timestamp.set(now)
                                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                                deferred <- continue.get
                                cbarrier <- deferred.get
                                deferred <- Deferred[F, Option[CyclicBarrier[F]]]
                                _        <- continue.set(deferred)
                                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                                deferred <- Deferred[F, Unit]
                                _        <- if cbarrier eq None then deferred.complete(()) >> sr.set(true) else enable[F](key) >> cbarrier.get.await
                              yield
                                it -> deferred
                            }.interruptWhen(sr).through1(t)
                          yield
                            it
          yield
            name

        /**
          * replication bound output guard w/ code
          */
        def apply[T](rate: Rate)(key: String)(code: => F[T])
                    (using % : %[F], / : /[F], \ : \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, `()`[F]] =
          for
            _        <- Stream.eval(exclude(key))
            deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
            continue <- Stream.eval(Ref[F].of(deferred))
            deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
            now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
            timestamp <- Stream.eval(Ref[F].of(now))
            _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp, (`()`[><[F]], Some(false), rate)))))
            cbarrier <- Stream.eval(deferred.get)
            name     <- if cbarrier eq None then Stream.empty
                        else
                          for
                            sr <- Stream.eval(SignallingRef[F].of(false))
                            _  <- Stream.unit.repeat
                            it <- sΠ.ν[F]
                            _  <- Stream.eval {
                              for
                                now      <- Clock[F].monotonic.map(_.toNanos)
                                enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                                _        <- if enabled then Temporal[F].unit else timestamp.set(now)
                                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                                deferred <- continue.get
                                cbarrier <- deferred.get
                                deferred <- Deferred[F, Option[CyclicBarrier[F]]]
                                _        <- continue.set(deferred)
                                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                                deferred <- Deferred[F, Unit]
                                _        <- if cbarrier eq None then deferred.complete(()) >> sr.set(true) else enable[F](key) >> cbarrier.get.await
                              yield
                                it -> deferred
                            }.interruptWhen(sr).through1(t).evalTap(_ => code)
                          yield
                            it
          yield
            name

        /**
          * replication bound output guard w/ pace
          */
        def apply(rate: Rate, pace: FiniteDuration)(key: String)
                 (using % : %[F], / : /[F], \ : \[F])
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, `()`[F]] =
          for
            _        <- Stream.eval(exclude(key))
            deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
            continue <- Stream.eval(Ref[F].of(deferred))
            deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
            now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
            timestamp <- Stream.eval(Ref[F].of(now))
            _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp, (`()`[><[F]], Some(false), rate)))))
            cbarrier <- Stream.eval(deferred.get)
            name     <- if cbarrier eq None then Stream.empty
                        else
                          for
                            sr <- Stream.eval(SignallingRef[F].of(false))
                            _  <- Stream.unit.repeat
                            it <- sΠ.ν[F]
                            _  <- Stream.eval {
                              for
                                now      <- Clock[F].monotonic.map(_.toNanos)
                                enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                                _        <- if enabled then Temporal[F].unit else timestamp.set(now)
                                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                                deferred <- continue.get
                                cbarrier <- deferred.get
                                deferred <- Deferred[F, Option[CyclicBarrier[F]]]
                                _        <- continue.set(deferred)
                                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                                deferred <- Deferred[F, Unit]
                                _        <- if cbarrier eq None then deferred.complete(()) >> sr.set(true) else enable[F](key) >> cbarrier.get.await
                              yield
                                it -> deferred
                            }.interruptWhen(sr).spaced(pace).through1(t)
                          yield
                            it
          yield
            name

        /**
          * replication bound output guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: => F[T])
                    (using % : %[F], / : /[F], \ : \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, `()`[F]] =
          for
            _        <- Stream.eval(exclude(key))
            deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
            continue <- Stream.eval(Ref[F].of(deferred))
            deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
            now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
            timestamp <- Stream.eval(Ref[F].of(now))
            _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp, (`()`[><[F]], Some(false), rate)))))
            cbarrier <- Stream.eval(deferred.get)
            name     <- if cbarrier eq None then Stream.empty
                        else
                          for
                            sr <- Stream.eval(SignallingRef[F].of(false))
                            _  <- Stream.unit.repeat
                            it <- sΠ.ν[F]
                            _  <- Stream.eval {
                              for
                                now      <- Clock[F].monotonic.map(_.toNanos)
                                enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                                _        <- if enabled then Temporal[F].unit else timestamp.set(now)
                                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                                deferred <- continue.get
                                cbarrier <- deferred.get
                                deferred <- Deferred[F, Option[CyclicBarrier[F]]]
                                _        <- continue.set(deferred)
                                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                                deferred <- Deferred[F, Unit]
                                _        <- if cbarrier eq None then deferred.complete(()) >> sr.set(true) else enable[F](key) >> cbarrier.get.await
                              yield
                                it -> deferred
                            }.interruptWhen(sr).spaced(pace).through1(t).evalTap(_ => code)
                          yield
                            it
          yield
            name

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
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          continue <- Stream.eval(Ref[F].of(deferred))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
          timestamp <- Stream.eval(Ref[F].of(now))
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp, (`()`[><[F]], Some(false), rate)))))
          cbarrier <- Stream.eval(deferred.get)
          _        <- if cbarrier eq None then Stream.empty
                      else
                        for
                          sr <- Stream.eval(SignallingRef[F].of(false))
                          _  <- Stream.repeatEval {
                            for
                              now      <- Clock[F].monotonic.map(_.toNanos)
                              enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                              _        <- if enabled then Temporal[F].unit else timestamp.set(now)
                              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                              deferred <- continue.get
                              cbarrier <- deferred.get
                              deferred <- Deferred[F, Option[CyclicBarrier[F]]]
                              _        <- continue.set(deferred)
                              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                              deferred <- Deferred[F, Unit]
                              _        <- if cbarrier eq None then deferred.complete(()) >> sr.set(true) else enable[F](key) >> cbarrier.get.await
                            yield
                              value -> deferred
                          }.interruptWhen(sr).through1(t)
                        yield
                          ()
        yield
          ()

      /**
        * constant replication output guard w/ pace
        */
      def apply(rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String)
               (using % : %[F], / : /[F], \ : \[F])
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, Unit] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          continue <- Stream.eval(Ref[F].of(deferred))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
          timestamp <- Stream.eval(Ref[F].of(now))
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp, (`()`[><[F]], Some(false), rate)))))
          cbarrier <- Stream.eval(deferred.get)
          _        <- if cbarrier eq None then Stream.empty
                      else
                        for
                          sr <- Stream.eval(SignallingRef[F].of(false))
                          _  <- Stream.repeatEval {
                            for
                              now      <- Clock[F].monotonic.map(_.toNanos)
                              enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                              _        <- if enabled then Temporal[F].unit else timestamp.set(now)
                              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                              deferred <- continue.get
                              cbarrier <- deferred.get
                              deferred <- Deferred[F, Option[CyclicBarrier[F]]]
                              _        <- continue.set(deferred)
                              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                              deferred <- Deferred[F, Unit]
                              _        <- if cbarrier eq None then deferred.complete(()) >> sr.set(true) else enable[F](key) >> cbarrier.get.await
                            yield
                              value -> deferred
                          }.interruptWhen(sr).spaced(pace).through1(t)
                        yield
                          ()
        yield
          ()

      /**
        * constant replication output guard w/ code
        */
      def apply[T](rate: Rate, value: `()`[F])(key: String)(code: => F[T])
               (using % : %[F], / : /[F], \ : \[F])
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, Unit] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          continue <- Stream.eval(Ref[F].of(deferred))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
          timestamp <- Stream.eval(Ref[F].of(now))
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp, (`()`[><[F]], Some(false), rate)))))
          cbarrier <- Stream.eval(deferred.get)
          _        <- if cbarrier eq None then Stream.empty
                      else
                        for
                          sr <- Stream.eval(SignallingRef[F].of(false))
                          _  <- Stream.repeatEval {
                            for
                              now      <- Clock[F].monotonic.map(_.toNanos)
                              enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                              _        <- if enabled then Temporal[F].unit else timestamp.set(now)
                              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                              deferred <- continue.get
                              cbarrier <- deferred.get
                              deferred <- Deferred[F, Option[CyclicBarrier[F]]]
                              _        <- continue.set(deferred)
                              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                              deferred <- Deferred[F, Unit]
                              _        <- if cbarrier eq None then deferred.complete(()) >> sr.set(true) else enable[F](key) >> cbarrier.get.await
                            yield
                              value -> deferred
                          }.interruptWhen(sr).through1(t).evalTap(_ => code)
                        yield
                          ()
        yield
          ()

      /**
        * constant replication output guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String)(code: => F[T])
               (using % : %[F], / : /[F], \ : \[F])
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, Unit] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          continue <- Stream.eval(Ref[F].of(deferred))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
          timestamp <- Stream.eval(Ref[F].of(now))
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp, (`()`[><[F]], Some(false), rate)))))
          cbarrier <- Stream.eval(deferred.get)
          _        <- if cbarrier eq None then Stream.empty
                      else
                        for
                          sr <- Stream.eval(SignallingRef[F].of(false))
                          _  <- Stream.repeatEval {
                            for
                              now      <- Clock[F].monotonic.map(_.toNanos)
                              enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                              _        <- if enabled then Temporal[F].unit else timestamp.set(now)
                              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                              deferred <- continue.get
                              cbarrier <- deferred.get
                              deferred <- Deferred[F, Option[CyclicBarrier[F]]]
                              _        <- continue.set(deferred)
                              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                              deferred <- Deferred[F, Unit]
                              _        <- if cbarrier eq None then deferred.complete(()) >> sr.set(true) else enable[F](key) >> cbarrier.get.await
                            yield
                              value -> deferred
                          }.interruptWhen(sr).spaced(pace).through1(t).evalTap(_ => code)
                        yield
                          ()
        yield
          ()

      object * :

        /**
          * variable replication output guard
          */
        def apply[S](rate: Rate, value: => F[S])(key: String)
                    (using % : %[F], / : /[F], \ : \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          for
            _        <- Stream.eval(exclude(key))
            deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
            continue <- Stream.eval(Ref[F].of(deferred))
            deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
            now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
            timestamp <- Stream.eval(Ref[F].of(now))
            _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp, (`()`[><[F]], Some(false), rate)))))
            cbarrier <- Stream.eval(deferred.get)
            _        <- if cbarrier eq None then Stream.empty
                        else
                          for
                            sr <- Stream.eval(SignallingRef[F].of(false))
                            _  <- Stream.repeatEval {
                              for
                                now      <- Clock[F].monotonic.map(_.toNanos)
                                enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                                _        <- if enabled then Temporal[F].unit else timestamp.set(now)
                                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                                deferred <- continue.get
                                cbarrier <- deferred.get
                                deferred <- Deferred[F, Option[CyclicBarrier[F]]]
                                _        <- continue.set(deferred)
                                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                                deferred <- Deferred[F, Unit]
                                it       <- if cbarrier eq None then deferred.complete(()) >> sr.set(true).as(null.asInstanceOf[S]) else value.flatTap(_ => enable[F](key) >> cbarrier.get.await)
                              yield
                                new `()`[F](it) -> deferred
                            }.interruptWhen(sr).through1(t)
                          yield
                            ()
          yield
            ()

        /**
          * variable replication output guard w/ pace
          */
        def apply[S](rate: Rate, pace: FiniteDuration, value: => F[S])(key: String)
                    (using % : %[F], / : /[F], \ : \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          for
            _        <- Stream.eval(exclude(key))
            deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
            continue <- Stream.eval(Ref[F].of(deferred))
            deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
            now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
            timestamp <- Stream.eval(Ref[F].of(now))
            _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp, (`()`[><[F]], Some(false), rate)))))
            cbarrier <- Stream.eval(deferred.get)
            _        <- if cbarrier eq None then Stream.empty
                        else
                          for
                            sr <- Stream.eval(SignallingRef[F].of(false))
                            _  <- Stream.repeatEval {
                              for
                                now      <- Clock[F].monotonic.map(_.toNanos)
                                enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                                _        <- if enabled then Temporal[F].unit else timestamp.set(now)
                                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                                deferred <- continue.get
                                cbarrier <- deferred.get
                                deferred <- Deferred[F, Option[CyclicBarrier[F]]]
                                _        <- continue.set(deferred)
                                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                                deferred <- Deferred[F, Unit]
                                it       <- if cbarrier eq None then deferred.complete(()) >> sr.set(true).as(null.asInstanceOf[S]) else value.flatTap(_ => enable[F](key) >> cbarrier.get.await)
                              yield
                                new `()`[F](it) -> deferred
                            }.interruptWhen(sr).spaced(pace).through1(t)
                          yield
                            ()
          yield
            ()

        /**
          * variable replication output guard w/ code
          */
        def apply[S, T](rate: Rate, value: => F[S])(key: String)(code: => F[T])
                       (using % : %[F], / : /[F], \ : \[F])
                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                 ^ : String): Stream[F, Unit] =
          for
            _        <- Stream.eval(exclude(key))
            deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
            continue <- Stream.eval(Ref[F].of(deferred))
            deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
            now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
            timestamp <- Stream.eval(Ref[F].of(now))
            _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp, (`()`[><[F]], Some(false), rate)))))
            cbarrier <- Stream.eval(deferred.get)
            _        <- if cbarrier eq None then Stream.empty
                        else
                          for
                            sr <- Stream.eval(SignallingRef[F].of(false))
                            _  <- Stream.repeatEval {
                              for
                                now      <- Clock[F].monotonic.map(_.toNanos)
                                enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                                _        <- if enabled then Temporal[F].unit else timestamp.set(now)
                                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                                deferred <- continue.get
                                cbarrier <- deferred.get
                                deferred <- Deferred[F, Option[CyclicBarrier[F]]]
                                _        <- continue.set(deferred)
                                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                                deferred <- Deferred[F, Unit]
                                it       <- if cbarrier eq None then deferred.complete(()) >> sr.set(true).as(null.asInstanceOf[S]) else value.flatTap(_ => enable[F](key) >> cbarrier.get.await)
                              yield
                                new `()`[F](it) -> deferred
                            }.interruptWhen(sr).through1(t).evalTap(_ => code)
                          yield
                            ()
          yield
            ()

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S, T](rate: Rate, pace: FiniteDuration, value: => F[S])(key: String)(code: => F[T])
                       (using % : %[F], / : /[F], \ : \[F])
                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                 ^ : String): Stream[F, Unit] =
          for
            _        <- Stream.eval(exclude(key))
            deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
            continue <- Stream.eval(Ref[F].of(deferred))
            deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
            now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
            timestamp <- Stream.eval(Ref[F].of(now))
            _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp, (`()`[><[F]], Some(false), rate)))))
            cbarrier <- Stream.eval(deferred.get)
            _        <- if cbarrier eq None then Stream.empty
                        else
                          for
                            sr <- Stream.eval(SignallingRef[F].of(false))
                            _  <- Stream.repeatEval {
                              for
                                now      <- Clock[F].monotonic.map(_.toNanos)
                                enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                                _        <- if enabled then Temporal[F].unit else timestamp.set(now)
                                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                                deferred <- continue.get
                                cbarrier <- deferred.get
                                deferred <- Deferred[F, Option[CyclicBarrier[F]]]
                                _        <- continue.set(deferred)
                                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                                deferred <- Deferred[F, Unit]
                                it       <- if cbarrier eq None then deferred.complete(()) >> sr.set(true).as(null.asInstanceOf[S]) else value.flatTap(_ => enable[F](key) >> cbarrier.get.await)
                              yield
                                new `()`[F](it) -> deferred
                            }.interruptWhen(sr).spaced(pace).through1(t).evalTap(_ => code)
                          yield
                            ()
          yield
            ()

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
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          continue <- Stream.eval(Ref[F].of(deferred))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
          timestamp <- Stream.eval(Ref[F].of(now))
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp, (`()`[><[F]], Some(true), rate)))))
          cbarrier <- Stream.eval(deferred.get)
          name     <- if cbarrier eq None then Stream.empty
                      else
                        for
                          sr <- Stream.eval(SignallingRef[F].of(false))
                          _  <- Stream.repeatEval {
                            for
                              now      <- Clock[F].monotonic.map(_.toNanos)
                              enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                              _        <- if enabled then Temporal[F].unit else timestamp.set(now)
                              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                              deferred <- continue.get
                              cbarrier <- deferred.get
                              deferred <- Deferred[F, Option[CyclicBarrier[F]]]
                              _        <- continue.set(deferred)
                              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                              _        <- if cbarrier eq None then sr.set(true) else enable[F](key) >> cbarrier.get.await
                            yield
                              ()
                          }.interruptWhen(sr)
                          it <- s.head
                        yield
                          it
        yield
          name

      /**
        * replication input guard w/ pace
        */
      def apply(rate: Rate, pace: FiniteDuration)(key: String)
               (using % : %[F], / : /[F], \ : \[F])
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, `()`[F]] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          continue <- Stream.eval(Ref[F].of(deferred))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
          timestamp <- Stream.eval(Ref[F].of(now))
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp, (`()`[><[F]], Some(true), rate)))))
          cbarrier <- Stream.eval(deferred.get)
          name     <- if cbarrier eq None then Stream.empty
                      else
                        for
                          sr <- Stream.eval(SignallingRef[F].of(false))
                          _  <- Stream.repeatEval {
                            for
                              now      <- Clock[F].monotonic.map(_.toNanos)
                              enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                              _        <- if enabled then Temporal[F].unit else timestamp.set(now)
                              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                              deferred <- continue.get
                              cbarrier <- deferred.get
                              deferred <- Deferred[F, Option[CyclicBarrier[F]]]
                              _        <- continue.set(deferred)
                              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                              _        <- if cbarrier eq None then sr.set(true) else enable[F](key) >> cbarrier.get.await
                            yield
                              ()
                          }.interruptWhen(sr).spaced(pace)
                          it <- s.head
                        yield
                          it
        yield
          name

      /**
        * replication input guard w/ code
        */
      def apply[T](rate: Rate)(key: String)(code: T => F[T])
                  (using % : %[F], / : /[F], \ : \[F])
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, `()`[F]] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          continue <- Stream.eval(Ref[F].of(deferred))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
          timestamp <- Stream.eval(Ref[F].of(now))
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp, (`()`[><[F]], Some(true), rate)))))
          cbarrier <- Stream.eval(deferred.get)
          name     <- if cbarrier eq None then Stream.empty
                      else
                        for
                          sr <- Stream.eval(SignallingRef[F].of(false))
                          _  <- Stream.repeatEval {
                            for
                              now      <- Clock[F].monotonic.map(_.toNanos)
                              enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                              _        <- if enabled then Temporal[F].unit else timestamp.set(now)
                              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                              deferred <- continue.get
                              cbarrier <- deferred.get
                              deferred <- Deferred[F, Option[CyclicBarrier[F]]]
                              _        <- continue.set(deferred)
                              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                              _        <- if cbarrier eq None then sr.set(true) else enable[F](key) >> cbarrier.get.await
                            yield
                              ()
                          }.interruptWhen(sr)
                          it <- s.head.evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) }
                        yield
                          it
        yield
          name

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: T => F[T])
                  (using % : %[F], / : /[F], \ : \[F])
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, `()`[F]] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          continue <- Stream.eval(Ref[F].of(deferred))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
          timestamp <- Stream.eval(Ref[F].of(now))
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (timestamp, (`()`[><[F]], Some(true), rate)))))
          cbarrier <- Stream.eval(deferred.get)
          name     <- if cbarrier eq None then Stream.empty
                      else
                        for
                          sr <- Stream.eval(SignallingRef[F].of(false))
                          _  <- Stream.repeatEval {
                            for
                              now      <- Clock[F].monotonic.map(_.toNanos)
                              enabled  <- %.modify { m => m -> m(^ + key).asInstanceOf[(Boolean, +[F])]._1 }
                              _        <- if enabled then Temporal[F].unit else timestamp.set(now)
                              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                              deferred <- continue.get
                              cbarrier <- deferred.get
                              deferred <- Deferred[F, Option[CyclicBarrier[F]]]
                              _        <- continue.set(deferred)
                              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                              _        <- if cbarrier eq None then sr.set(true) else enable[F](key) >> cbarrier.get.await
                            yield
                              ()
                          }.interruptWhen(sr).spaced(pace)
                          it <- s.head.evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) }
                        yield
                          it
        yield
          name

    object ν:

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
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
          timestamp <- Stream.eval(Ref[F].of(now))
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> null -> (timestamp, (`()`[><[F]], Some(false), rate)))))
          cbarrier <- Stream.eval(deferred.get)
          name     <- if cbarrier eq None then Stream.empty
                      else
                        for
                          it <- sΠ.ν[F]
                          _  <- Stream.eval(Deferred[F, Unit].map(it -> _).flatTap(_ => enable[F](key) >> cbarrier.get.await)).through1(t)
                        yield
                          it
        yield
          name

      /**
        * bound output prefix w/ code
        */
      def apply[T](rate: Rate)(key: String)(code: => F[T])
                  (using % : %[F], / : /[F])
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, `()`[F]] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
          timestamp <- Stream.eval(Ref[F].of(now))
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> null -> (timestamp, (`()`[><[F]], Some(false), rate)))))
          cbarrier <- Stream.eval(deferred.get)
          name     <- if cbarrier eq None then Stream.empty
                      else
                        for
                          it <- sΠ.ν[F]
                          _  <- Stream.eval(Deferred[F, Unit].map(it -> _).flatTap(_ => enable[F](key) >> cbarrier.get.await)).through1(t).evalTap(_ => code)
                        yield
                          it
        yield
          name

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
        deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
        now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
        timestamp <- Stream.eval(Ref[F].of(now))
        _        <- Stream.eval(/.offer(^ -> key -> (deferred -> null -> (timestamp, (`()`[><[F]], Some(false), rate)))))
        cbarrier <- Stream.eval(deferred.get)
        _        <- if cbarrier eq None then Stream.empty
                    else Stream.eval(Deferred[F, Unit].map(value -> _).flatTap(_ => enable[F](key) >> cbarrier.get.await)).through1(t)
      yield
        ()

    /**
      * constant output prefix w/ code
      */
    def apply[T](rate: Rate, value: `()`[F])(key: String)(code: => F[T])
                (using % : %[F], / : /[F])
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): Stream[F, Unit] =
      for
        _        <- Stream.eval(exclude(key))
        deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
        now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
        timestamp <- Stream.eval(Ref[F].of(now))
        _        <- Stream.eval(/.offer(^ -> key -> (deferred -> null -> (timestamp, (`()`[><[F]], Some(false), rate)))))
        cbarrier <- Stream.eval(deferred.get)
        _        <- if cbarrier eq None then Stream.empty
                    else Stream.eval(Deferred[F, Unit].map(value -> _).flatTap(_ => enable[F](key) >> cbarrier.get.await)).through1(t).evalTap(_ => code)
      yield
        ()

    object * :

      /**
        * variable output prefix
        */
      def apply[S](rate: Rate, value: => F[S])(key: String)
                  (using % : %[F], / : /[F])
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, Unit] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
          timestamp <- Stream.eval(Ref[F].of(now))
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> null -> (timestamp, (`()`[><[F]], Some(false), rate)))))
          cbarrier <- Stream.eval(deferred.get)
          _        <- if cbarrier eq None then Stream.empty
                      else Stream.eval(value).evalMap { it => Deferred[F, Unit].map(new `()`[F](it) -> _).flatTap(_ => enable[F](key) >> cbarrier.get.await) }.through1(t)
        yield
          ()

      /**
        * variable output prefix w/ code
        */
      def apply[S, T](rate: Rate, value: => F[S])(key: String)(code: => F[T])
                     (using % : %[F], / : /[F])
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                               ^ : String): Stream[F, Unit] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
          now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
          timestamp <- Stream.eval(Ref[F].of(now))
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> null -> (timestamp, (`()`[><[F]], Some(false), rate)))))
          cbarrier <- Stream.eval(deferred.get)
          _        <- if cbarrier eq None then Stream.empty
                      else Stream.eval(value).evalMap { it => Deferred[F, Unit].map(new `()`[F](it) -> _).flatTap(_ => enable[F](key) >> cbarrier.get.await) }.through1(t).evalTap(_ => code)
        yield
          ()

    /**
      * input prefix
      */
    def apply(rate: Rate)(key: String)
             (using % : %[F], / : /[F], \ : \[F])
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): Stream[F, `()`[F]] =
      for
        _        <- Stream.eval(exclude(key))
        deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
        now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
        timestamp <- Stream.eval(Ref[F].of(now))
        _        <- Stream.eval(/.offer(^ -> key -> (deferred -> null -> (timestamp, (`()`[><[F]], Some(true), rate)))))
        cbarrier <- Stream.eval(deferred.get)
        name     <- if cbarrier eq None then Stream.empty
                    else
                      for
                        _  <- Stream.eval(enable[F](key) >> cbarrier.get.await)
                        it <- s.head
                      yield
                        it
      yield
        name

    /**
      * input prefix w/ code
      */
    def apply[T](rate: Rate)(key: String)(code: T => F[T])
                (using % : %[F], / : /[F], \ : \[F])
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): Stream[F, `()`[F]] =
      for
        _        <- Stream.eval(exclude(key))
        deferred <- Stream.eval(Deferred[F, Option[CyclicBarrier[F]]])
        now      <- Stream.eval(Clock[F].monotonic.map(_.toNanos))
        timestamp <- Stream.eval(Ref[F].of(now))
        _        <- Stream.eval(/.offer(^ -> key -> (deferred -> null -> (timestamp, (`()`[><[F]], Some(true), rate)))))
        cbarrier <- Stream.eval(deferred.get)
        name     <- if cbarrier eq None then Stream.empty
                    else
                      for
                        _  <- Stream.eval(enable[F](key) >> cbarrier.get.await)
                        it <- s.head.evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) }
                      yield
                        it
      yield
        name

    override def toString: String = if name == null then "null" else name.toString

  }


  private object `Π-magic`:

    case class ><[F[_]](topic: Topic[F, (`()`[F], Deferred[F, Unit])],
                        queue: Queue[F, Unit],
                        limit: Ref[F, Boolean])

    extension [F[_]: Temporal, O](self: Stream[F, O])
      inline def through1(topic: Topic[F, O])
                         (using await: F[Unit]): Stream[F, Unit] =
        self.evalMap(await >> topic.publish1(_)).takeWhile(_.isRight).void
