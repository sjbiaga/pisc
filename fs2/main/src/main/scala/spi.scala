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
  import _root_.cats.effect.std.{ CyclicBarrier, Queue }

  import _root_.fs2.concurrent.{ SignallingRef, Topic }
  import _root_.fs2.{ Pull, Stream }

  import `Π-loop`.{ <>, +, %, /, \ }
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
                          (using %[F])
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
                                  (using %[F])
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
          topic <- Stream.eval(Topic[F, (`()`[F], Unique.Token)])
          queue <- Stream.eval(Queue.unbounded[F, Unit])
          limit <- Stream.eval(Ref[F].of(false))
        yield
          f(><[F](topic, queue, limit))
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
        def apply(rate: Rate)(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
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
            _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (new Object, None, rate))))
            cb_token <- Stream.eval(deferred.get)
            _        <- if None eq + then Stream.eval(?.complete(cb_token eq None)) else Stream.unit
            _        <- Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
            sr <- Stream.eval(SignallingRef[F].of(false))
            _  <- Stream.repeatEval {
              for
                _        <- -.await
                _        <- +.fold(Async[F].unit)(_.take)
                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                deferred <- continue.get
                cb_token <- deferred.get
                deferred <- Deferred[F, Option[<>[F]]]
                _        <- continue.set(deferred)
                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                _        <- if cb_token eq None then sr.set(true)
                            else
                              val (cbarrier, _) = cb_token.get
                              enable[F](key) >> cbarrier.await
              yield
                ()
            }.interruptWhen(sr)
            _  <- Stream.eval(*.fold(Async[F].unit)(_.offer(())))
          yield
            ()

        /**
          * linear replication guard w/ pace
          */
        def apply(rate: Rate, pace: FiniteDuration)(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                 (using %[F], /[F], \[F])
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, Unit] =
        apply(rate)(key)(?, -, +, *).spaced(pace)

        /**
          * linear replication guard w/ code
          */
        def apply[T](rate: Rate)(key: String)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                    (using %[F], /[F], \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          apply(rate)(key)(?, -, +, *).evalTap(_ => code)

        /**
          * linear replication guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                    (using %[F], /[F], \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          apply(rate, pace)(key)(?, -, +, *).evalTap(_ => code)

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
          continue <- Stream.eval(Ref[F].of(deferred))
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (new Object, None, rate))))
          cb_token <- Stream.eval(deferred.get)
          if cb_token ne None
          sr <- Stream.eval(SignallingRef[F].of(false))
          _  <- Stream.repeatEval {
            for
              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
              deferred <- continue.get
              cb_token <- deferred.get
              deferred <- Deferred[F, Option[<>[F]]]
              _        <- continue.set(deferred)
              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
              _        <- if cb_token eq None then sr.set(true)
                          else
                            val (cbarrier, _) = cb_token.get
                            enable[F](key) >> cbarrier.await
            yield
              ()
          }.interruptWhen(sr)
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
        apply(rate)(key).evalTap(_ => code)

      /**
        * replication guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: => F[T])
                  (using %[F], /[F], \[F])
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, Unit] =
        apply(rate, pace)(key).evalTap(_ => code)

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
        _        <- Stream.eval(/.offer(^ -> key -> (deferred -> null -> (new Object, None, rate))))
        cb_token <- Stream.eval(deferred.get)
        if cb_token ne None
        (cbarrier, _) = cb_token.get
        _        <- Stream.eval(enable[F](key) >> cbarrier.await)
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
      apply(rate)(key).evalTap(_ => code)

    /**
      * prefix w/ pace w/ code
      */
    def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: => F[T])
                (using %[F], /[F])
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): Stream[F, Unit] =
      apply(rate, pace)(key).evalTap(_ => code)

  /**
    * events, i.e., names (topics) and values
    */
  implicit final class `()`[F[_]: Async](private val name: Any) { self =>

    private inline def t = `()`[><[F]].topic
    private inline def q = `()`[><[F]].queue
    private inline def r = `()`[><[F]].limit
    private implicit def a: F[Unit] = q.take >> r.set(false)
    private def o =
      for
        b <- r.get
        s <- q.size
        _ <- if !b || s == 0 then q.offer(()) >> r.set(true) else Async[F].unit
      yield
        ()
    private def s = Stream.resource(t.subscribeAwaitUnbounded <* Resource.eval(o)).flatten

    extension (self: Stream[F, Unique.Token])
      private def `zipRight s` =
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
        this.t eq that.t
      catch
        case _ =>
          this.name == that.name

    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()`[F] = this

    lazy val `null` = new `()`[F](null)

    object ! :

      object + :

        object ν:

          /**
            * linear replication bound output guard
            */
          def apply(rate: Rate)(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                   (using % : %[F], / : /[F], \ : \[F])
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): Stream[F, `()`[F]] =
            for
              _        <- if None eq + then Stream.eval(exclude(key))
                          else Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
              deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
              continue <- Stream.eval(Ref[F].of(deferred))
              deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
              _        <- if None eq + then Stream.unit else Stream.eval(deferred.complete(None))
              _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (`()`[><[F]], Some(false), rate))))
              cb_token <- Stream.eval(deferred.get)
              _        <- if None eq + then Stream.eval(?.complete(cb_token eq None)) else Stream.unit
              _        <- Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
              sr <- Stream.eval(SignallingRef[F].of(false))
              it <- ( for
                        _  <- Stream.unit.repeat
                        it <- sΠ.ν[F]
                        it <- Stream.eval {
                          for
                            _        <- -.await
                            _        <- +.fold(Async[F].unit)(_.take)
                            _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                            deferred <- continue.get
                            cb_token <- deferred.get
                            deferred <- Deferred[F, Option[<>[F]]]
                            _        <- continue.set(deferred)
                            _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                            token    <- if cb_token eq None then sr.set(true).as(null)
                                        else
                                          val (cbarrier, token) = cb_token.get
                                          (enable[F](key) >> cbarrier.await).as(token)
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
          def apply(rate: Rate, pace: FiniteDuration)(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                   (using %[F], /[F], \[F])
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                             `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                             ^ : String): Stream[F, `()`[F]] =
            apply(rate)(key)(?, -, +, *).spaced(pace)

          /**
            * linear replication bound output guard w/ code
            */
          def apply[T](rate: Rate)(key: String)(code: F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                      (using %[F], /[F], \[F])
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): Stream[F, `()`[F]] =
            apply(rate)(key)(?, -, +, *).evalTap(_ => code)

          /**
            * linear replication bound output guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                      (using %[F], /[F], \[F])
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): Stream[F, `()`[F]] =
            apply(rate, pace)(key)(?, -, +, *).evalTap(_ => code)

        /**
          * linear constant replication output guard
          */
        def apply(rate: Rate, value: `()`[F])(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
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
            _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (`()`[><[F]], Some(false), rate))))
            cb_token <- Stream.eval(deferred.get)
            _        <- if None eq + then Stream.eval(?.complete(cb_token eq None)) else Stream.unit
            _        <- Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
            sr <- Stream.eval(SignallingRef[F].of(false))
            _  <- Stream.repeatEval {
              for
                _        <- -.await
                _        <- +.fold(Async[F].unit)(_.take)
                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                deferred <- continue.get
                cb_token <- deferred.get
                deferred <- Deferred[F, Option[<>[F]]]
                _        <- continue.set(deferred)
                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                token    <- if cb_token eq None then sr.set(true).as(null)
                            else
                              val (cbarrier, token) = cb_token.get
                              (enable[F](key) >> cbarrier.await).as(token)
              yield
                value -> token
            }.interruptWhen(sr).through1(t)
            _  <- Stream.eval(*.fold(Async[F].unit)(_.offer(())))
          yield
            ()

        /**
          * linear constant replication output guard w/ pace
          */
        def apply(rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                 (using %[F], /[F], \[F])
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, Unit] =
          apply(rate, value)(key)(?, -, +, *).spaced(pace)

        /**
          * linear constant replication output guard w/ code
          */
        def apply[T](rate: Rate, value: `()`[F])(key: String)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                    (using %[F], /[F], \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          apply(rate, value)(key)(?, -, +, *).evalTap(_ => code)

        /**
          * linear constant replication output guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                    (using %[F], /[F], \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          apply(rate, pace, value)(key)(?, -, +, *).evalTap(_ => code)

        object * :

          /**
            * linear variable replication output guard
            */
          def apply[S](rate: Rate, value: => S)(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                      (using %[F], /[F], \[F])
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): Stream[F, Unit] =
           apply[S](rate, Async[F].delay(value))(key)(?, -, +, *)

          /**
            * linear variable replication output guard w/ pace
            */
          def apply[S](rate: Rate, pace: FiniteDuration, value: => S)(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                      (using %[F], /[F], \[F])
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): Stream[F, Unit] =
           apply[S](rate, pace, Async[F].delay(value))(key)(?, -, +, *)

          /**
            * linear variable replication output guard w/ code
            */
          def apply[S, T](rate: Rate, value: => S)(key: String)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                         (using %[F], /[F], \[F])
                         (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                   `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                   ^ : String): Stream[F, Unit] =
           apply[S, T](rate, Async[F].delay(value))(key)(code)(?, -, +, *)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          def apply[S, T](rate: Rate, pace: FiniteDuration, value: => S)(key: String)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                         (using %[F], /[F], \[F])
                         (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                   `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                   ^ : String): Stream[F, Unit] =
           apply[S, T](rate, pace, Async[F].delay(value))(key)(code)(?, -, +, *)

          /**
            * linear variable replication output guard
            */
          @annotation.targetName("applyF")
          def apply[S](rate: Rate, value: => F[S])(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
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
              _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (`()`[><[F]], Some(false), rate))))
              cb_token <- Stream.eval(deferred.get)
              _        <- if None eq + then Stream.eval(?.complete(cb_token eq None)) else Stream.unit
              _        <- Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
              sr <- Stream.eval(SignallingRef[F].of(false))
              _  <- Stream.repeatEval {
                for
                  _        <- -.await
                  _        <- +.fold(Async[F].unit)(_.take)
                  _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                  deferred <- continue.get
                  cb_token <- deferred.get
                  deferred <- Deferred[F, Option[<>[F]]]
                  _        <- continue.set(deferred)
                  _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                  it       <- if cb_token eq None then sr.set(true).as(`null` -> null)
                              else
                                val (cbarrier, token) = cb_token.get
                                value.map(new `()`[F](_) -> token).flatTap(_ => enable[F](key) >> cbarrier.await)
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
          def apply[S](rate: Rate, pace: FiniteDuration, value: => F[S])(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                      (using %[F], /[F], \[F])
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                ^ : String): Stream[F, Unit] =
            apply[S](rate, value)(key)(?, -, +, *).spaced(pace)

          /**
            * linear variable replication output guard w/ code
            */
          @annotation.targetName("applyF")
          def apply[S, T](rate: Rate, value: => F[S])(key: String)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                         (using %[F], /[F], \[F])
                         (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                   `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                   ^ : String): Stream[F, Unit] =
            apply[S](rate, value)(key)(?, -, +, *).evalTap(_ => code)

          /**
            * linear variable replication output guard w/ pace w/ code
            */
          @annotation.targetName("applyF")
          def apply[S, T](rate: Rate, pace: FiniteDuration, value: => F[S])(key: String)(code: => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                         (using %[F], /[F], \[F])
                         (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                   `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                   ^ : String): Stream[F, Unit] =
            apply[S](rate, pace, value)(key)(?, -, +, *).evalTap(_ => code)

        /**
          * linear replication input guard
          */
        def apply(rate: Rate)(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                 (using % : %[F], / : /[F], \ : \[F])
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, `()`[F]] =
          for
            _        <- if None eq + then Stream.eval(exclude(key))
                        else Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            continue <- Stream.eval(Ref[F].of(deferred))
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            _        <- if None eq + then Stream.unit else Stream.eval(deferred.complete(None))
            _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (`()`[><[F]], Some(true), rate))))
            cb_token <- Stream.eval(deferred.get)
            _        <- if None eq + then Stream.eval(?.complete(cb_token eq None)) else Stream.unit
            _        <- Stream.eval(?.get).ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
            sr <- Stream.eval(SignallingRef[F].of(false))
            it <- Stream.repeatEval {
              for
                _        <- -.await
                _        <- +.fold(Async[F].unit)(_.take)
                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                deferred <- continue.get
                cb_token <- deferred.get
                deferred <- Deferred[F, Option[<>[F]]]
                _        <- continue.set(deferred)
                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                token    <- if cb_token eq None then sr.set(true).as(null)
                            else
                              val (cbarrier, token) = cb_token.get
                              (enable[F](key) >> cbarrier.await).as(token)
              yield
                token
            }.interruptWhen(sr).`zipRight s`
            _  <- Stream.eval(*.fold(Async[F].unit)(_.offer(())))
          yield
            it

        /**
          * linear replication input guard w/ pace
          */
        def apply(rate: Rate, pace: FiniteDuration)(key: String)(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                 (using %[F], /[F], \[F])
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                           `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String): Stream[F, `()`[F]] =
          apply(rate)(key)(?, -, +, *).spaced(pace)

        /**
          * linear replication input guard w/ code
          */
        def apply[T](rate: Rate)(key: String)(code: T => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                    (using %[F], /[F], \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, `()`[F]] =
          apply(rate)(key)(?, -, +, *).evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) }

        /**
          * linear replication input guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: T => F[T])(? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Option[Queue[F, Unit]], * : Option[Queue[F, Unit]])
                    (using %[F], /[F], \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, `()`[F]] =
          apply(rate, pace)(key)(?, -, +, *).evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) }

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
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            continue <- Stream.eval(Ref[F].of(deferred))
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (`()`[><[F]], Some(false), rate))))
            cb_token <- Stream.eval(deferred.get)
            if cb_token ne None
            sr <- Stream.eval(SignallingRef[F].of(false))
            it <- ( for
                      _  <- Stream.unit.repeat
                      it <- sΠ.ν[F]
                      it <- Stream.eval {
                        for
                          _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                          deferred <- continue.get
                          cb_token <- deferred.get
                          deferred <- Deferred[F, Option[<>[F]]]
                          _        <- continue.set(deferred)
                          _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                          token    <- if cb_token eq None then sr.set(true).as(null)
                                      else
                                        val (cbarrier, token) = cb_token.get
                                        (enable[F](key) >> cbarrier.await).as(token)
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
          apply(rate)(key).evalTap(_ => code)

        /**
          * replication bound output guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: => F[T])
                    (using %[F], /[F], \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, `()`[F]] =
          apply(rate, pace)(key).evalTap(_ => code)

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
          continue <- Stream.eval(Ref[F].of(deferred))
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (`()`[><[F]], Some(false), rate))))
          cb_token <- Stream.eval(deferred.get)
          if cb_token ne None
          sr <- Stream.eval(SignallingRef[F].of(false))
          _  <- Stream.repeatEval {
            for
              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
              deferred <- continue.get
              cb_token <- deferred.get
              deferred <- Deferred[F, Option[<>[F]]]
              _        <- continue.set(deferred)
              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
              token    <- if cb_token eq None then sr.set(true).as(null)
                          else
                            val (cbarrier, token) = cb_token.get
                            (enable[F](key) >> cbarrier.await).as(token)
            yield
              value -> token
          }.interruptWhen(sr).through1(t)
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
        apply(rate, value)(key).evalTap(_ => code)

      /**
        * constant replication output guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String)(code: => F[T])
               (using %[F], /[F], \[F])
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String): Stream[F, Unit] =
        apply(rate, pace, value)(key).evalTap(_ => code)

      object * :

        /**
          * variable replication output guard
          */
        def apply[S](rate: Rate, value: => S)(key: String)
                    (using %[F], /[F], \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          apply[S](rate, Async[F].delay(value))(key)

        /**
          * variable replication output guard w/ pace
          */
        def apply[S](rate: Rate, pace: FiniteDuration, value: => S)(key: String)
                    (using %[F], /[F], \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          apply[S](rate, pace, Async[F].delay(value))(key)

        /**
          * variable replication output guard w/ code
          */
        def apply[S, T](rate: Rate, value: => S)(key: String)(code: => F[T])
                       (using %[F], /[F], \[F])
                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                 ^ : String): Stream[F, Unit] =
          apply[S, T](rate, Async[F].delay(value))(key)(code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S, T](rate: Rate, pace: FiniteDuration, value: => S)(key: String)(code: => F[T])
                       (using %[F], /[F], \[F])
                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                 ^ : String): Stream[F, Unit] =
          apply[S, T](rate, pace, Async[F].delay(value))(key)(code)

        /**
          * variable replication output guard
          */
        @annotation.targetName("applyF")
        def apply[S](rate: Rate, value: => F[S])(key: String)
                    (using % : %[F], / : /[F], \ : \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          for
            _        <- Stream.eval(exclude(key))
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            continue <- Stream.eval(Ref[F].of(deferred))
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (`()`[><[F]], Some(false), rate))))
            cb_token <- Stream.eval(deferred.get)
            if cb_token ne None
            sr <- Stream.eval(SignallingRef[F].of(false))
            _  <- Stream.repeatEval {
              for
                _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
                deferred <- continue.get
                cb_token <- deferred.get
                deferred <- Deferred[F, Option[<>[F]]]
                _        <- continue.set(deferred)
                _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
                it       <- if cb_token eq None then sr.set(true).as(`null` -> null)
                            else
                              val (cbarrier, token) = cb_token.get
                              value.map(new `()`[F](_) -> token).flatTap(_ => enable[F](key) >> cbarrier.await)
              yield
                it
            }.interruptWhen(sr).through1(t)
          yield
            ()

        /**
          * variable replication output guard w/ pace
          */
        @annotation.targetName("applyF")
        def apply[S](rate: Rate, pace: FiniteDuration, value: => F[S])(key: String)
                    (using %[F], /[F], \[F])
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                              `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String): Stream[F, Unit] =
          apply[S](rate, value)(key).spaced(pace)

        /**
          * variable replication output guard w/ code
          */
        @annotation.targetName("applyF")
        def apply[S, T](rate: Rate, value: => F[S])(key: String)(code: => F[T])
                       (using %[F], /[F], \[F])
                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                 ^ : String): Stream[F, Unit] =
          apply[S](rate, value)(key).evalTap(_ => code)

        /**
          * variable replication output guard w/ pace w/ code
          */
        @annotation.targetName("applyF")
        def apply[S, T](rate: Rate, pace: FiniteDuration, value: => F[S])(key: String)(code: => F[T])
                       (using %[F], /[F], \[F])
                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                                 `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                 ^ : String): Stream[F, Unit] =
          apply[S](rate, pace, value)(key).evalTap(_ => code)

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
          continue <- Stream.eval(Ref[F].of(deferred))
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> continue -> (`()`[><[F]], Some(true), rate))))
          cb_token <- Stream.eval(deferred.get)
          if cb_token ne None
          sr <- Stream.eval(SignallingRef[F].of(false))
          it <- Stream.repeatEval {
            for
              _        <- %.update { m => m + (^ + key -> (true, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) } >> \()
              deferred <- continue.get
              cb_token <- deferred.get
              deferred <- Deferred[F, Option[<>[F]]]
              _        <- continue.set(deferred)
              _        <- %.update { m => m + (^ + key -> (false, m(^ + key).asInstanceOf[(Boolean, +[F])]._2)) }
              token    <- if cb_token eq None then sr.set(true).as(null)
                          else
                            val (cbarrier, token) = cb_token.get
                            (enable[F](key) >> cbarrier.await).as(token)
            yield
              token
          }.interruptWhen(sr).`zipRight s`
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
        apply(rate)(key).evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) }

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: T => F[T])
                  (using %[F], /[F], \[F])
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, `()`[F]] =
        apply(rate, pace)(key).evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) }

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
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> null -> (`()`[><[F]], Some(false), rate))))
          cb_token <- Stream.eval(deferred.get)
          if cb_token ne None
          (cbarrier, token) = cb_token.get
          it <- sΠ.ν[F]
          _  <- Stream.emit(it -> token).evalTap(_ => enable[F](key) >> cbarrier.await).through1(t)
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
        apply(rate)(key).evalTap(_ => code)

      /**
        * bound output prefix w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: => F[T])
                  (using %[F], /[F])
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, `()`[F]] =
        apply(rate, pace)(key).evalTap(_ => code)

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
        _        <- Stream.eval(/.offer(^ -> key -> (deferred -> null -> (`()`[><[F]], Some(false), rate))))
        cb_token <- Stream.eval(deferred.get)
        if cb_token ne None
        (cbarrier, token) = cb_token.get
        _        <- Stream.emit(value -> token).evalTap(_ => enable[F](key) >> cbarrier.await).through1(t)
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
      apply(rate, value)(key).evalTap(_ => code)

    /**
      * constant output prefix w/ pace w/ code
      */
    def apply[T](rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String)(code: => F[T])
                (using %[F], /[F])
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): Stream[F, Unit] =
      apply(rate, pace, value)(key).evalTap(_ => code)

    object * :

      /**
        * variable output prefix
        */
      def apply[S](rate: Rate, value: => S)(key: String)
                  (using %[F], /[F])
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, Unit] =
        apply[S](rate, Async[F].delay(value))(key)

      /**
        * variable output prefix w/ pace
        */
      def apply[S](rate: Rate, pace: FiniteDuration, value: => S)(key: String)
                  (using %[F], /[F])
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, Unit] =
        apply[S](rate, value)(key) <* Stream.sleep(pace)

      /**
        * variable output prefix w/ code
        */
      def apply[S, T](rate: Rate, value: => S)(key: String)(code: => F[T])
                     (using %[F], /[F])
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                               ^ : String): Stream[F, Unit] =
        apply[S](rate, value)(key).evalTap(_ => code)

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S, T](rate: Rate, pace: FiniteDuration, value: => S)(key: String)(code: => F[T])
                     (using %[F], /[F])
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                               ^ : String): Stream[F, Unit] =
        apply[S](rate, pace, value)(key).evalTap(_ => code)

      /**
        * variable output prefix
        */
      @annotation.targetName("applyF")
      def apply[S](rate: Rate, value: => F[S])(key: String)
                  (using % : %[F], / : /[F])
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, Unit] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          _        <- Stream.eval(/.offer(^ -> key -> (deferred -> null -> (`()`[><[F]], Some(false), rate))))
          cb_token <- Stream.eval(deferred.get)
          if cb_token ne None
          (cbarrier, token) = cb_token.get
          _        <- Stream.eval(value).map(new `()`[F](_) -> token).evalTap(_ => enable[F](key) >> cbarrier.await).through1(t)
        yield
          ()

      /**
        * variable output prefix w/ pace
        */
      @annotation.targetName("applyF")
      def apply[S](rate: Rate, pace: FiniteDuration, value: => F[S])(key: String)
                  (using %[F], /[F])
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                            ^ : String): Stream[F, Unit] =
        apply[S](rate, value)(key) <* Stream.sleep(pace)

      /**
        * variable output prefix w/ code
        */
      @annotation.targetName("applyF")
      def apply[S, T](rate: Rate, value: => F[S])(key: String)(code: => F[T])
                     (using %[F], /[F])
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                               ^ : String): Stream[F, Unit] =
        apply[S](rate, value)(key).evalTap(_ => code)

      /**
        * variable output prefix w/ pace w/ code
        */
      @annotation.targetName("applyF")
      def apply[S, T](rate: Rate, pace: FiniteDuration, value: => F[S])(key: String)(code: => F[T])
                     (using %[F], /[F])
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                               `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                               ^ : String): Stream[F, Unit] =
        apply[S](rate, pace, value)(key).evalTap(_ => code)

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
        _        <- Stream.eval(/.offer(^ -> key -> (deferred -> null -> (`()`[><[F]], Some(true), rate))))
        cb_token <- Stream.eval(deferred.get)
        if cb_token ne None
        (cbarrier, token) = cb_token.get
        it <- Stream.eval(enable[F](key) >> cbarrier.await).as(token).`zipRight s`.head
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
      apply(rate)(key).evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) }

    /**
      * input prefix w/ pace w/ code
      */
    def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: T => F[T])
                (using %[F], /[F])
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): Stream[F, `()`[F]] =
      apply(rate, pace)(key).evalMap { it => code(it.`()`[T]).map(new `()`[F](_)) }

    override def toString: String = if name == null then "null" else name.toString

  }


  private object `Π-magic`:

    case class ><[F[_]](topic: Topic[F, (`()`[F], Unique.Token)],
                        queue: Queue[F, Unit],
                        limit: Ref[F, Boolean])

    extension [F[_]: Async, O](self: Stream[F, O])
      def through1(topic: Topic[F, O])
                  (using await: F[Unit]): Stream[F, O] =
        self.evalMap { it => await >> topic.publish1(it).map(it -> _) }.takeWhile(_._2.isRight).map(_._1)
