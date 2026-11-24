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
  import _root_.cats.effect.Outcome
  import _root_.cats.effect.std.{ CyclicBarrier, Semaphore, Supervisor }

  import _root_.fs2.Stream

  import `Π-loop`.{ <>, +, %, /, \ }
  import `Π-stats`.Rate


  type `Π-Map`[K, +V] = Map[K, V]

  type `Π-Set`[A] = Set[A]

  type `Π-Function0`[F[_]] = () => String ?=> Stream[F, Unit]
  type `Π-Function1`[F[_]] = `()`[F] => String ?=> Stream[F, Unit]

  /**
    * Type of ether.
    */
  type `{}`[F[_]] = Option[Semaphore[F]]

  /**
    * Type of causal sets.
    */
  type `[]` = Set[Long]

  /**
    * Type for [[cats.effect.IOLocal]], [[zio.FiberRef]], etc.
    */
  open case class `Π-FiberLocal`[F[_], A](get: F[A], set: A => F[Unit])


  /**
    * Supervised [[code]].
    * @param code
    */
  private def exec[F[_]: Async, T](code: => F[T]): F[T] =
    Supervisor[F](await = true)
      .use(_.supervise(code).flatMap(_.join))
      .flatMap {
        case Outcome.Succeeded(it) => it
        case _                     => Async[F].pure(null.asInstanceOf[T])
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
    def flatMap[B](f: `()`[F] => Stream[F, B]): Stream[F, B] = Stream.eval(Semaphore[F](1)).map(Some(_)).flatMap(f(_))


  /**
    * silent transition
    */
  final class τ[F[_]: Async]:

    object `(!)`:

      object `(+)`:

        private def apply(rate: Rate)(key: String)(sleep: F[Unit])(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                         (using % : %[F], / : /[F], \ : \[F])
                         (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                   ^ : String, `[]`: `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
          for
            _        <- if ! then Stream.eval(exclude(key))
                        else Stream.eval(?.get)
                                   .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
            _        <- if ! then Stream.unit
                        else Stream.eval(deferred.complete(None))
            enabled  <- Stream.eval(deferred.tryGet.map(_ eq None) >>= Ref[F].of)
            `][`     <- Stream.eval(`[]`.get)
            timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
            _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, timestamp), (τ.`None {}`, None, rate, `][`))))
            cb_fb_in <- Stream.eval(deferred.get)
            _        <- if ! then Stream.eval(?.complete(cb_fb_in eq None) >> ?.get)
                                        .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                        else Stream.unit
            timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
            _  <- Stream.repeatEval {
              for
                _        <- enabled.get >>= +.acquire.unlessA
                _        <- enabled.get >>= timeset.unlessA
                _        <- enabled.get >>= \{
                  *.get.flatMap { s =>
                    %.update { m =>
                      val it = m(^ + key).asInstanceOf[(Boolean, +[F])]._2
                      if s eq null
                      then
                        m + (^ + key -> (true, it))
                      else
                        m + (^ + key -> (true, it.copy(_2 = it._2.copy(_4 = s))))
                    }
                  }
                }.unlessA
                cb_fb_in <- continue.get.flatMap(_.get)
                _        <- Deferred[F, Option[<>[F]]] >>= continue.set
                _        <- enabled.set(false)
                it       <- if cb_fb_in eq None then +.release >> -.await >> Async[F].pure(None)
                            else
                              val (cbarrier, fiber, _, s) = cb_fb_in.get
                              (cbarrier.await >> fiber.join).as(Some(()))
                              (cbarrier.await >> fiber.join >> *.set(s).unlessA(s.isEmpty) >> `[]`.set(s)).as(Some(()))
              yield
                it
            }.takeWhile(_.isDefined)
            _  <- Stream.eval(sleep)
            _  <- Stream.eval(+.release)
          yield
            ()

        /**
          * modulo replication guard
          */
        def apply(rate: Rate)(key: String)(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                 (using %[F], /[F], \[F])
                 (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
          this(rate)(key)(Async[F].unit)(!, ?, -, +, *)

        /**
          * modulo replication guard w/ pace
          */
        def apply(rate: Rate, pace: FiniteDuration)(key: String)(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                 (using %[F], /[F], \[F])
                 (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
          this(rate)(key)(Async[F].sleep(pace))(!, ?, -, +, *)

        /**
          * modulo replication guard w/ code
          */
        def apply[T](rate: Rate)(key: String)(code: => F[T])(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                    (using %[F], /[F], \[F])
                    (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
          this(rate)(key)(exec(code).void)(!, ?, -, +, *)

        /**
          * modulo replication guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: => F[T])(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                    (using %[F], /[F], \[F])
                    (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
          this(rate)(key)(exec(code) >> Async[F].sleep(pace))(!, ?, -, +, *)

      /**
        * replication guard
        */
      def apply(rate: Rate)(key: String)
               (using % : %[F], / : /[F], \ : \[F])
               (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String, `[]`: `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
          enabled  <- Stream.eval(Ref[F].of(true))
          `][`     <- Stream.eval(`[]`.get)
          timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
          _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, timestamp), (τ.`None {}`, None, rate, `][`))))
          cb_fb_in <- Stream.eval(deferred.get)
          if cb_fb_in ne None
          timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
          *  <- Stream.eval(Ref[F].of(null: `[]`))
          _  <- Stream.repeatEval {
            for
              _        <- enabled.get >>= timeset.unlessA
              _        <- enabled.get >>= \{
                *.get.flatMap { s =>
                  %.update { m =>
                    val it = m(^ + key).asInstanceOf[(Boolean, +[F])]._2
                    if s eq null
                    then
                      m + (^ + key -> (true, it))
                    else
                      m + (^ + key -> (true, it.copy(_2 = it._2.copy(_4 = s))))
                  }
                }
              }.unlessA
              cb_fb_in <- continue.get.flatMap(_.get)
              _        <- Deferred[F, Option[<>[F]]] >>= continue.set
              _        <- enabled.set(false)
              it       <- if cb_fb_in eq None then Async[F].pure(None)
                          else
                            val (cbarrier, fiber, _, s) = cb_fb_in.get
                            (cbarrier.await >> fiber.join >> *.set(s).unlessA(s.isEmpty) >> `[]`.set(s)).as(Some(()))
            yield
              it
          }.takeWhile(_.isDefined)
        yield
          ()

      /**
        * replication guard w/ pace
        */
      def apply(rate: Rate, pace: FiniteDuration)(key: String)
               (using %[F], /[F], \[F])
               (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
        apply(rate)(key) <* Stream.sleep(pace)

      /**
        * replication guard w/ code
        */
      def apply[T](rate: Rate)(key: String)(code: => F[T])
                  (using %[F], /[F], \[F])
                  (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
        apply(rate)(key).evalTap(_ => exec(code))

      /**
        * replication guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: => F[T])
                  (using %[F], /[F], \[F])
                  (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
        apply[T](rate)(key)(code) <* Stream.sleep(pace)

    /**
      * prefix
      */
    def apply(rate: Rate)(key: String)
             (using % : %[F], / : /[F])
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String, `[]`: `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
      for
        _        <- Stream.eval(exclude(key))
        deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
        `][`     <- Stream.eval(`[]`.get)
        timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
        _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> null, timestamp), (τ.`None {}`, None, rate, `][`))))
        cb_fb_in <- Stream.eval(deferred.get)
        if cb_fb_in ne None
        (cbarrier, fiber, _, s) = cb_fb_in.get
        _  <- Stream.eval(cbarrier.await >> fiber.join >> `[]`.set(s))
      yield
        ()

    /**
      * prefix w/ pace
      */
    def apply(rate: Rate, pace: FiniteDuration)(key: String)
             (using %[F], /[F])
             (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
      apply(rate)(key) <* Stream.sleep(pace)

    /**
      * prefix w/ code
      */
    def apply[T](rate: Rate)(key: String)(code: => F[T])
                (using %[F], /[F])
                (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
      apply(rate)(key).evalTap(_ => exec(code))

    /**
      * prefix w/ pace w/ code
      */
    def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: => F[T])
                (using %[F], /[F])
                (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
      apply[T](rate)(key)(code) <* Stream.sleep(pace)


  object τ:

    private def `None {}`[F[_]]: `{}`[F] = None


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
            * bound output
            */
          private def output(rate: Rate)(key: String)(sleep: F[Unit])(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                            (using % : %[F], / : /[F], \ : \[F])
                            (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                      ^ : String, `[]`: `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
            for
              _        <- if ! then Stream.eval(exclude(key))
                          else Stream.eval(?.get)
                                     .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
              deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
              continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
              _        <- if ! then Stream.unit
                          else Stream.eval(deferred.complete(None))
              enabled  <- Stream.eval(deferred.tryGet.map(_ eq None) >>= Ref[F].of)
              `][`     <- Stream.eval(`[]`.get)
              timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
              _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, timestamp), (`()`[`{}`[F]], Some(Left(())), rate, `][`))))
              cb_fb_in <- Stream.eval(deferred.get)
              _        <- if ! then Stream.eval(?.complete(cb_fb_in eq None) >> ?.get)
                                          .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                          else Stream.unit
              timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
              it <- Stream.repeatEval {
                for
                  _        <- enabled.get >>= +.acquire.unlessA
                  _        <- enabled.get >>= timeset.unlessA
                  _        <- enabled.get >>= \{
                    *.get.flatMap { s =>
                      %.update { m =>
                        val it = m(^ + key).asInstanceOf[(Boolean, +[F])]._2
                        if s eq null
                        then
                          m + (^ + key -> (true, it))
                        else
                          m + (^ + key -> (true, it.copy(_2 = it._2.copy(_4 = s))))
                      }
                    }
                  }.unlessA
                  cb_fb_in <- continue.get.flatMap(_.get)
                  _        <- Deferred[F, Option[<>[F]]] >>= continue.set
                  _        <- enabled.set(false)
                  eth      <- Semaphore[F](1).map(Some(_))
                  it       <- if cb_fb_in eq None then +.release >> -.await >> Async[F].pure(None)
                              else
                                val (cbarrier, fiber, input, s) = cb_fb_in.get
                                (input.set(eth) >> cbarrier.await >> fiber.join >> *.set(s).unlessA(s.isEmpty) >> `[]`.set(s)).as(Some(eth))
                yield
                  it
              }.takeWhile(_.isDefined).map(_.get)
              _  <- Stream.eval(sleep)
              _  <- Stream.eval(+.release)
            yield
              it

          /**
            * modulo replication bound output guard
            */
          def apply(rate: Rate)(key: String)(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                   (using %[F], /[F], \[F])
                   (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
            output(rate)(key)(Async[F].unit)(!, ?, -, +, *)

          /**
            * modulo replication bound output guard w/ pace
            */
          def apply(rate: Rate, pace: FiniteDuration)(key: String)(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                   (using %[F], /[F], \[F])
                   (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
            output(rate)(key)(Async[F].sleep(pace))(!, ?, -, +, *)

          /**
            * modulo replication bound output guard w/ code
            */
          def apply[T](rate: Rate)(key: String)(code: => F[T])(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                   (using %[F], /[F], \[F])
                   (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
            output(rate)(key)(exec(code).void)(!, ?, -, +, *)

          /**
            * modulo replication bound output guard w/ pace w/ code
            */
          def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: => F[T])(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                   (using %[F], /[F], \[F])
                   (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
            output(rate)(key)(exec(code) >> Async[F].sleep(pace))(!, ?, -, +, *)

        /**
          * constant output
          */
        private def output(rate: Rate, value: `()`[F])(key: String)(sleep: F[Unit])(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                          (using % : %[F], / : /[F], \ : \[F])
                          (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                    ^ : String, `[]`: `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
          for
            _        <- if ! then Stream.eval(exclude(key))
                        else Stream.eval(?.get)
                                   .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
            _        <- if ! then Stream.unit
                        else Stream.eval(deferred.complete(None))
            enabled  <- Stream.eval(deferred.tryGet.map(_ eq None) >>= Ref[F].of)
            `][`     <- Stream.eval(`[]`.get)
            timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
            _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, timestamp), (`()`[`{}`[F]], Some(Left(())), rate, `][`))))
            cb_fb_in <- Stream.eval(deferred.get)
            _        <- if ! then Stream.eval(?.complete(cb_fb_in eq None) >> ?.get)
                                        .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                        else Stream.unit
            timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
            _  <- Stream.repeatEval {
              for
                _        <- enabled.get >>= +.acquire.unlessA
                _        <- enabled.get >>= timeset.unlessA
                _        <- enabled.get >>= \{
                  *.get.flatMap { s =>
                    %.update { m =>
                      val it = m(^ + key).asInstanceOf[(Boolean, +[F])]._2
                      if s eq null
                      then
                        m + (^ + key -> (true, it))
                      else
                        m + (^ + key -> (true, it.copy(_2 = it._2.copy(_4 = s))))
                    }
                  }
                }.unlessA
                cb_fb_in <- continue.get.flatMap(_.get)
                _        <- Deferred[F, Option[<>[F]]] >>= continue.set
                _        <- enabled.set(false)
                it       <- if cb_fb_in eq None then +.release >> -.await >> Async[F].pure(None)
                            else
                              val (cbarrier, fiber, input, s) = cb_fb_in.get
                              (input.set(value) >> cbarrier.await >> fiber.join >> *.set(s).unlessA(s.isEmpty) >> `[]`.set(s)).as(Some(()))
              yield
                it
            }.takeWhile(_.isDefined)
            _  <- Stream.eval(sleep)
            _  <- Stream.eval(+.release)
          yield
            ()

        /**
          * modulo constant replication output guard
          */
        def apply(rate: Rate, value: `()`[F])(key: String)(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                 (using %[F], /[F], \[F])
                 (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
          output(rate, value)(key)(Async[F].unit)(!, ?, -, +, *)

        /**
          * modulo constant replication output guard w/ pace
          */
        def apply(rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String)(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                 (using %[F], /[F], \[F])
                 (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
          output(rate, value)(key)(Async[F].sleep(pace))(!, ?, -, +, *)

        /**
          * modulo constant replication output guard w/ code
          */
        def apply[T](rate: Rate, value: `()`[F])(key: String)(code: => F[T])(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                    (using %[F], /[F], \[F])
                    (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
          output(rate, value)(key)(exec(code).void)(!, ?, -, +, *)

        /**
          * modulo constant replication output guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String)(code: => F[T])(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                    (using %[F], /[F], \[F])
                    (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
          output(rate, value)(key)(exec(code) >> Async[F].sleep(pace))(!, ?, -, +, *)

        object `(*)`:

          /**
            * modulo variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(rate: Rate, value: => S)(key: String)(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                                       (using DummyImplicit)
                                       (using %[F], /[F], \[F])
                                       (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(rate, value.asInstanceOf[`()`[F]])(key)(!, ?, -, +, *)
            else
              apply[S](1)(rate, Async[F].delay(value))(key)(!, ?, -, +, *)

          /**
            * modulo variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(rate: Rate, pace: FiniteDuration, value: => S)(key: String)(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                                       (using DummyImplicit)
                                       (using %[F], /[F], \[F])
                                       (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(rate, pace, value.asInstanceOf[`()`[F]])(key)(!, ?, -, +, *)
            else
              apply[S](2)(rate, pace, Async[F].delay(value))(key)(!, ?, -, +, *)

          /**
            * modulo variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => S)(key: String)(code: => F[T])(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                                          (using DummyImplicit)
                                          (using %[F], /[F], \[F])
                                          (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(rate, value.asInstanceOf[`()`[F]])(key)(code)(!, ?, -, +, *)
            else
              apply[S, T](3)(rate, Async[F].delay(value))(key)(code)(!, ?, -, +, *)

          /**
            * modulo variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: FiniteDuration, value: => S)(key: String)(code: => F[T])(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                                          (using DummyImplicit)
                                          (using %[F], /[F], \[F])
                                          (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              self.`(!)`.`(+)`(rate, pace, value.asInstanceOf[`()`[F]])(key)(code)(!, ?, -, +, *)
            else
              apply[S, T](4)(rate, pace, Async[F].delay(value))(key)(code)(!, ?, -, +, *)

          /**
            * variable output
            */
          private def output[S](rate: Rate, value: => F[S])(key: String)(sleep: F[Unit])(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                               (using % : %[F], / : /[F], \ : \[F])
                               (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                         ^ : String, `[]`: `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
            for
              _        <- if ! then Stream.eval(exclude(key))
                          else Stream.eval(?.get)
                                     .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
              deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
              continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
              _        <- if ! then Stream.unit
                          else Stream.eval(deferred.complete(None))
              enabled  <- Stream.eval(deferred.tryGet.map(_ eq None) >>= Ref[F].of)
              `][`     <- Stream.eval(`[]`.get)
              timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
              _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, timestamp), (`()`[`{}`[F]], Some(Left(())), rate, `][`))))
              cb_fb_in <- Stream.eval(deferred.get)
              _        <- if ! then Stream.eval(?.complete(cb_fb_in eq None) >> ?.get)
                                          .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                          else Stream.unit
              timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
              _  <- Stream.repeatEval {
                for
                  _        <- enabled.get >>= +.acquire.unlessA
                  _        <- enabled.get >>= timeset.unlessA
                  _        <- enabled.get >>= \{
                    *.get.flatMap { s =>
                      %.update { m =>
                        val it = m(^ + key).asInstanceOf[(Boolean, +[F])]._2
                        if s eq null
                        then
                          m + (^ + key -> (true, it))
                        else
                          m + (^ + key -> (true, it.copy(_2 = it._2.copy(_4 = s))))
                      }
                    }
                  }.unlessA
                  cb_fb_in <- continue.get.flatMap(_.get)
                  _        <- Deferred[F, Option[<>[F]]] >>= continue.set
                  _        <- enabled.set(false)
                  it       <- if cb_fb_in eq None then +.release >> -.await >> Async[F].pure(None)
                              else
                                val (cbarrier, fiber, input, s) = cb_fb_in.get
                                value.map(new `()`[F](_)).flatMap(input.set(_) >> cbarrier.await >> fiber.join >> *.set(s).unlessA(s.isEmpty) >> `[]`.set(s)).as(Some(()))
                yield
                  it
              }.takeWhile(_.isDefined)
              _  <- Stream.eval(sleep)
              _  <- Stream.eval(+.release)
            yield
              ()

          /**
            * modulo variable replication output guard
            */
          def apply[S: ClassTag](_1: 1)(rate: Rate, value: => F[S])(key: String)(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                                       (using %[F], /[F], \[F])
                                       (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              Stream.eval(Async[F].defer(value.asInstanceOf[F[`()`[F]]])).flatMap(self.`(!)`.`(+)`(rate, _)(key)(!, ?, -, +, *))
            else
              output[S](rate, value)(key)(Async[F].unit)(!, ?, -, +, *)

          /**
            * modulo variable replication output guard w/ pace
            */
          def apply[S: ClassTag](_2: 2)(rate: Rate, pace: FiniteDuration, value: => F[S])(key: String)(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                                       (using %[F], /[F], \[F])
                                       (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              Stream.eval(Async[F].defer(value.asInstanceOf[F[`()`[F]]])).flatMap(self.`(!)`.`(+)`(rate, pace, _)(key)(!, ?, -, +, *))
            else
              output[S](rate, value)(key)(Async[F].sleep(pace))(!, ?, -, +, *)

          /**
            * modulo variable replication output guard w/ code
            */
          def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => F[S])(key: String)(code: => F[T])(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                                          (using %[F], /[F], \[F])
                                          (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              Stream.eval(Async[F].defer(value.asInstanceOf[F[`()`[F]]])).flatMap(self.`(!)`.`(+)`(rate, _)(key)(code)(!, ?, -, +, *))
            else
              output[S](rate, value)(key)(exec(code).void)(!, ?, -, +, *)

          /**
            * modulo variable replication output guard w/ pace w/ code
            */
          def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: FiniteDuration, value: => F[S])(key: String)(code: => F[T])(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                                          (using %[F], /[F], \[F])
                                          (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
            if classTag[S].runtimeClass eq self.getClass
            then
              Stream.eval(Async[F].defer(value.asInstanceOf[F[`()`[F]]])).flatMap(self.`(!)`.`(+)`(rate, pace, _)(key)(code)(!, ?, -, +, *))
            else
              output[S](rate, value)(key)(exec(code) >> Async[F].sleep(pace))(!, ?, -, +, *)

        /**
          * input
          */
        def input(rate: Rate)(key: String)(sleep: F[Unit])(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                 (using % : %[F], / : /[F], \ : \[F])
                 (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String, `[]`: `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
          for
            _        <- if ! then Stream.eval(exclude(key))
                        else Stream.eval(?.get)
                                   .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
            _        <- if ! then Stream.unit
                        else Stream.eval(deferred.complete(None))
            enabled  <- Stream.eval(deferred.tryGet.map(_ eq None) >>= Ref[F].of)
            result   <- Stream.eval(Ref[F].of[`()`[F]](null))
            `][`     <- Stream.eval(`[]`.get)
            timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
            _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, timestamp), (`()`[`{}`[F]], Some(Right(result)), rate, `][`))))
            cb_fb_in <- Stream.eval(deferred.get)
            _        <- if ! then Stream.eval(?.complete(cb_fb_in eq None) >> ?.get)
                                        .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                        else Stream.unit
            timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
            _  <- Stream.repeatEval {
              for
                _        <- enabled.get >>= +.acquire.unlessA
                _        <- enabled.get >>= timeset.unlessA
                _        <- enabled.get >>= \{
                  *.get.flatMap { s =>
                    %.update { m =>
                      val it = m(^ + key).asInstanceOf[(Boolean, +[F])]._2
                      if s eq null
                      then
                        m + (^ + key -> (true, it))
                      else
                        m + (^ + key -> (true, it.copy(_2 = it._2.copy(_4 = s))))
                    }
                  }
                }.unlessA
                cb_fb_in <- continue.get.flatMap(_.get)
                _        <- Deferred[F, Option[<>[F]]] >>= continue.set
                _        <- enabled.set(false)
                it       <- if cb_fb_in eq None then +.release >> -.await >> Async[F].pure(None)
                            else
                              val (cbarrier, fiber, _, s) = cb_fb_in.get
                              (cbarrier.await >> fiber.join >> *.set(s).unlessA(s.isEmpty) >> `[]`.set(s)).as(Some(()))
              yield
                it
            }.takeWhile(_.isDefined)
            it <- Stream.eval(result.get)
            _  <- Stream.eval(sleep)
            _  <- Stream.eval(+.release)
          yield
            it

        /**
          * input w/ code
          */
        def input[T](rate: Rate)(key: String)(sleep: F[Unit])(code: T => F[T])(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                    (using % : %[F], / : /[F], \ : \[F])
                    (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                              ^ : String, `[]`: `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
          for
            _        <- if ! then Stream.eval(exclude(key))
                        else Stream.eval(?.get)
                                   .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
            _        <- if ! then Stream.unit
                        else Stream.eval(deferred.complete(None))
            enabled  <- Stream.eval(deferred.tryGet.map(_ eq None) >>= Ref[F].of)
            result   <- Stream.eval(Ref[F].of[`()`[F]](null))
            `][`     <- Stream.eval(`[]`.get)
            timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
            _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, timestamp), (`()`[`{}`[F]], Some(Right(result)), rate, `][`))))
            cb_fb_in <- Stream.eval(deferred.get)
            _        <- if ! then Stream.eval(?.complete(cb_fb_in eq None) >> ?.get)
                                        .ifM(Stream.eval(-.await) >> Stream.empty, Stream.unit)
                        else Stream.unit
            timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
            _  <- Stream.repeatEval {
              for
                _        <- enabled.get >>= +.acquire.unlessA
                _        <- enabled.get >>= timeset.unlessA
                _        <- enabled.get >>= \{
                  *.get.flatMap { s =>
                    %.update { m =>
                      val it = m(^ + key).asInstanceOf[(Boolean, +[F])]._2
                      if s eq null
                      then
                        m + (^ + key -> (true, it))
                      else
                        m + (^ + key -> (true, it.copy(_2 = it._2.copy(_4 = s))))
                    }
                  }
                }.unlessA
                cb_fb_in <- continue.get.flatMap(_.get)
                _        <- Deferred[F, Option[<>[F]]] >>= continue.set
                _        <- enabled.set(false)
                it       <- if cb_fb_in eq None then +.release >> -.await >> Async[F].pure(None)
                            else
                              val (cbarrier, fiber, _, s) = cb_fb_in.get
                              (cbarrier.await >> fiber.join >> *.set(s).unlessA(s.isEmpty) >> `[]`.set(s)).as(Some(()))
              yield
                it
            }.takeWhile(_.isDefined)
            it <- Stream.eval(result.get).map(_.`()`[T]).evalMap((code andThen exec)(_).map(new `()`[F](_)))
            _  <- Stream.eval(sleep)
            _  <- Stream.eval(+.release)
          yield
            it

        /**
          * modulo replication input guard
          */
        def apply(rate: Rate)(key: String)(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                 (using %[F], /[F], \[F])
                 (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
          input(rate)(key)(Async[F].unit)(!, ?, -, +, *)

        /**
          * modulo replication input guard w/ pace
          */
        def apply(rate: Rate, pace: FiniteDuration)(key: String)(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                 (using %[F], /[F], \[F])
                 (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
          input(rate)(key)(Async[F].sleep(pace))(!, ?, -, +, *)

        /**
          * modulo replication input guard w/ code
          */
        def apply[T](rate: Rate)(key: String)(code: T => F[T])(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                    (using %[F], /[F], \[F])
                    (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
          input[T](rate)(key)(Async[F].unit)(code)(!, ?, -, +, *)

        /**
          * modulo replication input guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: T => F[T])(! : Boolean, ? : Deferred[F, Boolean], - : CyclicBarrier[F], + : Semaphore[F], * : Ref[F, `[]`])
                    (using %[F], /[F], \[F])
                    (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
          input[T](rate)(key)(Async[F].sleep(pace))(code)(!, ?, -, +, *)

      object `(ν)`:

        /**
          * replication bound output guard
          */
        def apply(rate: Rate)(key: String)
                 (using % : %[F], / : /[F], \ : \[F])
                 (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                           ^ : String, `[]`: `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
          for
            _        <- Stream.eval(exclude(key))
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
            enabled  <- Stream.eval(Ref[F].of(true))
            `][`     <- Stream.eval(`[]`.get)
            timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
            _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, timestamp), (`()`[`{}`[F]], Some(Left(())), rate, `][`))))
            cb_fb_in <- Stream.eval(deferred.get)
            if cb_fb_in ne None
            timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
            *  <- Stream.eval(Ref[F].of(null: `[]`))
            it <- Stream.repeatEval {
              for
                _        <- enabled.get >>= timeset.unlessA
                _        <- enabled.get >>= \{
                  *.get.flatMap { s =>
                    %.update { m =>
                      val it = m(^ + key).asInstanceOf[(Boolean, +[F])]._2
                      if s eq null
                      then
                        m + (^ + key -> (true, it))
                      else
                        m + (^ + key -> (true, it.copy(_2 = it._2.copy(_4 = s))))
                    }
                  }
                }.unlessA
                cb_fb_in <- continue.get.flatMap(_.get)
                _        <- Deferred[F, Option[<>[F]]] >>= continue.set
                _        <- enabled.set(false)
                eth      <- Semaphore[F](1).map(Some(_))
                it       <- if cb_fb_in eq None then Async[F].pure(None)
                            else
                              val (cbarrier, fiber, input, s) = cb_fb_in.get
                              (input.set(eth) >> cbarrier.await >> fiber.join >> *.set(s).unlessA(s.isEmpty) >> `[]`.set(s)).as(Some(eth))
              yield
                it
            }.takeWhile(_.isDefined).map(_.get)
          yield
            it

        /**
          * replication bound output guard w/ pace
          */
        def apply(rate: Rate, pace: FiniteDuration)(key: String)
                 (using %[F], /[F], \[F])
                 (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
          apply(rate)(key) <* Stream.sleep(pace)

        /**
          * replication bound output guard w/ code
          */
        def apply[T](rate: Rate)(key: String)(code: => F[T])
                    (using %[F], /[F], \[F])
                    (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
          apply(rate)(key).evalTap(_ => exec(code))

        /**
          * replication bound output guard w/ pace w/ code
          */
        def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: => F[T])
                    (using %[F], /[F], \[F])
                    (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
          apply[T](rate)(key)(code) <* Stream.sleep(pace)

      /**
        * constant replication output guard
        */
      def apply(rate: Rate, value: `()`[F])(key: String)
               (using % : %[F], / : /[F], \ : \[F])
               (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String, `[]`: `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
          enabled  <- Stream.eval(Ref[F].of(true))
          `][`     <- Stream.eval(`[]`.get)
          timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
          _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, timestamp), (`()`[`{}`[F]], Some(Left(())), rate, `][`))))
          cb_fb_in <- Stream.eval(deferred.get)
          if cb_fb_in ne None
          timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
          *  <- Stream.eval(Ref[F].of(null: `[]`))
          _  <- Stream.repeatEval {
            for
              _        <- enabled.get >>= timeset.unlessA
              _        <- enabled.get >>= \{
                *.get.flatMap { s =>
                  %.update { m =>
                    val it = m(^ + key).asInstanceOf[(Boolean, +[F])]._2
                    if s eq null
                    then
                      m + (^ + key -> (true, it))
                    else
                      m + (^ + key -> (true, it.copy(_2 = it._2.copy(_4 = s))))
                  }
                }
              }.unlessA
              cb_fb_in <- continue.get.flatMap(_.get)
              _        <- Deferred[F, Option[<>[F]]] >>= continue.set
              _        <- enabled.set(false)
              it       <- if cb_fb_in eq None then Async[F].pure(None)
                          else
                            val (cbarrier, fiber, input, s) = cb_fb_in.get
                            (input.set(value) >> cbarrier.await >> fiber.join >> *.set(s).unlessA(s.isEmpty) >> `[]`.set(s)).as(Some(()))
            yield
              it
          }.takeWhile(_.isDefined)
        yield
          ()

      /**
        * constant replication output guard w/ pace
        */
      def apply(rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String)
               (using %[F], /[F], \[F])
               (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
        apply(rate, value)(key) <* Stream.sleep(pace)

      /**
        * constant replication output guard w/ code
        */
      def apply[T](rate: Rate, value: `()`[F])(key: String)(code: => F[T])
                  (using %[F], /[F], \[F])
                  (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
        apply(rate, value)(key).evalTap(_ => exec(code))

      /**
        * constant replication output guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String)(code: => F[T])
                  (using %[F], /[F], \[F])
                  (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
        apply[T](rate, value)(key)(code) <* Stream.sleep(pace)

      object `(*)`:

        /**
          * variable replication output guard
          */
        def apply[S: ClassTag](_1: 1)(rate: Rate, value: => S)(key: String)
                                     (using DummyImplicit)
                                     (using %[F], /[F], \[F])
                                     (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
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
                                     (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
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
                                        (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
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
                                        (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
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
                                     (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                               ^ : String, `[]`: `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
          if classTag[S].runtimeClass eq self.getClass
          then
            Stream.eval(Async[F].defer(value.asInstanceOf[F[`()`[F]]])).flatMap(self.`(!)`(rate, _)(key))
          else
            for
              _        <- Stream.eval(exclude(key))
              deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
              continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
              enabled  <- Stream.eval(Ref[F].of(true))
              `][`     <- Stream.eval(`[]`.get)
              timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
              _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, timestamp), (`()`[`{}`[F]], Some(Left(())), rate, `][`))))
              cb_fb_in <- Stream.eval(deferred.get)
              if cb_fb_in ne None
              timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
              *  <- Stream.eval(Ref[F].of(null: `[]`))
              _  <- Stream.repeatEval {
                for
                  _        <- enabled.get >>= timeset.unlessA
                  _        <- enabled.get >>= \{
                    *.get.flatMap { s =>
                      %.update { m =>
                        val it = m(^ + key).asInstanceOf[(Boolean, +[F])]._2
                        if s eq null
                        then
                          m + (^ + key -> (true, it))
                        else
                          m + (^ + key -> (true, it.copy(_2 = it._2.copy(_4 = s))))
                      }
                    }
                  }.unlessA
                  cb_fb_in <- continue.get.flatMap(_.get)
                  _        <- Deferred[F, Option[<>[F]]] >>= continue.set
                  _        <- enabled.set(false)
                  it       <- if cb_fb_in eq None then Async[F].pure(None)
                              else
                                val (cbarrier, fiber, input, s) = cb_fb_in.get
                                value.map(new `()`[F](_)).flatMap(input.set(_) >> cbarrier.await >> fiber.join >> *.set(s).unlessA(s.isEmpty) >> `[]`.set(s)).as(Some(()))
                yield
                  it
              }.takeWhile(_.isDefined)
            yield
              ()

        /**
          * variable replication output guard w/ pace
          */
        def apply[S: ClassTag](_2: 2)(rate: Rate, pace: FiniteDuration, value: => F[S])(key: String)
                                     (using %[F], /[F], \[F])
                                     (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
          apply[S](1)(rate, value)(key) <* Stream.sleep(pace)

        /**
          * variable replication output guard w/ code
          */
        def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => F[S])(key: String)(code: => F[T])
                                        (using %[F], /[F], \[F])
                                        (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
          apply[S](1)(rate, value)(key).evalTap(_ => exec(code))

        /**
          * variable replication output guard w/ pace w/ code
          */
        def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: FiniteDuration, value: => F[S])(key: String)(code: => F[T])
                                        (using %[F], /[F], \[F])
                                        (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
          apply[S, T](3)(rate, value)(key)(code) <* Stream.sleep(pace)

      /**
        * replication input guard
        */
      def apply(rate: Rate)(key: String)
               (using % : %[F], / : /[F], \ : \[F])
               (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String, `[]`: `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          continue <- Stream.eval(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
          enabled  <- Stream.eval(Ref[F].of(true))
          result   <- Stream.eval(Ref[F].of[`()`[F]](null))
          `][`     <- Stream.eval(`[]`.get)
          timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
          _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> continue, timestamp), (`()`[`{}`[F]], Some(Right(result)), rate, `][`))))
          cb_fb_in <- Stream.eval(deferred.get)
          if cb_fb_in ne None
          timeset   =  Async[F].realTime.map(_.toMillis) >>= timestamp.set
          *  <- Stream.eval(Ref[F].of(null: `[]`))
          _  <- Stream.repeatEval {
            for
              _        <- enabled.get >>= timeset.unlessA
              _        <- enabled.get >>= \{
                *.get.flatMap { s =>
                  %.update { m =>
                    val it = m(^ + key).asInstanceOf[(Boolean, +[F])]._2
                    if s eq null
                    then
                      m + (^ + key -> (true, it))
                    else
                      m + (^ + key -> (true, it.copy(_2 = it._2.copy(_4 = s))))
                  }
                }
              }.unlessA
              cb_fb_in <- continue.get.flatMap(_.get)
              _        <- Deferred[F, Option[<>[F]]] >>= continue.set
              _        <- enabled.set(false)
              it       <- if cb_fb_in eq None then Async[F].pure(None)
                          else
                            val (cbarrier, fiber, _, s) = cb_fb_in.get
                            (cbarrier.await >> fiber.join >> *.set(s).unlessA(s.isEmpty) >> `[]`.set(s)).as(Some(()))
            yield
              it
          }.takeWhile(_.isDefined)
          it <- Stream.eval(result.get)
        yield
          it

      /**
        * replication input guard w/ pace
        */
      def apply(rate: Rate, pace: FiniteDuration)(key: String)
               (using %[F], /[F], \[F])
               (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
        apply(rate)(key) <* Stream.sleep(pace)

      /**
        * replication input guard w/ code
        */
      def apply[T](rate: Rate)(key: String)(code: T => F[T])
                  (using %[F], /[F], \[F])
                  (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
        apply(rate)(key).map(_.`()`[T]).evalMap((code andThen exec)(_).map(new `()`[F](_)))

      /**
        * replication input guard w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: T => F[T])
                  (using %[F], /[F], \[F])
                  (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
        apply[T](rate)(key)(code) <* Stream.sleep(pace)

    object `(ν)`:

      /**
        * bound output prefix
        */
      def apply(rate: Rate)(key: String)
               (using % : %[F], / : /[F])
               (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                         ^ : String, `[]`: `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
        for
          _        <- Stream.eval(exclude(key))
          deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
          `][`     <- Stream.eval(`[]`.get)
          timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
          _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> null, timestamp), (`()`[`{}`[F]], Some(Left(())), rate, `][`))))
          cb_fb_in <- Stream.eval(deferred.get)
          if cb_fb_in ne None
          (cbarrier, fiber, input, s) = cb_fb_in.get
          it <- sΠ.ν[F]
          _  <- Stream.eval(input.set(it) >> cbarrier.await >> fiber.join >> `[]`.set(s))
        yield
          it

      /**
        * bound output prefix w/ pace
        */
      def apply(rate: Rate, pace: FiniteDuration)(key: String)
               (using %[F], /[F])
               (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
        apply(rate)(key) <* Stream.sleep(pace)

      /**
        * bound output prefix w/ code
        */
      def apply[T](rate: Rate)(key: String)(code: => F[T])
                  (using %[F], /[F])
                  (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
        apply(rate)(key).evalTap(_ => exec(code))

      /**
        * bound output prefix w/ pace w/ code
        */
      def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: => F[T])
                  (using %[F], /[F])
                  (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
        apply[T](rate)(key)(code) <* Stream.sleep(pace)

    /**
      * constant output prefix
      */
    def apply(rate: Rate, value: `()`[F])(key: String)
             (using % : %[F], / : /[F])
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String, `[]`: `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
      for
        _        <- Stream.eval(exclude(key))
        deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
        `][`     <- Stream.eval(`[]`.get)
        timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
        _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> null, timestamp), (`()`[`{}`[F]], Some(Left(())), rate, `][`))))
        cb_fb_in <- Stream.eval(deferred.get)
        if cb_fb_in ne None
        (cbarrier, fiber, input, s) = cb_fb_in.get
        _  <- Stream.eval(input.set(value) >> cbarrier.await >> fiber.join >> `[]`.set(s))
      yield
        ()

    /**
      * constant output prefix w/ pace
      */
    def apply(rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String)
             (using %[F], /[F])
             (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
      apply(rate, value)(key) <* Stream.sleep(pace)

    /**
      * constant output prefix w/ code
      */
    def apply[T](rate: Rate, value: `()`[F])(key: String)(code: => F[T])
                (using %[F], /[F])
                (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
      apply(rate, value)(key).evalTap(_ => exec(code))

    /**
      * constant output prefix w/ pace w/ code
      */
    def apply[T](rate: Rate, pace: FiniteDuration, value: `()`[F])(key: String)(code: => F[T])
                (using %[F], /[F])
                (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
      apply[T](rate, value)(key)(code) <* Stream.sleep(pace)

    object `(*)`:

      /**
        * variable output prefix
        */
      def apply[S: ClassTag](_1: 1)(rate: Rate, value: => S)(key: String)
                                   (using DummyImplicit)
                                   (using %[F], /[F])
                                   (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
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
                                   (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self(rate, pace, value.asInstanceOf[`()`[F]])(key)
        else
          apply[S](2)(rate, pace, Async[F].delay(value))(key)

      /**
        * variable output prefix w/ code
        */
      def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => S)(key: String)(code: => F[T])
                                      (using DummyImplicit)
                                      (using %[F], /[F])
                                      (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self[T](rate, value.asInstanceOf[`()`[F]])(key)(code)
        else
          apply[S, T](3)(rate, Async[F].delay(value))(key)(code)

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: FiniteDuration, value: => S)(key: String)(code: => F[T])
                                      (using DummyImplicit)
                                      (using %[F], /[F])
                                      (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          self[T](rate, pace, value.asInstanceOf[`()`[F]])(key)(code)
        else
          apply[S, T](4)(rate, pace, Async[F].delay(value))(key)(code)

      /**
        * variable output prefix
        */
      def apply[S: ClassTag](_1: 1)(rate: Rate, value: => F[S])(key: String)
                                   (using % : %[F], / : /[F])
                                   (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                             ^ : String, `[]`: `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
        if classTag[S].runtimeClass eq self.getClass
        then
          Stream.eval(Async[F].defer(value.asInstanceOf[F[`()`[F]]])).flatMap(self(rate, _)(key))
        else
          for
            _        <- Stream.eval(exclude(key))
            deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
            `][`     <- Stream.eval(`[]`.get)
            timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
            _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> null, timestamp), (`()`[`{}`[F]], Some(Left(())), rate, `][`))))
            cb_fb_in <- Stream.eval(deferred.get)
            if cb_fb_in ne None
            (cbarrier, fiber, input, s) = cb_fb_in.get
            _  <- Stream.eval(value.map(new `()`[F](_)).flatMap(input.set(_) >> cbarrier.await >> fiber.join >> `[]`.set(s)))
          yield
            ()

      /**
        * variable output prefix w/ pace
        */
      def apply[S: ClassTag](_2: 2)(rate: Rate, pace: FiniteDuration, value: => F[S])(key: String)
                                   (using %[F], /[F])
                                   (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
        apply[S](1)(rate, value)(key) <* Stream.sleep(pace)

      /**
        * variable output prefix w/ code
        */
      def apply[S: ClassTag, T](_3: 3)(rate: Rate, value: => F[S])(key: String)(code: => F[T])
                                      (using %[F], /[F])
                                      (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
        apply[S](1)(rate, value)(key).evalTap(_ => exec(code))

      /**
        * variable output prefix w/ pace w/ code
        */
      def apply[S: ClassTag, T](_4: 4)(rate: Rate, pace: FiniteDuration, value: => F[S])(key: String)(code: => F[T])
                                      (using %[F], /[F])
                                      (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, Unit] =
        apply[S, T](3)(rate, value)(key)(code) <* Stream.sleep(pace)

    /**
      * input prefix
      */
    def apply(rate: Rate)(key: String)
             (using % : %[F], / : /[F])
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String, `[]`: `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
      for
        _        <- Stream.eval(exclude(key))
        deferred <- Stream.eval(Deferred[F, Option[<>[F]]])
        result   <- Stream.eval(Ref[F].of[`()`[F]](null))
        `][`     <- Stream.eval(`[]`.get)
        timestamp <- Stream.eval(Async[F].realTime.map(_.toMillis) >>= Ref[F].of)
        _        <- Stream.eval(/.offer(^ -> key -> ((deferred -> null, timestamp), (`()`[`{}`[F]], Some(Right(result)), rate, `][`))))
        cb_fb_in <- Stream.eval(deferred.get)
        if cb_fb_in ne None
        (cbarrier, fiber, _, s) = cb_fb_in.get
        _  <- Stream.eval(cbarrier.await >> fiber.join >> `[]`.set(s))
        it <- Stream.eval(result.get)
      yield
        it

    /**
      * input prefix w/ pace
      */
    def apply(rate: Rate, pace: FiniteDuration)(key: String)
             (using %[F], /[F])
             (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
      apply(rate)(key) <* Stream.sleep(pace)

    /**
      * input prefix w/ code
      */
    def apply[T](rate: Rate)(key: String)(code: T => F[T])
                (using %[F], /[F])
                (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
      apply(rate)(key).map(_.`()`[T]).evalMap((code andThen exec)(_).map(new `()`[F](_)))

    /**
      * input prefix w/ pace w/ code
      */
    def apply[T](rate: Rate, pace: FiniteDuration)(key: String)(code: T => F[T])
                (using %[F], /[F])
                (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): Stream[F, `()`[F]] =
      apply[T](rate)(key)(code) <* Stream.sleep(pace)

    override def toString: String = if name == null then "null" else name.toString

  }
