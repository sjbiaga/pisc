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

package sΠ:

  import _root_.scala.concurrent.duration.FiniteDuration

  import _root_.scala.quoted.{ Expr, Type, Quotes }

  import _root_.cats.syntax.applicative.*
  import _root_.cats.syntax.flatMap.*

  import _root_.cats.effect.IO
  import _root_.cats.effect.std.{ CyclicBarrier, Semaphore, UUIDGen }

  import `Π-loop`.{ <>, +, %, /, \ }
  import `Π-stats`.Rate


  trait Macros extends Any { this: `()` =>

    import Macros.*

    // LINEAR REPLICATION //////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////// BOUND //

    /**
      * linear replication bound output guard
      */
    protected inline def output(_nu: "ν")(_f: false)(inline parallelism: Int, inline rate: Rate)(inline key: String)(inline body: `Π-Function1`)
                                                    (using inline % : %, inline / : /, inline \ : \)
                                                    (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): IO[Unit] =
      ${ `(ν)`.outputCode('{ `()`[{}] })('parallelism, 'rate)('key)('body)('{ IO.unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear replication bound output guard w/ pace
      */
    protected inline def output(_nu: "ν")(_f: false)(inline pace: FiniteDuration, inline parallelism: Int, inline rate: Rate)(inline key: String)(inline body: `Π-Function1`)
                                                    (using inline % : %, inline / : /, inline \ : \)
                                                    (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): IO[Unit] =
      ${ `(ν)`.outputCode('{ `()`[{}] })('parallelism, 'rate)('key)('body)('{ IO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear replication bound output guard w/ code
      */
    protected inline def output(_nu: "ν")(_t: true)(inline parallelism: Int, inline rate: Rate)(inline key: String)(inline code: IO[Any])(inline body: `Π-Function1`)
                                                   (using inline % : %, inline / : /, inline \ : \)
                                                   (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): IO[Unit] =
      ${ `(ν)`.outputCode('{ `()`[{}] })('parallelism, 'rate)('key)('body)('{ exec(code).void })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear replication bound output guard w/ pace w/ code
      */
    protected inline def output(_nu: "ν")(_t: true)(inline pace: FiniteDuration, inline parallelism: Int, inline rate: Rate)(inline key: String)(inline code: IO[Any])(inline body: `Π-Function1`)
                                                   (using inline % : %, inline / : /, inline \ : \)
                                                   (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): IO[Unit] =
      ${ `(ν)`.outputCode('{ `()`[{}] })('parallelism, 'rate)('key)('body)('{ exec(code) >> IO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    //////////////////////////////////////////////////////////////// CONSTANT //

    /**
      * linear constant replication output guard
      */
    protected inline def output(_f: false)(inline parallelism: Int, inline rate: Rate, inline value: `()`)(inline key: String)(inline body: `Π-Function0`)
                                          (using inline % : %, inline / : /, inline \ : \)
                                          (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): IO[Unit] =
      ${ outputCode('{ `()`[{}] })('parallelism, 'rate, 'value)('key)('body)('{ IO.unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear constant replication output guard w/ pace
      */
    protected inline def output(_f: false)(inline pace: FiniteDuration, inline parallelism: Int, inline rate: Rate, inline value: `()`)(inline key: String)(inline body: `Π-Function0`)
                                          (using inline % : %, inline / : /, inline \ : \)
                                          (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): IO[Unit] =
      ${ outputCode('{ `()`[{}] })('parallelism, 'rate, 'value)('key)('body)('{ IO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear constant replication output guard w/ code
      */
    protected inline def output(_t: true)(inline parallelism: Int, inline rate: Rate, inline value: `()`)(inline key: String)(inline code: IO[Any])(inline body: `Π-Function0`)
                                         (using inline % : %, inline / : /, inline \ : \)
                                         (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): IO[Unit] =
      ${ outputCode('{ `()`[{}] })('parallelism, 'rate, 'value)('key)('body)('{ exec(code).void })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear constant replication output guard w/ pace w/ code
      */
    protected inline def output(_t: true)(inline pace: FiniteDuration, inline parallelism: Int, inline rate: Rate, inline value: `()`)(inline key: String)(inline code: IO[Any])(inline body: `Π-Function0`)
                                         (using inline % : %, inline / : /, inline \ : \)
                                         (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): IO[Unit] =
      ${ outputCode('{ `()`[{}] })('parallelism, 'rate, 'value)('key)('body)('{ exec(code) >> IO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    //////////////////////////////////////////////////////////////// VARIABLE //

    /**
      * linear variable replication output guard
      */
    protected inline def output[S](_s: "*")(_f: false)(inline parallelism: Int, inline rate: Rate, inline value: => IO[S])(inline key: String)(inline body: `Π-Function0`)
                                                      (using inline % : %, inline / : /, inline \ : \)
                                                      (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): IO[Unit] =
      ${ `(*)`.outputCode('{ `()`[{}] })('parallelism, 'rate, '{ () => value })('key)('body)('{ IO.unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear variable replication output guard w/ pace
      */
    protected inline def output[S](_s: "*")(_f: false)(inline pace: FiniteDuration, inline parallelism: Int, inline rate: Rate, inline value: => IO[S])(inline key: String)(inline body: `Π-Function0`)
                                                      (using inline % : %, inline / : /, inline \ : \)
                                                      (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): IO[Unit] =
      ${ `(*)`.outputCode('{ `()`[{}] })('parallelism, 'rate, '{ () => value })('key)('body)('{ IO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear variable replication output guard w/ code
      */
    protected inline def output[S](_s: "*")(_t: true)(inline parallelism: Int, inline rate: Rate, inline value: => IO[S])(inline key: String)(inline code: IO[Any])(inline body: `Π-Function0`)
                                                     (using inline % : %, inline / : /, inline \ : \)
                                                     (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): IO[Unit] =
      ${ `(*)`.outputCode('{ `()`[{}] })('parallelism, 'rate, '{ () => value })('key)('body)('{ exec(code).void })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear variable replication output guard w/ pace w/ code
      */
    protected inline def output[S](_s: "*")(_t: true)(inline pace: FiniteDuration, inline parallelism: Int, inline rate: Rate, inline value: => IO[S])(inline key: String)(inline code: IO[Any])(inline body: `Π-Function0`)
                                                     (using inline % : %, inline / : /, inline \ : \)
                                                     (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): IO[Unit] =
      ${ `(*)`.outputCode('{ `()`[{}] })('parallelism, 'rate, '{ () => value })('key)('body)('{ exec(code) >> IO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    // /////////////////////////////////////////////////////////////////// INPUT //

    /**
      * linear replication input guard
      */
    protected inline def input(_f: false)(inline parallelism: Int, inline rate: Rate)(inline key: String)(inline body: `Π-Function1`)
                                          (using inline % : %, inline / : /, inline \ : \)
                                          (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): IO[Unit] =
      ${ inputCode('{ `()`[{}] })('parallelism, 'rate)('key)('body)('{ IO.unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear replication input guard w/ pace
      */
    protected inline def input(_f: false)(inline pace: FiniteDuration, inline parallelism: Int, inline rate: Rate)(inline key: String)(inline body: `Π-Function1`)
                                          (using inline % : %, inline / : /, inline \ : \)
                                          (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): IO[Unit] =
      ${ inputCode('{ `()`[{}] })('parallelism, 'rate)('key)('body)('{ IO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear replication input guard w/ code
      */
    protected inline def input[T](_t: true)(inline parallelism: Int, inline rate: Rate)(inline key: String)(code: T => IO[T])(inline body: `Π-Function1`)
                                           (using inline % : %, inline / : /, inline \ : \)
                                           (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): IO[Unit] =
      ${ inputCode('{ `()`[{}] })('parallelism, 'rate)('key)('{ code andThen exec })('body)('{ IO.unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear replication input guard w/ pace w/ code
      */
    protected inline def input[T](_t: true)(inline pace: FiniteDuration, inline parallelism: Int, inline rate: Rate)(inline key: String)(code: T => IO[T])(inline body: `Π-Function1`)
                                           (using inline % : %, inline / : /, inline \ : \)
                                           (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): IO[Unit] =
      ${ inputCode('{ `()`[{}] })('parallelism, 'rate)('key)('{ code andThen exec })('body)('{ IO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    ////////////////////////////////////////////////////// linear replication //

  }


  trait τ:

    import Macros.τ.*

    /**
      * linear replication guard
      */
    protected inline def silent(_f: false)(inline parallelism: Int, inline rate: Rate)(inline key: String)(inline body: `Π-Function0`)
                                          (using inline % : %, inline / : /, inline \ : \)
                                          (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): IO[Unit] =
      ${ silentCode('parallelism, 'rate)('key)('body)('{ IO.unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear replication guard w/ pace
      */
    protected inline def silent(_f: false)(inline pace: FiniteDuration, inline parallelism: Int, inline rate: Rate)(inline key: String)(inline body: `Π-Function0`)
                                          (using inline % : %, inline / : /, inline \ : \)
                                          (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): IO[Unit] =
      ${ silentCode('parallelism, 'rate)('key)('body)('{ IO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear replication guard w/ code
      */
    protected inline def silent(_t: true)(inline parallelism: Int, inline rate: Rate)(inline key: String)(inline code: IO[Any])(inline body: `Π-Function0`)
                                         (using inline % : %, inline / : /, inline \ : \)
                                         (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): IO[Unit] =
      ${ silentCode('parallelism, 'rate)('key)('body)('{ exec(code).void })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear replication guard w/ pace w/ code
      */
    protected inline def silent(_t: true)(inline pace: FiniteDuration, inline parallelism: Int, inline rate: Rate)(inline key: String)(inline code: IO[Any])(inline body: `Π-Function0`)
                                         (using inline % : %, inline / : /, inline \ : \)
                                         (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): IO[Unit] =
      ${ silentCode('parallelism, 'rate)('key)('body)('{ exec(code) >> IO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }


  object Macros:

    // duplicated method to avoid cyclic dependencies
    private def exclude(key: String)
                       (% : %)
                       (`π-elvis`: `Π-Map`[String, `Π-Set`[String]]): IO[Unit] =
      `π-exclude`(`π-elvis`(key))(using %).whenA(`π-elvis`.contains(key))

    object τ:

      /**
        * linear replication guard
        */
      def silentCode(parallelism: Expr[Int], rate: Expr[Rate])(key: Expr[String])(body: Expr[`Π-Function0`])
                    (sleep: Expr[IO[Unit]])
                    (% : Expr[%], / : Expr[/], \ : Expr[\])
                    (`π-elvis`: Expr[`Π-Map`[String, `Π-Set`[String]]], ^ : Expr[String])
                    (using Quotes): Expr[IO[Unit]] =
       '{ for
            linearCB <- CyclicBarrier[IO]($parallelism)
            stopR    <- IO.ref(false)
            unfold    = {
              def unfold(remaining: Int, prevS: Option[Semaphore[IO]])(^ : String): IO[Unit] =
                for
                  nextS <- Semaphore[IO](0)
                  main   =
                    for
                      loop <- ( for
                                  _        <- exclude($key)(${%})(${`π-elvis`}).whenA(prevS eq None)
                                  continue <- IO.deferred[Option[<>]] >>= IO.ref
                                  deferred <- IO.deferred[Option[<>]]
                                  _        <- deferred.complete(None).unlessA(prevS eq None)
                                  _        <- ${/}.offer(^ -> $key -> (deferred -> continue -> (new {}, Some(Left(())), $rate)))
                                  opt      <- deferred.get
                                  _        <- stopR.set(true).whenA(opt eq None).whenA(prevS eq None)
                                  loop      = {
                                    def loop(enabled: Boolean = prevS eq None): IO[Unit] =
                                      for
                                        _ <- linearCB.await
                                        _ <- prevS.fold(IO.unit)(_.acquire)
                                        _ <- stopR.get >>= ( for
                                                               _   <- (${%}.update { m => m + (^ + $key -> (true, m(^ + $key).asInstanceOf[(Boolean, +)]._2)) } >> ${\}).unlessA(enabled)
                                                               opt <- continue.get.flatMap(_.get)
                                                               _   <- IO.deferred[Option[<>]] >>= continue.set
                                                               _   <- if (opt eq None)
                                                                      then
                                                                        stopR.set(true)
                                                                      else
                                                                        val (_, b, f, _) = opt.get
                                                                        for
                                                                          _ <- b.await
                                                                          _ <- f.join
                                                                        yield ()
                                                             yield ()
                                                           ).unlessA
                                        _ <- stopR.get.ifM(nextS.release >> loop(false).unlessA(remaining == 1),
                                                           $sleep >> nextS.release >> $body()(using ^) >> loop(false))
                                      yield ()
                                    loop()
                                  }
                                  stop     <- stopR.get
                                yield
                                  loop.unlessA(stop)
                              )
                      _    <- loop
                    yield
                      ()
                  _     <- if remaining == 1
                           then main
                           else main.background.use { _ => UUIDGen.randomString[IO] >>= unfold(remaining - 1, Some(nextS)) }
                yield
                  ()
              unfold
            }
            _        <- unfold($parallelism, None)(${^})
          yield
            ()
        }


    object `(ν)`:

      /**
        * linear replication bound output guard
        */
      def outputCode(ether: Expr[{}])
                    (parallelism: Expr[Int], rate: Expr[Rate])(key: Expr[String])(body: Expr[`Π-Function1`])
                    (sleep: Expr[IO[Unit]])
                    (% : Expr[%], / : Expr[/], \ : Expr[\])
                    (`π-elvis`: Expr[`Π-Map`[String, `Π-Set`[String]]], ^ : Expr[String])
                    (using Quotes): Expr[IO[Unit]] =
       '{ for
            linearCB <- CyclicBarrier[IO]($parallelism)
            stopR    <- IO.ref(false)
            unfold    = {
              def unfold(remaining: Int, prevS: Option[Semaphore[IO]])(^ : String): IO[Unit] =
                for
                  nextS <- Semaphore[IO](0)
                  main   =
                    for
                      loop <- ( for
                                  _        <- exclude($key)(${%})(${`π-elvis`}).whenA(prevS eq None)
                                  continue <- IO.deferred[Option[<>]] >>= IO.ref
                                  deferred <- IO.deferred[Option[<>]]
                                  _        <- deferred.complete(None).unlessA(prevS eq None)
                                  _        <- ${/}.offer(^ -> $key -> (deferred -> continue -> ($ether, Some(Left(())), $rate)))
                                  opt      <- deferred.get
                                  _        <- stopR.set(true).whenA(opt eq None).whenA(prevS eq None)
                                  loop      = {
                                    def loop(enabled: Boolean = prevS eq None): IO[Unit] =
                                      for
                                        _ <- linearCB.await
                                        _ <- prevS.fold(IO.unit)(_.acquire)
                                        n <- ν
                                        _ <- stopR.get >>= ( for
                                                               _   <- (${%}.update { m => m + (^ + $key -> (true, m(^ + $key).asInstanceOf[(Boolean, +)]._2)) } >> ${\}).unlessA(enabled)
                                                               opt <- continue.get.flatMap(_.get)
                                                               _   <- IO.deferred[Option[<>]] >>= continue.set
                                                               _   <- if (opt eq None)
                                                                      then
                                                                        stopR.set(true)
                                                                      else
                                                                        val (_, b, f, i) = opt.get
                                                                        for
                                                                          _ <- i.set(n)
                                                                          _ <- b.await
                                                                          _ <- f.join
                                                                        yield ()
                                                             yield ()
                                                           ).unlessA
                                        _ <- stopR.get.ifM(nextS.release >> loop(false).unlessA(remaining == 1),
                                                           $sleep >> nextS.release >> $body(n)(using ^) >> loop(false))
                                      yield ()
                                    loop()
                                  }
                                  stop     <- stopR.get
                                yield
                                  loop.unlessA(opt eq None)
                              )
                      _    <- loop
                    yield
                      ()
                  _     <- if remaining == 1
                           then main
                           else main.background.use { _ => UUIDGen.randomString[IO] >>= unfold(remaining - 1, Some(nextS)) }
                yield
                  ()
              unfold
            }
            _        <- unfold($parallelism, None)(${^})
          yield
            ()
        }

    /**
      * linear constant replication output guard
      */
    def outputCode(ether: Expr[{}])
                  (parallelism: Expr[Int], rate: Expr[Rate], value: Expr[`()`])(key: Expr[String])(body: Expr[`Π-Function0`])
                  (sleep: Expr[IO[Unit]])
                  (% : Expr[%], / : Expr[/], \ : Expr[\])
                  (`π-elvis`: Expr[`Π-Map`[String, `Π-Set`[String]]], ^ : Expr[String])
                  (using Quotes): Expr[IO[Unit]] =
     '{ for
          linearCB <- CyclicBarrier[IO]($parallelism)
          stopR    <- IO.ref(false)
          unfold    = {
            def unfold(remaining: Int, prevS: Option[Semaphore[IO]])(^ : String): IO[Unit] =
              for
                nextS <- Semaphore[IO](0)
                main   =
                  for
                    _    <- prevS.fold(IO.unit)(_.acquire)
                    loop <- ( for
                                _        <- exclude($key)(${%})(${`π-elvis`}).whenA(prevS eq None)
                                continue <- IO.deferred[Option[<>]] >>= IO.ref
                                deferred <- IO.deferred[Option[<>]]
                                _        <- deferred.complete(None).unlessA(prevS eq None)
                                _        <- ${/}.offer(^ -> $key -> (deferred -> continue -> ($ether, Some(Left(())), $rate)))
                                opt      <- deferred.get
                                _        <- stopR.set(true).whenA(opt eq None).whenA(prevS eq None)
                                loop      = {
                                  def loop(enabled: Boolean = prevS eq None): IO[Unit] =
                                    for
                                      _ <- linearCB.await
                                      _ <- prevS.fold(IO.unit)(_.acquire)
                                      _ <- stopR.get >>= ( for
                                                             _   <- (${%}.update { m => m + (^ + $key -> (true, m(^ + $key).asInstanceOf[(Boolean, +)]._2)) } >> ${\}).unlessA(enabled)
                                                             opt <- continue.get.flatMap(_.get)
                                                             _   <- IO.deferred[Option[<>]] >>= continue.set
                                                             _   <- if (opt eq None)
                                                                    then
                                                                      stopR.set(true)
                                                                    else
                                                                      val (_, b, f, i) = opt.get
                                                                      for
                                                                        _ <- i.set($value)
                                                                        _ <- b.await
                                                                        _ <- f.join
                                                                      yield ()
                                                           yield ()
                                                         ).unlessA
                                      _ <- stopR.get.ifM(nextS.release >> loop(false).unlessA(remaining == 1),
                                                         $sleep >> nextS.release >> $body()(using ^) >> loop(false))
                                    yield ()
                                  loop()
                                }
                                stop     <- stopR.get
                              yield loop.unlessA(stop)
                            )
                    _    <- loop
                  yield
                    ()
                _     <- if remaining == 1
                         then main
                         else main.background.use { _ => UUIDGen.randomString[IO] >>= unfold(remaining - 1, Some(nextS)) }
              yield
                ()
            unfold
          }
          _        <- unfold($parallelism, None)(${^})
        yield
          ()
      }


    object `(*)`:

      /**
        * linear variable replication output guard
        */
      def outputCode[S](ether: Expr[{}])
                       (parallelism: Expr[Int], rate: Expr[Rate], value: Expr[() => IO[S]])(key: Expr[String])(body: Expr[`Π-Function0`])
                       (sleep: Expr[IO[Unit]])
                       (% : Expr[%], / : Expr[/], \ : Expr[\])
                        (`π-elvis`: Expr[`Π-Map`[String, `Π-Set`[String]]], ^ : Expr[String])
                       (using Type[S], Quotes): Expr[IO[Unit]] =
       '{ for
          linearCB <- CyclicBarrier[IO]($parallelism)
          stopR    <- IO.ref(false)
          unfold    = {
            def unfold(remaining: Int, prevS: Option[Semaphore[IO]])(^ : String): IO[Unit] =
              for
                nextS <- Semaphore[IO](0)
                main   =
                  for
                    loop <- ( for
                                _        <- exclude($key)(${%})(${`π-elvis`}).whenA(prevS eq None)
                                continue <- IO.deferred[Option[<>]] >>= IO.ref
                                deferred <- IO.deferred[Option[<>]]
                                _        <- deferred.complete(None).unlessA(prevS eq None)
                                _        <- ${/}.offer(^ -> $key -> (deferred -> continue -> ($ether, Some(Left(())), $rate)))
                                opt      <- deferred.get
                                _        <- stopR.set(true).whenA(opt eq None).whenA(prevS eq None)
                                loop      = {
                                  def loop(enabled: Boolean = prevS eq None): IO[Unit] =
                                    for
                                      _ <- linearCB.await
                                      _ <- prevS.fold(IO.unit)(_.acquire)
                                      _ <- stopR.get >>= ( for
                                                             _   <- (${%}.update { m => m + (^ + $key -> (true, m(^ + $key).asInstanceOf[(Boolean, +)]._2)) } >> ${\}).unlessA(enabled)
                                                             opt <- continue.get.flatMap(_.get)
                                                             _   <- IO.deferred[Option[<>]] >>= continue.set
                                                             _   <- if (opt eq None)
                                                                    then
                                                                      stopR.set(true)
                                                                    else
                                                                      val (_, b, f, i) = opt.get
                                                                      for
                                                                        _ <- $value().map(new `()`(_)) >>= i.set
                                                                        _ <- b.await
                                                                        _ <- f.join
                                                                      yield ()
                                                           yield ()
                                                         ).unlessA
                                      _ <- stopR.get.ifM(nextS.release >> loop(false).unlessA(remaining == 1),
                                                         $sleep >> nextS.release >> $body()(using ^) >> loop(false))
                                    yield ()
                                  loop()
                                }
                                stop     <- stopR.get
                              yield loop.unlessA(stop)
                            )
                    _    <- loop
                  yield
                    ()
                _     <- if remaining == 1
                         then main
                         else main.background.use { _ => UUIDGen.randomString[IO] >>= unfold(remaining - 1, Some(nextS)) }
              yield
                ()
            unfold
          }
          _        <- unfold($parallelism, None)(${^})
        yield
          ()
        }


    /**
      * linear replication input guard
      */
    def inputCode(ether: Expr[{}])
                 (parallelism: Expr[Int], rate: Expr[Rate])(key: Expr[String])(body: Expr[`Π-Function1`])
                 (sleep: Expr[IO[Unit]])
                 (% : Expr[%], / : Expr[/], \ : Expr[\])
                 (`π-elvis`: Expr[`Π-Map`[String, `Π-Set`[String]]], ^ : Expr[String])
                 (using Quotes): Expr[IO[Unit]] =
     '{ for
          linearCB <- CyclicBarrier[IO]($parallelism)
          stopR    <- IO.ref(false)
          unfold    = {
            def unfold(remaining: Int, prevS: Option[Semaphore[IO]])(^ : String): IO[Unit] =
              for
                nextS <- Semaphore[IO](0)
                main   =
                  for
                    loop <- ( for
                                _        <- exclude($key)(${%})(${`π-elvis`}).whenA(prevS eq None)
                                continue <- IO.deferred[Option[<>]] >>= IO.ref
                                deferred <- IO.deferred[Option[<>]]
                                _        <- deferred.complete(None).unlessA(prevS eq None)
                                result   <- IO.ref(`null`)
                                _        <- ${/}.offer(^ -> $key -> (deferred -> continue -> ($ether, Some(Right(result)), $rate)))
                                opt      <- deferred.get
                                _        <- stopR.set(true).whenA(opt eq None).whenA(prevS eq None)
                                loop      = {
                                  def loop(enabled: Boolean = prevS eq None): IO[Unit] =
                                    for
                                      _ <- linearCB.await
                                      _ <- prevS.fold(IO.unit)(_.acquire)
                                      _ <- stopR.get >>= ( for
                                                             _   <- (${%}.update { m => m + (^ + $key -> (true, m(^ + $key).asInstanceOf[(Boolean, +)]._2)) } >> ${\}).unlessA(enabled)
                                                             opt <- continue.get.flatMap(_.get)
                                                             _   <- IO.deferred[Option[<>]] >>= continue.set
                                                             _   <- if (opt eq None)
                                                                    then
                                                                      stopR.set(true)
                                                                    else
                                                                      val (_, b, f, _) = opt.get
                                                                      for
                                                                        _ <- b.await
                                                                        _ <- f.join
                                                                      yield ()
                                                           yield ()
                                                         ).unlessA
                                      _ <- stopR.get.ifM(nextS.release >> loop(false).unlessA(remaining == 1),
                                                         (result.get >>= ($sleep >> nextS.release >> $body(_)(using ^))) >> loop(false))
                                    yield ()
                                  loop()
                                }
                                stop     <- stopR.get
                              yield loop.unlessA(stop)
                            )
                    _    <- loop
                  yield
                    ()
                _     <- if remaining == 1
                         then main
                         else main.background.use { _ => UUIDGen.randomString[IO] >>= unfold(remaining - 1, Some(nextS)) }
              yield
                ()
            unfold
          }
          _        <- unfold($parallelism, None)(${^})
        yield
          ()
      }

    /**
      * linear replication input guard w/ code
      */
    def inputCode[T](ether: Expr[{}])
                    (parallelism: Expr[Int], rate: Expr[Rate])(key: Expr[String])(code: Expr[T => IO[T]])(body: Expr[`Π-Function1`])
                    (sleep: Expr[IO[Unit]])
                    (% : Expr[%], / : Expr[/], \ : Expr[\])
                    (`π-elvis`: Expr[`Π-Map`[String, `Π-Set`[String]]], ^ : Expr[String])
                    (using Type[T], Quotes): Expr[IO[Unit]] =
     '{ for
          linearCB <- CyclicBarrier[IO]($parallelism)
          stopR    <- IO.ref(false)
          unfold    = {
            def unfold(remaining: Int, prevS: Option[Semaphore[IO]])(^ : String): IO[Unit] =
              for
                nextS <- Semaphore[IO](0)
                main   =
                  for
                    loop <- ( for
                                _        <- exclude($key)(${%})(${`π-elvis`}).whenA(prevS eq None)
                                continue <- IO.deferred[Option[<>]] >>= IO.ref
                                deferred <- IO.deferred[Option[<>]]
                                _        <- deferred.complete(None).unlessA(prevS eq None)
                                result   <- IO.ref(`null`)
                                _        <- ${/}.offer(^ -> $key -> (deferred -> continue -> ($ether, Some(Right(result)), $rate)))
                                opt      <- deferred.get
                                _        <- stopR.set(true).whenA(opt eq None).whenA(prevS eq None)
                                loop      = {
                                  def loop(enabled: Boolean = prevS eq None): IO[Unit] =
                                    for
                                      _ <- linearCB.await
                                      _ <- prevS.fold(IO.unit)(_.acquire)
                                      _ <- stopR.get >>= ( for
                                                             _   <- (${%}.update { m => m + (^ + $key -> (true, m(^ + $key).asInstanceOf[(Boolean, +)]._2)) } >> ${\}).unlessA(enabled)
                                                             opt <- continue.get.flatMap(_.get)
                                                             _   <- IO.deferred[Option[<>]] >>= continue.set
                                                             _   <- if (opt eq None)
                                                                    then
                                                                      stopR.set(true)
                                                                    else
                                                                      val (_, b, f, _) = opt.get
                                                                      for
                                                                        _ <- b.await
                                                                        _ <- f.join
                                                                      yield ()
                                                           yield ()
                                                         ).unlessA
                                      _ <- stopR.get.ifM(nextS.release >> loop(false).unlessA(remaining == 1),
                                                         (result.get.map(_.name.asInstanceOf[T]).flatMap($code).map(new `()`(_)) >>=
                                                         ($sleep >> nextS.release >> $body(_)(using ^))) >> loop(false))
                                    yield ()
                                  loop()
                                }
                                stop     <- stopR.get
                              yield loop.unlessA(opt eq None)
                            )
                    _    <- loop
                  yield
                    ()
                _     <- if remaining == 1
                         then main
                         else main.background.use { _ => UUIDGen.randomString[IO] >>= unfold(remaining - 1, Some(nextS)) }
              yield
                ()
            unfold
          }
          _        <- unfold($parallelism, None)(${^})
        yield
          ()
      }

    // duplicated method to avoid cyclic dependencies
    val `null` = new `()`(null)
