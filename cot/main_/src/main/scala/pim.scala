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

package Π:

  import _root_.scala.concurrent.duration.FiniteDuration

  import _root_.scala.reflect.{ ClassTag, classTag }

  import _root_.scala.quoted.{ Expr, Type, Quotes }

  import _root_.cats.syntax.applicative.*
  import _root_.cats.syntax.functor.*
  import _root_.cats.syntax.flatMap.*

  import _root_.cats.{ Defer, Functor, Monad }
  import _root_.cats.data.OptionT
  import _root_.cats.effect.{ Concurrent, Ref, Temporal }
  import _root_.cats.effect.std.{ CyclicBarrier, Semaphore }
  import _root_.cats.effect.syntax.spawn.*

  import Π.`Π-magic`.><


  trait Macros[F[_]: Concurrent: Defer: Temporal] { this: `()`[F] =>

    import Macros.*

    // MODULO REPLICATION //////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////// BOUND //

    /**
      * modulo replication bound output guard
      */
    protected inline def output(_nu: "ν")(_f: false)(inline parallelism: Int)(inline body: `()`[F] => OptionT[F, Unit]): OptionT[F, Unit] =
      ${ `(ν)`.outputCode('eth)('parallelism)('body)('{ OptionT.liftF(Concurrent[F].unit) })('{ summon[Concurrent[F]] }, '{ summon[Defer[F]] }, '{ summon[Temporal[F]] }) }

    /**
      * modulo replication bound output guard w/ pace
      */
    protected inline def output(_nu: "ν")(_f: false)(inline pace: FiniteDuration, inline parallelism: Int)(inline body: `()`[F] => OptionT[F, Unit]): OptionT[F, Unit] =
      ${ `(ν)`.outputCode('eth)('parallelism)('body)('{ OptionT.liftF(Temporal[F].sleep(pace)) })('{ summon[Concurrent[F]] }, '{ summon[Defer[F]] }, '{ summon[Temporal[F]] }) }

    /**
      * modulo replication bound output guard w/ code
      */
    protected inline def output(_nu: "ν")(_t: true)(inline parallelism: Int)(inline code: OptionT[F, Any])(inline body: `()`[F] => OptionT[F, Unit]): OptionT[F, Unit] =
      ${ `(ν)`.outputCode('eth)('parallelism)('body)('{ code.void })('{ summon[Concurrent[F]] }, '{ summon[Defer[F]] }, '{ summon[Temporal[F]] }) }

    /**
      * modulo replication bound output guard w/ pace w/ code
      */
    protected inline def output(_nu: "ν")(_t: true)(inline pace: FiniteDuration, inline parallelism: Int)(inline code: OptionT[F, Any])(inline body: `()`[F] => OptionT[F, Unit]): OptionT[F, Unit] =
      ${ `(ν)`.outputCode('eth)('parallelism)('body)('{ code.semiflatMap(_ => Temporal[F].sleep(pace)) })('{ summon[Concurrent[F]] }, '{ summon[Defer[F]] }, '{ summon[Temporal[F]] }) }

    //////////////////////////////////////////////////////////////// CONSTANT //

    /**
      * modulo constant replication output guard
      */
    protected inline def output(_f: false)(inline parallelism: Int, inline value: `()`[F])(inline body: OptionT[F, Unit]): OptionT[F, Unit] =
      ${ outputCode('eth)('parallelism, 'value)('body)('{ OptionT.liftF(Concurrent[F].unit) })('{ summon[Concurrent[F]] }, '{ summon[Defer[F]] }, '{ summon[Temporal[F]] }) }

    /**
      * modulo constant replication output guard w/ pace
      */
    protected inline def output(_f: false)(inline pace: FiniteDuration, inline parallelism: Int, inline value: `()`[F])(inline body: OptionT[F, Unit]): OptionT[F, Unit] =
      ${ outputCode('eth)('parallelism, 'value)('body)('{ OptionT.liftF(Temporal[F].sleep(pace)) })('{ summon[Concurrent[F]] }, '{ summon[Defer[F]] }, '{ summon[Temporal[F]] }) }

    /**
      * modulo constant replication output guard w/ code
      */
    protected inline def output(_t: true)(inline parallelism: Int, inline value: `()`[F])(inline code: OptionT[F, Any])(inline body: OptionT[F, Unit]): OptionT[F, Unit] =
      ${ outputCode('eth)('parallelism, 'value)('body)('{ code.void })('{ summon[Concurrent[F]] }, '{ summon[Defer[F]] }, '{ summon[Temporal[F]] }) }

    /**
      * modulo constant replication output guard w/ pace w/ code
      */
    protected inline def output(_t: true)(inline pace: FiniteDuration, inline parallelism: Int, inline value: `()`[F])(inline code: OptionT[F, Any])(inline body: OptionT[F, Unit]): OptionT[F, Unit] =
      ${ outputCode('eth)('parallelism, 'value)('body)('{ code.semiflatMap(_ => Temporal[F].sleep(pace)) })('{ summon[Concurrent[F]] }, '{ summon[Defer[F]] }, '{ summon[Temporal[F]] }) }

    //////////////////////////////////////////////////////////////// VARIABLE //

    /**
      * modulo variable replication output guard
      */
    protected inline def output[S](_s: "*")(_f: false)(inline parallelism: Int, inline value: => OptionT[F, S])(inline body: OptionT[F, Unit]): OptionT[F, Unit] =
     ${ `(*)`.outputCode('eth)('parallelism, '{ () => value })('body)('{ OptionT.liftF(Concurrent[F].unit) })('{ summon[Concurrent[F]] }, '{ summon[Defer[F]] }, '{ summon[Temporal[F]] }) }

    /**
      * modulo variable replication output guard w/ pace
      */
    protected inline def output[S](_s: "*")(_f: false)(inline pace: FiniteDuration, inline parallelism: Int, inline value: => OptionT[F, S])(inline body: OptionT[F, Unit]): OptionT[F, Unit] =
     ${ `(*)`.outputCode('eth)('parallelism, '{ () => value })('body)('{ OptionT.liftF(Temporal[F].sleep(pace)) })('{ summon[Concurrent[F]] }, '{ summon[Defer[F]] }, '{ summon[Temporal[F]] }) }

    /**
      * modulo variable replication output guard w/ code
      */
    protected inline def output[S](_s: "*")(_t: true)(inline parallelism: Int, inline value: => OptionT[F, S])(inline code: OptionT[F, Any])(inline body: OptionT[F, Unit]): OptionT[F, Unit] =
     ${ `(*)`.outputCode('eth)('parallelism, '{ () => value })('body)('{ code.void })('{ summon[Concurrent[F]] }, '{ summon[Defer[F]] }, '{ summon[Temporal[F]] }) }

    /**
      * modulo variable replication output guard w/ pace w/ code
      */
    protected inline def output[S](_s: "*")(_t: true)(inline pace: FiniteDuration, inline parallelism: Int, inline value: => OptionT[F, S])(inline code: OptionT[F, Any])(inline body: OptionT[F, Unit]): OptionT[F, Unit] =
     ${ `(*)`.outputCode('eth)('parallelism, '{ () => value })('body)('{ code.semiflatMap(_ => Temporal[F].sleep(pace)) })('{ summon[Concurrent[F]] }, '{ summon[Defer[F]] }, '{ summon[Temporal[F]] }) }

    // /////////////////////////////////////////////////////////////////// INPUT //

    /**
      * modulo replication input guard
      */
    protected inline def input(_f: false)(inline parallelism: Int)(inline body: `()`[F] => OptionT[F, Unit]): OptionT[F, Unit] =
      ${ inputCode('eth)('parallelism)('body)('{ OptionT.liftF(Concurrent[F].unit) })('{ summon[Concurrent[F]] }, '{ summon[Defer[F]] }, '{ summon[Temporal[F]] }) }

    /**
      * modulo replication input guard w/ pace
      */
    protected inline def input(_f: false)(inline pace: FiniteDuration, inline parallelism: Int)(inline body: `()`[F] => OptionT[F, Unit]): OptionT[F, Unit] =
      ${ inputCode('eth)('parallelism)('body)('{ OptionT.liftF(Temporal[F].sleep(pace)) })('{ summon[Concurrent[F]] }, '{ summon[Defer[F]] }, '{ summon[Temporal[F]] }) }

    /**
      * modulo replication input guard w/ code
      */
    protected inline def input[T: ClassTag](_t: true)(inline parallelism: Int)(code: T => OptionT[F, Any])(inline body: `()`[F] => OptionT[F, Unit]): OptionT[F, Unit] =
      ${ inputCode('eth)('parallelism)('code)('body)('{ OptionT.liftF(Concurrent[F].unit) })('{ summon[Concurrent[F]] }, '{ summon[Defer[F]] }, '{ summon[Temporal[F]] })('{ classTag[T] }) }

    /**
      * modulo replication input guard w/ pace w/ code
      */
    protected inline def input[T: ClassTag](_t: true)(inline pace: FiniteDuration, inline parallelism: Int)(code: T => OptionT[F, Any])(inline body: `()`[F] => OptionT[F, Unit]): OptionT[F, Unit] =
      ${ inputCode('eth)('parallelism)('code)('body)('{ OptionT.liftF(Temporal[F].sleep(pace)) })('{ summon[Concurrent[F]] }, '{ summon[Defer[F]] }, '{ summon[Temporal[F]] })('{ classTag[T] }) }

    ////////////////////////////////////////////////////// modulo replication //

  }


  trait τMacros[F[_]: Concurrent: Temporal]:

    import Macros.τ.*

    /**
      * modulo replication guard
      */
    protected inline def silent(_f: false)(inline parallelism: Int)(inline body: OptionT[F, Unit]): OptionT[F, Unit] =
      ${ silentCode('parallelism)('body)('{ OptionT.liftF(Concurrent[F].unit) })('{ summon[Concurrent[F]] }) }

    /**
      * modulo replication guard w/ pace
      */
    protected inline def silent(_f: false)(inline pace: FiniteDuration, inline parallelism: Int)(inline body: OptionT[F, Unit]): OptionT[F, Unit] =
      ${ silentCode('parallelism)('body)('{ OptionT.liftF(Temporal[F].sleep(pace)) })('{ summon[Concurrent[F]] }) }

    /**
      * modulo replication guard w/ code
      */
    protected inline def silent(_t: true)(inline parallelism: Int)(inline code: OptionT[F, Any])(inline body: OptionT[F, Unit]): OptionT[F, Unit] =
      ${ silentCode('parallelism)('body)('{ code.void })('{ summon[Concurrent[F]] }) }

    /**
      * modulo replication guard w/ pace w/ code
      */
    protected inline def silent(_t: true)(inline pace: FiniteDuration, inline parallelism: Int)(inline code: OptionT[F, Any])(inline body: OptionT[F, Unit]): OptionT[F, Unit] =
      ${ silentCode('parallelism)('body)('{ code.semiflatMap(_ => Temporal[F].sleep(pace)) })('{ summon[Concurrent[F]] }) }


  object Macros:

    object τ:

      def silentCode[F[_]](parallelism: Expr[Int])(body: Expr[OptionT[F, Unit]])
                          (sleep: Expr[OptionT[F, Unit]])
                          (concurrentF: Expr[Concurrent[F]])
                          (using Quotes, Type[F]): Expr[OptionT[F, Unit]] =
       '{ given Concurrent[F] = $concurrentF
          for
            moduloCB <- OptionT.liftF(CyclicBarrier[F]($parallelism))
            unfold    = {
              def unfold(remaining: Int, prevS: Option[Semaphore[F]]): OptionT[F, Unit] =
                for
                  nextS <- OptionT.liftF(Semaphore[F](0))
                  loop   = {
                    lazy val loop: OptionT[F, Unit] =
                      for
                        _ <- OptionT.liftF(moduloCB.await)
                        _ <- OptionT.liftF(prevS.fold(Concurrent[F].unit)(_.acquire))
                        _ <- $sleep.flatTapNone(nextS.release)
                        _ <- OptionT.liftF(nextS.release)
                        _ <- $body
                        _ <- loop
                      yield
                        ()
                    loop
                  }
                  _     <- if remaining == 1
                           then loop
                           else loop.background.use { _ => unfold(remaining - 1, Some(nextS)) }
                yield
                  ()
              unfold
            }
            _        <- unfold($parallelism, None)
          yield
            ()
        }


    object `(ν)`:

      /**
        * modulo replication bound output guard
        */
      def outputCode[F[_]](eth: Expr[><[F]])
                          (parallelism: Expr[Int])(body: Expr[`()`[F] => OptionT[F, Unit]])
                          (sleep: Expr[OptionT[F, Unit]])
                          (concurrentF: Expr[Concurrent[F]], deferF: Expr[Defer[F]], temporalF: Expr[Temporal[F]])
                          (using Quotes, Type[F]): Expr[OptionT[F, Unit]] =
       '{ given Concurrent[F] = $concurrentF
          given Defer[F] = $deferF
          given Temporal[F] = $temporalF
          for
            moduloCB <- OptionT.liftF(CyclicBarrier[F]($parallelism))
            unfold    = {
              def unfold(remaining: Int, prevS: Option[Semaphore[F]]): OptionT[F, Unit] =
                for
                  nextS <- OptionT.liftF(Semaphore[F](0))
                  loop   = {
                    lazy val loop: OptionT[F, Unit] =
                      for
                        _ <- OptionT.liftF(moduloCB.await)
                        _ <- OptionT.liftF(prevS.fold(Concurrent[F].unit)(_.acquire))
                        n <- ν[F]()
                        s <- OptionT.liftF($eth.stop.get)
                        _ <- OptionT.unless(s)(><[F](n.name)($eth)).flatten.flatTapNone(nextS.release)
                        _ <- $sleep.flatTapNone(nextS.release)
                        _ <- OptionT.liftF(nextS.release)
                        _ <- $body(n)
                        _ <- loop
                      yield
                        ()
                    loop
                  }
                  _     <- if remaining == 1
                           then loop
                           else loop.background.use { _ => unfold(remaining - 1, Some(nextS)) }
                yield
                  ()
              unfold
            }
            _        <- unfold($parallelism, None)
          yield
            ()
        }

    /**
      * modulo constant replication output guard
      */
    def outputCode[F[_]](eth: Expr[><[F]])
                        (parallelism: Expr[Int], value: Expr[`()`[F]])(body: Expr[OptionT[F, Unit]])
                        (sleep: Expr[OptionT[F, Unit]])
                        (concurrentF: Expr[Concurrent[F]], deferF: Expr[Defer[F]], temporalF: Expr[Temporal[F]])
                        (using Quotes, Type[F]): Expr[OptionT[F, Unit]] =
     '{ given Concurrent[F] = $concurrentF
        given Defer[F] = $deferF
        given Temporal[F] = $temporalF
        for
          moduloCB <- OptionT.liftF(CyclicBarrier[F]($parallelism))
          unfold    = {
            def unfold(remaining: Int, prevS: Option[Semaphore[F]]): OptionT[F, Unit] =
              for
                nextS <- OptionT.liftF(Semaphore[F](0))
                loop   = {
                  lazy val loop: OptionT[F, Unit] =
                    for
                      _ <- OptionT.liftF(moduloCB.await)
                      _ <- OptionT.liftF(prevS.fold(Concurrent[F].unit)(_.acquire))
                      s <- OptionT.liftF($eth.stop.get)
                      _ <- OptionT.unless(s)(><[F]($value.name)($eth)).flatten.flatTapNone(nextS.release)
                      _ <- $sleep.flatTapNone(nextS.release)
                      _ <- OptionT.liftF(nextS.release)
                      _ <- $body
                      _ <- loop
                    yield
                      ()
                  loop
                }
                _     <- if remaining == 1
                         then loop
                         else loop.background.use { _ => unfold(remaining - 1, Some(nextS)) }
              yield
                ()
            unfold
          }
          _        <- unfold($parallelism, None)
        yield
          ()
    }


    object `(*)`:

      /**
        * modulo variable replication output guard
        */
      def outputCode[F[_], S](eth: Expr[><[F]])
                             (parallelism: Expr[Int], value: Expr[() => OptionT[F, S]])(body: Expr[OptionT[F, Unit]])
                             (sleep: Expr[OptionT[F, Unit]])
                             (concurrentF: Expr[Concurrent[F]], deferF: Expr[Defer[F]], temporalF: Expr[Temporal[F]])
                             (using Quotes, Type[F], Type[S]): Expr[OptionT[F, Unit]] =
       '{ given Concurrent[F] = $concurrentF
          given Defer[F] = $deferF
          given Temporal[F] = $temporalF
          for
            moduloCB <- OptionT.liftF(CyclicBarrier[F]($parallelism))
            unfold    = {
              def unfold(remaining: Int, prevS: Option[Semaphore[F]]): OptionT[F, Unit] =
                for
                  nextS <- OptionT.liftF(Semaphore[F](0))
                  loop   = {
                    lazy val loop: OptionT[F, Unit] =
                      for
                        _ <- OptionT.liftF(moduloCB.await)
                        _ <- OptionT.liftF(prevS.fold(Concurrent[F].unit)(_.acquire))
                        s <- OptionT.liftF($eth.stop.get)
                        _ <- OptionT.unless(s)($value().flatMap(><(_)($eth))).flatten.flatTapNone(nextS.release)
                        _ <- $sleep.flatTapNone(nextS.release)
                        _ <- OptionT.liftF(nextS.release)
                        _ <- $body
                        _ <- loop
                      yield
                        ()
                    loop
                  }
                  _     <- if remaining == 1
                           then loop
                           else loop.background.use { _ => unfold(remaining - 1, Some(nextS)) }
                yield
                  ()
              unfold
            }
            _        <- unfold($parallelism, None)
          yield
            ()
      }

    /**
      * modulo replication input guard
      */
    def inputCode[F[_]](eth: Expr[><[F]])
                       (parallelism: Expr[Int])(body: Expr[`()`[F] => OptionT[F, Unit]])
                       (sleep: Expr[OptionT[F, Unit]])
                       (concurrentF: Expr[Concurrent[F]], deferF: Expr[Defer[F]], temporalF: Expr[Temporal[F]])
                       (using Quotes, Type[F]): Expr[OptionT[F, Unit]] =
     '{ given Concurrent[F] = $concurrentF
        given Defer[F] = $deferF
        given Temporal[F] = $temporalF
        for
          moduloCB <- OptionT.liftF(CyclicBarrier[F]($parallelism))
          unfold    = {
            def unfold(remaining: Int, prevS: Option[Semaphore[F]]): OptionT[F, Unit] =
              for
                nextS <- OptionT.liftF(Semaphore[F](0))
                loop   = {
                  lazy val loop: OptionT[F, Unit] =
                    for
                      _ <- OptionT.liftF(moduloCB.await)
                      _ <- OptionT.liftF(prevS.fold(Concurrent[F].unit)(_.acquire))
                      s <- OptionT.liftF($eth.stop.get)
                      n <- OptionT.unless(s)(><()($eth)).flatten.flatTapNone(nextS.release)
                      _ <- $sleep.flatTapNone(nextS.release)
                      _ <- OptionT.liftF(nextS.release)
                      _ <- $body(new `()`[F](n))
                      _ <- loop
                    yield
                      ()
                  loop
                }
                _     <- if remaining == 1
                         then loop
                         else loop.background.use { _ => unfold(remaining - 1, Some(nextS)) }
              yield
                ()
            unfold
          }
          _        <- unfold($parallelism, None)
        yield
          ()
      }

    /**
      * modulo replication input guard w/ code
      */
    def inputCode[F[_], T](eth: Expr[><[F]])
                          (parallelism: Expr[Int])(code: Expr[T => OptionT[F, Any]])(body: Expr[`()`[F] => OptionT[F, Unit]])
                          (sleep: Expr[OptionT[F, Unit]])
                          (concurrentF: Expr[Concurrent[F]], deferF: Expr[Defer[F]], temporalF: Expr[Temporal[F]])
                          (classTagT: Expr[ClassTag[T]])
                          (using Quotes, Type[F], Type[T]): Expr[OptionT[F, Unit]] =
     '{ given Concurrent[F] = $concurrentF
        given Defer[F] = $deferF
        given Temporal[F] = $temporalF
        given ClassTag[T] = $classTagT
        for
          moduloCB <- OptionT.liftF(CyclicBarrier[F]($parallelism))
          unfold    = {
            def unfold(remaining: Int, prevS: Option[Semaphore[F]]): OptionT[F, Unit] =
              for
                nextS <- OptionT.liftF(Semaphore[F](0))
                loop   = {
                  lazy val loop: OptionT[F, Unit] =
                    for
                      _ <- OptionT.liftF(moduloCB.await)
                      _ <- OptionT.liftF(prevS.fold(Concurrent[F].unit)(_.acquire))
                      s <- OptionT.liftF($eth.stop.get)
                      n <- OptionT.unless(s)(><()($eth)($code)).flatten.flatTapNone(nextS.release)
                      _ <- $sleep.flatTapNone(nextS.release)
                      _ <- OptionT.liftF(nextS.release)
                      _ <- $body(new `()`[F](n))
                      _ <- loop
                    yield
                      ()
                  loop
                }
                _     <- if remaining == 1
                         then loop
                         else loop.background.use { _ => unfold(remaining - 1, Some(nextS)) }
              yield
                ()
            unfold
          }
          _        <- unfold($parallelism, None)
        yield
          ()
      }
