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

  import _root_.scala.quoted.{ Expr, Type, Quotes }

  import _root_.cats.syntax.applicative.*

  import _root_.cats.effect.IO
  import _root_.cats.effect.std.{ CyclicBarrier, Semaphore }

  import Π.{ exec, ν }
  import Π.`Π-magic`.{ ><, >*< }


  trait Macros extends Any { this: `()` =>

    import Macros.*

    // LINEAR REPLICATION //////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////// BOUND //

    /**
      * linear replication bound output guard
      */
    protected inline def output(_nu: "ν")(_f: false)(inline arity: Int)(inline parallelism: Int)(inline body: Seq[`()`] => IO[Any]): IO[Unit] =
      ${ `(ν)`.outputCode('arity)('ref)('parallelism)('body)('{ IO.unit }) }

    /**
      * linear replication bound output guard w/ pace
      */
    protected inline def output(_nu: "ν")(_f: false)(inline arity: Int)(inline pace: FiniteDuration, inline parallelism: Int)(inline body: Seq[`()`] => IO[Any]): IO[Unit] =
      ${ `(ν)`.outputCode('arity)('ref)('parallelism)('body)('{ IO.sleep(pace) }) }

    /**
      * linear replication bound output guard w/ code
      */
    protected inline def output(_nu: "ν")(_t: true)(inline arity: Int)(inline parallelism: Int)(inline code: IO[Any])(inline body: Seq[`()`] => IO[Any]): IO[Unit] =
      ${ `(ν)`.outputCode('arity)('ref)('parallelism)('body)('{ exec(code).void }) }

    /**
      * linear replication bound output guard w/ pace w/ code
      */
    protected inline def output(_nu: "ν")(_t: true)(inline arity: Int)(inline pace: FiniteDuration, inline parallelism: Int)(inline code: IO[Any])(inline body: Seq[`()`] => IO[Any]): IO[Unit] =
      ${ `(ν)`.outputCode('arity)('ref)('parallelism)('{ exec(code) >> body(_) })('{ exec(code) >> IO.sleep(pace) }) }

    //////////////////////////////////////////////////////////////// CONSTANT //

    /**
      * linear constant replication output guard
      */
    protected inline def output(_f: false)(inline parallelism: Int, inline value: `()`*)(inline body: IO[Any]): IO[Unit] =
      ${ outputCode('ref)('parallelism, 'value)('body)('{ IO.unit }) }

    /**
      * linear constant replication output guard w/ pace
      */
    protected inline def output(_f: false)(inline pace: FiniteDuration, inline parallelism: Int, inline value: `()`*)(inline body: IO[Any]): IO[Unit] =
      ${ outputCode('ref)('parallelism, 'value)('body)('{ IO.sleep(pace) }) }

    /**
      * linear constant replication output guard w/ code
      */
    protected inline def output(_t: true)(inline parallelism: Int, inline value: `()`*)(inline code: IO[Any])(inline body: IO[Any]): IO[Unit] =
      ${ outputCode('ref)('parallelism, 'value)('body)('{ exec(code).void }) }

    /**
      * linear constant replication output guard w/ pace w/ code
      */
    protected inline def output(_t: true)(inline pace: FiniteDuration, inline parallelism: Int, inline value: `()`*)(inline code: IO[Any])(inline body: IO[Any]): IO[Unit] =
      ${ outputCode('ref)('parallelism, 'value)('body)('{ exec(code) >> IO.sleep(pace) }) }

    //////////////////////////////////////////////////////////////// VARIABLE //

    /**
      * linear variable replication output guard
      */
    protected inline def output[S](_s: "*")(_f: false)(inline parallelism: Int, inline value: IO[Seq[S]])(inline body: IO[Any]): IO[Unit] =
     ${ `(*)`.outputCode('ref)('parallelism, 'value)('body)('{ IO.unit }) }

    /**
      * linear variable replication output guard w/ pace
      */
    protected inline def output[S](_s: "*")(_f: false)(inline pace: FiniteDuration, inline parallelism: Int, inline value: IO[Seq[S]])(inline body: IO[Any]): IO[Unit] =
     ${ `(*)`.outputCode('ref)('parallelism, 'value)('body)('{ IO.sleep(pace) }) }

    /**
      * linear variable replication output guard w/ code
      */
    protected inline def output[S](_s: "*")(_t: true)(inline parallelism: Int, inline value: IO[Seq[S]])(inline code: IO[Any])(inline body: IO[Any]): IO[Unit] =
     ${ `(*)`.outputCode('ref)('parallelism, 'value)('body)('{ exec(code).void }) }

    /**
      * linear variable replication output guard w/ pace w/ code
      */
    protected inline def output[S](_s: "*")(_t: true)(inline pace: FiniteDuration, inline parallelism: Int, inline value: IO[Seq[S]])(inline code: IO[Any])(inline body: IO[Any]): IO[Unit] =
     ${ `(*)`.outputCode('ref)('parallelism, 'value)('body)('{ exec(code) >> IO.sleep(pace) }) }

    /////////////////////////////////////////////////////////////////// INPUT //

    /**
      * linear replication input guard
      */
    protected inline def input(_f: false)(inline parallelism: Int)(inline body: Seq[`()`] => IO[Any]): IO[Unit] =
      ${ inputCode('ref)('parallelism)('body)('{ IO.unit }) }

    /**
      * linear replication input guard w/ pace
      */
    protected inline def input(_f: false)(inline pace: FiniteDuration, inline parallelism: Int)(inline body: Seq[`()`] => IO[Any]): IO[Unit] =
      ${ inputCode('ref)('parallelism)('body)('{ IO.sleep(pace) }) }

    /**
      * linear replication input guard w/ code
      */
    protected inline def input[T](_t: true)(inline parallelism: Int)(code: Seq[T] => IO[Seq[T]])(inline body: Seq[`()`] => IO[Any]): IO[Unit] =
      ${ inputCode('ref)('parallelism)('code)('body)('{ IO.unit }) }

    /**
      * linear replication input guard w/ pace w/ code
      */
    protected inline def input[T](_t: true)(inline pace: FiniteDuration, inline parallelism: Int)(code: Seq[T] => IO[Seq[T]])(inline body: Seq[`()`] => IO[Any]): IO[Unit] =
      ${ inputCode('ref)('parallelism)('code)('body)('{ IO.sleep(pace) }) }

    ////////////////////////////////////////////////////// linear replication //

  }


  trait τ:

    import Macros.τ.*

    /**
      * linear replication guard
      */
    protected inline def silent(_f: false)(inline parallelism: Int)(inline body: IO[Any]): IO[Unit] =
      ${ silentCode('parallelism)('body)('{ IO.unit }) }

    /**
      * linear replication guard w/ pace
      */
    protected inline def silent(_f: false)(inline pace: FiniteDuration, inline parallelism: Int)(inline body: IO[Any]): IO[Unit] =
      ${ silentCode('parallelism)('body)('{ IO.sleep(pace) }) }

    /**
      * linear replication guard w/ code
      */
    protected inline def silent(_t: true)(inline parallelism: Int)(inline code: IO[Any])(inline body: IO[Any]): IO[Unit] =
      ${ silentCode('parallelism)('body)('{ exec(code).void }) }

    /**
      * linear replication guard w/ pace w/ code
      */
    protected inline def silent(_t: true)(inline pace: FiniteDuration, inline parallelism: Int)(inline code: IO[Any])(inline body: IO[Any]): IO[Unit] =
      ${ silentCode('parallelism)('body)('{ exec(code) >> IO.sleep(pace) }) }


  object Macros:

    object τ:

      def silentCode(parallelism: Expr[Int])(body: Expr[IO[Any]])
                    (sleep: Expr[IO[Unit]])
                    (using Quotes): Expr[IO[Unit]] =
       '{ for
            linearCB <- CyclicBarrier[IO]($parallelism)
            unfold    = {
              def unfold(remaining: Int, prevS: Option[Semaphore[IO]]): IO[Unit] =
                for
                  nextS <- Semaphore[IO](0)
                  loop   = {
                    lazy val loop: IO[Unit] =
                      for
                        _ <- linearCB.await
                        _ <- prevS.fold(IO.unit)(_.acquire)
                        _ <- $sleep
                        _ <- nextS.release
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
        * linear replication bound output guard
        */
      def outputCode(arity: Expr[Int])(ref: Expr[>*<])
                    (parallelism: Expr[Int])(body: Expr[Seq[`()`] => IO[Any]])
                    (sleep: Expr[IO[Unit]])
                    (using Quotes): Expr[IO[Unit]] =
       '{ for
            linearCB <- CyclicBarrier[IO]($parallelism)
            stopR    <- IO.ref(false)
            unfold    = {
              def unfold(remaining: Int, prevS: Option[Semaphore[IO]]): IO[Unit] =
                for
                  nextS <- Semaphore[IO](0)
                  loop   = {
                    lazy val loop: IO[Unit] =
                      for
                        _ <- linearCB.await
                        _ <- prevS.fold(IO.unit)(_.acquire)
                        n <- ν.map(identity).replicateA($arity)
                        _ <- stopR.get.ifM(IO.unit,
                                           for
                                             opt <- ><(n.map(_.name))($ref)
                                             _   <- stopR.set(true).whenA(opt eq None)
                                           yield ())
                        _ <- stopR.get.ifM(nextS.release >> loop.unlessA(remaining == 1),
                                           $sleep >> nextS.release >> $body(n) >> loop)
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
      * linear constant replication output guard
      */
    def outputCode(ref: Expr[>*<])
                  (parallelism: Expr[Int], value: Expr[Seq[`()`]])(body: Expr[IO[Any]])
                  (sleep: Expr[IO[Unit]])
                  (using Quotes): Expr[IO[Unit]] =
     '{ for
          linearCB <- CyclicBarrier[IO]($parallelism)
          stopR    <- IO.ref(false)
          unfold    = {
            def unfold(remaining: Int, prevS: Option[Semaphore[IO]]): IO[Unit] =
              for
                nextS <- Semaphore[IO](0)
                loop   = {
                  lazy val loop: IO[Unit] =
                    for
                      _ <- linearCB.await
                      _ <- prevS.fold(IO.unit)(_.acquire)
                      _ <- stopR.get.ifM(IO.unit,
                                         for
                                           opt <- ><($value.map(_.name))($ref)
                                           _   <- stopR.set(true).whenA(opt eq None)
                                         yield ())
                      _ <- stopR.get.ifM(nextS.release >> loop.unlessA(remaining == 1),
                                         $sleep >> nextS.release >> $body >> loop)
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
        * linear variable replication output guard
        */
      def outputCode[S](ref: Expr[>*<])
                       (parallelism: Expr[Int], value: Expr[IO[Seq[S]]])(body: Expr[IO[Any]])
                       (sleep: Expr[IO[Unit]])
                       (using Type[S], Quotes): Expr[IO[Unit]] =
       '{ for
            linearCB <- CyclicBarrier[IO]($parallelism)
            stopR    <- IO.ref(false)
            unfold    = {
              def unfold(remaining: Int, prevS: Option[Semaphore[IO]]): IO[Unit] =
                for
                  nextS <- Semaphore[IO](0)
                  loop   = {
                    lazy val loop: IO[Unit] =
                      for
                        _ <- linearCB.await
                        _ <- prevS.fold(IO.unit)(_.acquire)
                        _ <- stopR.get.ifM(IO.unit,
                                           for
                                             opt <- $value.flatMap(><(_)($ref))
                                             _   <- stopR.set(true).whenA(opt eq None)
                                           yield ())
                        _ <- stopR.get.ifM(nextS.release >> loop.unlessA(remaining == 1),
                                           $sleep >> nextS.release >> $body >> loop)
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
      * linear replication input guard
      */
    def inputCode(ref: Expr[>*<])
                 (parallelism: Expr[Int])(body: Expr[Seq[`()`] => IO[Any]])
                 (sleep: Expr[IO[Unit]])
                 (using Quotes): Expr[IO[Unit]] =
     '{ for
          linearCB <- CyclicBarrier[IO]($parallelism)
          stopR    <- IO.ref(false)
          unfold    = {
            def unfold(remaining: Int, prevS: Option[Semaphore[IO]]): IO[Unit] =
              for
                nextS <- Semaphore[IO](0)
                loop   = {
                  lazy val loop: IO[Unit] =
                    for
                      _ <- linearCB.await
                      _ <- prevS.fold(IO.unit)(_.acquire)
                      n <- stopR.get.ifM(IO.pure(null),
                                         for
                                           name <- ><()($ref)
                                           _    <- stopR.set(true).whenA(name == null)
                                         yield name)
                      _ <- stopR.get.ifM(nextS.release >> loop.unlessA(remaining == 1),
                                         $sleep >> nextS.release >> $body(new `()`(n)) >> loop)
                    yield
                      ()
                  loop
                }
                _     <- if remaining == 1 then loop
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
      * linear replication input guard w/ code
      */
    def inputCode[T](ref: Expr[>*<])
                    (parallelism: Expr[Int])(code: Expr[Seq[T] => IO[Seq[T]]])(body: Expr[Seq[`()`] => IO[Any]])
                    (sleep: Expr[IO[Unit]])
                    (using Type[T], Quotes): Expr[IO[Unit]] =
     '{ for
          linearCB <- CyclicBarrier[IO]($parallelism)
          stopR    <- IO.ref(false)
          unfold    = {
            def unfold(remaining: Int, prevS: Option[Semaphore[IO]]): IO[Unit] =
              for
                nextS <- Semaphore[IO](0)
                loop   = {
                  lazy val loop: IO[Unit] =
                    for
                      _ <- linearCB.await
                      _ <- prevS.fold(IO.unit)(_.acquire)
                      n <- stopR.get.ifM(IO.pure(null),
                                         for
                                           name <- ><()($ref)($code)
                                           _    <- stopR.set(true).whenA(name == null)
                                         yield name)
                      _ <- stopR.get.ifM(nextS.release >> loop.unlessA(remaining == 1),
                                         $sleep >> nextS.release >> $body(new `()`(n)) >> loop)
                    yield
                      ()
                  loop
                }
                _     <- if remaining == 1 then loop
                         else loop.background.use { _ => unfold(remaining - 1, Some(nextS)) }
              yield
                ()
            unfold
          }
          _        <- unfold($parallelism, None)
        yield
          ()
      }
