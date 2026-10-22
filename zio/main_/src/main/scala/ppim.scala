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

  import _root_.scala.quoted.{ Expr, Type, Quotes }

  import _root_.cats.effect.std.Semaphore
  import _root_.zio.interop.catz.generic.*

  import _root_.zio.{ Duration, Ref, Task, UIO, ZIO }
  import _root_.zio.concurrent.CyclicBarrier

  import Π.`Π-magic`.{ ><, >*< }


  trait Macros extends Any { this: `()` =>

    import Macros.*

    // MODULO REPLICATION //////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////// BOUND //

    /**
      * modulo replication bound output guard
      */
    protected inline def output(_nu: "ν")(_f: false)(inline arity: Int)(inline parallelism: Int)(inline body: Seq[`()`] => UIO[Any]): UIO[Unit] =
      ${ `(ν)`.outputCode('arity)('ref)('parallelism)('body)('{ ZIO.unit }) }

    /**
      * modulo replication bound output guard w/ pace
      */
    protected inline def output(_nu: "ν")(_f: false)(inline arity: Int)(inline pace: Duration, inline parallelism: Int)(inline body: Seq[`()`] => UIO[Any]): UIO[Unit] =
      ${ `(ν)`.outputCode('arity)('ref)('parallelism)('body)('{ ZIO.sleep(pace) }) }

    /**
      * modulo replication bound output guard w/ code
      */
    protected inline def output(_nu: "ν")(_t: true)(inline arity: Int)(inline parallelism: Int)(inline code: UIO[Any])(inline body: Seq[`()`] => UIO[Any]): UIO[Unit] =
      ${ `(ν)`.outputCode('arity)('ref)('parallelism)('body)('{ exec(code).unit }) }

    /**
      * modulo replication bound output guard w/ pace w/ code
      */
    protected inline def output(_nu: "ν")(_t: true)(inline arity: Int)(inline pace: Duration, inline parallelism: Int)(inline code: UIO[Any])(inline body: Seq[`()`] => UIO[Any]): UIO[Unit] =
      ${ `(ν)`.outputCode('arity)('ref)('parallelism)('{ exec(code) *> body(_) })('{ exec(code) *> ZIO.sleep(pace) }) }

    //////////////////////////////////////////////////////////////// CONSTANT //

    /**
      * modulo constant replication output guard
      */
    protected inline def output(_f: false)(inline parallelism: Int, inline value: `()`*)(inline body: UIO[Any]): UIO[Unit] =
      ${ outputCode('ref)('parallelism, 'value)('body)('{ ZIO.unit }) }

    /**
      * modulo constant replication output guard w/ pace
      */
    protected inline def output(_f: false)(inline pace: Duration, inline parallelism: Int, inline value: `()`*)(inline body: UIO[Any]): UIO[Unit] =
      ${ outputCode('ref)('parallelism, 'value)('body)('{ ZIO.sleep(pace) }) }

    /**
      * modulo constant replication output guard w/ code
      */
    protected inline def output(_t: true)(inline parallelism: Int, inline value: `()`*)(inline code: UIO[Any])(inline body: UIO[Any]): UIO[Unit] =
      ${ outputCode('ref)('parallelism, 'value)('body)('{ exec(code).unit }) }

    /**
      * modulo constant replication output guard w/ pace w/ code
      */
    protected inline def output(_t: true)(inline pace: Duration, inline parallelism: Int, inline value: `()`*)(inline code: UIO[Any])(inline body: UIO[Any]): UIO[Unit] =
      ${ outputCode('ref)('parallelism, 'value)('body)('{ exec(code) *> ZIO.sleep(pace) }) }

    //////////////////////////////////////////////////////////////// VARIABLE //

    /**
      * modulo variable replication output guard
      */
    protected inline def output[S](_s: "*")(_f: false)(inline parallelism: Int, inline value: Task[Seq[S]])(inline body: UIO[Any]): UIO[Unit] =
     ${ `(*)`.outputCode('ref)('parallelism, 'value)('body)('{ ZIO.unit }) }

    /**
      * modulo variable replication output guard w/ pace
      */
    protected inline def output[S](_s: "*")(_f: false)(inline pace: Duration, inline parallelism: Int, inline value: Task[Seq[S]])(inline body: UIO[Any]): UIO[Unit] =
     ${ `(*)`.outputCode('ref)('parallelism, 'value)('body)('{ ZIO.sleep(pace) }) }

    /**
      * modulo variable replication output guard w/ code
      */
    protected inline def output[S](_s: "*")(_t: true)(inline parallelism: Int, inline value: Task[Seq[S]])(inline code: UIO[Any])(inline body: UIO[Any]): UIO[Unit] =
     ${ `(*)`.outputCode('ref)('parallelism, 'value)('body)('{ exec(code).unit }) }

    /**
      * modulo variable replication output guard w/ pace w/ code
      */
    protected inline def output[S](_s: "*")(_t: true)(inline pace: Duration, inline parallelism: Int, inline value: Task[Seq[S]])(inline code: UIO[Any])(inline body: UIO[Any]): UIO[Unit] =
     ${ `(*)`.outputCode('ref)('parallelism, 'value)('body)('{ exec(code) *> ZIO.sleep(pace) }) }

    /////////////////////////////////////////////////////////////////// INPUT //

    /**
      * modulo replication input guard
      */
    protected inline def input(_f: false)(inline parallelism: Int)(inline body: Seq[`()`] => UIO[Any]): UIO[Unit] =
      ${ inputCode('ref)('parallelism)('body)('{ ZIO.unit }) }

    /**
      * modulo replication input guard w/ pace
      */
    protected inline def input(_f: false)(inline pace: Duration, inline parallelism: Int)(inline body: Seq[`()`] => UIO[Any]): UIO[Unit] =
      ${ inputCode('ref)('parallelism)('body)('{ ZIO.sleep(pace) }) }

    /**
      * modulo replication input guard w/ code
      */
    protected inline def input[T](_t: true)(inline parallelism: Int)(code: Seq[T] => Task[Seq[T]])(inline body: Seq[`()`] => UIO[Any]): UIO[Unit] =
      ${ inputCode('ref)('parallelism)('code)('body)('{ ZIO.unit }) }

    /**
      * modulo replication input guard w/ pace w/ code
      */
    protected inline def input[T](_t: true)(inline pace: Duration, inline parallelism: Int)(code: Seq[T] => Task[Seq[T]])(inline body: Seq[`()`] => UIO[Any]): UIO[Unit] =
      ${ inputCode('ref)('parallelism)('code)('body)('{ ZIO.sleep(pace) }) }

    ////////////////////////////////////////////////////// modulo replication //

  }


  trait τ:

    import Macros.τ.*

    /**
      * modulo replication guard
      */
    protected inline def silent(_f: false)(inline parallelism: Int)(inline body: UIO[Any]): UIO[Unit] =
      ${ silentCode('parallelism)('body)('{ ZIO.unit }) }

    /**
      * modulo replication guard w/ pace
      */
    protected inline def silent(_f: false)(inline pace: Duration, inline parallelism: Int)(inline body: UIO[Any]): UIO[Unit] =
      ${ silentCode('parallelism)('body)('{ ZIO.sleep(pace) }) }

    /**
      * modulo replication guard w/ code
      */
    protected inline def silent(_t: true)(inline parallelism: Int)(inline code: UIO[Any])(inline body: UIO[Any]): UIO[Unit] =
      ${ silentCode('parallelism)('body)('{ exec(code).unit }) }

    /**
      * modulo replication guard w/ pace w/ code
      */
    protected inline def silent(_t: true)(inline pace: Duration, inline parallelism: Int)(inline code: UIO[Any])(inline body: UIO[Any]): UIO[Unit] =
      ${ silentCode('parallelism)('body)('{ exec(code) *> ZIO.sleep(pace) }) }


  object Macros:

    object τ:

      def silentCode(parallelism: Expr[Int])(body: Expr[UIO[Any]])
                    (sleep: Expr[UIO[Unit]])
                    (using Quotes): Expr[UIO[Unit]] =
       '{ for
            moduloCB <- CyclicBarrier.make($parallelism)
            unfold    = {
              def unfold(remaining: Int, prevS: Option[Semaphore[UIO]]): UIO[Unit] =
                for
                  nextS <- Semaphore[UIO](0)
                  loop   = {
                    lazy val loop: UIO[Unit] =
                      for
                        _ <- moduloCB.await.exit
                        _ <- prevS.fold(ZIO.unit)(_.acquire)
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
                           else ZIO.scoped(loop.forkScoped *> unfold(remaining - 1, Some(nextS)))
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
      def outputCode(arity: Expr[Int])(ref: Expr[>*<])
                    (parallelism: Expr[Int])(body: Expr[Seq[`()`] => UIO[Any]])
                    (sleep: Expr[UIO[Unit]])
                    (using Quotes): Expr[UIO[Unit]] =
       '{ for
            moduloCB <- CyclicBarrier.make($parallelism)
            stopR    <- Ref.make(false)
            unfold    = {
              def unfold(remaining: Int, prevS: Option[Semaphore[UIO]]): UIO[Unit] =
                for
                  nextS <- Semaphore[UIO](0)
                  loop   = {
                    lazy val loop: UIO[Unit] =
                      for
                        _ <- moduloCB.await.exit
                        _ <- prevS.fold(ZIO.unit)(_.acquire)
                        n <- ZIO.replicateZIO($arity)(ν.map(identity)).map(_.toSeq)
                        _ <- stopR.get.flatMap {
                          if _
                          then
                            ZIO.unit
                          else
                            for
                              opt <- ><(n.map(_.name))($ref)
                              _   <- stopR.set(true).when(opt eq None)
                            yield ()
                        }
                        _ <- stopR.get.flatMap {
                          if _
                          then
                            nextS.release *> ZIO.never.unless(remaining == 1)
                          else
                            $sleep *> nextS.release *> $body(n) *> loop
                        }
                      yield
                        ()
                    loop
                  }
                  _     <- if remaining == 1
                           then loop
                           else ZIO.scoped(loop.forkScoped *> unfold(remaining - 1, Some(nextS)))
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
    def outputCode(ref: Expr[>*<])
                  (parallelism: Expr[Int], value: Expr[Seq[`()`]])(body: Expr[UIO[Any]])
                  (sleep: Expr[UIO[Unit]])
                  (using Quotes): Expr[UIO[Unit]] =
     '{ for
          moduloCB <- CyclicBarrier.make($parallelism)
          stopR    <- Ref.make(false)
          unfold    = {
            def unfold(remaining: Int, prevS: Option[Semaphore[UIO]]): UIO[Unit] =
              for
                nextS <- Semaphore[UIO](0)
                loop   = {
                  lazy val loop: UIO[Unit] =
                    for
                      _ <- moduloCB.await.exit
                      _ <- prevS.fold(ZIO.unit)(_.acquire)
                      _ <- stopR.get.flatMap {
                        if _
                        then
                          ZIO.unit
                        else
                          for
                            opt <- ><($value.map(_.name))($ref)
                            _   <- stopR.set(true).when(opt eq None)
                          yield ()
                      }
                      _ <- stopR.get.flatMap {
                        if _
                        then
                          nextS.release *> ZIO.never.unless(remaining == 1)
                        else
                          $sleep *> nextS.release *> $body *> loop
                      }
                    yield
                      ()
                  loop
                }
                _     <- if remaining == 1
                         then loop
                         else ZIO.scoped(loop.forkScoped *> unfold(remaining - 1, Some(nextS)))
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
      def outputCode[S](ref: Expr[>*<])
                       (parallelism: Expr[Int], value: Expr[Task[Seq[S]]])(body: Expr[UIO[Any]])
                       (sleep: Expr[UIO[Unit]])
                       (using Type[S], Quotes): Expr[UIO[Unit]] =
       '{ for
            moduloCB <- CyclicBarrier.make($parallelism)
            stopR    <- Ref.make(false)
            unfold    = {
              def unfold(remaining: Int, prevS: Option[Semaphore[UIO]]): UIO[Unit] =
                for
                  nextS <- Semaphore[UIO](0)
                  loop   = {
                    lazy val loop: UIO[Unit] =
                      for
                        _ <- moduloCB.await.exit
                        _ <- prevS.fold(ZIO.unit)(_.acquire)
                        _ <- stopR.get.flatMap {
                          if _
                          then
                            ZIO.unit
                          else
                            for
                              opt <- $value.flatMap(><(_)($ref))
                              _   <- stopR.set(true).when(opt eq None)
                            yield ()
                        }
                        _ <- stopR.get.flatMap {
                          if _
                          then
                            nextS.release *> ZIO.never.unless(remaining == 1)
                          else
                            $sleep *> nextS.release *> $body *> loop
                        }
                      yield
                        ()
                    loop
                  }
                  _     <- if remaining == 1
                           then loop
                           else ZIO.scoped(loop.forkScoped *> unfold(remaining - 1, Some(nextS)))
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
    def inputCode(ref: Expr[>*<])
                 (parallelism: Expr[Int])(body: Expr[Seq[`()`] => UIO[Any]])
                 (sleep: Expr[UIO[Unit]])
                 (using Quotes): Expr[UIO[Unit]] =
     '{ for
          moduloCB <- CyclicBarrier.make($parallelism)
          stopR    <- Ref.make(false)
          unfold    = {
            def unfold(remaining: Int, prevS: Option[Semaphore[UIO]]): UIO[Unit] =
              for
                nextS <- Semaphore[UIO](0)
                loop   = {
                  lazy val loop: UIO[Unit] =
                    for
                      _ <- moduloCB.await.exit
                      _ <- prevS.fold(ZIO.unit)(_.acquire)
                      n <- stopR.get.flatMap {
                        if _
                        then
                          ZIO.succeed(null)
                        else
                          for
                            name <- ><()($ref)
                            _    <- stopR.set(true).when(name.head == null)
                          yield
                            name
                      }
                      _ <- stopR.get.flatMap {
                        if _
                        then
                          nextS.release *> ZIO.never.unless(remaining == 1)
                        else
                          $sleep *> nextS.release *> $body(n.map(new `()`(_))) *> loop
                      }
                    yield
                      ()
                  loop
                }
                _     <- if remaining == 1 then loop
                         else ZIO.scoped(loop.forkScoped *> unfold(remaining - 1, Some(nextS)))
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
    def inputCode[T](ref: Expr[>*<])
                    (parallelism: Expr[Int])(code: Expr[Seq[T] => Task[Seq[T]]])(body: Expr[Seq[`()`] => UIO[Any]])
                    (sleep: Expr[UIO[Unit]])
                    (using Type[T], Quotes): Expr[UIO[Unit]] =
     '{ for
          moduloCB <- CyclicBarrier.make($parallelism)
          stopR    <- Ref.make(false)
          unfold    = {
            def unfold(remaining: Int, prevS: Option[Semaphore[UIO]]): UIO[Unit] =
              for
                nextS <- Semaphore[UIO](0)
                loop   = {
                  lazy val loop: UIO[Unit] =
                    for
                      _ <- moduloCB.await.exit
                      _ <- prevS.fold(ZIO.unit)(_.acquire)
                      n <- stopR.get.flatMap {
                        if _
                        then
                          ZIO.succeed(null)
                        else
                          for
                            name <- ><()($ref)($code)
                            _    <- stopR.set(true).when(name.head == null)
                          yield
                            name
                      }
                      _ <- stopR.get.flatMap {
                        if _
                        then
                          nextS.release *> ZIO.never.unless(remaining == 1)
                        else
                          $sleep *> nextS.release *> $body(n.map(new `()`(_))) *> loop
                      }
                    yield
                      ()
                  loop
                }
                _     <- if remaining == 1 then loop
                         else ZIO.scoped(loop.forkScoped *> unfold(remaining - 1, Some(nextS)))
              yield
                ()
            unfold
          }
          _        <- unfold($parallelism, None)
        yield
          ()
      }
