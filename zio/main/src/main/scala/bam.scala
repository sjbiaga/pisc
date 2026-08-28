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

  import _root_.scala.quoted.{ Expr, Type, Quotes }

  import _root_.cats.effect.std.Semaphore
  import _root_.zio.interop.catz.generic.*

  import _root_.zio.{ Duration, Exit, FiberRef, Promise, Random, Ref, Task, UIO, ZIO }
  import _root_.zio.concurrent.CyclicBarrier

  import `Π-loop`.{ <>, +, %, /, \ }
  import `Π-stats`.Rate

  import `π-$`.*, `π-ζ`.*


  trait Macros extends Any { this: `()` =>

    import Macros.*

    // LINEAR REPLICATION ///////////////////////////////////////////////// π //

    /////////////////////////////////////////////////////////////////// BOUND //

    /**
      * linear replication bound output guard
      */
    protected inline def output(_nu: "ν")(_f: false)(inline parallelism: Int, inline rate: Rate)(inline key: String, inline `)(`: FiberRef[`)(`], inline dir: `π-$`)(inline body: `Π-Function1`)
                                                    (using inline % : %, inline / : /, inline \ : \)
                                                    (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ `(ν)`.outputCode('{ map(dir.ord) })('parallelism, 'rate)('key, '{`)(`}, 'dir)('body)('{ ZIO.unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear replication bound output guard w/ pace
      */
    protected inline def output(_nu: "ν")(_f: false)(inline pace: Duration, inline parallelism: Int, inline rate: Rate)(inline key: String, inline `)(`: FiberRef[`)(`], inline dir: `π-$`)(inline body: `Π-Function1`)
                                                    (using inline % : %, inline / : /, inline \ : \)
                                                    (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ `(ν)`.outputCode('{ map(dir.ord) })('parallelism, 'rate)('key, '{`)(`}, 'dir)('body)('{ ZIO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear replication bound output guard w/ code
      */
    protected inline def output(_nu: "ν")(_t: true)(inline parallelism: Int, inline rate: Rate)(inline key: String, inline `)(`: FiberRef[`)(`], inline dir: `π-$`)(inline code: => Task[Any])(inline body: `Π-Function1`)
                                                   (using inline % : %, inline / : /, inline \ : \)
                                                   (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ `(ν)`.outputCode('{ map(dir.ord) })('parallelism, 'rate)('key, '{`)(`}, 'dir)('body)('{ exec(code).unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear replication bound output guard w/ pace w/ code
      */
    protected inline def output(_nu: "ν")(_t: true)(inline pace: Duration, inline parallelism: Int, inline rate: Rate)(inline key: String, inline `)(`: FiberRef[`)(`], inline dir: `π-$`)(inline code: => Task[Any])(inline body: `Π-Function1`)
                                                   (using inline % : %, inline / : /, inline \ : \)
                                                   (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ `(ν)`.outputCode('{ map(dir.ord) })('parallelism, 'rate)('key, '{`)(`}, 'dir)('body)('{ exec(code) *> ZIO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    //////////////////////////////////////////////////////////////// CONSTANT //

    /**
      * linear constant replication output guard
      */
    protected inline def output(_f: false)(inline parallelism: Int, inline rate: Rate, inline value: `()`)(inline key: String, inline `)(`: FiberRef[`)(`], inline dir: `π-$`)(inline body: `Π-Function0`)
                                          (using inline % : %, inline / : /, inline \ : \)
                                          (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ outputCode('{ map(dir.ord) })('parallelism, 'rate, 'value)('key, '{`)(`}, 'dir)('body)('{ ZIO.unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear constant replication output guard w/ pace
      */
    protected inline def output(_f: false)(inline pace: Duration, inline parallelism: Int, inline rate: Rate, inline value: `()`)(inline key: String, inline `)(`: FiberRef[`)(`], inline dir: `π-$`)(inline body: `Π-Function0`)
                                          (using inline % : %, inline / : /, inline \ : \)
                                          (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ outputCode('{ map(dir.ord) })('parallelism, 'rate, 'value)('key, '{`)(`}, 'dir)('body)('{ ZIO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear constant replication output guard w/ code
      */
    protected inline def output(_t: true)(inline parallelism: Int, inline rate: Rate, inline value: `()`)(inline key: String, inline `)(`: FiberRef[`)(`], inline dir: `π-$`)(inline code: => Task[Any])(inline body: `Π-Function0`)
                                         (using inline % : %, inline / : /, inline \ : \)
                                         (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ outputCode('{ map(dir.ord) })('parallelism, 'rate, 'value)('key, '{`)(`}, 'dir)('body)('{ exec(code).unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear constant replication output guard w/ pace w/ code
      */
    protected inline def output(_t: true)(inline pace: Duration, inline parallelism: Int, inline rate: Rate, inline value: `()`)(inline key: String, inline `)(`: FiberRef[`)(`], inline dir: `π-$`)(inline code: => Task[Any])(inline body: `Π-Function0`)
                                         (using inline % : %, inline / : /, inline \ : \)
                                         (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ outputCode('{ map(dir.ord) })('parallelism, 'rate, 'value)('key, '{`)(`}, 'dir)('body)('{ exec(code) *> ZIO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    //////////////////////////////////////////////////////////////// VARIABLE //

    /**
      * linear variable replication output guard
      */
    protected inline def output[S](_s: "*")(_f: false)(inline parallelism: Int, inline rate: Rate, inline value: => Task[S])(inline key: String, inline `)(`: FiberRef[`)(`], inline dir: `π-$`)(inline body: `Π-Function0`)
                                                      (using inline % : %, inline / : /, inline \ : \)
                                                      (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ `(*)`.outputCode('{ map(dir.ord) })('parallelism, 'rate, '{ () => value })('key, '{`)(`}, 'dir)('body)('{ ZIO.unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear variable replication output guard w/ pace
      */
    protected inline def output[S](_s: "*")(_f: false)(inline pace: Duration, inline parallelism: Int, inline rate: Rate, inline value: => Task[S])(inline key: String, inline `)(`: FiberRef[`)(`], inline dir: `π-$`)(inline body: `Π-Function0`)
                                                      (using inline % : %, inline / : /, inline \ : \)
                                                      (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ `(*)`.outputCode('{ map(dir.ord) })('parallelism, 'rate, '{ () => value })('key, '{`)(`}, 'dir)('body)('{ ZIO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear variable replication output guard w/ code
      */
    protected inline def output[S](_s: "*")(_t: true)(inline parallelism: Int, inline rate: Rate, inline value: => Task[S])(inline key: String, inline `)(`: FiberRef[`)(`], inline dir: `π-$`)(inline code: => Task[Any])(inline body: `Π-Function0`)
                                                     (using inline % : %, inline / : /, inline \ : \)
                                                     (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ `(*)`.outputCode('{ map(dir.ord) })('parallelism, 'rate, '{ () => value })('key, '{`)(`}, 'dir)('body)('{ exec(code).unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear variable replication output guard w/ pace w/ code
      */
    protected inline def output[S](_s: "*")(_t: true)(inline pace: Duration, inline parallelism: Int, inline rate: Rate, inline value: => Task[S])(inline key: String, inline `)(`: FiberRef[`)(`], inline dir: `π-$`)(inline code: => Task[Any])(inline body: `Π-Function0`)
                                                     (using inline % : %, inline / : /, inline \ : \)
                                                     (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ `(*)`.outputCode('{ map(dir.ord) })('parallelism, 'rate, '{ () => value })('key, '{`)(`}, 'dir)('body)('{ exec(code) *> ZIO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /////////////////////////////////////////////////////////////////// INPUT //

    /**
      * linear replication input guard
      */
    protected inline def input(_f: false)(inline parallelism: Int, inline rate: Rate)(inline key: String, inline `)(`: FiberRef[`)(`], inline dir: `π-$`)(inline body: `Π-Function1`)
                                          (using inline % : %, inline / : /, inline \ : \)
                                          (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ inputCode('{ map(dir.ord) })('parallelism, 'rate)('key, '{`)(`}, 'dir)('body)('{ ZIO.unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear replication input guard w/ pace
      */
    protected inline def input(_f: false)(inline pace: Duration, inline parallelism: Int, inline rate: Rate)(inline key: String, inline `)(`: FiberRef[`)(`], inline dir: `π-$`)(inline body: `Π-Function1`)
                                          (using inline % : %, inline / : /, inline \ : \)
                                          (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ inputCode('{ map(dir.ord) })('parallelism, 'rate)('key, '{`)(`}, 'dir)('body)('{ ZIO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear replication input guard w/ code
      */
    protected inline def input[T](_t: true)(inline parallelism: Int, inline rate: Rate)(inline key: String, inline `)(`: FiberRef[`)(`], inline dir: `π-$`)(code: T => Task[T])(inline body: `Π-Function1`)
                                           (using inline % : %, inline / : /, inline \ : \)
                                           (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ inputCode('{ map(dir.ord) })('parallelism, 'rate)('key, '{`)(`}, 'dir)('{ code andThen exec })('body)('{ ZIO.unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear replication input guard w/ pace w/ code
      */
    protected inline def input[T](_t: true)(inline pace: Duration, inline parallelism: Int, inline rate: Rate)(inline key: String, inline `)(`: FiberRef[`)(`], inline dir: `π-$`)(code: T => Task[T])(inline body: `Π-Function1`)
                                           (using inline % : %, inline / : /, inline \ : \)
                                           (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ inputCode('{ map(dir.ord) })('parallelism, 'rate)('key, '{`)(`}, 'dir)('{ code andThen exec })('body)('{ ZIO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    // π ///////////////////////////////////////////////// linear replication //

    // LINEAR REPLICATION ///////////////////////////////////////////////// ζ //

    /**
      * linear capability replication guard
      */
    protected inline def capability(_f: false)(inline parallelism: Int, inline rate: Rate)(inline key: String, inline `)(`: FiberRef[`)(`], inline cap: `π-ζ`)(inline body: `Π-Function0`)
                                               (using inline % : %, inline / : /, inline \ : \)
                                               (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ capabilityCode('{ map(cap.ord) })('parallelism, 'rate)('key, '{`)(`}, 'cap)('body)('{ ZIO.unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear capability replication guard w/ pace
      */
    protected inline def capability(_f: false)(inline pace: Duration, inline parallelism: Int, inline rate: Rate)(inline key: String, inline `)(`: FiberRef[`)(`], inline cap: `π-ζ`)(inline body: `Π-Function0`)
                                               (using inline % : %, inline / : /, inline \ : \)
                                               (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ capabilityCode('{ map(cap.ord) })('parallelism, 'rate)('key, '{`)(`}, 'cap)('body)('{ ZIO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear capability replication guard w/ code
      */
    protected inline def capability(_t: true)(inline parallelism: Int, inline rate: Rate)(inline key: String, inline `)(`: FiberRef[`)(`], inline cap: `π-ζ`)(inline code: => Task[Any])(inline body: `Π-Function0`)
                                             (using inline % : %, inline / : /, inline \ : \)
                                             (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ capabilityCode('{ map(cap.ord) })('parallelism, 'rate)('key, '{`)(`}, 'cap)('body)('{ exec(code).unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear capability replication guard w/ pace w/ code
      */
    protected inline def capability(_t: true)(inline pace: Duration, inline parallelism: Int, inline rate: Rate)(inline key: String, inline `)(`: FiberRef[`)(`], inline cap: `π-ζ`)(inline code: => Task[Any])(inline body: `Π-Function0`)
                                             (using inline % : %, inline / : /, inline \ : \)
                                             (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ capabilityCode('{ map(cap.ord) })('parallelism, 'rate)('key, '{`)(`}, 'cap)('body)('{ exec(code) *> ZIO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    // ζ ///////////////////////////////////////////////// linear replication //

  }


  trait τ:

    protected val `new {}` = new {}

    import Macros.τ.*

    /**
      * linear replication guard
      */
    protected inline def silent(_f: false)(inline parallelism: Int, inline rate: Rate)(inline key: String, inline `)(`: FiberRef[`)(`], inline `π-τ`: Ordʹ)(inline body: `Π-Function0`)
                                          (using inline % : %, inline / : /, inline \ : \)
                                          (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ silentCode('{`new {}`})('parallelism, 'rate)('key, '{`)(`}, '{`π-τ`})('body)('{ ZIO.unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear replication guard w/ pace
      */
    protected inline def silent(_f: false)(inline pace: Duration, inline parallelism: Int, inline rate: Rate)(inline key: String, inline `)(`: FiberRef[`)(`], inline `π-τ`: Ordʹ)(inline body: `Π-Function0`)
                                          (using inline % : %, inline / : /, inline \ : \)
                                          (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ silentCode('{`new {}`})('parallelism, 'rate)('key, '{`)(`}, '{`π-τ`})('body)('{ ZIO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear replication guard w/ code
      */
    protected inline def silent(_t: true)(inline parallelism: Int, inline rate: Rate)(inline key: String, inline `)(`: FiberRef[`)(`], inline `π-τ`: Ordʹ)(inline code: => Task[Any])(inline body: `Π-Function0`)
                                         (using inline % : %, inline / : /, inline \ : \)
                                         (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ silentCode('{`new {}`})('parallelism, 'rate)('key, '{`)(`}, '{`π-τ`})('body)('{ exec(code).unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }

    /**
      * linear replication guard w/ pace w/ code
      */
    protected inline def silent(_t: true)(inline pace: Duration, inline parallelism: Int, inline rate: Rate)(inline key: String, inline `)(`: FiberRef[`)(`], inline `π-τ`: Ordʹ)(inline code: => Task[Any])(inline body: `Π-Function0`)
                                         (using inline % : %, inline / : /, inline \ : \)
                                         (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): UIO[Unit] =
      ${ silentCode('{`new {}`})('parallelism, 'rate)('key, '{`)(`}, '{`π-τ`})('body)('{ exec(code) *> ZIO.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^}) }


  object Macros:

    // duplicated method to avoid cyclic dependencies
    private def exclude(key: String)
                       (% : %)
                       (`π-elvis`: `Π-Map`[String, `Π-Set`[String]]): UIO[Unit] =
      ZIO.when(`π-elvis`.contains(key))(`π-exclude`(`π-elvis`(key))(using %)).unit

    object τ:

      /**
        * linear replication guard
        */
      def silentCode(ether: Expr[{}])
                    (parallelism: Expr[Int], rate: Expr[Rate])(key: Expr[String], `)(`: Expr[FiberRef[`)(`]], `π-τ`: Expr[Ordʹ])(body: Expr[`Π-Function0`])
                    (sleep: Expr[UIO[Unit]])
                    (% : Expr[%], / : Expr[/], \ : Expr[\])
                    (`π-elvis`: Expr[`Π-Map`[String, `Π-Set`[String]]], ^ : Expr[String])
                    (using Quotes): Expr[UIO[Unit]] =
       '{ for
            linearP  <- Promise.make[Nothing, Boolean]
            linearCB <- CyclicBarrier.make($parallelism)
            firstS   <- Semaphore[UIO](1)
            unfold    = {
              def unfold(remaining: Int, prevS: Semaphore[UIO], ints: List[Ref[Boolean]])(^ : String)(^^ : String): UIO[Unit] =
                val first = remaining == $parallelism
                val last = remaining == 1
                val intR = ints.head
                val interrupt = ZIO.collectAllDiscard(ints.map(_.set(true))) *> firstS.release *> ZIO.interrupt
                for
                  nextS <- if last then ZIO.succeed(firstS) else Semaphore[UIO](0)
                  sync   = linearCB.await.exit *> prevS.acquire *> intR.get.flatMap { if _ then nextS.release *> ZIO.interrupt else ZIO.unit }
                  main   =
                    for
                      stop <- if first then ZIO.succeed(false) else linearP.await
                      _    <- (interrupt.when(last) *> ZIO.interrupt).when(stop)
                      loop <- ( for
                                  _        <- exclude($key)(${%})(${`π-elvis`}).when(first)
                                  continue <- Promise.make[Nothing, Option[<>]].flatMap(Ref.make)
                                  promise  <- Promise.make[Nothing, Option[<>]]
                                  _        <- promise.succeed(None).unless(first)
                                  `)(`     <- ${`)(`}.get
                                  _        <- ${/}.offer(^ -> $key -> ((promise -> continue, `)(` -> ${`π-τ`}), ($ether, None, $rate)))
                                  opt      <- promise.await
                                  _        <- (linearP.succeed(opt eq None) *> (interrupt.when(last) *> ZIO.interrupt).when(opt eq None)).when(first)
                                yield {
                                  def loop(enabled: Boolean = first): UIO[Unit] =
                                    for
                                      _   <- sync
                                      _   <- ${\}(${%}.update { m => m + (^ + $key -> (true, m(^ + $key).asInstanceOf[(Boolean, +)]._2)) }).unless(enabled)
                                      opt <- continue.get.flatMap(_.await)
                                      _   <- ((linearCB.await.exit *> interrupt).when(last) *> nextS.release *> sync).when(opt eq None)
                                      _   <- Promise.make[Nothing, Option[<>]].flatMap(continue.set)
                                      (_, b,
                                       f, _) = opt.get
                                      _   <- b.await.exit
                                      _   <- f.join
                                      _   <- $sleep
                                      _   <- nextS.release
                                      _   <- $body()(using ^^).exit
                                      _   <- loop(false)
                                    yield ()
                                  loop()
                                }
                              )
                      _    <- loop
                    yield
                      ()
                  intR  <- Ref.make(false)
                  _     <- if last
                           then main
                           else ZIO.scoped(main.forkScoped *> Random.nextUUID.map(_.toString).flatMap(unfold(remaining - 1, nextS, intR :: ints)(^^)))
                yield
                  ()
              unfold
            }
            intR     <- Ref.make(false)
            _        <- if $parallelism == 1
                        then unfold($parallelism, firstS, intR :: Nil)(${^})(${^}).exit
                        else Random.nextUUID.map(_.toString).flatMap(unfold($parallelism, firstS, intR :: Nil)(${^})(_).exit)
          yield
            ()
        }


    object `(ν)`:

      /**
        * linear replication bound output guard
        */
      def outputCode(ether: Expr[{}])
                    (parallelism: Expr[Int], rate: Expr[Rate])(key: Expr[String], `)(`: Expr[FiberRef[`)(`]], dir: Expr[`π-$`])(body: Expr[`Π-Function1`])
                    (sleep: Expr[UIO[Unit]])
                    (% : Expr[%], / : Expr[/], \ : Expr[\])
                    (`π-elvis`: Expr[`Π-Map`[String, `Π-Set`[String]]], ^ : Expr[String])
                    (using Quotes): Expr[UIO[Unit]] =
       '{ for
            linearP  <- Promise.make[Nothing, Boolean]
            linearCB <- CyclicBarrier.make($parallelism)
            firstS   <- Semaphore[UIO](1)
            unfold    = {
              def unfold(remaining: Int, prevS: Semaphore[UIO], ints: List[Ref[Boolean]])(^ : String)(^^ : String): UIO[Unit] =
                val first = remaining == $parallelism
                val last = remaining == 1
                val intR = ints.head
                val interrupt = ZIO.collectAllDiscard(ints.map(_.set(true))) *> firstS.release *> ZIO.interrupt
                for
                  nextS <- if last then ZIO.succeed(firstS) else Semaphore[UIO](0)
                  sync   = linearCB.await.exit *> prevS.acquire *> intR.get.flatMap { if _ then nextS.release *> ZIO.interrupt else ZIO.unit }
                  main   =
                    for
                      stop <- if first then ZIO.succeed(false) else linearP.await
                      _    <- (interrupt.when(last) *> ZIO.interrupt).when(stop)
                      loop <- ( for
                                  _        <- exclude($key)(${%})(${`π-elvis`}).when(first)
                                  continue <- Promise.make[Nothing, Option[<>]].flatMap(Ref.make)
                                  promise  <- Promise.make[Nothing, Option[<>]]
                                  _        <- promise.succeed(None).unless(first)
                                  `)(`     <- ${`)(`}.get
                                  _        <- ${/}.offer(^ -> $key -> ((promise -> continue, `)(` -> $dir), ($ether, Some(Left(())), $rate)))
                                  opt      <- promise.await
                                  _        <- (linearP.succeed(opt eq None) *> (interrupt.when(last) *> ZIO.interrupt).when(opt eq None)).when(first)
                                yield {
                                  def loop(enabled: Boolean = first): UIO[Unit] =
                                    for
                                      _   <- sync
                                      _   <- ${\}(${%}.update { m => m + (^ + $key -> (true, m(^ + $key).asInstanceOf[(Boolean, +)]._2)) }).unless(enabled)
                                      opt <- continue.get.flatMap(_.await)
                                      _   <- ((linearCB.await.exit *> interrupt).when(last) *> nextS.release *> sync).when(opt eq None)
                                      _   <- Promise.make[Nothing, Option[<>]].flatMap(continue.set)
                                      (_, b,
                                       f, i) = opt.get
                                      n   <- ν
                                      _   <- i.set(n)
                                      _   <- b.await.exit
                                      _   <- f.join
                                      _   <- $sleep
                                      _   <- nextS.release
                                      _   <- $body(n)(using ^^).exit
                                      _   <- loop(false)
                                    yield ()
                                  loop()
                                }
                              )
                      _    <- loop
                    yield
                      ()
                  intR  <- Ref.make(false)
                  _     <- if last
                           then main
                           else ZIO.scoped(main.forkScoped *> Random.nextUUID.map(_.toString).flatMap(unfold(remaining - 1, nextS, intR :: ints)(^^)))
                yield
                  ()
              unfold
            }
            intR     <- Ref.make(false)
            _        <- if $parallelism == 1
                        then unfold($parallelism, firstS, intR :: Nil)(${^})(${^}).exit
                        else Random.nextUUID.map(_.toString).flatMap(unfold($parallelism, firstS, intR :: Nil)(${^})(_).exit)
          yield
            ()
        }


    /**
      * linear constant replication output guard
      */
    def outputCode(ether: Expr[{}])
                  (parallelism: Expr[Int], rate: Expr[Rate], value: Expr[`()`])(key: Expr[String], `)(`: Expr[FiberRef[`)(`]], dir: Expr[`π-$`])(body: Expr[`Π-Function0`])
                  (sleep: Expr[UIO[Unit]])
                  (% : Expr[%], / : Expr[/], \ : Expr[\])
                  (`π-elvis`: Expr[`Π-Map`[String, `Π-Set`[String]]], ^ : Expr[String])
                  (using Quotes): Expr[UIO[Unit]] =
     '{ for
          linearP  <- Promise.make[Nothing, Boolean]
          linearCB <- CyclicBarrier.make($parallelism)
          firstS   <- Semaphore[UIO](1)
          unfold    = {
            def unfold(remaining: Int, prevS: Semaphore[UIO], ints: List[Ref[Boolean]])(^ : String)(^^ : String): UIO[Unit] =
              val first = remaining == $parallelism
              val last = remaining == 1
              val intR = ints.head
              val interrupt = ZIO.collectAllDiscard(ints.map(_.set(true))) *> firstS.release *> ZIO.interrupt
              for
                nextS <- if last then ZIO.succeed(firstS) else Semaphore[UIO](0)
                sync   = linearCB.await.exit *> prevS.acquire *> intR.get.flatMap { if _ then nextS.release *> ZIO.interrupt else ZIO.unit }
                main   =
                  for
                    stop <- if first then ZIO.succeed(false) else linearP.await
                    _    <- (interrupt.when(last) *> ZIO.interrupt).when(stop)
                    loop <- ( for
                                _        <- exclude($key)(${%})(${`π-elvis`}).when(first)
                                continue <- Promise.make[Nothing, Option[<>]].flatMap(Ref.make)
                                promise  <- Promise.make[Nothing, Option[<>]]
                                _        <- promise.succeed(None).unless(first)
                                `)(`     <- ${`)(`}.get
                                _        <- ${/}.offer(^ -> $key -> ((promise -> continue, `)(` -> $dir), ($ether, Some(Left(())), $rate)))
                                opt      <- promise.await
                                _        <- (linearP.succeed(opt eq None) *> (interrupt.when(last) *> ZIO.interrupt).when(opt eq None)).when(first)
                              yield {
                                def loop(enabled: Boolean = first): UIO[Unit] =
                                  for
                                    _   <- sync
                                    _   <- ${\}(${%}.update { m => m + (^ + $key -> (true, m(^ + $key).asInstanceOf[(Boolean, +)]._2)) }).unless(enabled)
                                    opt <- continue.get.flatMap(_.await)
                                    _   <- ((linearCB.await.exit *> interrupt).when(last) *> nextS.release *> sync).when(opt eq None)
                                    _   <- Promise.make[Nothing, Option[<>]].flatMap(continue.set)
                                    (_, b,
                                     f, i) = opt.get
                                    _   <- i.set($value)
                                    _   <- b.await.exit
                                    _   <- f.join
                                    _   <- $sleep
                                    _   <- nextS.release
                                    _   <- $body()(using ^^).exit
                                    _   <- loop(false)
                                  yield ()
                                loop()
                              }
                            )
                    _    <- loop
                  yield
                    ()
                intR  <- Ref.make(false)
                _     <- if last
                         then main
                         else ZIO.scoped(main.forkScoped *> Random.nextUUID.map(_.toString).flatMap(unfold(remaining - 1, nextS, intR :: ints)(^^)))
              yield
                ()
            unfold
          }
          intR     <- Ref.make(false)
          _        <- if $parallelism == 1
                      then unfold($parallelism, firstS, intR :: Nil)(${^})(${^}).exit
                      else Random.nextUUID.map(_.toString).flatMap(unfold($parallelism, firstS, intR :: Nil)(${^})(_).exit)
        yield
          ()
      }


    object `(*)`:

      /**
        * linear variable replication output guard
        */
      def outputCode[S](ether: Expr[{}])
                       (parallelism: Expr[Int], rate: Expr[Rate], value: Expr[() => Task[S]])(key: Expr[String], `)(`: Expr[FiberRef[`)(`]], dir: Expr[`π-$`])(body: Expr[`Π-Function0`])
                       (sleep: Expr[UIO[Unit]])
                       (% : Expr[%], / : Expr[/], \ : Expr[\])
                       (`π-elvis`: Expr[`Π-Map`[String, `Π-Set`[String]]], ^ : Expr[String])
                       (using Type[S], Quotes): Expr[UIO[Unit]] =
       '{ for
            linearP  <- Promise.make[Nothing, Boolean]
            linearCB <- CyclicBarrier.make($parallelism)
            firstS   <- Semaphore[UIO](1)
            unfold    = {
              def unfold(remaining: Int, prevS: Semaphore[UIO], ints: List[Ref[Boolean]])(^ : String)(^^ : String): UIO[Unit] =
                val first = remaining == $parallelism
                val last = remaining == 1
                val intR = ints.head
                val interrupt = ZIO.collectAllDiscard(ints.map(_.set(true))) *> firstS.release *> ZIO.interrupt
                for
                  nextS <- if last then ZIO.succeed(firstS) else Semaphore[UIO](0)
                  sync   = linearCB.await.exit *> prevS.acquire *> intR.get.flatMap { if _ then nextS.release *> ZIO.interrupt else ZIO.unit }
                  main   =
                    for
                      stop <- if first then ZIO.succeed(false) else linearP.await
                      _    <- (interrupt.when(last) *> ZIO.interrupt).when(stop)
                      loop <- ( for
                                  _        <- exclude($key)(${%})(${`π-elvis`}).when(first)
                                  continue <- Promise.make[Nothing, Option[<>]].flatMap(Ref.make)
                                  promise  <- Promise.make[Nothing, Option[<>]]
                                  _        <- promise.succeed(None).unless(first)
                                  `)(`     <- ${`)(`}.get
                                  _        <- ${/}.offer(^ -> $key -> ((promise -> continue, `)(` -> $dir), ($ether, Some(Left(())), $rate)))
                                  opt      <- promise.await
                                  _        <- (linearP.succeed(opt eq None) *> (interrupt.when(last) *> ZIO.interrupt).when(opt eq None)).when(first)
                                yield {
                                  def loop(enabled: Boolean = first): UIO[Unit] =
                                    for
                                      _   <- sync
                                      _   <- ${\}(${%}.update { m => m + (^ + $key -> (true, m(^ + $key).asInstanceOf[(Boolean, +)]._2)) }).unless(enabled)
                                      opt <- continue.get.flatMap(_.await)
                                      _   <- ((linearCB.await.exit *> interrupt).when(last) *> nextS.release *> sync).when(opt eq None)
                                      _   <- Promise.make[Nothing, Option[<>]].flatMap(continue.set)
                                      (_, b,
                                       f, i) = opt.get
                                      _   <- $value().map(new `()`(_)).flatMap(i.set)
                                      _   <- b.await.exit
                                      _   <- f.join
                                      _   <- $sleep
                                      _   <- nextS.release
                                      _   <- $body()(using ^^).exit
                                      _   <- loop(false)
                                    yield ()
                                  loop()
                                }
                              )
                      _    <- loop
                    yield
                      ()
                  intR  <- Ref.make(false)
                  _     <- if last
                           then main
                           else ZIO.scoped(main.forkScoped *> Random.nextUUID.map(_.toString).flatMap(unfold(remaining - 1, nextS, intR :: ints)(^^)))
                yield
                  ()
              unfold
            }
            intR     <- Ref.make(false)
            _        <- if $parallelism == 1
                        then unfold($parallelism, firstS, intR :: Nil)(${^})(${^}).exit
                        else Random.nextUUID.map(_.toString).flatMap(unfold($parallelism, firstS, intR :: Nil)(${^})(_).exit)
          yield
            ()
        }


    /**
      * linear replication input guard
      */
    def inputCode(ether: Expr[{}])
                 (parallelism: Expr[Int], rate: Expr[Rate])(key: Expr[String], `)(`: Expr[FiberRef[`)(`]], dir: Expr[`π-$`])(body: Expr[`Π-Function1`])
                 (sleep: Expr[UIO[Unit]])
                 (% : Expr[%], / : Expr[/], \ : Expr[\])
                 (`π-elvis`: Expr[`Π-Map`[String, `Π-Set`[String]]], ^ : Expr[String])
                 (using Quotes): Expr[UIO[Unit]] =
     '{ for
          linearP  <- Promise.make[Nothing, Boolean]
          linearCB <- CyclicBarrier.make($parallelism)
          firstS   <- Semaphore[UIO](1)
          unfold    = {
            def unfold(remaining: Int, prevS: Semaphore[UIO], ints: List[Ref[Boolean]])(^ : String)(^^ : String): UIO[Unit] =
              val first = remaining == $parallelism
              val last = remaining == 1
              val intR = ints.head
              val interrupt = ZIO.collectAllDiscard(ints.map(_.set(true))) *> firstS.release *> ZIO.interrupt
              for
                nextS <- if last then ZIO.succeed(firstS) else Semaphore[UIO](0)
                sync   = linearCB.await.exit *> prevS.acquire *> intR.get.flatMap { if _ then nextS.release *> ZIO.interrupt else ZIO.unit }
                main   =
                  for
                    stop <- if first then ZIO.succeed(false) else linearP.await
                    _    <- (interrupt.when(last) *> ZIO.interrupt).when(stop)
                    loop <- ( for
                                _        <- exclude($key)(${%})(${`π-elvis`}).when(first)
                                continue <- Promise.make[Nothing, Option[<>]].flatMap(Ref.make)
                                promise  <- Promise.make[Nothing, Option[<>]]
                                _        <- promise.succeed(None).unless(first)
                                result   <- Ref.make(`null`)
                                `)(`     <- ${`)(`}.get
                                _        <- ${/}.offer(^ -> $key -> ((promise -> continue, `)(` -> $dir), ($ether, Some(Right(result)), $rate)))
                                opt      <- promise.await
                                _        <- (linearP.succeed(opt eq None) *> (interrupt.when(last) *> ZIO.interrupt).when(opt eq None)).when(first)
                              yield {
                                def loop(enabled: Boolean = first): UIO[Unit] =
                                  for
                                    _   <- sync
                                    _   <- ${\}(${%}.update { m => m + (^ + $key -> (true, m(^ + $key).asInstanceOf[(Boolean, +)]._2)) }).unless(enabled)
                                    opt <- continue.get.flatMap(_.await)
                                    _   <- ((linearCB.await.exit *> interrupt).when(last) *> nextS.release *> sync).when(opt eq None)
                                    _   <- Promise.make[Nothing, Option[<>]].flatMap(continue.set)
                                    (_, b,
                                     f, _) = opt.get
                                    _   <- b.await.exit
                                    _   <- f.join
                                    n   <- result.get
                                    _   <- $sleep
                                    _   <- nextS.release
                                    _   <- $body(n)(using ^^).exit
                                    _   <- loop(false)
                                  yield ()
                                loop()
                              }
                            )
                    _    <- loop
                  yield
                    ()
                intR  <- Ref.make(false)
                _     <- if last
                         then main
                         else ZIO.scoped(main.forkScoped *> Random.nextUUID.map(_.toString).flatMap(unfold(remaining - 1, nextS, intR :: ints)(^^)))
              yield
                ()
            unfold
          }
          intR     <- Ref.make(false)
          _        <- if $parallelism == 1
                      then unfold($parallelism, firstS, intR :: Nil)(${^})(${^}).exit
                      else Random.nextUUID.map(_.toString).flatMap(unfold($parallelism, firstS, intR :: Nil)(${^})(_).exit)
        yield
          ()
      }

    /**
      * linear replication input guard w/ code
      */
    def inputCode[T](ether: Expr[{}])
                    (parallelism: Expr[Int], rate: Expr[Rate])(key: Expr[String], `)(`: Expr[FiberRef[`)(`]], dir: Expr[`π-$`])(code: Expr[T => Task[T]])(body: Expr[`Π-Function1`])
                    (sleep: Expr[UIO[Unit]])
                    (% : Expr[%], / : Expr[/], \ : Expr[\])
                    (`π-elvis`: Expr[`Π-Map`[String, `Π-Set`[String]]], ^ : Expr[String])
                    (using Type[T], Quotes): Expr[UIO[Unit]] =
     '{ for
          linearP  <- Promise.make[Nothing, Boolean]
          linearCB <- CyclicBarrier.make($parallelism)
          firstS   <- Semaphore[UIO](1)
          unfold    = {
            def unfold(remaining: Int, prevS: Semaphore[UIO], ints: List[Ref[Boolean]])(^ : String)(^^ : String): UIO[Unit] =
              val first = remaining == $parallelism
              val last = remaining == 1
              val intR = ints.head
              val interrupt = ZIO.collectAllDiscard(ints.map(_.set(true))) *> firstS.release *> ZIO.interrupt
              for
                nextS <- if last then ZIO.succeed(firstS) else Semaphore[UIO](0)
                sync   = linearCB.await.exit *> prevS.acquire *> intR.get.flatMap { if _ then nextS.release *> ZIO.interrupt else ZIO.unit }
                main   =
                  for
                    stop <- if first then ZIO.succeed(false) else linearP.await
                    _    <- (interrupt.when(last) *> ZIO.interrupt).when(stop)
                    loop <- ( for
                                _        <- exclude($key)(${%})(${`π-elvis`}).when(first)
                                continue <- Promise.make[Nothing, Option[<>]].flatMap(Ref.make)
                                promise  <- Promise.make[Nothing, Option[<>]]
                                _        <- promise.succeed(None).unless(first)
                                result   <- Ref.make(`null`)
                                `)(`     <- ${`)(`}.get
                                _        <- ${/}.offer(^ -> $key -> ((promise -> continue, `)(` -> $dir), ($ether, Some(Right(result)), $rate)))
                                opt      <- promise.await
                                _        <- (linearP.succeed(opt eq None) *> (interrupt.when(last) *> ZIO.interrupt).when(opt eq None)).when(first)
                              yield {
                                def loop(enabled: Boolean = first): UIO[Unit] =
                                  for
                                    _   <- sync
                                    _   <- ${\}(${%}.update { m => m + (^ + $key -> (true, m(^ + $key).asInstanceOf[(Boolean, +)]._2)) }).unless(enabled)
                                    opt <- continue.get.flatMap(_.await)
                                    _   <- ((linearCB.await.exit *> interrupt).when(last) *> nextS.release *> sync).when(opt eq None)
                                    _   <- Promise.make[Nothing, Option[<>]].flatMap(continue.set)
                                    (_, b,
                                     f, _) = opt.get
                                    _   <- b.await.exit
                                    _   <- f.join
                                    n   <- result.get.map(_.name.asInstanceOf[T]).flatMap($code).map(new `()`(_))
                                    _   <- $sleep
                                    _   <- nextS.release
                                    _   <- $body(n)(using ^^).exit
                                    _   <- loop(false)
                                  yield ()
                                loop()
                              }
                            )
                    _    <- loop
                  yield
                    ()
                intR  <- Ref.make(false)
                _     <- if last
                         then main
                         else ZIO.scoped(main.forkScoped *> Random.nextUUID.map(_.toString).flatMap(unfold(remaining - 1, nextS, intR :: ints)(^^)))
              yield
                ()
            unfold
          }
          intR     <- Ref.make(false)
          _        <- if $parallelism == 1
                      then unfold($parallelism, firstS, intR :: Nil)(${^})(${^}).exit
                      else Random.nextUUID.map(_.toString).flatMap(unfold($parallelism, firstS, intR :: Nil)(${^})(_).exit)
        yield
          ()
      }

    /**
      * linear capability replication guard
      */
    def capabilityCode(ether: Expr[{}])
                      (parallelism: Expr[Int], rate: Expr[Rate])(key: Expr[String], `)(`: Expr[FiberRef[`)(`]], cap: Expr[`π-ζ`])(body: Expr[`Π-Function0`])
                      (sleep: Expr[UIO[Unit]])
                      (% : Expr[%], / : Expr[/], \ : Expr[\])
                      (`π-elvis`: Expr[`Π-Map`[String, `Π-Set`[String]]], ^ : Expr[String])
                      (using Quotes): Expr[UIO[Unit]] =
     '{ for
          linearP  <- Promise.make[Nothing, Boolean]
          linearCB <- CyclicBarrier.make($parallelism)
          firstS   <- Semaphore[UIO](1)
          unfold    = {
            def unfold(remaining: Int, prevS: Semaphore[UIO], ints: List[Ref[Boolean]])(^ : String)(^^ : String): UIO[Unit] =
              val first = remaining == $parallelism
              val last = remaining == 1
              val intR = ints.head
              val interrupt = ZIO.collectAllDiscard(ints.map(_.set(true))) *> firstS.release *> ZIO.interrupt
              for
                nextS <- if last then ZIO.succeed(firstS) else Semaphore[UIO](0)
                sync   = linearCB.await.exit *> prevS.acquire *> intR.get.flatMap { if _ then nextS.release *> ZIO.interrupt else ZIO.unit }
                main   =
                  for
                    stop <- if first then ZIO.succeed(false) else linearP.await
                    _    <- (interrupt.when(last) *> ZIO.interrupt).when(stop)
                    loop <- ( for
                                _        <- exclude($key)(${%})(${`π-elvis`}).when(first)
                                continue <- Promise.make[Nothing, Option[<>]].flatMap(Ref.make)
                                promise  <- Promise.make[Nothing, Option[<>]]
                                _        <- promise.succeed(None).unless(first)
                                polarity  = $cap == `π-enter` || $cap == `π-exit` || $cap == `π-merge+`
                                `)(`     <- ${`)(`}.get
                                _        <- ${/}.offer(^ -> $key -> ((promise -> continue, `)(` -> $cap), ($ether, Some(if polarity then Right(null) else Left(())), $rate)))
                                opt      <- promise.await
                                _        <- (linearP.succeed(opt eq None) *> (interrupt.when(last) *> ZIO.interrupt).when(opt eq None)).when(first)
                              yield {
                                def loop(enabled: Boolean = first): UIO[Unit] =
                                  for
                                    _   <- sync
                                    _   <- ${\}(${%}.update { m => m + (^ + $key -> (true, m(^ + $key).asInstanceOf[(Boolean, +)]._2)) }).unless(enabled)
                                    opt <- continue.get.flatMap(_.await)
                                    _   <- ((linearCB.await.exit *> interrupt).when(last) *> nextS.release *> sync).when(opt eq None)
                                    _   <- Promise.make[Nothing, Option[<>]].flatMap(continue.set)
                                    (_, b,
                                     f, _) = opt.get
                                    _   <- b.await.exit
                                    _   <- f.join
                                    _   <- $sleep
                                    _   <- nextS.release
                                    _   <- $body()(using ^^).exit
                                    _   <- loop(false)
                                  yield ()
                                loop()
                              }
                            )
                    _    <- loop
                  yield
                    ()
                intR  <- Ref.make(false)
                _     <- if last
                         then main
                         else ZIO.scoped(main.forkScoped *> Random.nextUUID.map(_.toString).flatMap(unfold(remaining - 1, nextS, intR :: ints)(^^)))
              yield
                ()
            unfold
          }
          intR     <- Ref.make(false)
          _        <- if $parallelism == 1
                      then unfold($parallelism, firstS, intR :: Nil)(${^})(${^}).exit
                      else Random.nextUUID.map(_.toString).flatMap(unfold($parallelism, firstS, intR :: Nil)(${^})(_).exit)
        yield
          ()
      }

    // duplicated value to avoid cyclic dependencies
    private val `null` = new `()`(null)
