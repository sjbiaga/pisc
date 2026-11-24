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

  import _root_.scala.reflect.{ ClassTag, classTag }

  import _root_.scala.quoted.{ Expr, Type, Quotes }

  import _root_.cats.syntax.applicative.*
  import _root_.cats.syntax.flatMap.*
  import _root_.cats.syntax.functor.*

  import _root_.cats.{ Applicative, Defer, Monad }
  import _root_.cats.data.OptionT
  import _root_.cats.effect.{ Deferred, Ref, Temporal }
  import _root_.cats.effect.std.{ CyclicBarrier, Semaphore, UUIDGen }
  import _root_.cats.effect.syntax.spawn.*

  import `Π-loop`.{ <>, +, %, /, \ }
  import `Π-stats`.Rate


  trait Macros[F[_]: Defer: Temporal: UUIDGen] { this: `()`[F] =>

    import Macros.*

    // MODULO REPLICATION //////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////// BOUND //

    /**
      * modulo replication bound output guard
      */
    protected inline def output(_nu: "ν")(_f: false)(inline parallelism: Int, inline rate: Rate)(inline key: String)(inline body: `Π-Function1`[F])
                                                    (using inline % : %[F], inline / : /[F], inline \ : \[F])
                                                    (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): OptionT[F, Unit] =
      ${ `(ν)`.outputCode('{ `()`[`{}`[F]] })('parallelism, 'rate)('key)('body)('{ OptionT.unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^})('{ summon[Temporal[F]] }, '{ summon[Defer[F]] }, '{ summon[UUIDGen[F]] }) }

    /**
      * modulo replication bound output guard w/ pace
      */
    protected inline def output(_nu: "ν")(_f: false)(inline pace: FiniteDuration, inline parallelism: Int, inline rate: Rate)(inline key: String)(inline body: `Π-Function1`[F])
                                                    (using inline % : %[F], inline / : /[F], inline \ : \[F])
                                                    (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): OptionT[F, Unit] =
      ${ `(ν)`.outputCode('{ `()`[`{}`[F]] })('parallelism, 'rate)('key)('body)('{ OptionT.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^})('{ summon[Temporal[F]] }, '{ summon[Defer[F]] }, '{ summon[UUIDGen[F]] }) }

    /**
      * modulo replication bound output guard w/ code
      */
    protected inline def output(_nu: "ν")(_t: true)(inline parallelism: Int, inline rate: Rate)(inline key: String)(inline code: => OptionT[F, Any])(inline body: `Π-Function1`[F])
                                                   (using inline % : %[F], inline / : /[F], inline \ : \[F])
                                                   (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): OptionT[F, Unit] =
      ${ `(ν)`.outputCode('{ `()`[`{}`[F]] })('parallelism, 'rate)('key)('body)('{ code.void })('{%}, '{/}, '{\})('{`π-elvis`}, '{^})('{ summon[Temporal[F]] }, '{ summon[Defer[F]] }, '{ summon[UUIDGen[F]] }) }

    /**
      * modulo replication bound output guard w/ pace w/ code
      */
    protected inline def output(_nu: "ν")(_t: true)(inline pace: FiniteDuration, inline parallelism: Int, inline rate: Rate)(inline key: String)(inline code: => OptionT[F, Any])(inline body: `Π-Function1`[F])
                                                   (using inline % : %[F], inline / : /[F], inline \ : \[F])
                                                   (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): OptionT[F, Unit] =
      ${ `(ν)`.outputCode('{ `()`[`{}`[F]] })('parallelism, 'rate)('key)('body)('{ code >> OptionT.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^})('{ summon[Temporal[F]] }, '{ summon[Defer[F]] }, '{ summon[UUIDGen[F]] }) }

    //////////////////////////////////////////////////////////////// CONSTANT //

    /**
      * modulo constant replication output guard
      */
    protected inline def output(_f: false)(inline parallelism: Int, inline rate: Rate, inline value: `()`[F])(inline key: String)(inline body: `Π-Function0`[F])
                                          (using inline % : %[F], inline / : /[F], inline \ : \[F])
                                          (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): OptionT[F, Unit] =
      ${ outputCode('{ `()`[`{}`[F]] })('parallelism, 'rate, 'value)('key)('body)('{ OptionT.unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^})('{ summon[Temporal[F]] }, '{ summon[UUIDGen[F]] }) }

    /**
      * modulo constant replication output guard w/ pace
      */
    protected inline def output(_f: false)(inline pace: FiniteDuration, inline parallelism: Int, inline rate: Rate, inline value: `()`[F])(inline key: String)(inline body: `Π-Function0`[F])
                                          (using inline % : %[F], inline / : /[F], inline \ : \[F])
                                          (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): OptionT[F, Unit] =
      ${ outputCode('{ `()`[`{}`[F]] })('parallelism, 'rate, 'value)('key)('body)('{ OptionT.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^})('{ summon[Temporal[F]] }, '{ summon[UUIDGen[F]] }) }

    /**
      * modulo constant replication output guard w/ code
      */
    protected inline def output(_t: true)(inline parallelism: Int, inline rate: Rate, inline value: `()`[F])(inline key: String)(inline code: => OptionT[F, Any])(inline body: `Π-Function0`[F])
                                         (using inline % : %[F], inline / : /[F], inline \ : \[F])
                                         (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): OptionT[F, Unit] =
      ${ outputCode('{ `()`[`{}`[F]] })('parallelism, 'rate, 'value)('key)('body)('{ code.void })('{%}, '{/}, '{\})('{`π-elvis`}, '{^})('{ summon[Temporal[F]] }, '{ summon[UUIDGen[F]] }) }

    /**
      * modulo constant replication output guard w/ pace w/ code
      */
    protected inline def output(_t: true)(inline pace: FiniteDuration, inline parallelism: Int, inline rate: Rate, inline value: `()`[F])(inline key: String)(inline code: => OptionT[F, Any])(inline body: `Π-Function0`[F])
                                         (using inline % : %[F], inline / : /[F], inline \ : \[F])
                                         (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): OptionT[F, Unit] =
      ${ outputCode('{ `()`[`{}`[F]] })('parallelism, 'rate, 'value)('key)('body)('{ code >> OptionT.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^})('{ summon[Temporal[F]] }, '{ summon[UUIDGen[F]] }) }

    //////////////////////////////////////////////////////////////// VARIABLE //

    /**
      * modulo variable replication output guard
      */
    protected inline def output[S](_s: "*")(_f: false)(inline parallelism: Int, inline rate: Rate, inline value: => OptionT[F, S])(inline key: String)(inline body: `Π-Function0`[F])
                                                      (using inline % : %[F], inline / : /[F], inline \ : \[F])
                                                      (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): OptionT[F, Unit] =
      ${ `(*)`.outputCode('{ `()`[`{}`[F]] })('parallelism, 'rate, '{ () => value })('key)('body)('{ OptionT.unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^})('{ summon[Temporal[F]] }, '{ summon[Defer[F]] }, '{ summon[UUIDGen[F]] }) }

    /**
      * modulo variable replication output guard w/ pace
      */
    protected inline def output[S](_s: "*")(_f: false)(inline pace: FiniteDuration, inline parallelism: Int, inline rate: Rate, inline value: => OptionT[F, S])(inline key: String)(inline body: `Π-Function0`[F])
                                                      (using inline % : %[F], inline / : /[F], inline \ : \[F])
                                                      (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): OptionT[F, Unit] =
      ${ `(*)`.outputCode('{ `()`[`{}`[F]] })('parallelism, 'rate, '{ () => value })('key)('body)('{ OptionT.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^})('{ summon[Temporal[F]] }, '{ summon[Defer[F]] }, '{ summon[UUIDGen[F]] }) }

    /**
      * modulo variable replication output guard w/ code
      */
    protected inline def output[S](_s: "*")(_t: true)(inline parallelism: Int, inline rate: Rate, inline value: => OptionT[F, S])(inline key: String)(inline code: => OptionT[F, Any])(inline body: `Π-Function0`[F])
                                                     (using inline % : %[F], inline / : /[F], inline \ : \[F])
                                                     (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): OptionT[F, Unit] =
      ${ `(*)`.outputCode('{ `()`[`{}`[F]] })('parallelism, 'rate, '{ () => value })('key)('body)('{ code.void })('{%}, '{/}, '{\})('{`π-elvis`}, '{^})('{ summon[Temporal[F]] }, '{ summon[Defer[F]] }, '{ summon[UUIDGen[F]] }) }

    /**
      * modulo variable replication output guard w/ pace w/ code
      */
    protected inline def output[S](_s: "*")(_t: true)(inline pace: FiniteDuration, inline parallelism: Int, inline rate: Rate, inline value: => OptionT[F, S])(inline key: String)(inline code: => OptionT[F, Any])(inline body: `Π-Function0`[F])
                                                     (using inline % : %[F], inline / : /[F], inline \ : \[F])
                                                     (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): OptionT[F, Unit] =
      ${ `(*)`.outputCode('{ `()`[`{}`[F]] })('parallelism, 'rate, '{ () => value })('key)('body)('{ code >> OptionT.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^})('{ summon[Temporal[F]] }, '{ summon[Defer[F]] }, '{ summon[UUIDGen[F]] }) }

    /////////////////////////////////////////////////////////////////// INPUT //

    /**
      * modulo replication input guard
      */
    protected inline def input(_f: false)(inline parallelism: Int, inline rate: Rate)(inline key: String)(inline body: `Π-Function1`[F])
                                          (using inline % : %[F], inline / : /[F], inline \ : \[F])
                                          (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): OptionT[F, Unit] =
      ${ inputCode('{ `()`[`{}`[F]] })('parallelism, 'rate)('key)('body)('{ OptionT.unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^})('{ summon[Temporal[F]] }, '{ summon[UUIDGen[F]] }) }

    /**
      * modulo replication input guard w/ pace
      */
    protected inline def input(_f: false)(inline pace: FiniteDuration, inline parallelism: Int, inline rate: Rate)(inline key: String)(inline body: `Π-Function1`[F])
                                          (using inline % : %[F], inline / : /[F], inline \ : \[F])
                                          (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): OptionT[F, Unit] =
      ${ inputCode('{ `()`[`{}`[F]] })('parallelism, 'rate)('key)('body)('{ OptionT.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^})('{ summon[Temporal[F]] }, '{ summon[UUIDGen[F]] }) }

    /**
      * modulo replication input guard w/ code
      */
    protected inline def input[T: ClassTag](_t: true)(inline parallelism: Int, inline rate: Rate)(inline key: String)(code: T => OptionT[F, Any])(inline body: `Π-Function1`[F])
                                                     (using inline % : %[F], inline / : /[F], inline \ : \[F])
                                                     (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): OptionT[F, Unit] =
      ${ inputCode('{ `()`[`{}`[F]] })('parallelism, 'rate)('key)('code)('body)('{ OptionT.unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^})('{ summon[Temporal[F]] }, '{ summon[Defer[F]] }, '{ summon[UUIDGen[F]] }, '{ classTag[T] }) }

    /**
      * modulo replication input guard w/ pace w/ code
      */
    protected inline def input[T: ClassTag](_t: true)(inline pace: FiniteDuration, inline parallelism: Int, inline rate: Rate)(inline key: String)(code: T => OptionT[F, Any])(inline body: `Π-Function1`[F])
                                                     (using inline % : %[F], inline / : /[F], inline \ : \[F])
                                                     (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): OptionT[F, Unit] =
      ${ inputCode('{ `()`[`{}`[F]] })('parallelism, 'rate)('key)('code)('body)('{ OptionT.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^})('{ summon[Temporal[F]] }, '{ summon[Defer[F]] }, '{ summon[UUIDGen[F]] }, '{ classTag[T] }) }

    ////////////////////////////////////////////////////// modulo replication //

  }


  trait τMacros[F[_]: Temporal: UUIDGen]:

    protected val `None {}`: `{}`[F] = None

    import Macros.τ.*

    /**
      * modulo replication guard
      */
    protected inline def silent(_f: false)(inline parallelism: Int, inline rate: Rate)(inline key: String)(inline body: `Π-Function0`[F])
                                          (using inline % : %[F], inline / : /[F], inline \ : \[F])
                                          (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): OptionT[F, Unit] =
      ${ silentCode('{`None {}`})('parallelism, 'rate)('key)('body)('{ OptionT.unit })('{%}, '{/}, '{\})('{`π-elvis`}, '{^})('{ summon[Temporal[F]] }, '{ summon[UUIDGen[F]] }) }

    /**
      * modulo replication guard w/ pace
      */
    protected inline def silent(_f: false)(inline pace: FiniteDuration, inline parallelism: Int, inline rate: Rate)(inline key: String)(inline body: `Π-Function0`[F])
                                          (using inline % : %[F], inline / : /[F], inline \ : \[F])
                                          (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): OptionT[F, Unit] =
      ${ silentCode('{`None {}`})('parallelism, 'rate)('key)('body)('{ OptionT.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^})('{ summon[Temporal[F]] }, '{ summon[UUIDGen[F]] }) }

    /**
      * modulo replication guard w/ code
      */
    protected inline def silent(_t: true)(inline parallelism: Int, inline rate: Rate)(inline key: String)(inline code: => OptionT[F, Any])(inline body: `Π-Function0`[F])
                                         (using inline % : %[F], inline / : /[F], inline \ : \[F])
                                         (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): OptionT[F, Unit] =
      ${ silentCode('{`None {}`})('parallelism, 'rate)('key)('body)('{ code.void })('{%}, '{/}, '{\})('{`π-elvis`}, '{^})('{ summon[Temporal[F]] }, '{ summon[UUIDGen[F]] }) }

    /**
      * modulo replication guard w/ pace w/ code
      */
    protected inline def silent(_t: true)(inline pace: FiniteDuration, inline parallelism: Int, inline rate: Rate)(inline key: String)(inline code: => OptionT[F, Any])(inline body: `Π-Function0`[F])
                                         (using inline % : %[F], inline / : /[F], inline \ : \[F])
                                         (using inline `π-elvis`: `Π-Map`[String, `Π-Set`[String]], inline ^ : String): OptionT[F, Unit] =
      ${ silentCode('{`None {}`})('parallelism, 'rate)('key)('body)('{ code >> OptionT.sleep(pace) })('{%}, '{/}, '{\})('{`π-elvis`}, '{^})('{ summon[Temporal[F]] }, '{ summon[UUIDGen[F]] }) }


  object Macros:

    // duplicated method to avoid cyclic dependencies
    private def exclude[F[_]: Applicative](key: String)
                                          (% : %[F])
                                          (`π-elvis`: `Π-Map`[String, `Π-Set`[String]]): F[Unit] =
      `π-exclude`(`π-elvis`(key))(using %).whenA(`π-elvis`.contains(key))

    object τ:

      /**
        * modulo replication guard
        */
      def silentCode[F[_]](ether: Expr[`{}`[F]])
                          (parallelism: Expr[Int], rate: Expr[Rate])(key: Expr[String])(body: Expr[`Π-Function0`[F]])
                          (sleep: Expr[OptionT[F, Unit]])
                          (% : Expr[%[F]], / : Expr[/[F]], \ : Expr[\[F]])
                          (`π-elvis`: Expr[`Π-Map`[String, `Π-Set`[String]]], ^ : Expr[String])
                          (temporalF: Expr[Temporal[F]], uuidGenF: Expr[UUIDGen[F]])
                          (using Quotes, Type[F]): Expr[OptionT[F, Unit]] =
       '{ given Temporal[F] = $temporalF
          given UUIDGen[F] = $uuidGenF
          for
            moduloCB <- OptionT.liftF(CyclicBarrier[F]($parallelism))
            moduloD  <- OptionT.liftF(Deferred[F, Boolean])
            moduloS  <- OptionT.liftF(Semaphore[F](0))
            unfold    = {
              def unfold(remaining: Int)(^ : String)(^^ : String): OptionT[F, Unit] =
                val first = remaining == $parallelism
                val last = remaining == 1
                val main =
                  for
                    _    <- OptionT.unlessM(if first then Temporal[F].pure(false) else moduloD.get)(Temporal[F].unit)
                    loop <- ( for
                                _        <- OptionT.liftF(exclude($key)(${%})(${`π-elvis`}).whenA(first))
                                continue <- OptionT.liftF(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
                                deferred <- OptionT.liftF(Deferred[F, Option[<>[F]]])
                                _        <- OptionT.liftF(deferred.complete(None).unlessA(first))
                                _        <- OptionT.liftF(${/}.offer(^ -> $key -> (deferred -> continue, ($ether, None, $rate))))
                                _        <- OptionT.liftF(deferred.get)
                                              .semiflatten
                                              .flatTapNone(moduloD.complete(true).whenA(first))
                                              .orElse(OptionT.unless(first)(null))
                                _        <- OptionT.liftF(moduloD.complete(false).whenA(first))
                              yield {
                                def loop(enabled: Boolean = first): OptionT[F, Unit] =
                                  for
                                    _     <- OptionT.liftF(moduloS.acquire.unlessA(enabled))
                                    _     <- OptionT.liftF(${\}(${%}.update { m => m + (^ + $key -> (true, m(^ + $key).asInstanceOf[(Boolean, +[F])]._2)) }).unlessA(enabled))
                                    tuple <- OptionT.liftF(continue.get.flatMap(_.get)).semiflatten.flatTapNone(moduloS.release >> moduloCB.await)
                                    _     <- OptionT.liftF(Deferred[F, Option[<>[F]]] >>= continue.set)
                                    (b, f,
                                     _   ) = tuple
                                    _     <- OptionT.liftF(b.await)
                                    _     <- OptionT.liftF(f.join)
                                    _     <- $sleep
                                    _     <- OptionT.liftF(moduloS.release)
                                    _     <- $body()(using ^^).orElse(OptionT.unit[F])
                                    _     <- loop(false)
                                  yield ()
                                loop()
                              }
                            )
                    _    <- loop
                  yield
                    ()
                if last
                then main
                else main.background.use { _ => OptionT.liftF(UUIDGen.randomString[F]) >>= unfold(remaining - 1)(^^) }
              unfold
            }
            _        <- if $parallelism == 1
                        then unfold($parallelism)(${^})(${^})
                        else OptionT.liftF(UUIDGen.randomString[F]) >>= unfold($parallelism)(${^})
          yield
            ()
        }


    object `(ν)`:

      /**
        * modulo replication bound output guard
        */
      def outputCode[F[_]](ether: Expr[`{}`[F]])
                          (parallelism: Expr[Int], rate: Expr[Rate])(key: Expr[String])(body: Expr[`Π-Function1`[F]])
                          (sleep: Expr[OptionT[F, Unit]])
                          (% : Expr[%[F]], / : Expr[/[F]], \ : Expr[\[F]])
                          (`π-elvis`: Expr[`Π-Map`[String, `Π-Set`[String]]], ^ : Expr[String])
                          (temporalF: Expr[Temporal[F]], deferF: Expr[Defer[F]], uuidGenF: Expr[UUIDGen[F]])
                          (using Quotes, Type[F]): Expr[OptionT[F, Unit]] =
       '{ given Temporal[F] = $temporalF
          given Defer[F] = $deferF
          given UUIDGen[F] = $uuidGenF
          for
            moduloCB <- OptionT.liftF(CyclicBarrier[F]($parallelism))
            moduloD  <- OptionT.liftF(Deferred[F, Boolean])
            moduloS  <- OptionT.liftF(Semaphore[F](0))
            unfold    = {
              def unfold(remaining: Int)(^ : String)(^^ : String): OptionT[F, Unit] =
                val first = remaining == $parallelism
                val last = remaining == 1
                val main =
                  for
                    _    <- OptionT.unlessM(if first then Temporal[F].pure(false) else moduloD.get)(Temporal[F].unit)
                    loop <- ( for
                                _        <- OptionT.liftF(exclude($key)(${%})(${`π-elvis`}).whenA(first))
                                continue <- OptionT.liftF(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
                                deferred <- OptionT.liftF(Deferred[F, Option[<>[F]]])
                                _        <- OptionT.liftF(deferred.complete(None).unlessA(first))
                                _        <- OptionT.liftF(${/}.offer(^ -> $key -> (deferred -> continue, ($ether, Some(Left(())), $rate))))
                                _        <- OptionT.liftF(deferred.get)
                                              .semiflatten
                                              .flatTapNone(moduloD.complete(true).whenA(first))
                                              .orElse(OptionT.unless(first)(null))
                                _        <- OptionT.liftF(moduloD.complete(false).whenA(first))
                              yield {
                                def loop(enabled: Boolean = first): OptionT[F, Unit] =
                                  for
                                    _     <- OptionT.liftF(moduloS.acquire.unlessA(enabled))
                                    _     <- OptionT.liftF(${\}(${%}.update { m => m + (^ + $key -> (true, m(^ + $key).asInstanceOf[(Boolean, +[F])]._2)) }).unlessA(enabled))
                                    tuple <- OptionT.liftF(continue.get.flatMap(_.get)).semiflatten.flatTapNone(moduloS.release >> moduloCB.await)
                                    _     <- OptionT.liftF(Deferred[F, Option[<>[F]]] >>= continue.set)
                                    (b, f,
                                     i   ) = tuple
                                    n     <- ν[F]
                                    _     <- OptionT.liftF(i.set(n))
                                    _     <- OptionT.liftF(b.await)
                                    _     <- OptionT.liftF(f.join)
                                    _     <- $sleep
                                    _     <- OptionT.liftF(moduloS.release)
                                    _     <- $body(n)(using ^^).orElse(OptionT.unit[F])
                                    _     <- loop(false)
                                  yield ()
                                loop()
                              }
                            )
                    _    <- loop
                  yield
                    ()
                if last
                then main
                else main.background.use { _ => OptionT.liftF(UUIDGen.randomString[F]) >>= unfold(remaining - 1)(^^) }
              unfold
            }
            _        <- if $parallelism == 1
                        then unfold($parallelism)(${^})(${^})
                        else OptionT.liftF(UUIDGen.randomString[F]) >>= unfold($parallelism)(${^})
          yield
            ()
        }

    /**
      * modulo constant replication output guard
      */
    def outputCode[F[_]](ether: Expr[`{}`[F]])
                        (parallelism: Expr[Int], rate: Expr[Rate], value: Expr[`()`[F]])(key: Expr[String])(body: Expr[`Π-Function0`[F]])
                        (sleep: Expr[OptionT[F, Unit]])
                        (% : Expr[%[F]], / : Expr[/[F]], \ : Expr[\[F]])
                        (`π-elvis`: Expr[`Π-Map`[String, `Π-Set`[String]]], ^ : Expr[String])
                        (temporalF: Expr[Temporal[F]], uuidGenF: Expr[UUIDGen[F]])
                        (using Quotes, Type[F]): Expr[OptionT[F, Unit]] =
     '{ given Temporal[F] = $temporalF
        given UUIDGen[F] = $uuidGenF
        for
          moduloCB <- OptionT.liftF(CyclicBarrier[F]($parallelism))
          moduloD  <- OptionT.liftF(Deferred[F, Boolean])
          moduloS  <- OptionT.liftF(Semaphore[F](0))
          unfold    = {
            def unfold(remaining: Int)(^ : String)(^^ : String): OptionT[F, Unit] =
              val first = remaining == $parallelism
              val last = remaining == 1
              val main =
                for
                  _    <- OptionT.unlessM(if first then Temporal[F].pure(false) else moduloD.get)(Temporal[F].unit)
                  loop <- ( for
                              _        <- OptionT.liftF(exclude($key)(${%})(${`π-elvis`}).whenA(first))
                              continue <- OptionT.liftF(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
                              deferred <- OptionT.liftF(Deferred[F, Option[<>[F]]])
                              _        <- OptionT.liftF(deferred.complete(None).unlessA(first))
                              _        <- OptionT.liftF(${/}.offer(^ -> $key -> (deferred -> continue, ($ether, Some(Left(())), $rate))))
                              _        <- OptionT.liftF(deferred.get)
                                            .semiflatten
                                            .flatTapNone(moduloD.complete(true).whenA(first))
                                            .orElse(OptionT.unless(first)(null))
                              _        <- OptionT.liftF(moduloD.complete(false).whenA(first))
                            yield {
                              def loop(enabled: Boolean = first): OptionT[F, Unit] =
                                for
                                  _     <- OptionT.liftF(moduloS.acquire.unlessA(enabled))
                                  _     <- OptionT.liftF(${\}(${%}.update { m => m + (^ + $key -> (true, m(^ + $key).asInstanceOf[(Boolean, +[F])]._2)) }).unlessA(enabled))
                                  tuple <- OptionT.liftF(continue.get.flatMap(_.get)).semiflatten.flatTapNone(moduloS.release >> moduloCB.await)
                                  _     <- OptionT.liftF(Deferred[F, Option[<>[F]]] >>= continue.set)
                                  (b, f,
                                   i   ) = tuple
                                  _     <- OptionT.liftF(i.set($value))
                                  _     <- OptionT.liftF(b.await)
                                  _     <- OptionT.liftF(f.join)
                                  _     <- $sleep
                                  _     <- OptionT.liftF(moduloS.release)
                                  _     <- $body()(using ^^).orElse(OptionT.unit[F])
                                  _     <- loop(false)
                                yield ()
                              loop()
                            }
                          )
                  _    <- loop
                yield
                  ()
              if last
              then main
              else main.background.use { _ => OptionT.liftF(UUIDGen.randomString[F]) >>= unfold(remaining - 1)(^^) }
            unfold
          }
          _        <- if $parallelism == 1
                      then unfold($parallelism)(${^})(${^})
                      else OptionT.liftF(UUIDGen.randomString[F]) >>= unfold($parallelism)(${^})
        yield
          ()
      }


    object `(*)`:

      /**
        * modulo variable replication output guard
        */
      def outputCode[F[_], S](ether: Expr[`{}`[F]])
                          (parallelism: Expr[Int], rate: Expr[Rate], value: Expr[() => OptionT[F, S]])(key: Expr[String])(body: Expr[`Π-Function0`[F]])
                          (sleep: Expr[OptionT[F, Unit]])
                          (% : Expr[%[F]], / : Expr[/[F]], \ : Expr[\[F]])
                          (`π-elvis`: Expr[`Π-Map`[String, `Π-Set`[String]]], ^ : Expr[String])
                          (temporalF: Expr[Temporal[F]], deferF: Expr[Defer[F]], uuidGenF: Expr[UUIDGen[F]])
                          (using Quotes, Type[F], Type[S]): Expr[OptionT[F, Unit]] =
       '{ given Temporal[F] = $temporalF
          given Defer[F] = $deferF
          given UUIDGen[F] = $uuidGenF
          for
            moduloCB <- OptionT.liftF(CyclicBarrier[F]($parallelism))
            moduloD  <- OptionT.liftF(Deferred[F, Boolean])
            moduloS  <- OptionT.liftF(Semaphore[F](0))
            unfold    = {
              def unfold(remaining: Int)(^ : String)(^^ : String): OptionT[F, Unit] =
                val first = remaining == $parallelism
                val last = remaining == 1
                val main =
                  for
                    _    <- OptionT.unlessM(if first then Temporal[F].pure(false) else moduloD.get)(Temporal[F].unit)
                    loop <- ( for
                                _        <- OptionT.liftF(exclude($key)(${%})(${`π-elvis`}).whenA(first))
                                continue <- OptionT.liftF(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
                                deferred <- OptionT.liftF(Deferred[F, Option[<>[F]]])
                                _        <- OptionT.liftF(deferred.complete(None).unlessA(first))
                                _        <- OptionT.liftF(${/}.offer(^ -> $key -> (deferred -> continue, ($ether, Some(Left(())), $rate))))
                                _        <- OptionT.liftF(deferred.get)
                                              .semiflatten
                                              .flatTapNone(moduloD.complete(true).whenA(first))
                                              .orElse(OptionT.unless(first)(null))
                                _        <- OptionT.liftF(moduloD.complete(false).whenA(first))
                              yield {
                                def loop(enabled: Boolean = first): OptionT[F, Unit] =
                                  for
                                    _     <- OptionT.liftF(moduloS.acquire.unlessA(enabled))
                                    _     <- OptionT.liftF(${\}(${%}.update { m => m + (^ + $key -> (true, m(^ + $key).asInstanceOf[(Boolean, +[F])]._2)) }).unlessA(enabled))
                                    tuple <- OptionT.liftF(continue.get.flatMap(_.get)).semiflatten.flatTapNone(moduloS.release >> moduloCB.await)
                                    _     <- OptionT.liftF(Deferred[F, Option[<>[F]]] >>= continue.set)
                                    (b, f,
                                     i   ) = tuple
                                    _     <- $value().map(new `()`[F](_)).semiflatMap(i.set)
                                    _     <- OptionT.liftF(b.await)
                                    _     <- OptionT.liftF(f.join)
                                    _     <- $sleep
                                    _     <- OptionT.liftF(moduloS.release)
                                    _     <- $body()(using ^^).orElse(OptionT.unit[F])
                                    _     <- loop(false)
                                  yield ()
                                loop()
                              }
                            )
                    _    <- loop
                  yield
                    ()
                if last
                then main
                else main.background.use { _ => OptionT.liftF(UUIDGen.randomString[F]) >>= unfold(remaining - 1)(^^) }
              unfold
            }
            _        <- if $parallelism == 1
                        then unfold($parallelism)(${^})(${^})
                        else OptionT.liftF(UUIDGen.randomString[F]) >>= unfold($parallelism)(${^})
          yield
            ()
        }


    /**
      * modulo replication input guard
      */
    def inputCode[F[_]](ether: Expr[`{}`[F]])
                       (parallelism: Expr[Int], rate: Expr[Rate])(key: Expr[String])(body: Expr[`Π-Function1`[F]])
                       (sleep: Expr[OptionT[F, Unit]])
                       (% : Expr[%[F]], / : Expr[/[F]], \ : Expr[\[F]])
                       (`π-elvis`: Expr[`Π-Map`[String, `Π-Set`[String]]], ^ : Expr[String])
                       (temporalF: Expr[Temporal[F]], uuidGenF: Expr[UUIDGen[F]])
                       (using Quotes, Type[F]): Expr[OptionT[F, Unit]] =
     '{ given Temporal[F] = $temporalF
        given UUIDGen[F] = $uuidGenF
        for
          moduloCB <- OptionT.liftF(CyclicBarrier[F]($parallelism))
          moduloD  <- OptionT.liftF(Deferred[F, Boolean])
          moduloS  <- OptionT.liftF(Semaphore[F](0))
          unfold    = {
            def unfold(remaining: Int)(^ : String)(^^ : String): OptionT[F, Unit] =
              val first = remaining == $parallelism
              val last = remaining == 1
              val main =
                for
                  _    <- OptionT.unlessM(if first then Temporal[F].pure(false) else moduloD.get)(Temporal[F].unit)
                  loop <- ( for
                              _        <- OptionT.liftF(exclude($key)(${%})(${`π-elvis`}).whenA(first))
                              continue <- OptionT.liftF(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
                              deferred <- OptionT.liftF(Deferred[F, Option[<>[F]]])
                              _        <- OptionT.liftF(deferred.complete(None).unlessA(first))
                              result   <- OptionT.liftF(Ref[F].of[`()`[F]](null))
                              _        <- OptionT.liftF(${/}.offer(^ -> $key -> (deferred -> continue, ($ether, Some(Right(result)), $rate))))
                              _        <- OptionT.liftF(deferred.get)
                                            .semiflatten
                                            .flatTapNone(moduloD.complete(true).whenA(first))
                                            .orElse(OptionT.unless(first)(null))
                              _        <- OptionT.liftF(moduloD.complete(false).whenA(first))
                            yield {
                              def loop(enabled: Boolean = first): OptionT[F, Unit] =
                                for
                                  _     <- OptionT.liftF(moduloS.acquire.unlessA(enabled))
                                  _     <- OptionT.liftF(${\}(${%}.update { m => m + (^ + $key -> (true, m(^ + $key).asInstanceOf[(Boolean, +[F])]._2)) }).unlessA(enabled))
                                  tuple <- OptionT.liftF(continue.get.flatMap(_.get)).semiflatten.flatTapNone(moduloS.release >> moduloCB.await)
                                  _     <- OptionT.liftF(Deferred[F, Option[<>[F]]] >>= continue.set)
                                  (b, f,
                                   _   ) = tuple
                                  _     <- OptionT.liftF(b.await)
                                  _     <- OptionT.liftF(f.join)
                                  n     <- OptionT.liftF(result.get)
                                  _     <- $sleep
                                  _     <- OptionT.liftF(moduloS.release)
                                  _     <- $body(n)(using ^^).orElse(OptionT.unit[F])
                                  _     <- loop(false)
                                yield ()
                              loop()
                            }
                          )
                  _    <- loop
                yield
                  ()
              if last
              then main
              else main.background.use { _ => OptionT.liftF(UUIDGen.randomString[F]) >>= unfold(remaining - 1)(^^) }
            unfold
          }
          _        <- if $parallelism == 1
                      then unfold($parallelism)(${^})(${^})
                      else OptionT.liftF(UUIDGen.randomString[F]) >>= unfold($parallelism)(${^})
        yield
          ()
      }

    /**
      * modulo replication input guard w/ code
      */
    def inputCode[F[_], T](ether: Expr[`{}`[F]])
                          (parallelism: Expr[Int], rate: Expr[Rate])(key: Expr[String])(code: Expr[T => OptionT[F, Any]])(body: Expr[`Π-Function1`[F]])
                          (sleep: Expr[OptionT[F, Unit]])
                          (% : Expr[%[F]], / : Expr[/[F]], \ : Expr[\[F]])
                          (`π-elvis`: Expr[`Π-Map`[String, `Π-Set`[String]]], ^ : Expr[String])
                          (temporalF: Expr[Temporal[F]], deferF: Expr[Defer[F]], uuidGenF: Expr[UUIDGen[F]], classTag: Expr[ClassTag[T]])
                          (using Quotes, Type[F], Type[T]): Expr[OptionT[F, Unit]] =
     '{ given Temporal[F] = $temporalF
        given Defer[F] = $deferF
        given UUIDGen[F] = $uuidGenF
        given ClassTag[T] = $classTag
        for
          moduloCB <- OptionT.liftF(CyclicBarrier[F]($parallelism))
          moduloD  <- OptionT.liftF(Deferred[F, Boolean])
          moduloS  <- OptionT.liftF(Semaphore[F](0))
          unfold    = {
            def unfold(remaining: Int)(^ : String)(^^ : String): OptionT[F, Unit] =
              val first = remaining == $parallelism
              val last = remaining == 1
              val main =
                for
                  _    <- OptionT.unlessM(if first then Temporal[F].pure(false) else moduloD.get)(Temporal[F].unit)
                  loop <- ( for
                              _        <- OptionT.liftF(exclude($key)(${%})(${`π-elvis`}).whenA(first))
                              continue <- OptionT.liftF(Deferred[F, Option[<>[F]]] >>= Ref[F].of)
                              deferred <- OptionT.liftF(Deferred[F, Option[<>[F]]])
                              _        <- OptionT.liftF(deferred.complete(None).unlessA(first))
                              result   <- OptionT.liftF(Ref[F].of[`()`[F]](null))
                              _        <- OptionT.liftF(${/}.offer(^ -> $key -> (deferred -> continue, ($ether, Some(Right(result)), $rate))))
                              _        <- OptionT.liftF(deferred.get)
                                            .semiflatten
                                            .flatTapNone(moduloD.complete(true).whenA(first))
                                            .orElse(OptionT.unless(first)(null))
                              _        <- OptionT.liftF(moduloD.complete(false).whenA(first))
                            yield {
                              def loop(enabled: Boolean = first): OptionT[F, Unit] =
                                for
                                  _     <- OptionT.liftF(moduloS.acquire.unlessA(enabled))
                                  _     <- OptionT.liftF(${\}(${%}.update { m => m + (^ + $key -> (true, m(^ + $key).asInstanceOf[(Boolean, +[F])]._2)) }).unlessA(enabled))
                                  tuple <- OptionT.liftF(continue.get.flatMap(_.get)).semiflatten.flatTapNone(moduloS.release >> moduloCB.await)
                                  _     <- OptionT.liftF(Deferred[F, Option[<>[F]]] >>= continue.set)
                                  (b, f,
                                   _   ) = tuple
                                  _     <- OptionT.liftF(b.await)
                                  _     <- OptionT.liftF(f.join)
                                  n     <- OptionT.liftF(result.get)
                                             .map(_.name)
                                             .flatMap {
                                               case it @ null => OptionT.pure(it)
                                               case it: T => $code(it)
                                             }.map(new `()`[F](_))
                                  _     <- $sleep
                                  _     <- OptionT.liftF(moduloS.release)
                                  _     <- $body(n)(using ^^).orElse(OptionT.unit[F])
                                  _     <- loop(false)
                                yield ()
                              loop()
                            }
                          )
                  _    <- loop
                yield
                  ()
              if last
              then main
              else main.background.use { _ => OptionT.liftF(UUIDGen.randomString[F]) >>= unfold(remaining - 1)(^^) }
            unfold
          }
          _        <- if $parallelism == 1
                      then unfold($parallelism)(${^})(${^})
                      else OptionT.liftF(UUIDGen.randomString[F]) >>= unfold($parallelism)(${^})
        yield
          ()
      }
