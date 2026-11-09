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

  import _root_.scala.concurrent.duration.FiniteDuration

  import _root_.scala.collection.immutable.{ Map, Set }

  import _root_.scala.reflect.{ ClassTag, classTag }

  import _root_.cats.syntax.applicative.*
  import _root_.cats.syntax.flatMap.*
  import _root_.cats.syntax.functor.*

  import _root_.cats.{ Applicative, Defer, Functor, Monad }
  import _root_.cats.data.OptionT
  import _root_.cats.effect.{ Deferred, Ref, Temporal }
  import _root_.cats.effect.std.UUIDGen

  import `Π-loop`.{ <>, %, /, \ }
  import `Π-stats`.Rate


  type `Π-Map`[K, +V] = Map[K, V]

  type `Π-Set`[A] = Set[A]

  type `Π-Function0`[F[_]] = () => String ?=> OptionT[F, Unit]
  type `Π-Function1`[F[_]] = `()`[F] => String ?=> OptionT[F, Unit]

  /**
    * Type of causal sets.
    */
  type `[]` = Set[Long]


  /**
    * Type for [[cats.effect.IOLocal]], [[zio.FiberRef]], etc.
    */
  open case class `Π-FiberLocal`[F[_], A](get: F[A], set: A => F[Unit])


  extension [F[_]: Functor, A](self: OptionT[F, Option[A]])
    def semiflatten: OptionT[F, A] =
      OptionT(self.value.map(_.flatten))

  extension (_self: OptionT.type)
    def sleep[F[_]: Temporal](time: FiniteDuration): OptionT[F, Unit] =
      Temporal[[X] =>> OptionT[F, X]].sleep(time)
    def unit[F[_]: Monad]: OptionT[F, Unit] =
      Monad[[X] =>> OptionT[F, X]].unit


  inline def `π-exclude`[F[_]](enabled: String*)
                              (using % : %[F], \ : \[F]): F[Unit] =
    \(`π-exclude`(Set.from(enabled)))

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

  private def exclude[F[_]: Applicative](key: String)
                           (using %[F])
                           (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]]): F[Unit] =
    `π-exclude`(`π-elvis`(key)).whenA(`π-elvis`.contains(key))


  /**
    * restriction aka new name
    */
  final class ν[F[_]: Defer: Temporal: UUIDGen]:

    def map[B](f: `()`[F] => B): OptionT[F, B] = flatMap(f andThen OptionT.pure[F].apply)
    def flatMap[B](f: `()`[F] => OptionT[F, B]): OptionT[F, B] = f(new {})


  /**
    * silent transition
    */
  final class τ[F[_]: Temporal: UUIDGen] extends τMacros[F]:

    def apply(rate: Rate)(key: String)
             (using % : %[F], / : /[F])
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String, `[]`: `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      for
        _        <- OptionT.liftF(exclude(key))
        deferred <- OptionT.liftF(Deferred[F, Option[<>[F]]])
        `][`     <- OptionT.liftF(`[]`.get)
        timestamp <- OptionT.liftF(Temporal[F].realTime.map(_.toMillis) >>= Ref[F].of[Long])
        _        <- OptionT.liftF(/.offer(^ -> key -> ((deferred -> null, timestamp), (`new {}`, None, rate, `][`))))
        tuple    <- OptionT.liftF(deferred.get).semiflatten
        (b, f,
         _, s)    = tuple
        _        <- OptionT.liftF(b.await)
        _        <- OptionT.liftF(f.join)
        _        <- OptionT.liftF(`[]`.set(s))
      yield
        ()

    /**
      * modulo replication guard
      */
    def apply(_f: false)(parallelism: Int, rate: Rate)(key: String)(body: `Π-Function0`[F])
                        (using %[F], /[F], \[F])
                        (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      super.silent(false)(parallelism, rate)(key)(body)

    /**
      * modulo replication guard w/ pace
      */
    def apply(_f: false)(pace: FiniteDuration, parallelism: Int, rate: Rate)(key: String)(body: `Π-Function0`[F])
                        (using %[F], /[F], \[F])
                        (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      super.silent(false)(pace, parallelism, rate)(key)(body)

    /**
      * modulo replication guard w/ code
      */
    def apply(_t: true)(parallelism: Int, rate: Rate)(key: String)(code: => OptionT[F, Any])(body: `Π-Function0`[F])
                       (using %[F], /[F], \[F])
                       (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      super.silent(true)(parallelism, rate)(key)(code)(body)

    /**
      * modulo replication guard w/ pace w/ code
      */
    def apply(_t: true)(pace: FiniteDuration, parallelism: Int, rate: Rate)(key: String)(code: => OptionT[F, Any])(body: `Π-Function0`[F])
                       (using %[F], /[F], \[F])
                       (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      super.silent(true)(pace, parallelism, rate)(key)(code)(body)


  /**
    * names and values
    */
  final implicit class `()`[F[_]: Defer: Temporal: UUIDGen](/*private[sΠ] */val name: Any) extends Macros[F]:

    def ====(that: `()`[F]) = this.name == that.name

    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()`[F] = this

    // MODULO REPLICATION //////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////// BOUND //

    /**
      * modulo replication bound output guard
      */
    def apply(_nu: "ν")(_f: false)(parallelism: Int, rate: Rate)(key: String)(body: `Π-Function1`[F])
                                  (using %[F], /[F], \[F])
                                  (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      super.output("ν")(false)(parallelism, rate)(key)(body)

    /**
      * modulo replication bound output guard w/ pace
      */
    def apply(_nu: "ν")(_f: false)(pace: FiniteDuration, parallelism: Int, rate: Rate)(key: String)(body: `Π-Function1`[F])
                                  (using %[F], /[F], \[F])
                                  (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      super.output("ν")(false)(pace, parallelism, rate)(key)(body)

    /**
      * modulo replication bound output guard w/ code
      */
    def apply(_nu: "ν")(_t: true)(parallelism: Int, rate: Rate)(key: String)(code: => OptionT[F, Any])(body: `Π-Function1`[F])
                                 (using %[F], /[F], \[F])
                                 (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      super.output("ν")(true)(parallelism, rate)(key)(code)(body)

    /**
      * modulo replication bound output guard w/ pace w/ code
      */
    def apply(_nu: "ν")(_t: true)(pace: FiniteDuration, parallelism: Int, rate: Rate)(key: String)(code: => OptionT[F, Any])(body: `Π-Function1`[F])
                                 (using %[F], /[F], \[F])
                                 (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      super.output("ν")(true)(pace, parallelism, rate)(key)(code)(body)

    //////////////////////////////////////////////////////////////// CONSTANT //

    /**
      * modulo constant replication output guard
      */
    def apply(_f: false)(parallelism: Int, rate: Rate, value: `()`[F])(key: String)(body: `Π-Function0`[F])
                        (using %[F], /[F], \[F])
                        (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      super.output(false)(parallelism, rate, value)(key)(body)

    /**
      * modulo constant replication output guard w/ pace
      */
    def apply(_f: false)(pace: FiniteDuration, parallelism: Int, rate: Rate, value: `()`[F])(key: String)(body: `Π-Function0`[F])
                        (using %[F], /[F], \[F])
                        (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      super.output(false)(pace, parallelism, rate, value)(key)(body)

    /**
      * modulo constant replication output guard w/ code
      */
    def apply(_t: true)(parallelism: Int, rate: Rate, value: `()`[F])(key: String)(code: => OptionT[F, Any])(body: `Π-Function0`[F])
                       (using %[F], /[F], \[F])
                       (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      super.output(true)(parallelism, rate, value)(key)(code)(body)

    /**
      * modulo constant replication output guard w/ pace w/ code
      */
    def apply(_t: true)(pace: FiniteDuration, parallelism: Int, rate: Rate, value: `()`[F])(key: String)(code: => OptionT[F, Any])(body: `Π-Function0`[F])
                       (using %[F], /[F], \[F])
                       (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      super.output(true)(pace, parallelism, rate, value)(key)(code)(body)

    //////////////////////////////////////////////////////////////// VARIABLE //

    /**
      * modulo variable replication output guard
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(parallelism: Int, rate: Rate, value: => S)(key: String)(body: `Π-Function0`[F])(using DummyImplicit)
                                              (using %[F], /[F], \[F])
                                              (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(false)(parallelism, rate, value.asInstanceOf[`()`[F]])(key)(body)
      else
        Defer[[X] =>> OptionT[F, X]].defer(apply("*")(false)(parallelism, rate, OptionT.pure(value))(key)(body))

    /**
      * modulo variable replication output guard w/ pace
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(pace: FiniteDuration, parallelism: Int, rate: Rate, value: => S)(key: String)(body: `Π-Function0`[F])(using DummyImplicit)
                                              (using %[F], /[F], \[F])
                                              (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(false)(pace, parallelism, rate, value.asInstanceOf[`()`[F]])(key)(body)
      else
        Defer[[X] =>> OptionT[F, X]].defer(apply("*")(false)(pace, parallelism, rate, OptionT.pure(value))(key)(body))

    /**
      * modulo variable replication output guard w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(parallelism: Int, rate: Rate, value: => S)(key: String)(code: => OptionT[F, Any])(body: `Π-Function0`[F])(using DummyImplicit)
                                             (using %[F], /[F], \[F])
                                             (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(true)(parallelism, rate, value.asInstanceOf[`()`[F]])(key)(code)(body)
      else
        Defer[[X] =>> OptionT[F, X]].defer(apply("*")(true)(parallelism, rate, OptionT.pure(value))(key)(code)(body))

    /**
      * modulo variable replication output guard w/ pace w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(pace: FiniteDuration, parallelism: Int, rate: Rate, value: => S)(key: String)(code: => OptionT[F, Any])(body: `Π-Function0`[F])(using DummyImplicit)
                                             (using %[F], /[F], \[F])
                                             (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(true)(pace, parallelism, rate, value.asInstanceOf[`()`[F]])(key)(code)(body)
      else
        Defer[[X] =>> OptionT[F, X]].defer(apply("*")(true)(pace, parallelism, rate, OptionT.pure(value))(key)(code)(body))

    /**
      * modulo variable replication output guard
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(parallelism: Int, rate: Rate, value: => OptionT[F, S])(key: String)(body: `Π-Function0`[F])
                                              (using %[F], /[F], \[F])
                                              (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        Defer[[X] =>> OptionT[F, X]].defer(value.asInstanceOf[OptionT[F, `()`[F]]].flatMap(apply(false)(parallelism, rate, _)(key)(body)))
      else
        super.output("*")(false)(parallelism, rate, value)(key)(body)

    /**
      * modulo variable replication output guard w/ pace
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(pace: FiniteDuration, parallelism: Int, rate: Rate, value: => OptionT[F, S])(key: String)(body: `Π-Function0`[F])
                                              (using %[F], /[F], \[F])
                                              (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        Defer[[X] =>> OptionT[F, X]].defer(value.asInstanceOf[OptionT[F, `()`[F]]].flatMap(apply(false)(pace, parallelism, rate, _)(key)(body)))
      else
        super.output("*")(false)(pace, parallelism, rate, value)(key)(body)

    /**
      * modulo variable replication output guard w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(parallelism: Int, rate: Rate, value: => OptionT[F, S])(key: String)(code: => OptionT[F, Any])(body: `Π-Function0`[F])
                                             (using %[F], /[F], \[F])
                                             (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        Defer[[X] =>> OptionT[F, X]].defer(value.asInstanceOf[OptionT[F, `()`[F]]].flatMap(apply(true)(parallelism, rate, _)(key)(code)(body)))
      else
        super.output("*")(true)(parallelism, rate, value)(key)(code)(body)

    /**
      * modulo variable replication output guard w/ pace w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(pace: FiniteDuration, parallelism: Int, rate: Rate, value: => OptionT[F, S])(key: String)(code: => OptionT[F, Any])(body: `Π-Function0`[F])
                                             (using %[F], /[F], \[F])
                                             (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        Defer[[X] =>> OptionT[F, X]].defer(value.asInstanceOf[OptionT[F, `()`[F]]].flatMap(apply(true)(pace, parallelism, rate, _)(key)(code)(body)))
      else
        super.output("*")(true)(pace, parallelism, rate, value)(key)(code)(body)

    /////////////////////////////////////////////////////////////////// INPUT //

    /**
      * modulo replication input guard
      */
    def apply(_n: Null)(_f: false)(parallelism: Int, rate: Rate)(key: String)(body: `Π-Function1`[F])
                                  (using %[F], /[F], \[F])
                                  (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      super.input(false)(parallelism, rate)(key)(body)

    /**
      * modulo replication input guard w/ pace
      */
    def apply(_n: Null)(_f: false)(pace: FiniteDuration, parallelism: Int, rate: Rate)(key: String)(body: `Π-Function1`[F])
                                  (using %[F], /[F], \[F])
                                  (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      super.input(false)(pace, parallelism, rate)(key)(body)

    /**
      * modulo replication input guard w/ code
      */
    def apply[T: ClassTag](_n: Null)(_t: true)(parallelism: Int, rate: Rate)(key: String)(code: T => OptionT[F, Any])(body: `Π-Function1`[F])
                                              (using %[F], /[F], \[F])
                                              (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      super.input(true)(parallelism, rate)(key)(code)(body)

    /**
      * modulo replication input guard w/ pace w/ code
      */
    def apply[T: ClassTag](_n: Null)(_t: true)(pace: FiniteDuration, parallelism: Int, rate: Rate)(key: String)(code: T => OptionT[F, Any])(body: `Π-Function1`[F])
                                              (using %[F], /[F], \[F])
                                              (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      super.input(true)(pace, parallelism, rate)(key)(code)(body)

    ////////////////////////////////////////////////////// modulo replication //

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(rate: Rate, value: => S)(key: String)
                                     (using DummyImplicit)
                                     (using %[F], /[F])
                                     (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(rate, value.asInstanceOf[`()`[F]])(key)
      else
        Defer[[X] =>> OptionT[F, X]].defer(apply(false)(rate, OptionT.pure(value))(key))

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(rate: Rate, value: => S)(key: String)(code: => OptionT[F, Any])
                                    (using DummyImplicit)
                                    (using %[F], /[F])
                                    (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(rate, value.asInstanceOf[`()`[F]])(key)(code)
      else
        Defer[[X] =>> OptionT[F, X]].defer(apply(true)(rate, OptionT.pure(value))(key)(code))

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(rate: Rate, value: => OptionT[F, S])(key: String)
                                     (using %[F], /[F])
                                     (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        Defer[[X] =>> OptionT[F, X]].defer(value.asInstanceOf[OptionT[F, `()`[F]]].flatMap(apply(rate, _)(key)))
      else
        Defer[[X] =>> OptionT[F, X]].defer(value.map(new `()`(_)).flatMap(apply(rate, _)(key)))

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(rate: Rate, value: => OptionT[F, S])(key: String)(code: => OptionT[F, Any])
                                    (using %[F], /[F])
                                    (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        Defer[[X] =>> OptionT[F, X]].defer(value.asInstanceOf[OptionT[F, `()`[F]]].flatMap(apply(rate, _)(key)(code)))
      else
        Defer[[X] =>> OptionT[F, X]].defer(value.map(new `()`(_)).flatMap(apply(rate, _)(key)(code)))

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`[F])(key: String)
             (using % : %[F], / : /[F])
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String, `[]`: `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      for
        _        <- OptionT.liftF(exclude(key))
        deferred <- OptionT.liftF(Deferred[F, Option[<>[F]]])
        `][`     <- OptionT.liftF(`[]`.get)
        timestamp <- OptionT.liftF(Temporal[F].realTime.map(_.toMillis) >>= Ref[F].of[Long])
        _        <- OptionT.liftF(/.offer(^ -> key -> ((deferred -> null, timestamp), (`()`[{}], Some(Left(())), rate, `][`))))
        tuple    <- OptionT.liftF(deferred.get).semiflatten
        (b, f,
         i, s)    = tuple
        _        <- OptionT.liftF(i.set(value))
        _        <- OptionT.liftF(b.await)
        _        <- OptionT.liftF(f.join)
        _        <- OptionT.liftF(`[]`.set(s))
      yield
        ()

    /**
      * negative prefix i.e. output w/ code
      */
    def apply(rate: Rate, value: `()`[F])(key: String)(code: => OptionT[F, Any])
             (using %[F], /[F])
             (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, Unit] =
      apply(rate, value)(key).flatTap(_ => code)

    /**
      * positive prefix i.e. input
      */
    def apply(rate: Rate)(key: String)
             (using % : %[F], / : /[F])
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String, `[]`: `Π-FiberLocal`[F, `[]`]): OptionT[F, `()`[F]] =
      for
        _        <- OptionT.liftF(exclude(key))
        deferred <- OptionT.liftF(Deferred[F, Option[<>[F]]])
        result   <- OptionT.liftF(Ref[F].of[`()`[F]](null))
        `][`     <- OptionT.liftF(`[]`.get)
        timestamp <- OptionT.liftF(Temporal[F].realTime.map(_.toMillis) >>= Ref[F].of[Long])
        _        <- OptionT.liftF(/.offer(^ -> key -> ((deferred -> null, timestamp), (`()`[{}], Some(Right(result)), rate, `][`))))
        tuple    <- OptionT.liftF(deferred.get).semiflatten
        (b, f,
         _, s)    = tuple
        _        <- OptionT.liftF(b.await)
        _        <- OptionT.liftF(f.join)
        _        <- OptionT.liftF(`[]`.set(s))
        name     <- OptionT.liftF(result.get)
      yield
        name

    /**
      * positive prefix i.e. input w/ cdoe
      */
    def apply[T: ClassTag](rate: Rate)(key: String)(code: T => OptionT[F, Any])
                          (using %[F], /[F])
                          (using `Π-Map`[String, `Π-Set`[String]], String, `Π-FiberLocal`[F, `[]`]): OptionT[F, `()`[F]] =
      apply(rate)(key)
        .map(_.name)
        .flatMap {
          case null  => OptionT.pure[F](null)
          case it: T => code(it)
        }.map(new `()`(_))

    override def toString: String = if name == null then "null" else name.toString
