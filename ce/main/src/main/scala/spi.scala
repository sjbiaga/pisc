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

  import _root_.cats.effect.IO
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.Supervisor

  import `Π-loop`.{ <>, %, /, \ }
  import `Π-stats`.Rate


  type `Π-Map`[K, +V] = Map[K, V]

  type `Π-Set`[A] = Set[A]

  type `Π-Function0` = () => String ?=> IO[Any]
  type `Π-Function1` = `()` => String ?=> IO[Any]


  /**
    * Supervised [[code]].
    * @param code
    */
  private def exec[T](code: => IO[T]): IO[T] =
    Supervisor[IO](await = true)
      .use(_.supervise(code))
      .flatMap(_.join)
      .flatMap {
        case Succeeded(it) => it
        case _             => IO.pure(null.asInstanceOf[T])
      }


  inline def `π-exclude`(enabled: String*)
                        (using % : %, \ : \): IO[Unit] =
    `π-exclude`(Set.from(enabled)) >> \

  private def `π-exclude`(enabled: `Π-Set`[String])
                         (using % : %): IO[Unit] =
    %.update(enabled.foldLeft(_) { (m, key) =>
                                   val n = m(key).asInstanceOf[Int] - 1
                                   if n == 0
                                   then
                                     m - key
                                   else
                                     m + (key -> n)
                                 }
    )

  private def exclude(key: String)
                     (using % : %)
                     (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]]): IO[Unit] =
    `π-exclude`(`π-elvis`(key)).whenA(`π-elvis`.contains(key))


  /**
    * restriction aka new name
    */
  object ν:

    def map[B](f: `()` => B): IO[B] = flatMap(f andThen IO.pure)
    def flatMap[B](f: `()` => IO[B]): IO[B] = f(new {})


  /**
    * silent transition
    */
  object τ extends τ:

    def apply(rate: Rate)(key: String)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[Double] =
      for
        _        <- exclude(key)
        deferred <- IO.deferred[Option[<>]]
        _        <- /.offer(^ -> key -> (deferred -> null -> (new {}, None, rate)))
        opt      <- deferred.get
        _        <- if opt eq None then IO.canceled else IO.unit
        (delay,
         b, f, _) = opt.get
        _        <- b.await
        _        <- f.join
      yield
        delay

    /**
      * linear replication guard
      */
    def apply(_f: false)(parallelism: Int, rate: Rate)(key: String)(body: `Π-Function0`)
                        (using %, /, \)
                        (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.silent(false)(parallelism, rate)(key)(body)

    /**
      * linear replication guard w/ pace
      */
    def apply(_f: false)(pace: FiniteDuration, parallelism: Int, rate: Rate)(key: String)(body: `Π-Function0`)
                        (using %, /, \)
                        (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.silent(false)(pace, parallelism, rate)(key)(body)

    /**
      * linear replication guard w/ code
      */
    def apply(_t: true)(parallelism: Int, rate: Rate)(key: String)(code: IO[Any])(body: `Π-Function0`)
                       (using %, /, \)
                       (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.silent(true)(parallelism, rate)(key)(code)(body)

    /**
      * linear replication guard w/ pace w/ code
      */
    def apply(_t: true)(pace: FiniteDuration, parallelism: Int, rate: Rate)(key: String)(code: IO[Any])(body: `Π-Function0`)
                       (using %, /, \)
                       (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.silent(true)(pace, parallelism, rate)(key)(code)(body)


  /**
    * prefix
    */
  final implicit class `()`(private[sΠ] val name: Any) extends AnyVal with Macros:

    def ====(that: `()`) = this.name == that.name

    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

    // LINEAR REPLICATION //////////////////////////////////////////////////////

    /////////////////////////////////////////////////////////////////// BOUND //

    /**
      * linear replication bound output guard
      */
    def apply(_nu: "ν")(_f: false)(parallelism: Int, rate: Rate)(key: String)(body: `Π-Function1`)
                                  (using %, /, \)
                                  (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.output("ν")(false)(parallelism, rate)(key)(body)

    /**
      * linear replication bound output guard w/ pace
      */
    def apply(_nu: "ν")(_f: false)(pace: FiniteDuration, parallelism: Int, rate: Rate)(key: String)(body: `Π-Function1`)
                                  (using %, /, \)
                                  (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.output("ν")(false)(pace, parallelism, rate)(key)(body)

    /**
      * linear replication bound output guard w/ code
      */
    def apply(_nu: "ν")(_t: true)(parallelism: Int, rate: Rate)(key: String)(code: => IO[Any])(body: `Π-Function1`)
                                 (using %, /, \)
                                 (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.output("ν")(true)(parallelism, rate)(key)(code)(body)

    /**
      * linear replication bound output guard w/ pace w/ code
      */
    def apply(_nu: "ν")(_t: true)(pace: FiniteDuration, parallelism: Int, rate: Rate)(key: String)(code: => IO[Any])(body: `Π-Function1`)
                                 (using %, /, \)
                                 (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.output("ν")(true)(pace, parallelism, rate)(key)(code)(body)

    //////////////////////////////////////////////////////////////// CONSTANT //

    /**
      * linear constant replication output guard
      */
    def apply(_f: false)(parallelism: Int, rate: Rate, value: `()`)(key: String)(body: `Π-Function0`)
                        (using %, /, \)
                        (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.output(false)(parallelism, rate, value)(key)(body)

    /**
      * linear constant replication output guard w/ pace
      */
    def apply(_f: false)(pace: FiniteDuration, parallelism: Int, rate: Rate, value: `()`)(key: String)(body: `Π-Function0`)
                        (using %, /, \)
                        (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.output(false)(pace, parallelism, rate, value)(key)(body)

    /**
      * linear constant replication output guard w/ code
      */
    def apply(_t: true)(parallelism: Int, rate: Rate, value: `()`)(key: String)(code: IO[Any])(body: `Π-Function0`)
                       (using %, /, \)
                       (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.output(true)(parallelism, rate, value)(key)(code)(body)

    /**
      * linear constant replication output guard w/ pace w/ code
      */
    def apply(_t: true)(pace: FiniteDuration, parallelism: Int, rate: Rate, value: `()`)(key: String)(code: IO[Any])(body: `Π-Function0`)
                       (using %, /, \)
                       (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.output(true)(pace, parallelism, rate, value)(key)(code)(body)

    //////////////////////////////////////////////////////////////// VARIABLE //

    /**
      * linear variable replication output guard
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(parallelism: Int, rate: Rate, value: => S)(key: String)(body: `Π-Function0`)(using DummyImplicit)
                                              (using %, /, \)
                                              (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
     if classTag[S].runtimeClass eq getClass
     then
       apply(false)(parallelism, rate, value.asInstanceOf[`()`])(key)(body)
     else
       apply("*")(false)(parallelism, rate, IO.delay(value))(key)(body)

    /**
      * linear variable replication output guard w/ pace
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(pace: FiniteDuration, parallelism: Int, rate: Rate, value: => S)(key: String)(body: `Π-Function0`)(using DummyImplicit)
                                              (using %, /, \)
                                              (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
     if classTag[S].runtimeClass eq getClass
     then
       apply(false)(pace, parallelism, rate, value.asInstanceOf[`()`])(key)(body)
     else
       apply("*")(false)(pace, parallelism, rate, IO.delay(value))(key)(body)

    /**
      * linear variable replication output guard w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(parallelism: Int, rate: Rate, value: => S)(key: String)(code: IO[Any])(body: `Π-Function0`)(using DummyImplicit)
                                             (using %, /, \)
                                             (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
     if classTag[S].runtimeClass eq getClass
     then
       apply(true)(parallelism, rate, value.asInstanceOf[`()`])(key)(code)(body)
     else
       apply("*")(true)(parallelism, rate, IO.delay(value))(key)(code)(body)

    /**
      * linear variable replication output guard w/ pace w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(pace: FiniteDuration, parallelism: Int, rate: Rate, value: => S)(key: String)(code: IO[Any])(body: `Π-Function0`)(using DummyImplicit)
                                             (using %, /, \)
                                             (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
     if classTag[S].runtimeClass eq getClass
     then
       apply(true)(pace, parallelism, rate, value.asInstanceOf[`()`])(key)(code)(body)
     else
       apply("*")(true)(pace, parallelism, rate, IO.delay(value))(key)(code)(body)

    /**
      * linear variable replication output guard
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(parallelism: Int, rate: Rate, value: => IO[S])(key: String)(body: `Π-Function0`)
                                              (using %, /, \)
                                              (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        IO.defer(value.asInstanceOf[IO[`()`]].flatMap(apply(false)(parallelism, rate, _)(key)(body)))
      else
        super.output("*")(false)(parallelism, rate, value)(key)(body)

    /**
      * linear variable replication output guard w/ pace
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(pace: FiniteDuration, parallelism: Int, rate: Rate, value: => IO[S])(key: String)(body: `Π-Function0`)
                                              (using %, /, \)
                                              (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        IO.defer(value.asInstanceOf[IO[`()`]].flatMap(apply(false)(pace, parallelism, rate, _)(key)(body)))
      else
        super.output("*")(false)(pace, parallelism, rate, value)(key)(body)

    /**
      * linear variable replication output guard w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(parallelism: Int, rate: Rate, value: => IO[S])(key: String)(code: IO[Any])(body: `Π-Function0`)
                                             (using %, /, \)
                                             (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        IO.defer(value.asInstanceOf[IO[`()`]].flatMap(apply(true)(parallelism, rate, _)(key)(code)(body)))
      else
        super.output("*")(true)(parallelism, rate, value)(key)(code)(body)

    /**
      * linear variable replication output guard w/ pace w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(pace: FiniteDuration, parallelism: Int, rate: Rate, value: => IO[S])(key: String)(code: IO[Any])(body: `Π-Function0`)
                                             (using %, /, \)
                                             (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        IO.defer(value.asInstanceOf[IO[`()`]].flatMap(apply(true)(pace, parallelism, rate, _)(key)(code)(body)))
      else
        super.output("*")(true)(pace, parallelism, rate, value)(key)(code)(body)

    /////////////////////////////////////////////////////////////////// INPUT //

    /**
      * linear replication input guard
      */
    def apply(_n: Null)(_f: false)(parallelism: Int, rate: Rate)(key: String)(body: `Π-Function1`)
                                  (using %, /, \)
                                  (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.input(false)(parallelism, rate)(key)(body)

    /**
      * linear replication input guard w/ pace
      */
    def apply(_n: Null)(_f: false)(pace: FiniteDuration, parallelism: Int, rate: Rate)(key: String)(body: `Π-Function1`)
                                  (using %, /, \)
                                  (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.input(false)(pace, parallelism, rate)(key)(body)

    /**
      * linear replication input guard w/ code
      */
    def apply[T](_n: Null)(_t: true)(parallelism: Int, rate: Rate)(key: String)(code: T => IO[T])(body: `Π-Function1`)
                                    (using %, /, \)
                                    (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.input(true)(parallelism, rate)(key)(code)(body)

    /**
      * linear replication input guard w/ pace w/ code
      */
    def apply[T](_n: Null)(_t: true)(pace: FiniteDuration, parallelism: Int, rate: Rate)(key: String)(code: T => IO[T])(body: `Π-Function1`)
                                    (using %, /, \)
                                    (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.input(true)(pace, parallelism, rate)(key)(code)(body)

    ////////////////////////////////////////////////////// linear replication //

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(rate: Rate, value: => S)(key: String)
                                     (using DummyImplicit)
                                     (using %, /)
                                     (using `Π-Map`[String, `Π-Set`[String]], String): IO[Double] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(rate, value.asInstanceOf[`()`])(key)
      else
        apply[S](false)(rate, IO.delay(value))(key)

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(rate: Rate, value: => S)(key: String)(code: => IO[Any])
                                    (using DummyImplicit)
                                    (using %, /)
                                    (using `Π-Map`[String, `Π-Set`[String]], String): IO[Double] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(rate, value.asInstanceOf[`()`])(key)(code)
      else
        apply[S](true)(rate, IO.delay(value))(key)(code)

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(rate: Rate, value: => IO[S])(key: String)
                                     (using %, /)
                                     (using `Π-Map`[String, `Π-Set`[String]], String): IO[Double] =
      if classTag[S].runtimeClass eq getClass
      then
        IO.defer(value.asInstanceOf[IO[`()`]].flatMap(apply(rate, _)(key)))
      else
        IO.defer(value.map(new `()`(_)).flatMap(apply(rate, _)(key)))

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(rate: Rate, value: => IO[S])(key: String)(code: => IO[Any])
                                    (using %, /)
                                    (using `Π-Map`[String, `Π-Set`[String]], String): IO[Double] =
      if classTag[S].runtimeClass eq getClass
      then
        IO.defer(value.asInstanceOf[IO[`()`]].flatMap(apply(rate, _)(key)(code)))
      else
        IO.defer(value.map(new `()`(_)).flatMap(apply(rate, _)(key)(code)))

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`)(key: String)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[Double] =
      for
        _        <- exclude(key)
        deferred <- IO.deferred[Option[<>]]
        _        <- /.offer(^ -> key -> (deferred -> null -> (`()`[{}], Some(Left(())), rate)))
        opt      <- deferred.get
        _        <- if opt eq None then IO.canceled else IO.unit
        (delay,
         b, f, i) = opt.get
        _        <- i.set(value)
        _        <- b.await
        _        <- f.join
      yield
        delay

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`)(key: String)(code: => IO[Any])
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[Double] =
      apply(rate, value)(key) <* exec(code)

    /**
      * positive prefix i.e. input
      */
    def apply(rate: Rate)(key: String)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[(`()`, Double)] =
      for
        _        <- exclude(key)
        deferred <- IO.deferred[Option[<>]]
        result   <- IO.ref[`()`](sΠ.`()`.`null`)
        _        <- /.offer(^ -> key -> (deferred -> null -> (`()`[{}], Some(Right(result)), rate)))
        opt      <- deferred.get
        _        <- if opt eq None then IO.canceled else IO.unit
        (delay,
         b, f, _) = opt.get
        _        <- b.await
        _        <- f.join
        name     <- result.get
      yield
        name -> delay

    /**
      * positive prefix i.e. input
      */
    def apply[T](rate: Rate)(key: String)(code: T => IO[T])
                (using % : %, / : /)
                (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): IO[(`()`, Double)] =
      apply(rate)(key)
        .map(_.name -> _)
        .flatMap {
          case (null, delay)  => IO.pure(sΠ.`()`.`null` -> delay)
          case (it: T, delay) => (code andThen exec)(it).map(new `()`(_) -> delay)
        }

    override def toString: String = if name == null then "null" else name.toString


  private object `()`:

    val `null` = new `()`(null)
