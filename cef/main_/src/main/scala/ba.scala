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

  import _root_.cats.instances.list.*
  import _root_.cats.syntax.applicative.*
  import _root_.cats.syntax.flatMap.*
  import _root_.cats.syntax.traverse.*

  import _root_.cats.effect.{ Clock, IO, IOLocal }
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.{ CyclicBarrier, Supervisor, UUIDGen }

  import _root_.io.github.timwspence.cats.stm.STM

  import `Π-loop`.{ <>, %, /, \ }
  import `Π-stats`.Rate

  import `π-$`.*, `π-ζ`.*


  type `Π-Map`[K, +V] = Map[K, V]

  type `Π-Set`[A] = Set[A]

  type `Π-Function0` = () => String ?=> IO[Any]
  type `Π-Function1` = `()` => String ?=> IO[Any]


  /**
    * Wraps ambient keys.
    *
    * @param value
    */
  final class `)(`(private val value: Any) extends AnyVal:
    override def toString: String = value.toString

  object `)(`:
    /**
      * Initial ambient unique key.
      */
    def apply(): IO[`)(`] =
      UUIDGen.randomUUID[IO].map(new `)(`(_))

  /**
    * Type of keys in [[`][`]].
    */
  type `)*(` = Set[`)(`]


  sealed abstract trait Ordʹ { val ord: Int }
  sealed abstract trait Ord(val ord: Int) extends Ordʹ

  val `π-τ` = new Ord(-1) {}

  /**
    * Type of directions.
    */
  enum `π-$` extends Ordʹ {
    case `π-local` extends `π-$` with Ord(0)
    case `π-s2s`   extends `π-$` with Ord(1)
    case `π-p2c`   extends `π-$` with Ord(2)
    case `π-c2p`   extends `π-$` with Ord(2)
  }

  /**
    * Type of capabilities.
    */
  enum `π-ζ` extends Ordʹ {
    case `π-enter`  extends `π-ζ` with Ord(3)
    case `π-accept` extends `π-ζ` with Ord(3)
    case `π-exit`   extends `π-ζ` with Ord(4)
    case `π-expel`  extends `π-ζ` with Ord(4)
    case `π-merge+` extends `π-ζ` with Ord(5)
    case `π-merge-` extends `π-ζ` with Ord(5)
  }


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
    \(`π-exclude`(Set.from(enabled)))

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
    def flatMap[B](f: `()` => IO[B]): IO[B] =
      f {
        Map(
          `π-local`.ord  -> new {},
          `π-s2s`.ord    -> new {},
          `π-p2c`.ord    -> new {},
          `π-accept`.ord -> new {},
          `π-expel`.ord  -> new {},
          `π-merge+`.ord -> new {}
        )
      }


  /**
    * silent transition
    */
  object τ extends τ:

    def apply(rate: Rate)(key: String, `)(`: IOLocal[`)(`])
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[java.lang.Double] =
      for
        _        <- exclude(key)
        deferred <- IO.deferred[Option[<>]]
        `)(`     <- `)(`.get
        timestamp <- IO.monotonic.map(_.toNanos) >>= IO.ref
        _        <- /.offer(^ -> key -> ((deferred -> null, `)(` -> `π-τ`, timestamp), (new {}, None, rate)))
        opt      <- deferred.get
        delay    <- ( if opt eq None
                      then
                        IO.pure(null: java.lang.Double)
                      else
                        val (delay, b, f, _)  = opt.get
                        for
                          _       <- b.await
                          _       <- f.join
                        yield
                          java.lang.Double(delay)
                    )
      yield
        delay

    /**
      * linear replication guard
      */
    def apply(_f: false)(parallelism: Int, rate: Rate)(key: String, `)(`: IOLocal[`)(`])(body: `Π-Function0`)
                        (using %, /, \)
                        (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.silent(false)(parallelism, rate)(key, `)(`, `π-τ`)(body)

    /**
      * linear replication guard w/ pace
      */
    def apply(_f: false)(pace: FiniteDuration, parallelism: Int, rate: Rate)(key: String, `)(`: IOLocal[`)(`])(body: `Π-Function0`)
                        (using %, /, \)
                        (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.silent(false)(pace, parallelism, rate)(key, `)(`, `π-τ`)(body)

    /**
      * linear replication guard w/ code
      */
    def apply(_t: true)(parallelism: Int, rate: Rate)(key: String, `)(`: IOLocal[`)(`])(code: => IO[Any])(body: `Π-Function0`)
                       (using %, /, \)
                       (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.silent(true)(parallelism, rate)(key, `)(`, `π-τ`)(code)(body)

    /**
      * linear replication guard w/ pace w/ code
      */
    def apply(_t: true)(pace: FiniteDuration, parallelism: Int, rate: Rate)(key: String, `)(`: IOLocal[`)(`])(code: => IO[Any])(body: `Π-Function0`)
                       (using %, /, \)
                       (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.silent(true)(pace, parallelism, rate)(key, `)(`, `π-τ`)(code)(body)

  /**
    * names and values
    */
  final implicit class `()`(private[sΠ] val name: Any) extends AnyVal with Macros:

    protected def map = `()`[Map[Int, {}]]

    def ====(that: `()`) =
      try
        this.map eq that.map
      catch _ =>
        this.name == that.name

    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

    // LINEAR REPLICATION ///////////////////////////////////////////////// π //

    /////////////////////////////////////////////////////////////////// BOUND //

    /**
      * linear replication bound output guard
      */
    def apply(_nu: "ν")(_f: false)(parallelism: Int, rate: Rate)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(body: `Π-Function1`)
                                  (using %, /, \)
                                  (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.output("ν")(false)(parallelism, rate)(key, `)(`, dir)(body)

    /**
      * linear replication bound output guard w/ pace
      */
    def apply(_nu: "ν")(_f: false)(pace: FiniteDuration, parallelism: Int, rate: Rate)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(body: `Π-Function1`)
                                  (using %, /, \)
                                  (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.output("ν")(false)(pace, parallelism, rate)(key, `)(`, dir)(body)

    /**
      * linear replication bound output guard w/ code
      */
    def apply(_nu: "ν")(_t: true)(parallelism: Int, rate: Rate)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: => IO[Any])(body: `Π-Function1`)
                                 (using %, /, \)
                                 (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.output("ν")(true)(parallelism, rate)(key, `)(`, dir)(code)(body)

    /**
      * linear replication bound output guard w/ pace w/ code
      */
    def apply(_nu: "ν")(_t: true)(pace: FiniteDuration, parallelism: Int, rate: Rate)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: => IO[Any])(body: `Π-Function1`)
                                 (using %, /, \)
                                 (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.output("ν")(true)(pace, parallelism, rate)(key, `)(`, dir)(code)(body)

    //////////////////////////////////////////////////////////////// CONSTANT //

    /**
      * linear constant replication output guard
      */
    def apply(_f: false)(parallelism: Int, rate: Rate, value: `()`)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(body: `Π-Function0`)
                        (using %, /, \)
                        (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.output(false)(parallelism, rate, value)(key, `)(`, dir)(body)

    /**
      * linear constant replication output guard w/ pace
      */
    def apply(_f: false)(pace: FiniteDuration, parallelism: Int, rate: Rate, value: `()`)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(body: `Π-Function0`)
                        (using %, /, \)
                        (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.output(false)(pace, parallelism, rate, value)(key, `)(`, dir)(body)

    /**
      * linear constant replication output guard w/ code
      */
    def apply(_t: true)(parallelism: Int, rate: Rate, value: `()`)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: => IO[Any])(body: `Π-Function0`)
                       (using %, /, \)
                       (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.output(true)(parallelism, rate, value)(key, `)(`, dir)(code)(body)

    /**
      * linear constant replication output guard w/ pace w/ code
      */
    def apply(_t: true)(pace: FiniteDuration, parallelism: Int, rate: Rate, value: `()`)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: => IO[Any])(body: `Π-Function0`)
                       (using %, /, \)
                       (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.output(true)(pace, parallelism, rate, value)(key, `)(`, dir)(code)(body)

    //////////////////////////////////////////////////////////////// VARIABLE //

    /**
      * linear variable replication output guard
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(parallelism: Int, rate: Rate, value: => S)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(body: `Π-Function0`)(using DummyImplicit)
                                              (using %, /, \)
                                              (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
     if classTag[S].runtimeClass eq getClass
     then
       apply(false)(parallelism, rate, value.asInstanceOf[`()`])(key, `)(`, dir)(body)
     else
       apply("*")(false)(parallelism, rate, IO.delay(value))(key, `)(`, dir)(body)

    /**
      * linear variable replication output guard w/ pace
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(pace: FiniteDuration, parallelism: Int, rate: Rate, value: => S)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(body: `Π-Function0`)(using DummyImplicit)
                                              (using %, /, \)
                                              (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
     if classTag[S].runtimeClass eq getClass
     then
       apply(false)(pace, parallelism, rate, value.asInstanceOf[`()`])(key, `)(`, dir)(body)
     else
       apply("*")(false)(pace, parallelism, rate, IO.delay(value))(key, `)(`, dir)(body)

    /**
      * linear variable replication output guard w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(parallelism: Int, rate: Rate, value: => S)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: => IO[Any])(body: `Π-Function0`)(using DummyImplicit)
                                             (using %, /, \)
                                             (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
     if classTag[S].runtimeClass eq getClass
     then
       apply(true)(parallelism, rate, value.asInstanceOf[`()`])(key, `)(`, dir)(code)(body)
     else
       apply("*")(true)(parallelism, rate, IO.delay(value))(key, `)(`, dir)(code)(body)

    /**
      * linear variable replication output guard w/ pace w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(pace: FiniteDuration, parallelism: Int, rate: Rate, value: => S)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: => IO[Any])(body: `Π-Function0`)(using DummyImplicit)
                                             (using %, /, \)
                                             (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
     if classTag[S].runtimeClass eq getClass
     then
       apply(true)(pace, parallelism, rate, value.asInstanceOf[`()`])(key, `)(`, dir)(code)(body)
     else
       apply("*")(true)(pace, parallelism, rate, IO.delay(value))(key, `)(`, dir)(code)(body)

    /**
      * linear variable replication output guard
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(parallelism: Int, rate: Rate, value: => IO[S])(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(body: `Π-Function0`)
                                              (using %, /, \)
                                              (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        IO.defer(value.asInstanceOf[IO[`()`]].flatMap(apply(false)(parallelism, rate, _)(key, `)(`, dir)(body)))
      else
        super.output("*")(false)(parallelism, rate, value)(key, `)(`, dir)(body)

    /**
      * linear variable replication output guard w/ pace
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(pace: FiniteDuration, parallelism: Int, rate: Rate, value: => IO[S])(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(body: `Π-Function0`)
                                              (using %, /, \)
                                              (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        IO.defer(value.asInstanceOf[IO[`()`]].flatMap(apply(false)(pace, parallelism, rate, _)(key, `)(`, dir)(body)))
      else
        super.output("*")(false)(pace, parallelism, rate, value)(key, `)(`, dir)(body)

    /**
      * linear variable replication output guard w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(parallelism: Int, rate: Rate, value: => IO[S])(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: => IO[Any])(body: `Π-Function0`)
                                             (using %, /, \)
                                             (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        IO.defer(value.asInstanceOf[IO[`()`]].flatMap(apply(true)(parallelism, rate, _)(key, `)(`, dir)(code)(body)))
      else
        super.output("*")(true)(parallelism, rate, value)(key, `)(`, dir)(code)(body)

    /**
      * linear variable replication output guard w/ pace w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(pace: FiniteDuration, parallelism: Int, rate: Rate, value: => IO[S])(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: => IO[Any])(body: `Π-Function0`)
                                             (using %, /, \)
                                             (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        IO.defer(value.asInstanceOf[IO[`()`]].flatMap(apply(true)(pace, parallelism, rate, _)(key, `)(`, dir)(code)(body)))
      else
        super.output("*")(true)(pace, parallelism, rate, value)(key, `)(`, dir)(code)(body)

    /////////////////////////////////////////////////////////////////// INPUT //

    /**
      * linear replication input guard
      */
    def apply(_n: Null)(_f: false)(parallelism: Int, rate: Rate)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(body: `Π-Function1`)
                                  (using %, /, \)
                                  (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.input(false)(parallelism, rate)(key, `)(`, dir)(body)

    /**
      * linear replication input guard w/ pace
      */
    def apply(_n: Null)(_f: false)(pace: FiniteDuration, parallelism: Int, rate: Rate)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(body: `Π-Function1`)
                                  (using %, /, \)
                                  (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.input(false)(pace, parallelism, rate)(key, `)(`, dir)(body)

    /**
      * linear replication input guard w/ code
      */
    def apply[T](_n: Null)(_t: true)(parallelism: Int, rate: Rate)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: T => IO[T])(body: `Π-Function1`)
                                    (using %, /, \)
                                    (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.input(true)(parallelism, rate)(key, `)(`, dir)(code)(body)

    /**
      * linear replication input guard w/ pace w/ code
      */
    def apply[T](_n: Null)(_t: true)(pace: FiniteDuration, parallelism: Int, rate: Rate)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: T => IO[T])(body: `Π-Function1`)
                                    (using %, /, \)
                                    (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.input(true)(pace, parallelism, rate)(key, `)(`, dir)(code)(body)

    // π ///////////////////////////////////////////////// linear replication //

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(rate: Rate, value: => S)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)
                                     (using DummyImplicit)
                                     (using %, /)
                                     (using `Π-Map`[String, `Π-Set`[String]], String): IO[java.lang.Double] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(rate, value.asInstanceOf[`()`])(key, `)(`, dir)
      else
        apply(false)(rate, IO.delay(value))(key, `)(`, dir)

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(rate: Rate, value: => S)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: => IO[Any])
                                    (using DummyImplicit)
                                    (using %, /)
                                    (using `Π-Map`[String, `Π-Set`[String]], String): IO[java.lang.Double] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(rate, value.asInstanceOf[`()`])(key, `)(`, dir)(code)
      else
        apply(true)(rate, IO.delay(value))(key, `)(`, dir)(code)

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(rate: Rate, value: => IO[S])(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)
                                     (using %, /)
                                     (using `Π-Map`[String, `Π-Set`[String]], String): IO[java.lang.Double] =
      if classTag[S].runtimeClass eq getClass
      then
        IO.defer(value.asInstanceOf[IO[`()`]].flatMap(apply(rate, _)(key, `)(`, dir)))
      else
        IO.defer(value.map(new `()`(_)).flatMap(apply(rate, _)(key, `)(`, dir)))

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(rate: Rate, value: => IO[S])(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: => IO[Any])
                                    (using %, /)
                                    (using `Π-Map`[String, `Π-Set`[String]], String): IO[java.lang.Double] =
      if classTag[S].runtimeClass eq getClass
      then
        IO.defer(value.asInstanceOf[IO[`()`]].flatMap(apply(rate, _)(key, `)(`, dir)(code)))
      else
        IO.defer(value.map(new `()`(_)).flatMap(apply(rate, _)(key, `)(`, dir)(code)))

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[java.lang.Double] =
      for
        _        <- exclude(key)
        deferred <- IO.deferred[Option[<>]]
        `)(`     <- `)(`.get
        timestamp <- IO.monotonic.map(_.toNanos) >>= IO.ref
        _        <- /.offer(^ -> key -> ((deferred -> null, `)(` -> dir, timestamp), (map(dir.ord), Some(Left(())), rate)))
        opt      <- deferred.get
        delay    <- ( if opt eq None
                      then
                        IO.pure(null: java.lang.Double)
                      else
                        val (delay, b, f, i) = opt.get
                        for
                          _ <- i.set(value)
                          _ <- b.await
                          _ <- f.join
                        yield
                          java.lang.Double(delay)
                    )
      yield
        delay

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: => IO[Any])
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[java.lang.Double] =
      for
        _        <- exclude(key)
        deferred <- IO.deferred[Option[<>]]
        `)(`     <- `)(`.get
        timestamp <- IO.monotonic.map(_.toNanos) >>= IO.ref
        _        <- /.offer(^ -> key -> ((deferred -> null, `)(` -> dir, timestamp), (map(dir.ord), Some(Left(())), rate)))
        opt      <- deferred.get
        delay    <- ( if opt eq None
                      then
                        IO.pure(null: java.lang.Double)
                      else
                        val (delay, b, f, i) = opt.get
                        for
                          _ <- i.set(value)
                          _ <- b.await
                          _ <- f.join
                          _ <- exec(code)
                        yield
                          java.lang.Double(delay)
                    )
      yield
        delay

    /**
      * positive prefix i.e. input
      */
    def apply(rate: Rate)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[(`()`, java.lang.Double)] =
      for
        _        <- exclude(key)
        deferred <- IO.deferred[Option[<>]]
        result   <- IO.ref[`()`](sΠ.`()`.`null`)
        `)(`     <- `)(`.get
        timestamp <- IO.monotonic.map(_.toNanos) >>= IO.ref
        _        <- /.offer(^ -> key -> ((deferred -> null, `)(` -> dir, timestamp), (map(dir.ord), Some(Right(result)), rate)))
        opt      <- deferred.get
        (name,
         delay)  <- ( if opt eq None
                      then
                        IO.pure(sΠ.`()`.`null` -> (null: java.lang.Double))
                      else
                        val (delay, b, f, _) = opt.get
                        for
                          _    <- b.await
                          _    <- f.join
                          name <- result.get
                        yield
                          name -> java.lang.Double(delay)
                    )
      yield
        name -> delay

    /**
      * positive prefix i.e. input
      */
    def apply[T](rate: Rate)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: T => IO[T])
                (using % : %, / : /)
                (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): IO[(`()`, java.lang.Double)] =
      for
        _        <- exclude(key)
        deferred <- IO.deferred[Option[<>]]
        result   <- IO.ref[`()`](sΠ.`()`.`null`)
        `)(`     <- `)(`.get
        timestamp <- IO.monotonic.map(_.toNanos) >>= IO.ref
        _        <- /.offer(^ -> key -> ((deferred -> null, `)(` -> dir, timestamp), (map(dir.ord), Some(Right(result)), rate)))
        opt      <- deferred.get
        (name,
         delay)  <- ( if opt eq None
                      then
                        IO.pure((null: Any) -> (null: java.lang.Double))
                      else
                        val (delay, b, f, _) = opt.get
                        for
                          _    <- b.await
                          _    <- f.join
                          name <- result.get.map(_.name).flatMap { case it: T => (code andThen exec)(it) }
                        yield
                          name -> java.lang.Double(delay)
                    )
      yield
        new `()`(name) -> delay

    // LINEAR REPLICATION ///////////////////////////////////////////////// ζ //

    /**
      * linear capability replication guard
      */
    def apply(_z: "ζ")(_f: false)(parallelism: Int, rate: Rate)(key: String, `)(`: IOLocal[`)(`], cap: `π-ζ`)(body: `Π-Function0`)
                                 (using %, /, \)
                                 (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.capability(false)(parallelism, rate)(key, `)(`, cap)(body)

    /**
      * linear capability constant replication guard w/ pace
      */
    def apply(_z: "ζ")(_f: false)(pace: FiniteDuration, parallelism: Int, rate: Rate)(key: String, `)(`: IOLocal[`)(`], cap: `π-ζ`)(body: `Π-Function0`)
                                 (using %, /, \)
                                 (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.capability(false)(pace, parallelism, rate)(key, `)(`, cap)(body)

    /**
      * linear capability replication guard w/ code
      */
    def apply(_z: "ζ")(_t: true)(parallelism: Int, rate: Rate)(key: String, `)(`: IOLocal[`)(`], cap: `π-ζ`)(code: => IO[Any])(body: `Π-Function0`)
                                (using %, /, \)
                                (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.capability(true)(parallelism, rate)(key, `)(`, cap)(code)(body)

    /**
      * linear capability replication guard w/ pace w/ code
      */
    def apply(_z: "ζ")(_t: true)(pace: FiniteDuration, parallelism: Int, rate: Rate)(key: String, `)(`: IOLocal[`)(`], cap: `π-ζ`)(code: => IO[Any])(body: `Π-Function0`)
                                (using %, /, \)
                                (using `Π-Map`[String, `Π-Set`[String]], String): IO[Unit] =
      super.capability(true)(pace, parallelism, rate)(key, `)(`, cap)(code)(body)

    // ζ ///////////////////////////////////////////////// linear replication //

    /**
      * capability prefix
      */
    def apply(rate: Rate)(key: String, `)(`: IOLocal[`)(`], cap: `π-ζ`)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[java.lang.Double] =
      for
        _        <- exclude(key)
        deferred <- IO.deferred[Option[<>]]
        polarity  = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
        `)(`     <- `)(`.get
        timestamp <- IO.monotonic.map(_.toNanos) >>= IO.ref
        _        <- /.offer(^ -> key -> ((deferred -> null, `)(` -> cap, timestamp), (map(cap.ord), Some(if polarity then Right(null) else Left(())), rate)))
        opt      <- deferred.get
        delay    <- ( if opt eq None
                      then
                        IO.pure(null: java.lang.Double)
                      else
                        val (delay, b, f, _) = opt.get
                        for
                          _ <- b.await
                          _ <- f.join
                        yield
                          java.lang.Double(delay)
                    )
      yield
        delay

    /**
      * capability prefix
      */
    def apply(rate: Rate)(key: String, `)(`: IOLocal[`)(`], cap: `π-ζ`)(code: => IO[Any])
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[java.lang.Double] =
      for
        _        <- exclude(key)
        deferred <- IO.deferred[Option[<>]]
        polarity  = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
        `)(`     <- `)(`.get
        timestamp <- IO.monotonic.map(_.toNanos) >>= IO.ref
        _        <- /.offer(^ -> key -> ((deferred -> null, `)(` -> cap, timestamp), (map(cap.ord), Some(if polarity then Right(null) else Left(())), rate)))
        opt      <- deferred.get
        delay    <- ( if opt eq None
                      then
                        IO.pure(null: java.lang.Double)
                      else
                        val (delay, b, f, _) = opt.get
                        for
                          _ <- b.await
                          _ <- f.join
                          _ <- exec(code)
                        yield
                          java.lang.Double(delay)
                    )
      yield
        delay

    override def toString: String = if name == null then "null" else name.toString


  private object `()`:

    val `null` = new `()`(null)


  final class `}{`(val stm: STM[IO]):

    import stm.*

    /**
      * Ambients' trees' nodes.
      */
    final case class `}{`(label: Option[String],
                          root: `)*(`,
                          children: Set[`)*(`],
                          siblings: Set[`)*(`])

    object `}{`:
      def apply(`)(`: IOLocal[`)(`], label: Option[String])
               (using `][`: `][`, `1`: TSemaphore): IO[Unit] =
        for
          key  <- `)(`.get
          uuid <- sΠ.`)(`()
          node  = Set(uuid)
          _    <- `)(`.set(uuid)
          _    <- stm.commit {
            for
              _ <- `1`.acquire
              _ <- `][`.modify { m =>
                                 val root = m.keys.find(_.contains(key)).get
                                 val tree @ `}{`(_, _, children, _) = m(root)
                                 children.foldLeft {
                                   m + (node -> `}{`(label, root, Set.empty, children))
                                     + (root -> tree.copy(children = children + node))
                                 } { (m, child) =>
                                   val tree @ `}{`(_, _, _, siblings) = m(child)
                                   m + (child -> tree.copy(siblings = siblings + node))
                                 }
                               }
              _ <- `1`.release
            yield
              ()
          }
        yield
          ()

      def apply(key: `)(`, snapshot: Boolean = false)
               (using `][`: `][`): Txn[(String, String)] =
        `][`.get.map { m =>
                       var root = m.keys.find(_.contains(key)).get
                       def label(node: `)*(`): String = m(node).label.getOrElse("")
                       label(root) -> (
                         if !snapshot
                         then ""
                         else
                           while m(root).root ne null do root = m(root).root
                           var id = 0
                           var tree = Map[`)*(`, Int](root -> id)
                           def make(root: `)*(`): Unit =
                             for
                               node <- m(root).children
                             do
                               id += 1
                               tree += node -> id
                               make(node)
                           make(root)
                           def xml(root: `)*(`, count: Int, indent: String): StringBuilder =
                             val pid = tree(root)
                             def siblings(node: `)*(`, count: Int): StringBuilder =
                               val sid = tree(node)
                               val sb = StringBuilder()
                               sb.append(s"$indent\t\t<siblings count=$count sibling=$sid>\n")
                                 .append {
                                   ( for
                                       nodeʹ <- m(node).siblings
                                       sidʹ   = tree(nodeʹ)
                                     yield
                                       StringBuilder(s"""$indent\t\t\t<node id=$sidʹ label="${label(nodeʹ)}" parent=$pid sibling=$sid/>""")
                                   ).reduce(_.append("\n").append(_)).append("\n")
                                 }
                                 .append(s"$indent\t\t</siblings>\n")
                             def children: StringBuilder =
                               val sb = StringBuilder()
                               sb.append(s"$indent<children count=$count parent=$pid>\n")
                                 .append {
                                   ( for
                                       node <- m(root).children
                                       cid   = tree(node)
                                     yield
                                       val sbʹ = StringBuilder()
                                       val count = m(node).children.size
                                       if count == 0
                                       then
                                         val count = m(node).siblings.size
                                         if count == 0
                                         then
                                           sbʹ.append(s"""$indent\t<node id=$cid label="${label(node)}" parent=$pid/>""")
                                         else
                                           sbʹ.append(s"""$indent\t<node id=$cid label="${label(node)}" parent=$pid>\n""")
                                              .append(siblings(node, count))
                                              .append(s"$indent\t</node>")
                                       else
                                         sbʹ.append(s"""$indent\t<node id=$cid label="${label(node)}" parent=$pid>\n""")
                                            .append(xml(node, count, indent + "\t\t"))
                                            .append("\n")
                                            .append {
                                              val count = m(node).siblings.size
                                              if count == 0
                                              then
                                                StringBuilder()
                                              else
                                                siblings(node, count)
                                            }
                                            .append(s"$indent\t</node>")
                                   ).reduce(_.append("\n").append(_)).append("\n")
                                 }
                                 .append(s"$indent</children>")
                             children
                           val count = m(root).children.size
                           val sb = StringBuilder()
                           if count == 0
                           then
                             sb.append(s"""<root id=${tree(root)} label="${label(root)}"/>\n""")
                               .toString
                           else
                             sb.append(s"""<root id=${tree(root)} label="${label(root)}">\n""")
                               .append(xml(root, count, "\t"))
                               .append("\n</root>")
                               .toString
                       )
                     }

    /**
      * Type of ambients' trees.
      */
    type `][` = TVar[Map[`)*(`, `}{`]]

    object `][`:
      def apply(): IO[(IOLocal[`)(`], `][`, TSemaphore)] =
        for
          uuid <- `)(`()
          root  = Set(uuid)
          lo   <- IOLocal[`)(`](uuid)
          map   = Map(root -> `}{`(None, null, Set.empty, Set.empty))
          tree <- stm.commit { TVar.of[Map[`)*(`, `}{`]](map) }
          sem  <- stm.commit { TSemaphore.make(1) }
        yield
          (lo, tree, sem)

    object >< :

      @annotation.tailrec
      private def check(node: `)*(`,
                        nodeʹ: `)*(`,
                        dir_cap: `π-$` | `π-ζ`,
                        dir_capʹ: `π-$` | `π-ζ`)
                       (using `][`: `][`): Txn[Boolean] =
        (dir_cap, dir_capʹ) match
          case (`π-local`, `π-local`)   =>
            stm.pure(node == nodeʹ)
          case (`π-s2s`, `π-s2s`)
             | (`π-enter`, `π-accept`)
             | (`π-merge+`, `π-merge-`) =>
            `][`.get.map(_(node).siblings.contains(nodeʹ))
          case (`π-p2c`, `π-c2p`)
             | (`π-expel`, `π-exit`)    =>
            `][`.get.map(_(nodeʹ).root == node)
          case (`π-c2p`, `π-p2c`)       => check(nodeʹ, node, dir_capʹ, dir_cap)
          case (`π-accept`, `π-enter`)  => check(nodeʹ, node, dir_capʹ, dir_cap)
          case (`π-exit`, `π-expel`)    => check(nodeʹ, node, dir_capʹ, dir_cap)
          case (`π-merge-`, `π-merge+`) => check(nodeʹ, node, dir_capʹ, dir_cap)

      object π:

        def apply(key: `)(`, dir: `π-$`, keyʹ: `)(`, dirʹ: `π-$`)
                 (using `][`: `][`, `1`: TSemaphore): IO[Unit] =
          stm.commit {
            for
              _     <- `1`.acquire
              node  <- `][`.get.map(_.keys.find(_.contains(key)).get)
              nodeʹ <- `][`.get.map(_.keys.find(_.contains(keyʹ)).get)
              _     <- check(node, nodeʹ, dir, dirʹ).flatMap(stm.check(_))
            yield
              ()
          }

      object ζ:

        private def remove(node: `)*(`, tree: `}{`)
                          (using `][`: `][`): Txn[Unit] =
          val `}{`(_, root, _, siblings) = tree
          `][`.modify { m =>
                        val rtree = m(root)
                        siblings.foldLeft {
                          m + (root -> rtree.copy(children = siblings))
                        } { (m, sibling) =>
                          val tree @ `}{`(_, _, _, siblings) = m(sibling)
                          m + (sibling -> tree.copy(siblings = siblings - node))
                        }
                      }

        private def insert(node: `)*(`, root: `)*(`)
                          (using `][`: `][`): Txn[Unit] =
          for
            _ <- `][`.modify { m =>
                               val tree = m(root)
                               tree.children.foldLeft(m) { (m, child) =>
                                 val tree @ `}{`(_, _, _, siblings) = m(child)
                                 m + (child -> tree.copy(siblings = siblings + node))
                               }
                             }
            _ <- `][`.modify { m =>
                               val ntree = m(node)
                               val rtree @ `}{`(_, _, children, _) = m(root)
                               m + (root -> rtree.copy(children = children + node))
                                 + (node -> ntree.copy(root = root, siblings = children))
                             }
          yield
            ()

        private def update(temp: `}{`, root: `)*(`, join: `)*(`)
                          (using `][`: `][`): Txn[Unit] =
          `][`.modify { m =>
                        val tree @ `}{`(_, _, children, _) = m(temp.root)
                        temp.siblings.foldLeft {
                          m + (temp.root -> tree.copy(children = children - root + join))
                        } { (m, sibling) =>
                          val tree @ `}{`(_, _, _, siblings) = m(sibling)
                          m + (sibling -> tree.copy(siblings = siblings - root + join))
                        }
                      }

        private def merge(tree: `}{`, join: `)*(`)
                         (using `][`: `][`): Txn[Unit] =
          for
            _ <- `][`.modify { tree.children.foldLeft(_) { (m, node) =>
                                val tree = m(node)
                                m + (node -> tree.copy(root = join))
                               }
                             }
            _ <- `][`.modify { m =>
                               val temp @ `}{`(_, _, children, _) = m(join)
                               tree.children.foldLeft {
                                 m + (join -> temp.copy(children = children ++ tree.children))
                               } { (m, node) =>
                                 val tree = m(node)
                                 m + (node -> tree.copy(siblings = tree.siblings ++ children))
                               }
                             }
          yield
            ()

        @annotation.tailrec
        private def apply(node: `)*(`, nodeʹ: `)*(`, cap: `π-ζ`, capʹ: `π-ζ`)
                         (using `][`: `][`): Txn[Unit] =
          cap match
            case `π-enter` | `π-exit` =>
              for
                m            <- `][`.get
                (root, tree)  = cap match
                                  case `π-enter` =>
                                    (nodeʹ, m(node))
                                  case `π-exit` =>
                                    (m(nodeʹ).root, m(node))
                _            <- remove(node, tree)
                _            <- insert(node, root)
              yield
                ()

            case `π-merge+` =>
              for
                m    <- `][`.get
                tree  = m(nodeʹ)
                _    <- remove(nodeʹ, tree)
                m    <- `][`.get
                temp  =  m(node)
                join  = node ++ nodeʹ
                _    <- `][`.modify { _ - node - nodeʹ + (join -> temp) }
                _    <- update(temp, node, join)
                _    <- merge(tree, join)
              yield
                ()

            case _ =>
              apply(nodeʹ, node, capʹ, cap)

        def apply(key: `)(`, cap: `π-ζ`, keyʹ: `)(`, capʹ: `π-ζ`)
                 (using `][`: `][`, `1`: TSemaphore): IO[Unit] =
          stm.commit {
            for
              _     <- `1`.acquire
              node  <- `][`.get.map(_.keys.find(_.contains(key)).get)
              nodeʹ <- `][`.get.map(_.keys.find(_.contains(keyʹ)).get)
              _     <- check(node, nodeʹ, cap, capʹ).flatMap(stm.check(_))
              _     <- this(node, nodeʹ, cap, capʹ)
            yield
              ()
          }
