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

  import _root_.scala.reflect.{ ClassTag, classTag }

  import _root_.zio.{ Duration, Exit, FiberRef, Promise, Random, Ref, Task, UIO, ZIO }
  import _root_.zio.stm.{ TRef, TSemaphore }
  import _root_.zio.stm.{ USTM, ZSTM }

  import `Π-loop`.{ <>, %, /, \ }
  import `Π-stats`.Rate

  import `π-$`.*, `π-ζ`.*


  type `Π-Map`[K, +V] = Map[K, V]

  type `Π-Set`[A] = Set[A]

  type `Π-Function0` = () => String ?=> UIO[Any]
  type `Π-Function1` = `()` => String ?=> UIO[Any]


  given [A]: Conversion[Task[A], UIO[A]] =
    _.either.map {
      case Right(it) => it
      case _         => null.asInstanceOf[A]
    }

  extension (self: ZIO.type)
    def apply[A](a: => A): UIO[A] =
      ZIO.attempt(a)


  private def exec[T](code: Task[T]): UIO[T] =
    code.fork.flatMap(_.join.exit).map {
      case Exit.Success(it) => it
      case _                => null.asInstanceOf[T]
    }


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
    def apply(): UIO[`)(`] =
      Random.nextUUID.map(new `)(`(_))

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


  inline def `π-exclude`(enabled: String*)
                        (using % : %, \ : \): UIO[Unit] =
    \(`π-exclude`(Set.from(enabled)))

  private def `π-exclude`(enabled: `Π-Set`[String])
                         (using % : %): UIO[Unit] =
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
                     (using %)
                     (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]]): UIO[Unit] =
    ZIO.when(`π-elvis`.contains(key))(`π-exclude`(`π-elvis`(key))).unit


  /**
    * restriction aka new name
    */
  object ν:

    def map[B](f: `()` => B): UIO[B] = flatMap(f andThen ZIO.succeed)
    def flatMap[B](f: `()` => UIO[B]): UIO[B] =
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

    def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`])
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): UIO[Double] =
      for
        _        <- exclude(key)
        promise  <- Promise.make[Nothing, Option[<>]]
        `)(`     <- `)(`.get
        _        <- /.offer(^ -> key -> ((promise -> null, `)(` -> `π-τ`), (`new {}`, None, rate)))
        opt      <- promise.await
        _        <- if opt eq None then ZIO.interrupt else ZIO.unit
        (delay,
         b, f, _) = opt.get
        _        <- b.await.exit
        _        <- f.join
      yield
        delay

    /**
      * linear replication guard
      */
    def apply(_f: false)(parallelism: Int, rate: Rate)(key: String, `)(`: FiberRef[`)(`])(body: `Π-Function0`)
                        (using %, /, \)
                        (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.silent(false)(parallelism, rate)(key, `)(`, `π-τ`)(body)

    /**
      * linear replication guard w/ pace
      */
    def apply(_f: false)(pace: Duration, parallelism: Int, rate: Rate)(key: String, `)(`: FiberRef[`)(`])(body: `Π-Function0`)
                        (using %, /, \)
                        (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.silent(false)(pace, parallelism, rate)(key, `)(`, `π-τ`)(body)

    /**
      * linear replication guard w/ code
      */
    def apply(_t: true)(parallelism: Int, rate: Rate)(key: String, `)(`: FiberRef[`)(`])(code: => Task[Any])(body: `Π-Function0`)
                       (using %, /, \)
                       (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.silent(true)(parallelism, rate)(key, `)(`, `π-τ`)(code)(body)

    /**
      * linear replication guard w/ pace w/ code
      */
    def apply(_t: true)(pace: Duration, parallelism: Int, rate: Rate)(key: String, `)(`: FiberRef[`)(`])(code: => Task[Any])(body: `Π-Function0`)
                       (using %, /, \)
                       (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
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
    def apply(_nu: "ν")(_f: false)(parallelism: Int, rate: Rate)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(body: `Π-Function1`)
                                  (using %, /, \)
                                  (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.output("ν")(false)(parallelism, rate)(key, `)(`, dir)(body)

    /**
      * linear replication bound output guard w/ pace
      */
    def apply(_nu: "ν")(_f: false)(pace: Duration, parallelism: Int, rate: Rate)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(body: `Π-Function1`)
                                  (using %, /, \)
                                  (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.output("ν")(false)(pace, parallelism, rate)(key, `)(`, dir)(body)

    /**
      * linear replication bound output guard w/ code
      */
    def apply(_nu: "ν")(_t: true)(parallelism: Int, rate: Rate)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(code: => Task[Any])(body: `Π-Function1`)
                                 (using %, /, \)
                                 (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.output("ν")(true)(parallelism, rate)(key, `)(`, dir)(code)(body)

    /**
      * linear replication bound output guard w/ pace w/ code
      */
    def apply(_nu: "ν")(_t: true)(pace: Duration, parallelism: Int, rate: Rate)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(code: => Task[Any])(body: `Π-Function1`)
                                 (using %, /, \)
                                 (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.output("ν")(true)(pace, parallelism, rate)(key, `)(`, dir)(code)(body)

    //////////////////////////////////////////////////////////////// CONSTANT //

    /**
      * linear constant replication output guard
      */
    def apply(_f: false)(parallelism: Int, rate: Rate, value: `()`)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(body: `Π-Function0`)
                        (using %, /, \)
                        (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.output(false)(parallelism, rate, value)(key, `)(`, dir)(body)

    /**
      * linear constant replication output guard w/ pace
      */
    def apply(_f: false)(pace: Duration, parallelism: Int, rate: Rate, value: `()`)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(body: `Π-Function0`)
                        (using %, /, \)
                        (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.output(false)(pace, parallelism, rate, value)(key, `)(`, dir)(body)

    /**
      * linear constant replication output guard w/ code
      */
    def apply(_t: true)(parallelism: Int, rate: Rate, value: `()`)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(code: => Task[Any])(body: `Π-Function0`)
                       (using %, /, \)
                       (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.output(true)(parallelism, rate, value)(key, `)(`, dir)(code)(body)

    /**
      * linear constant replication output guard w/ pace w/ code
      */
    def apply(_t: true)(pace: Duration, parallelism: Int, rate: Rate, value: `()`)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(code: => Task[Any])(body: `Π-Function0`)
                       (using %, /, \)
                       (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.output(true)(pace, parallelism, rate, value)(key, `)(`, dir)(code)(body)

    //////////////////////////////////////////////////////////////// VARIABLE //

    /**
      * linear variable replication output guard
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(parallelism: Int, rate: Rate, value: => S)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(body: `Π-Function0`)(using DummyImplicit)
                                              (using %, /, \)
                                              (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
     if classTag[S].runtimeClass eq getClass
     then
       apply(false)(parallelism, rate, value.asInstanceOf[`()`])(key, `)(`, dir)(body)
     else
       apply("*")(false)(parallelism, rate, ZIO.attempt(value))(key, `)(`, dir)(body)

    /**
      * linear variable replication output guard w/ pace
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(pace: Duration, parallelism: Int, rate: Rate, value: => S)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(body: `Π-Function0`)(using DummyImplicit)
                                              (using %, /, \)
                                              (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
     if classTag[S].runtimeClass eq getClass
     then
       apply(false)(pace, parallelism, rate, value.asInstanceOf[`()`])(key, `)(`, dir)(body)
     else
       apply("*")(false)(pace, parallelism, rate, ZIO.attempt(value))(key, `)(`, dir)(body)

    /**
      * linear variable replication output guard w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(parallelism: Int, rate: Rate, value: => S)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(code: => Task[Any])(body: `Π-Function0`)(using DummyImplicit)
                                             (using %, /, \)
                                             (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
     if classTag[S].runtimeClass eq getClass
     then
       apply(true)(parallelism, rate, value.asInstanceOf[`()`])(key, `)(`, dir)(code)(body)
     else
       apply("*")(true)(parallelism, rate, ZIO.attempt(value))(key, `)(`, dir)(code)(body)

    /**
      * linear variable replication output guard w/ pace w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(pace: Duration, parallelism: Int, rate: Rate, value: => S)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(code: => Task[Any])(body: `Π-Function0`)(using DummyImplicit)
                                             (using %, /, \)
                                             (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
     if classTag[S].runtimeClass eq getClass
     then
       apply(true)(pace, parallelism, rate, value.asInstanceOf[`()`])(key, `)(`, dir)(code)(body)
     else
       apply("*")(true)(pace, parallelism, rate, ZIO.attempt(value))(key, `)(`, dir)(code)(body)

    /**
      * linear variable replication output guard
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(parallelism: Int, rate: Rate, value: => Task[S])(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(body: `Π-Function0`)
                                              (using %, /, \)
                                              (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]].flatMap(apply(false)(parallelism, rate, _)(key, `)(`, dir)(body)))
      else
        super.output("*")(false)(parallelism, rate, value)(key, `)(`, dir)(body)

    /**
      * linear variable replication output guard w/ pace
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(pace: Duration, parallelism: Int, rate: Rate, value: => Task[S])(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(body: `Π-Function0`)
                                              (using %, /, \)
                                              (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]].flatMap(apply(false)(pace, parallelism, rate, _)(key, `)(`, dir)(body)))
      else
        super.output("*")(false)(pace, parallelism, rate, value)(key, `)(`, dir)(body)

    /**
      * linear variable replication output guard w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(parallelism: Int, rate: Rate, value: => Task[S])(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(code: => Task[Any])(body: `Π-Function0`)
                                             (using %, /, \)
                                             (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]].flatMap(apply(true)(parallelism, rate, _)(key, `)(`, dir)(code)(body)))
      else
        super.output("*")(true)(parallelism, rate, value)(key, `)(`, dir)(code)(body)

    /**
      * linear variable replication output guard w/ pace w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(pace: Duration, parallelism: Int, rate: Rate, value: => Task[S])(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(code: => Task[Any])(body: `Π-Function0`)
                                             (using %, /, \)
                                             (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]].flatMap(apply(true)(pace, parallelism, rate, _)(key, `)(`, dir)(code)(body)))
      else
        super.output("*")(true)(pace, parallelism, rate, value)(key, `)(`, dir)(code)(body)

    /////////////////////////////////////////////////////////////////// INPUT //

    /**
      * linear replication input guard
      */
    def apply(_n: Null)(_f: false)(parallelism: Int, rate: Rate)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(body: `Π-Function1`)
                                  (using %, /, \)
                                  (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.input(false)(parallelism, rate)(key, `)(`, dir)(body)

    /**
      * linear replication input guard w/ pace
      */
    def apply(_n: Null)(_f: false)(pace: Duration, parallelism: Int, rate: Rate)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(body: `Π-Function1`)
                                  (using %, /, \)
                                  (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.input(false)(pace, parallelism, rate)(key, `)(`, dir)(body)

    /**
      * linear replication input guard w/ code
      */
    def apply[T](_n: Null)(_t: true)(parallelism: Int, rate: Rate)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(code: T => Task[T])(body: `Π-Function1`)
                                    (using %, /, \)
                                    (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.input(true)(parallelism, rate)(key, `)(`, dir)(code)(body)

    /**
      * linear replication input guard w/ pace w/ code
      */
    def apply[T](_n: Null)(_t: true)(pace: Duration, parallelism: Int, rate: Rate)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(code: T => Task[T])(body: `Π-Function1`)
                                    (using %, /, \)
                                    (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.input(true)(pace, parallelism, rate)(key, `)(`, dir)(code)(body)

    // π ///////////////////////////////////////////////// linear replication //

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(rate: Rate, value: => S)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)
                                     (using DummyImplicit)
                                     (using %, /)
                                     (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Double] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(rate, value.asInstanceOf[`()`])(key, `)(`, dir)
      else
        apply(false)(rate, ZIO.attempt(value))(key, `)(`, dir)

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(rate: Rate, value: => S)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(code: => Task[Any])
                                    (using DummyImplicit)
                                    (using %, /)
                                    (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Double] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(rate, value.asInstanceOf[`()`])(key, `)(`, dir)(code)
      else
        apply(true)(rate, ZIO.attempt(value))(key, `)(`, dir)(code)

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(rate: Rate, value: => Task[S])(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)
                                     (using %, /)
                                     (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Double] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.suspendSucceed(value.asInstanceOf[UIO[`()`]].flatMap(apply(rate, _)(key, `)(`, dir)))
      else
        ZIO.suspendSucceed(value.map(new `()`(_)).flatMap(apply(rate, _)(key, `)(`, dir)))

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(rate: Rate, value: => Task[S])(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(code: => Task[Any])
                                    (using %, /)
                                    (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Double] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.suspendSucceed(value.asInstanceOf[UIO[`()`]].flatMap(apply(rate, _)(key, `)(`, dir)(code)))
      else
        ZIO.suspendSucceed(value.map(new `()`(_)).flatMap(apply(rate, _)(key, `)(`, dir)(code)))

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): UIO[Double] =
      for
        _        <- exclude(key)
        promise  <- Promise.make[Nothing, Option[<>]]
        `)(`     <- `)(`.get
        _        <- /.offer(^ -> key -> ((promise -> null, `)(` -> dir), (map(dir.ord), Some(Left(())), rate)))
        opt      <- promise.await
        _        <- if opt eq None then ZIO.interrupt else ZIO.unit
        (delay,
         b, f, i) = opt.get
        _        <- i.set(value)
        _        <- b.await.exit
        _        <- f.join
      yield
        delay

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(code: => Task[Any])
             (using %, /)
             (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Double] =
      apply(rate, value)(key, `)(`, dir) <* exec(code)

    /**
      * positive prefix i.e. input
      */
    def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): UIO[(`()`, Double)] =
      for
        _        <- exclude(key)
        promise  <- Promise.make[Nothing, Option[<>]]
        result   <- Ref.make[`()`](sΠ.`()`.`null`)
        `)(`     <- `)(`.get
        _        <- /.offer(^ -> key -> ((promise -> null, `)(` -> dir), (map(dir.ord), Some(Right(result)), rate)))
        opt      <- promise.await
        _        <- if opt eq None then ZIO.interrupt else ZIO.unit
        (delay,
         b, f, _) = opt.get
        _        <- b.await.exit
        _        <- f.join
        name     <- result.get
      yield
        name -> delay

    /**
      * positive prefix i.e. input
      */
    def apply[T](rate: Rate)(key: String, `)(`: FiberRef[`)(`], dir: `π-$`)(code: T => Task[T])
                (using %, /)
                (using `Π-Map`[String, `Π-Set`[String]], String): UIO[(`()`, Double)] =
      apply(rate)(key, `)(`, dir)
        .map(_.name -> _)
        .flatMap {
          case (null, delay)  => ZIO.succeed(sΠ.`()`.`null` -> delay)
          case (it: T, delay) => (code andThen exec)(it).map(new `()`(_) -> delay)
        }

    // LINEAR REPLICATION ///////////////////////////////////////////////// ζ //

    /**
      * linear capability replication guard
      */
    def apply(_z: "ζ")(_f: false)(parallelism: Int, rate: Rate)(key: String, `)(`: FiberRef[`)(`], cap: `π-ζ`)(body: `Π-Function0`)
                                 (using %, /, \)
                                 (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.capability(false)(parallelism, rate)(key, `)(`, cap)(body)

    /**
      * linear capability constant replication guard w/ pace
      */
    def apply(_z: "ζ")(_f: false)(pace: Duration, parallelism: Int, rate: Rate)(key: String, `)(`: FiberRef[`)(`], cap: `π-ζ`)(body: `Π-Function0`)
                                 (using %, /, \)
                                 (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.capability(false)(pace, parallelism, rate)(key, `)(`, cap)(body)

    /**
      * linear capability replication guard w/ code
      */
    def apply(_z: "ζ")(_t: true)(parallelism: Int, rate: Rate)(key: String, `)(`: FiberRef[`)(`], cap: `π-ζ`)(code: => Task[Any])(body: `Π-Function0`)
                                (using %, /, \)
                                (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.capability(true)(parallelism, rate)(key, `)(`, cap)(code)(body)

    /**
      * linear capability replication guard w/ pace w/ code
      */
    def apply(_z: "ζ")(_t: true)(pace: Duration, parallelism: Int, rate: Rate)(key: String, `)(`: FiberRef[`)(`], cap: `π-ζ`)(code: => Task[Any])(body: `Π-Function0`)
                                (using %, /, \)
                                (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.capability(true)(pace, parallelism, rate)(key, `)(`, cap)(code)(body)

    // ζ ///////////////////////////////////////////////// linear replication //

    /**
      * capability prefix
      */
    def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`], cap: `π-ζ`)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): UIO[Double] =
      for
        _        <- exclude(key)
        promise  <- Promise.make[Nothing, Option[<>]]
        polarity  = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
        `)(`     <- `)(`.get
        _        <- /.offer(^ -> key -> ((promise -> null, `)(` -> cap), (map(cap.ord), Some(if polarity then Right(null) else Left(())), rate)))
        opt      <- promise.await
        _        <- if opt eq None then ZIO.interrupt else ZIO.unit
        (delay,
         b, f, _) = opt.get
        _        <- b.await.exit
        _        <- f.join
      yield
        delay

    /**
      * capability prefix
      */
    def apply(rate: Rate)(key: String, `)(`: FiberRef[`)(`], cap: `π-ζ`)(code: => Task[Any])
             (using %, /)
             (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Double] =
      apply(rate)(key, `)(`, cap) <* exec(code)

    override def toString: String = if name == null then "null" else name.toString


  private object `()`:

    val `null` = new `()`(null)


  object `}{`:

    /**
      * Ambients' trees' nodes.
      */
    final case class `}{`(label: Option[String],
                          root: `)*(`,
                          children: Set[`)*(`],
                          siblings: Set[`)*(`])

    object `}{`:
      def apply(`)(`: FiberRef[`)(`], label: Option[String])
               (using `][`: `][`, `1`: TSemaphore): UIO[Unit] =
        for
          key  <- `)(`.get
          uuid <- sΠ.`)(`()
          node  = Set(uuid)
          _    <- `)(`.set(uuid)
          _    <- {
            for
              _ <- `1`.acquire
              _ <- `][`.update { m =>
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
          }.commit
        yield
          ()

    /**
      * Type of ambients' trees.
      */
    type `][` = TRef[Map[`)*(`, `}{`]]

    object `][`:
      def apply(): UIO[(FiberRef[`)(`], `][`, TSemaphore)] =
        for
          uuid <- `)(`()
          root  = Set(uuid)
          ref  <- ZIO.scoped(FiberRef.make[`)(`](uuid))
          map   = Map(root -> `}{`(None, null, Set.empty, Set.empty))
          tree <- TRef.make[Map[`)*(`, `}{`]](map).commit
          sem  <- TSemaphore.make(1).commit
        yield
          (ref, tree, sem)

    object >< :

      @annotation.tailrec
      private def check(node: `)*(`,
                        nodeʹ: `)*(`,
                        dir_cap: `π-$` | `π-ζ`,
                        dir_capʹ: `π-$` | `π-ζ`)
                       (using `][`: `][`): USTM[Boolean] =
        (dir_cap, dir_capʹ) match
          case (`π-local`, `π-local`)   =>
            ZSTM.succeed(node == nodeʹ)
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
                 (using `][`: `][`, `1`: TSemaphore): UIO[Unit] =
          ( for
              _     <- `1`.acquire
              node  <- `][`.get.map(_.keys.find(_.contains(key)).get)
              nodeʹ <- `][`.get.map(_.keys.find(_.contains(keyʹ)).get)
              _     <- check(node, nodeʹ, dir, dirʹ).flatMap(ZSTM.check(_))
              _     <- `1`.release
            yield
              ()
          ).commit

      object ζ:

        private def remove(node: `)*(`, tree: `}{`)
                          (using `][`: `][`): USTM[Unit] =
          val `}{`(_, root, _, siblings) = tree
          `][`.update { m =>
                        val rtree = m(root)
                        siblings.foldLeft {
                          m + (root -> rtree.copy(children = siblings))
                        } { (m, sibling) =>
                          val tree @ `}{`(_, _, _, siblings) = m(sibling)
                          m + (sibling -> tree.copy(siblings = siblings - node))
                        }
                      }

        private def insert(node: `)*(`, root: `)*(`)
                          (using `][`: `][`): USTM[Unit] =
          for
            _ <- `][`.update { m =>
                               val tree = m(root)
                               tree.children.foldLeft(m) { (m, child) =>
                                 val tree @ `}{`(_, _, _, siblings) = m(child)
                                 m + (child -> tree.copy(siblings = siblings + node))
                               }
                             }
            _ <- `][`.update { m =>
                               val ntree = m(node)
                               val rtree @ `}{`(_, _, children, _) = m(root)
                               m + (root -> rtree.copy(children = children + node))
                                 + (node -> ntree.copy(root = root, siblings = children))
                             }
          yield
            ()

        private def update(temp: `}{`, root: `)*(`, join: `)*(`)
                          (using `][`: `][`): USTM[Unit] =
          `][`.update { m =>
                        val tree @ `}{`(_, _, children, _) = m(temp.root)
                        temp.siblings.foldLeft {
                          m + (temp.root -> tree.copy(children = children - root + join))
                        } { (m, sibling) =>
                          val tree @ `}{`(_, _, _, siblings) = m(sibling)
                          m + (sibling -> tree.copy(siblings = siblings - root + join))
                        }
                      }

        private def merge(tree: `}{`, join: `)*(`)
                         (using `][`: `][`): USTM[Unit] =
          for
            _ <- `][`.update { tree.children.foldLeft(_) { (m, node) =>
                                val tree = m(node)
                                m + (node -> tree.copy(root = join))
                               }
                             }
            _ <- `][`.update { m =>
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
                         (using `][`: `][`): USTM[Unit] =
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
                _    <- `][`.update { _ - node - nodeʹ + (join -> temp) }
                _    <- update(temp, node, join)
                _    <- merge(tree, join)
              yield
                ()

            case _ =>
              apply(nodeʹ, node, capʹ, cap)

        def apply(key: `)(`, cap: `π-ζ`, keyʹ: `)(`, capʹ: `π-ζ`)
                 (using `][`: `][`, `1`: TSemaphore): UIO[Unit] =
          ( for
              _     <- `1`.acquire
              node  <- `][`.get.map(_.keys.find(_.contains(key)).get)
              nodeʹ <- `][`.get.map(_.keys.find(_.contains(keyʹ)).get)
              _     <- check(node, nodeʹ, cap, capʹ).flatMap(ZSTM.check(_))
              _     <- this(node, nodeʹ, cap, capʹ)
              _     <- `1`.release
            yield
              ()
          ).commit
