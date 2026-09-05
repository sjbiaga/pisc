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

  import _root_.zio.{ Duration, Exit, Promise, Ref, Task, UIO, ZIO }

  import `Π-loop`.{ <>, %, /, \, currentTimeMillis }
  import `Π-stats`.Rate


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
                     (using % : %)
                     (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]]): UIO[Unit] =
    ZIO.when(`π-elvis`.contains(key))(`π-exclude`(`π-elvis`(key))).unit


  /**
    * restriction aka new name
    */
  object ν:

    def map[B](f: `()` => B): UIO[B] = flatMap(f andThen ZIO.succeed)
    def flatMap[B](f: `()` => Task[B]): UIO[B] = f(new {})


  /**
    * silent transition
    */
  object τ extends τ:

    def apply(rate: Rate)(key: String)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): UIO[java.lang.Double] =
      for
        _        <- exclude(key)
        promise  <- Promise.make[Nothing, Option[<>]]
        timestamp <- currentTimeMillis.flatMap(Ref.make)
        _        <- /.offer(^ -> key -> ((promise -> null, timestamp), (`new {}`, None, rate)))
        opt      <- promise.await
        delay    <- ( if opt eq None
                      then
                        ZIO.succeed(null: java.lang.Double)
                      else
                        val (delay, b, f, _) = opt.get
                        for
                          _ <- b.await.exit
                          _ <- f.join
                        yield
                          java.lang.Double(delay)
                    )
      yield
        delay

    /**
      * linear replication guard
      */
    def apply(_f: false)(parallelism: Int, rate: Rate)(key: String)(body: `Π-Function0`)
                        (using %, /, \)
                        (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.silent(false)(parallelism, rate)(key)(body)

    /**
      * linear replication guard w/ pace
      */
    def apply(_f: false)(pace: Duration, parallelism: Int, rate: Rate)(key: String)(body: `Π-Function0`)
                        (using %, /, \)
                        (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.silent(false)(pace, parallelism, rate)(key)(body)

    /**
      * linear replication guard w/ code
      */
    def apply(_t: true)(parallelism: Int, rate: Rate)(key: String)(code: => Task[Any])(body: `Π-Function0`)
                       (using %, /, \)
                       (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.silent(true)(parallelism, rate)(key)(code)(body)

    /**
      * linear replication guard w/ pace w/ code
      */
    def apply(_t: true)(pace: Duration, parallelism: Int, rate: Rate)(key: String)(code: => Task[Any])(body: `Π-Function0`)
                       (using %, /, \)
                       (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
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
                                  (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.output("ν")(false)(parallelism, rate)(key)(body)

    /**
      * linear replication bound output guard w/ pace
      */
    def apply(_nu: "ν")(_f: false)(pace: Duration, parallelism: Int, rate: Rate)(key: String)(body: `Π-Function1`)
                                  (using %, /, \)
                                  (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.output("ν")(false)(pace, parallelism, rate)(key)(body)

    /**
      * linear replication bound output guard w/ code
      */
    def apply(_nu: "ν")(_t: true)(parallelism: Int, rate: Rate)(key: String)(code: => Task[Any])(body: `Π-Function1`)
                                 (using %, /, \)
                                 (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.output("ν")(true)(parallelism, rate)(key)(code)(body)

    /**
      * linear replication bound output guard w/ pace w/ code
      */
    def apply(_nu: "ν")(_t: true)(pace: Duration, parallelism: Int, rate: Rate)(key: String)(code: => Task[Any])(body: `Π-Function1`)
                                 (using %, /, \)
                                 (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.output("ν")(true)(pace, parallelism, rate)(key)(code)(body)

    //////////////////////////////////////////////////////////////// CONSTANT //

    /**
      * linear constant replication output guard
      */
    def apply(_f: false)(parallelism: Int, rate: Rate, value: `()`)(key: String)(body: `Π-Function0`)
                        (using %, /, \)
                        (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.output(false)(parallelism, rate, value)(key)(body)

    /**
      * linear constant replication output guard w/ pace
      */
    def apply(_f: false)(pace: Duration, parallelism: Int, rate: Rate, value: `()`)(key: String)(body: `Π-Function0`)
                        (using %, /, \)
                        (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.output(false)(pace, parallelism, rate, value)(key)(body)

    /**
      * linear constant replication output guard w/ code
      */
    def apply(_t: true)(parallelism: Int, rate: Rate, value: `()`)(key: String)(code: => Task[Any])(body: `Π-Function0`)
                       (using %, /, \)
                       (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.output(true)(parallelism, rate, value)(key)(code)(body)

    /**
      * linear constant replication output guard w/ pace w/ code
      */
    def apply(_t: true)(pace: Duration, parallelism: Int, rate: Rate, value: `()`)(key: String)(code: => Task[Any])(body: `Π-Function0`)
                       (using %, /, \)
                       (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.output(true)(pace, parallelism, rate, value)(key)(code)(body)

    //////////////////////////////////////////////////////////////// VARIABLE //

    /**
      * linear variable replication output guard
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(parallelism: Int, rate: Rate, value: => S)(key: String)(body: `Π-Function0`)(using DummyImplicit)
                                              (using %, /, \)
                                              (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
     if classTag[S].runtimeClass eq getClass
     then
       apply(false)(parallelism, rate, value.asInstanceOf[`()`])(key)(body)
     else
       apply("*")(false)(parallelism, rate, ZIO.attempt(value))(key)(body)

    /**
      * linear variable replication output guard w/ pace
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(pace: Duration, parallelism: Int, rate: Rate, value: => S)(key: String)(body: `Π-Function0`)(using DummyImplicit)
                                              (using %, /, \)
                                              (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
     if classTag[S].runtimeClass eq getClass
     then
       apply(false)(pace, parallelism, rate, value.asInstanceOf[`()`])(key)(body)
     else
       apply("*")(false)(pace, parallelism, rate, ZIO.attempt(value))(key)(body)

    /**
      * linear variable replication output guard w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(parallelism: Int, rate: Rate, value: => S)(key: String)(code: => Task[Any])(body: `Π-Function0`)(using DummyImplicit)
                                             (using %, /, \)
                                             (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
     if classTag[S].runtimeClass eq getClass
     then
       apply(true)(parallelism, rate, value.asInstanceOf[`()`])(key)(code)(body)
     else
       apply("*")(true)(parallelism, rate, ZIO.attempt(value))(key)(code)(body)

    /**
      * linear variable replication output guard w/ pace w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(pace: Duration, parallelism: Int, rate: Rate, value: => S)(key: String)(code: => Task[Any])(body: `Π-Function0`)(using DummyImplicit)
                                             (using %, /, \)
                                             (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
     if classTag[S].runtimeClass eq getClass
     then
       apply(true)(pace, parallelism, rate, value.asInstanceOf[`()`])(key)(code)(body)
     else
       apply("*")(true)(pace, parallelism, rate, ZIO.attempt(value))(key)(code)(body)

    /**
      * linear variable replication output guard
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(parallelism: Int, rate: Rate, value: => Task[S])(key: String)(body: `Π-Function0`)
                                              (using %, /, \)
                                              (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]].flatMap(apply(false)(parallelism, rate, _)(key)(body)))
      else
        super.output("*")(false)(parallelism, rate, value)(key)(body)

    /**
      * linear variable replication output guard w/ pace
      */
    def apply[S: ClassTag](_s: "*")(_f: false)(pace: Duration, parallelism: Int, rate: Rate, value: => Task[S])(key: String)(body: `Π-Function0`)
                                              (using %, /, \)
                                              (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]].flatMap(apply(false)(pace, parallelism, rate, _)(key)(body)))
      else
        super.output("*")(false)(pace, parallelism, rate, value)(key)(body)

    /**
      * linear variable replication output guard w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(parallelism: Int, rate: Rate, value: => Task[S])(key: String)(code: => Task[Any])(body: `Π-Function0`)
                                             (using %, /, \)
                                             (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]].flatMap(apply(true)(parallelism, rate, _)(key)(code)(body)))
      else
        super.output("*")(true)(parallelism, rate, value)(key)(code)(body)

    /**
      * linear variable replication output guard w/ pace w/ code
      */
    def apply[S: ClassTag](_s: "*")(_t: true)(pace: Duration, parallelism: Int, rate: Rate, value: => Task[S])(key: String)(code: => Task[Any])(body: `Π-Function0`)
                                             (using %, /, \)
                                             (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.suspendSucceed(value.asInstanceOf[Task[`()`]].flatMap(apply(true)(pace, parallelism, rate, _)(key)(code)(body)))
      else
        super.output("*")(true)(pace, parallelism, rate, value)(key)(code)(body)

    /////////////////////////////////////////////////////////////////// INPUT //

    /**
      * linear replication input guard
      */
    def apply(_n: Null)(_f: false)(parallelism: Int, rate: Rate)(key: String)(body: `Π-Function1`)
                                  (using %, /, \)
                                  (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.input(false)(parallelism, rate)(key)(body)

    /**
      * linear replication input guard w/ pace
      */
    def apply(_n: Null)(_f: false)(pace: Duration, parallelism: Int, rate: Rate)(key: String)(body: `Π-Function1`)
                                  (using %, /, \)
                                  (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.input(false)(pace, parallelism, rate)(key)(body)

    /**
      * linear replication input guard w/ code
      */
    def apply[T](_n: Null)(_t: true)(parallelism: Int, rate: Rate)(key: String)(code: T => Task[T])(body: `Π-Function1`)
                                    (using %, /, \)
                                    (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.input(true)(parallelism, rate)(key)(code)(body)

    /**
      * linear replication input guard w/ pace w/ code
      */
    def apply[T](_n: Null)(_t: true)(pace: Duration, parallelism: Int, rate: Rate)(key: String)(code: T => Task[T])(body: `Π-Function1`)
                                    (using %, /, \)
                                    (using `Π-Map`[String, `Π-Set`[String]], String): UIO[Unit] =
      super.input(true)(pace, parallelism, rate)(key)(code)(body)

    ////////////////////////////////////////////////////// linear replication //

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(rate: Rate, value: => S)(key: String)
                                     (using DummyImplicit)
                                     (using %, /)
                                     (using `Π-Map`[String, `Π-Set`[String]], String): UIO[java.lang.Double] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(rate, value.asInstanceOf[`()`])(key)
      else
        apply[S](false)(rate, ZIO.attempt(value))(key)

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(rate: Rate, value: => S)(key: String)(code: => Task[Any])
                                    (using DummyImplicit)
                                    (using %, /)
                                    (using `Π-Map`[String, `Π-Set`[String]], String): UIO[java.lang.Double] =
      if classTag[S].runtimeClass eq getClass
      then
        apply(rate, value.asInstanceOf[`()`])(key)(code)
      else
        apply[S](true)(rate, ZIO.attempt(value))(key)(code)

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(rate: Rate, value: => Task[S])(key: String)
                                     (using %, /)
                                     (using `Π-Map`[String, `Π-Set`[String]], String): UIO[java.lang.Double] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.suspendSucceed((value.asInstanceOf[Task[`()`]]: UIO[`()`]).flatMap(apply(rate, _)(key)))
      else
        ZIO.suspendSucceed((value: UIO[S]).map(new `()`(_)).flatMap(apply(rate, _)(key)))

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(rate: Rate, value: => Task[S])(key: String)(code: => Task[Any])
                                    (using %, /)
                                    (using `Π-Map`[String, `Π-Set`[String]], String): UIO[java.lang.Double] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.suspendSucceed((value.asInstanceOf[Task[`()`]]: UIO[`()`]).flatMap(apply(rate, _)(key)(code)))
      else
        ZIO.suspendSucceed((value: UIO[S]).map(new `()`(_)).flatMap(apply(rate, _)(key)(code)))

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`)(key: String)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): UIO[java.lang.Double] =
      for
        _        <- exclude(key)
        promise  <- Promise.make[Nothing, Option[<>]]
        timestamp <- currentTimeMillis.flatMap(Ref.make)
        _        <- /.offer(^ -> key -> ((promise -> null, timestamp), (`()`[{}], Some(Left(())), rate)))
        opt      <- promise.await
        delay    <- ( if opt eq None
                      then
                        ZIO.succeed(null: java.lang.Double)
                      else
                        val (delay, b, f, i) = opt.get
                        for
                          _ <- i.set(value)
                          _ <- b.await.exit
                          _ <- f.join
                        yield
                          java.lang.Double(delay)
                    )
      yield
        delay

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`)(key: String)(code: => Task[Any])
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): UIO[java.lang.Double] =
      for
        _        <- exclude(key)
        promise  <- Promise.make[Nothing, Option[<>]]
        timestamp <- currentTimeMillis.flatMap(Ref.make)
        _        <- /.offer(^ -> key -> ((promise -> null, timestamp), (`()`[{}], Some(Left(())), rate)))
        opt      <- promise.await
        delay    <- ( if opt eq None
                      then
                        ZIO.succeed(null: java.lang.Double)
                      else
                        val (delay, b, f, i) = opt.get
                        for
                          _ <- i.set(value)
                          _ <- b.await.exit
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
    def apply(rate: Rate)(key: String)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): UIO[(`()`, java.lang.Double)] =
      for
        _             <- exclude(key)
        promise       <- Promise.make[Nothing, Option[<>]]
        result        <- Ref.make[`()`](sΠ.`()`.`null`)
        timestamp <- currentTimeMillis.flatMap(Ref.make)
        _        <- /.offer(^ -> key -> ((promise -> null, timestamp), (`()`[{}], Some(Right(result)), rate)))
        opt           <- promise.await
        (name, delay) <- ( if opt eq None
                           then
                             ZIO.succeed(sΠ.`()`.`null` -> (null: java.lang.Double))
                           else
                             val (delay, b, f, _) = opt.get
                             for
                               _    <- b.await.exit
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
    def apply[T](rate: Rate)(key: String)(code: T => Task[T])
                (using % : %, / : /)
                (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): UIO[(`()`, java.lang.Double)] =
      for
        _             <- exclude(key)
        promise       <- Promise.make[Nothing, Option[<>]]
        result        <- Ref.make[`()`](sΠ.`()`.`null`)
        timestamp <- currentTimeMillis.flatMap(Ref.make)
        _        <- /.offer(^ -> key -> ((promise -> null, timestamp), (`()`[{}], Some(Right(result)), rate)))
        opt           <- promise.await
        (name, delay) <- ( if opt eq None
                           then
                             ZIO.succeed(sΠ.`()`.`null` -> (null: java.lang.Double))
                           else
                             val (delay, b, f, _) = opt.get
                             for
                               _    <- b.await.exit
                               _    <- f.join
                               name <- result.get.map(_.name).flatMap { case null  => ZIO.succeed(sΠ.`()`.`null`)
                                                                        case it: T => (code andThen exec)(it).map(new `()`(_))
                                                                      }
                             yield
                               name -> java.lang.Double(delay)
                         )
      yield
        name -> delay

    override def toString: String = if name == null then "null" else name.toString


  private object `()`:

    val `null` = new `()`(null)
