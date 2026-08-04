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

  import _root_.zio.{ Exit, Promise, Ref, Task, UIO, ZIO }

  import `Π-loop`.{ <>, %, /, \ }
  import `Π-stats`.Rate


  type `Π-Map`[K, +V] = Map[K, V]

  type `Π-Set`[A] = Set[A]


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
    `π-exclude`(Set.from(enabled)) *> \

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
  object τ:

    def apply(rate: Rate)(key: String)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): UIO[java.lang.Double] =
      for
        _        <- exclude(key)
        promise  <- Promise.make[Nothing, Option[<>]]
        _        <- /.offer(^ -> key -> (promise -> (new {}, None, rate)))
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
    * prefix
    */
  final implicit class `()`(private val name: Any) extends AnyVal:

    def ====(that: `()`) = this.name == that.name

    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_f: false)(rate: Rate, value: => S)(key: String)
                                     (using DummyImplicit)
                                     (using %, /)
                                     (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                               ^ : String): UIO[java.lang.Double] =
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
                                    (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                              ^ : String): UIO[java.lang.Double] =
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
                                     (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                               ^ : String): UIO[java.lang.Double] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.suspendSucceed((value.asInstanceOf[Task[`()`]]: UIO[`()`]).flatMap(apply(rate, _)(key)))
      else
        (value: UIO[S]).map(new `()`(_)).flatMap(apply(rate, _)(key))

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S: ClassTag](_t: true)(rate: Rate, value: => Task[S])(key: String)(code: => Task[Any])
                                    (using %, /)
                                    (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                              ^ : String): UIO[java.lang.Double] =
      if classTag[S].runtimeClass eq getClass
      then
        ZIO.suspendSucceed((value.asInstanceOf[Task[`()`]]: UIO[`()`]).flatMap(apply(rate, _)(key)(code)))
      else
        (value: UIO[S]).map(new `()`(_)).flatMap(apply(rate, _)(key)(code))

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
        _        <- /.offer(^ -> key -> (promise -> (`()`[{}], Some(Left(())), rate)))
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
      apply(rate, value)(key) <* exec(code)

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
        _             <- /.offer(^ -> key -> (promise -> (`()`[{}], Some(Right(result)), rate)))
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
      apply(rate)(key)
        .map(_.name -> _)
        .flatMap {
          case (null, delay)  => ZIO.succeed(sΠ.`()`.`null` -> delay)
          case (it: T, delay) => (code andThen exec)(it).map(new `()`(_) -> delay)
        }

    override def toString: String = if name == null then "null" else name.toString


  private object `()`:

     val `null` = new `()`(null)
