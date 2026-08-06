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

  import _root_.cats.syntax.applicative.*

  import _root_.cats.effect.{ IO, Clock, Deferred }
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.Supervisor

  import `Π-loop`.{ <>, %, /, \ }
  import `Π-stats`.Rate


  type `Π-Map`[K, +V] = Map[K, V]

  type `Π-Set`[A] = Set[A]


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
  object τ:

    def apply(rate: Rate)(key: String)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[java.lang.Double] =
      for
        _         <- exclude(key)
        deferred  <- Deferred[IO, Option[<>]]
        timestamp <- Clock[IO].monotonic.map(_.toNanos)
        _         <- /.offer(^ -> key -> (deferred -> (timestamp, (new {}, None, rate))))
        opt       <- deferred.get
        delay     <- ( if opt eq None
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
                                               ^ : String): IO[java.lang.Double] =
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
                                    (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                              ^ : String): IO[java.lang.Double] =
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
                                     (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                               ^ : String): IO[java.lang.Double] =
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
                                    (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                                              ^ : String): IO[java.lang.Double] =
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
                       ^ : String): IO[java.lang.Double] =
      for
        _        <- exclude(key)
        deferred <- Deferred[IO, Option[<>]]
        timestamp <- Clock[IO].monotonic.map(_.toNanos)
        _         <- /.offer(^ -> key -> (deferred -> (timestamp, (`()`[{}], Some(Left(())), rate))))
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
    def apply(rate: Rate, value: `()`)(key: String)(code: => IO[Any])
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[java.lang.Double] =
      apply(rate, value)(key) <* exec(code)

    /**
      * positive prefix i.e. input
      */
    def apply(rate: Rate)(key: String)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[(`()`, java.lang.Double)] =
      for
        _             <- exclude(key)
        deferred      <- Deferred[IO, Option[<>]]
        result        <- IO.ref[`()`](sΠ.`()`.`null`)
        timestamp     <- Clock[IO].monotonic.map(_.toNanos)
        _             <- /.offer(^ -> key -> (deferred -> (timestamp, (`()`[{}], Some(Right(result)), rate))))
        opt           <- deferred.get
        (name, delay) <- ( if opt eq None
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
    def apply[T](rate: Rate)(key: String)(code: T => IO[T])
                (using % : %, / : /)
                (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): IO[(`()`, java.lang.Double)] =
      apply(rate)(key)
        .map(_.name -> _)
        .flatMap {
          case (null, delay)  => IO.pure(sΠ.`()`.`null` -> delay)
          case (it: T, delay) => (code andThen exec)(it).map(new `()`(_) -> delay)
        }

    override def toString: String = if name == null then "null" else name.toString


  private object `()`:

     val `null` = new `()`(null)
