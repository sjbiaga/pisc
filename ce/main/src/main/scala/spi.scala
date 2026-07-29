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

  import _root_.cats.syntax.applicative.*

  import _root_.cats.effect.{ IO, Deferred }
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
      .flatMap(_.join
                .flatMap
                { case Succeeded(it) => it
                  case _ => IO(null.asInstanceOf[T]) }
              )


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
                       ^ : String): IO[Double] =
      for
        _        <- exclude(key)
        deferred <- Deferred[IO, Option[<>]]
        _        <- /.offer(^ -> key -> (deferred -> (new {}, None, rate)))
        opt      <- deferred.get
        _        <- if opt eq None then IO.canceled else IO.unit
        (delay,
         b, f, _) = opt.get
        _        <- b.await
        _        <- f.join
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
    def apply[S](_f: false)(rate: Rate, value: => S)(key: String)
                (using DummyImplicit)
                (using %, /)
                (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): IO[Double] =
      value match
        case it: `()` =>
          apply(rate, it)(key)
        case _ =>
          apply[S](false)(rate, IO.delay(value))(key)

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S](_t: true)(rate: Rate, value: => S)(key: String)(code: => IO[Any])
                (using DummyImplicit)
                (using %, /)
                (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): IO[Double] =
      value match
        case it: `()` =>
          apply(rate, it)(key)(code)
        case _ =>
          apply[S](true)(rate, IO.delay(value))(key)(code)

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S](_f: false)(rate: Rate, value: => IO[S])(key: String)
                (using %, /)
                (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): IO[Double] =
      value.map(new `()`(_)).flatMap(apply(rate, _)(key))

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S](_t: true)(rate: Rate, value: => IO[S])(key: String)(code: => IO[Any])
                (using %, /)
                (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): IO[Double] =
      value.map(new `()`(_)).flatMap(apply(rate, _)(key)(code))

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`)(key: String)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[Double] =
      for
        _        <- exclude(key)
        deferred <- Deferred[IO, Option[<>]]
        _        <- /.offer(^ -> key -> (deferred -> (`()`[{}], Some(Left(())), rate)))
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
      for
        _        <- exclude(key)
        deferred <- Deferred[IO, Option[<>]]
        _        <- /.offer(^ -> key -> (deferred -> (`()`[{}], Some(Left(())), rate)))
        opt      <- deferred.get
        _        <- if opt eq None then IO.canceled else IO.unit
        (delay,
         b, f, i) = opt.get
        _        <- i.set(value)
        _        <- b.await
        _        <- f.join
        _        <- exec(code)
      yield
        delay

    /**
      * positive prefix i.e. input
      */
    def apply(rate: Rate)(key: String)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[(`()`, Double)] =
      for
        _        <- exclude(key)
        deferred <- Deferred[IO, Option[<>]]
        result   <- IO.ref[`()`](sΠ.`()`.`null`)
        _        <- /.offer(^ -> key -> (deferred -> (`()`[{}], Some(Right(result)), rate)))
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
      for
        _        <- exclude(key)
        deferred <- Deferred[IO, Option[<>]]
        result   <- IO.ref[`()`](sΠ.`()`.`null`)
        _        <- /.offer(^ -> key -> (deferred -> (`()`[{}], Some(Right(result)), rate)))
        opt      <- deferred.get
        _        <- if opt eq None then IO.canceled else IO.unit
        (delay,
         b, f, _) = opt.get
        _        <- b.await
        _        <- f.join
        name     <- result.get.map(_.name).flatMap { case it: T => (code andThen exec)(it) }
      yield
        new `()`(name) -> delay

    override def toString: String = if name == null then "null" else name.toString


  private object `()`:

     val `null` = new `()`(null)
