/*
 * Copyright (c) 2023-2025 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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

  import _root_.cats.effect.{ IO, Deferred, Ref }
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.{ Queue, Supervisor }

  import `Π-loop`.{ <>, %, /, \ }
  export `Π-magic`.><
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
    if `π-elvis`.contains(key)
    then
      `π-exclude`(`π-elvis`(key))
    else
      IO.unit


  /**
    * restriction aka new name
    */
  object ν:

    def map[B](f: `()` => B): IO[B] = flatMap(f andThen IO.pure)
    def flatMap[B](f: `()` => IO[B]): IO[B] =
      ( for
          q <- Queue.unbounded[IO, Any]
        yield
          f(q)
      ).flatten


  /**
    * silent transition
    */

  object τ:

    def apply(rate: Rate)(key: String)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[java.lang.Double] =
      for
        _        <- exclude(key)
        deferred <- Deferred[IO, Option[<>]]
        dummy_q  <- Queue.unbounded[IO, Any]
        _        <- /.offer(^ -> key -> (deferred -> (dummy_q, None, rate)))
        opt      <- deferred.get
        delay    <- ( if opt eq None
                      then
                        IO.pure(null: java.lang.Double)
                      else
                        val (delay, b, f) = opt.get
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

    private def q = `()`[><]

    def ====(that: `()`) =
      try
        this.q eq that.q
      catch
        case _ =>
          this.name == that.name

    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

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
        _        <- /.offer(^ -> key -> (deferred -> (q, Some(false), rate)))
        opt      <- deferred.get
        delay    <- ( if opt eq None
                      then
                        IO.pure(null: java.lang.Double)
                      else
                        val (delay, b, f) = opt.get
                        for
                          _ <- ><(value.name)(q)
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
      for
        _        <- exclude(key)
        deferred <- Deferred[IO, Option[<>]]
        _        <- /.offer(^ -> key -> (deferred -> (q, Some(false), rate)))
        opt      <- deferred.get
        delay    <- ( if opt eq None
                      then
                        IO.pure(null: java.lang.Double)
                      else
                        val (delay, b, f) = opt.get
                        for
                          _ <- ><(value.name)(q)(code)
                          _ <- b.await
                          _ <- f.join
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
                       ^ : String): IO[(`()`, java.lang.Double)] =
      for
        _             <- exclude(key)
        deferred      <- Deferred[IO, Option[<>]]
        _             <- /.offer(^ -> key -> (deferred -> (q, Some(true), rate)))
        opt           <- deferred.get
        (name, delay) <- ( if opt eq None
                           then
                             IO.pure((null: Any) -> (null: java.lang.Double))
                           else
                             val (delay, b, f) = opt.get
                             for
                               name <- ><()(q)
                               _    <- b.await
                               _    <- f.join
                             yield
                               name -> java.lang.Double(delay)
                         )
      yield
        new `()`(name) -> delay

    /**
      * positive prefix i.e. input
      */
    def apply[T](rate: Rate)(key: String)(code: T => IO[T])
                (using % : %, / : /)
                (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): IO[(`()`, java.lang.Double)] =
      for
        _             <- exclude(key)
        deferred      <- Deferred[IO, Option[<>]]
        _             <- /.offer(^ -> key -> (deferred -> (q, Some(true), rate)))
        opt           <- deferred.get
        (name, delay) <- ( if opt eq None
                           then
                             IO.pure((null: Any) -> (null: java.lang.Double))
                           else
                             val (delay, b, f) = opt.get
                             for
                               name <- ><()(q)(code)
                               _    <- b.await
                               _    <- f.join
                             yield
                               name -> java.lang.Double(delay)
                         )
      yield
        new `()`(name) -> delay

    override def toString: String = if name == null then "null" else name.toString


  private object `Π-magic`:

    type >< = Queue[IO, Any]

    object >< :

      inline def apply(name: Any)(`>Q`: ><): IO[Unit] =
        `>Q`.offer(name)

      inline def apply(name: Any)(`>Q`: ><)(code: => IO[Any]): IO[Unit] =
        `>Q`.offer(name) <* exec(code)

      inline def apply()(`<Q`: ><): IO[Any] =
        `<Q`.take

      inline def apply[T]()(`<Q`: ><)(code: T => IO[T]): IO[Any] =
        `<Q`.take.flatMap { case it: T => (code andThen exec)(it) }
