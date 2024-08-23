/*
 * Copyright (c) 2023-2024 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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

  import _root_.scala.collection.immutable.{ List, Map, Set }

  import _root_.cats.effect.{ Deferred, Ref, IO }
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.Supervisor

  import `Π-loop`._
  import `Π-magic`._
  export `Π-magic`.>*<
  import `Π-stats`.Rate


  type `Π-Map`[K, +V] = Map[K, V]

  type `Π-Set`[A] = Set[A]


  /**
    * Supervised [[code]].
    * @param code
    */
  private def supervised[T](code: IO[T]): IO[T] =
    ( for
        fiber <- Supervisor[IO](await = true).use(_.supervise(code))
        Succeeded(it) <- fiber.join
      yield
        it
    ).flatten


  def `π-enable`(enabled: `Π-Set`[String])
                (using % : %): IO[Unit] =
    %.update(enabled.foldLeft(_) { (m, key) =>
                                   val n = if m.contains(key)
                                           then m(key).asInstanceOf[Int]
                                           else 0
                                   m + (key -> (n + 1))
                                 }
    )

  private def ready(key: String, - : -)
                   (using % : %)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    val (_, spell) = `π-wand`
    ( if spell.contains(key)
      then
        `π-enable`(spell(key))
      else
        IO.unit
    ) >> -.await


  private def unblock(m: Map[String, Int | +], head: String, tail: `Π-Set`[String])
                     (implicit ^ : String): IO[Unit] =
    val deferred = m(^ + head).asInstanceOf[+]._1
    for
      _        <- deferred.complete(None)
      _        <- if tail.isEmpty then IO.unit
                  else unblock(m, tail.head, tail.tail)
    yield
      ()

  private def `π-discard`(discarded: `Π-Set`[String])
                         (using % : %)
                         (implicit ^ : String): IO[Unit] =
    for
      m <- %.get
      _ <- if discarded.isEmpty then IO.unit
           else unblock(m, discarded.head, discarded.tail)
      _ <- %.update(discarded.map(^ + _).foldLeft(_)(_ - _))
    yield
      ()

  private def discard(key: String)
                     (using % : %)
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                               ^ : String): IO[Unit] =
    val (trick, _) = `π-wand`
    if trick.contains(key)
    then
      `π-discard`(trick(key))
    else
      IO.unit

  private def `π-release`(enabled: `Π-Set`[String])
                         (using % : %): IO[Unit] =
    %.update(enabled.foldLeft(_) { (m, key) =>
                                   if m(key) == 1
                                   then
                                     m - key
                                   else
                                     m + (key -> (m(key).asInstanceOf[Int] - 1))
                                 }
    )

  private def exclude(key: String)
                     (using % : %)
                     (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]]): IO[Unit] =
    if `π-elvis`.contains(key)
    then
      `π-release`(`π-elvis`(key))
    else
      IO.unit


  /**
    * restriction aka new name
    */
  object ν:

    def map(f: `()` => Unit): IO[Unit] = flatMap(f andThen IO.pure)
    def flatMap(f: `()` => IO[Unit]): IO[Unit] =
      ( for
          ref <- Ref.of[IO, ><](><())
        yield
          f(ref)
      ).flatten


  /**
    * silent transition
    */

  object τ:

    def apply(rate: Rate)(key: String)
             (using % : %, / : /)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[java.lang.Double] =
      for
        _          <- exclude(key)
        deferred   <- Deferred[IO, Option[(Double, -)]]
        _dummy_ref <- Ref.of[IO, ><](><())
        _          <- /.offer(^ -> key -> (deferred -> (_dummy_ref, None, rate)))
        opt        <- deferred.get
        _          <- if opt eq None then IO.canceled else IO.unit
        (delay, b) <- IO.pure(opt.get)
        _          <- discard(key)
        _          <- ready(key, b)
      yield
        delay

  /**
    * prefix
    */
  final implicit class `()`(private val name: Any) extends AnyVal:

    private def ref = as[>*<]

    def ====(that: `()`) =
      try
        this.ref eq that.ref
      catch
        case _ =>
          this.name == that.name

    inline def unary_! : Boolean = name == null
    inline def as[T]: T = name.asInstanceOf[T]

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`)(key: String)
             (using % : %, / : /)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[java.lang.Double] =
      for
        _        <- exclude(key)
        deferred <- Deferred[IO, Option[(Double, -)]]
        _        <- /.offer(^ -> key -> (deferred -> (ref, Some(false), rate)))
        delay    <- ><(key, value.name)(deferred)(ref)
      yield
        delay

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`)(key: String)(code: => IO[Any])
             (using % : %, / : /)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[java.lang.Double] =
      for
        _        <- exclude(key)
        deferred <- Deferred[IO, Option[(Double, -)]]
        _        <- /.offer(^ -> key -> (deferred -> (ref, Some(false), rate)))
        delay    <- ><(key, value.name)(code)(deferred)(ref)
      yield
        delay

    /**
      * positive prefix i.e. input
      */
    def apply(rate: Rate)(key: String)
             (using % : %, / : /)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[(`()`, Double)] =
      for
        _          <- exclude(key)
        deferred   <- Deferred[IO, Option[(Double, -)]]
        _          <- /.offer(^ -> key -> (deferred -> (ref, Some(true), rate)))
        (r, delay) <- ><(key)(deferred)(ref)
      yield
        `()`(r) -> delay

    /**
      * positive prefix i.e. input
      */
    def apply[T](rate: Rate)(key: String)(code: T => IO[T])
                (using % : %, / : /)
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): IO[(`()`, Double)] =
      for
        _          <- exclude(key)
        deferred   <- Deferred[IO, Option[(Double, -)]]
        _          <- /.offer(^ -> key -> (deferred -> (ref, Some(true), rate)))
        (r, delay) <- ><(key)(code)(deferred)(ref)
      yield
        `()`(r) -> delay

    override def toString: String = if name == null then "null" else name.toString


  private object `Π-magic`:

    /**
      * Adapted from cats-effect tutorial [[https://typelevel.org/cats-effect/docs/tutorial]].
      *
      * @see [[https://github.com/lrodero/cats-effect-tutorial/blob/series/3.x/src/main/scala/catseffecttutorial/producerconsumer/ProducerConsumerBoundedCancelable.scala]]
      */
    /*
     *
     * Copyright (c) 2020 Luis Rodero-Merino
     *
     * Licensed under the Apache License, Version 2.0 (the "License");
     * you may not use this file except in compliance with the License.
     * You may obtain a copy of the License at.
     *
     *     http://www.apache.org/licenses/LICENSE-2.0
     *
     * Unless required by applicable law or agreed to in writing, software
     * distributed under the License is distributed on an "AS IS" BASIS,
     * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
     * See the License for the specific language governing permissions and
     * limitations under the License.
     */

    final case class ><(takers: List[Deferred[IO, Any]],
                        offerers: List[(Any, Deferred[IO, Unit])],
                        stop: Boolean)

    type >*< = Ref[IO, ><]

    object >< :

      inline def apply(): >< = ><(Nil, Nil, false)

      def apply(key: String, name: Any)
               (deferred: Deferred[IO, Option[(Double, -)]])
               (`>R`: >*<)
               (using % : %)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         ^ : String): IO[java.lang.Double] =
        for
          opt        <- deferred.get
          _          <- if opt eq None then IO.canceled else IO.unit
          (delay, b) <- IO.pure(opt.get)
          _          <- discard(key)
          offerer    <- Deferred[IO, Unit]
          _          <- IO.uncancelable { poll =>
                          `>R`.modify {
                            case it @ ><(takers, _, _) if takers.nonEmpty =>
                              val (taker, rest) = takers.head -> takers.tail
                              it.copy(takers = rest) -> taker.complete(name).void
                            case it =>
                              val cleanup = `>R`.update { it => it.copy(offerers = it.offerers.filter(_._2 ne offerer)) }
                              it.copy(offerers = name -> offerer :: it.offerers) -> poll(offerer.get).onCancel(cleanup)
                          }.flatten
                        }
          _          <- ready(key, b)
          stop       <- `>R`.modify { it => it -> it.stop }
        yield
          if stop then null else delay

      def apply(key: String, name: Any)(code: => IO[Any])
               (deferred: Deferred[IO, Option[(Double, -)]])
               (`>R`: >*<)
               (using % : %)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         ^ : String): IO[java.lang.Double] =
        for
          opt        <- deferred.get
          _          <- if opt eq None then IO.canceled else IO.unit
          (delay, b) <- IO.pure(opt.get)
          _          <- discard(key)
          offerer    <- Deferred[IO, Unit]
          _          <- IO.uncancelable { poll =>
                          `>R`.modify {
                            case it @ ><(takers, _, _) if takers.nonEmpty =>
                              val (taker, rest) = takers.head -> takers.tail
                              it.copy(takers = rest) -> taker.complete(name).void
                            case it =>
                              val cleanup = `>R`.update { it => it.copy(offerers = it.offerers.filter(_._2 ne offerer)) }
                              it.copy(offerers = name -> offerer :: it.offerers) -> poll(offerer.get).onCancel(cleanup)
                          }.flatten <* supervised(code)
                        }
          _          <- ready(key, b)
          stop       <- `>R`.modify { it => it -> it.stop }
        yield
          if stop then null else delay

      def apply(key: String)
               (deferred: Deferred[IO, Option[(Double, -)]])
               (`<R`: >*<)
               (using % : %)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         ^ : String): IO[(Any, Double)] =
        for
          opt        <- deferred.get
          _          <- if opt eq None then IO.canceled else IO.unit
          (delay, b) <- IO.pure(opt.get)
          _          <- discard(key)
          taker      <- Deferred[IO, Any]
          name       <- IO.uncancelable { poll =>
                          `<R`.modify {
                            case it @ ><(_, offerers, _) if offerers.nonEmpty =>
                              val ((name, offerer), rest) = offerers.head -> offerers.tail
                              it.copy(offerers = rest) -> offerer.complete(()).as(name)
                            case it =>
                              val cleanup = `<R`.update { it => it.copy(takers = it.takers.filter(_ ne taker)) }
                              it.copy(takers = taker :: it.takers) -> poll(taker.get).onCancel(cleanup)
                          }.flatten
                        }
          _          <- ready(key, b)
        yield
          name -> delay

      def apply[T](key: String)(code: T => IO[T])
                  (deferred: Deferred[IO, Option[(Double, -)]])
                  (`<R`: >*<)
                  (using % : %)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            ^ : String): IO[(Any, Double)] =
        for
          opt        <- deferred.get
          _          <- if opt eq None then IO.canceled else IO.unit
          (delay, b) <- IO.pure(opt.get)
          _          <- discard(key)
          taker      <- Deferred[IO, Any]
          name       <- IO.uncancelable { poll =>
                          `<R`.modify {
                            case it @ ><(_, offerers, _) if offerers.nonEmpty =>
                              val ((name, offerer), rest) = offerers.head -> offerers.tail
                              it.copy(offerers = rest) -> offerer.complete(()).as(name)
                            case it =>
                              val cleanup = `<R`.update { it => it.copy(takers = it.takers.filter(_ ne taker)) }
                              it.copy(takers = taker :: it.takers) -> poll(taker.get).onCancel(cleanup)
                          }.flatten.flatMap {
                            case null => IO.pure(null)
                            case it: T => (code andThen supervised)(it)
                                            .flatTap {
                                              case null => `<R`.update(_.copy(stop = true))
                                              case _ => IO.unit
                                            }
                          }
                        }
          _          <- ready(key, b)
        yield
          name -> delay
