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

  import _root_.cats.syntax.parallel._
  import _root_.cats.effect.{ Deferred, Ref, IO }

  import `Π-loop`._
  import `Π-magic`.`><`
  import `Π-stats`.Rate


  type `Π-Map`[K, +V] = _root_.scala.collection.immutable.Map[K, V]

  type `Π-Set`[A] = _root_.scala.collection.immutable.Set[A]


  def `π-incr`(enabled: `Π-Set`[String])
              (using % : %): IO[Unit] =
    %.update(enabled.foldLeft(_) { (m, key) =>
                                   val n = if m.contains(key)
                                           then m(key).asInstanceOf[Int]
                                           else 0
                                   m + (key -> (n + 1))
                                 }
    )

  private def ready(key: String)
                   (using % : %, - : -)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    val (_, spell) = `π-wand`
    `π-incr`(spell.getOrElse(key, _root_.scala.collection.immutable.Set.empty)) >> -.await


  def `π-decr`(discarded: `Π-Set`[String])
              (using % : %)
              (implicit ^ : String): IO[Unit] =
    %.update(discarded.map(^ + _).foldLeft(_)(_ - _))

  private def discard(key: String)
                     (using % : %)
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                               ^ : String): IO[Unit] =
    val (trick, _) = `π-wand`
    `π-decr`(trick.getOrElse(key, _root_.scala.collection.immutable.Set.empty))


  /**
    * restriction aka new name
    */
  object ν:

    def map[B](f: `()` => B): IO[B] = flatMap(f andThen IO.pure)
    def flatMap[B](f: `()` => IO[B]): IO[B] =
      ( for
          ref <- Ref.of[IO, `><`](`><`())
        yield
          f(`()`(ref))
      ).flatten


  /**
    * silent transition
    */

  object τ:

    def apply(rate: Rate)(key: String)
             (using % : %, / : /, - : -)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       ^ : String): IO[BigDecimal] =
      for
        deferred <- Deferred[IO, BigDecimal]
        _        <- /.offer(^ -> key -> (deferred -> (null, None, rate)))
        delta    <- deferred.get
        _        <- discard(key)
        _        <- deferred.get
        _        <- (
                      ready(key)
                    ,
                      -.await
                    ).parMapN { (_, _) => }
      yield
        delta

  /**
    * prefix
    */
  final implicit class `()`(val name: Any) extends AnyVal:

    private def ref = name.asInstanceOf[Ref[IO, `><`]]

    inline def ====(that: `()`) = this.name == that.name

    /**
      * positive prefix i.e. input
      */
    def apply(rate: Rate)(key: String)
             (using % : %, / : /, - : -)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       ^ : String): IO[(`()`, BigDecimal)] =
      for
        deferred   <- Deferred[IO, BigDecimal]
        _          <- /.offer(^ -> key -> (deferred -> (ref, Some(true), rate)))
        (r, delta) <- `><`(key)(deferred)(ref)
      yield
        `()`(r) -> delta

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`)(key: String)
             (using % : %, / : /, - : -)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       ^ : String): IO[BigDecimal] =
      for
        deferred <- Deferred[IO, BigDecimal]
        _        <- /.offer(^ -> key -> (deferred -> (ref, Some(false), rate)))
        delta    <- `><`(key, value.name)(deferred)(ref)
      yield
        delta

    override def toString: String = name.toString


  object `Π-magic`:

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

    final case class `><`(takers: Option[Deferred[IO, Any]], offerers: Option[(Any, Deferred[IO, Unit])])

    object `><`:

      inline def apply(): `><` = `><`(None, None)

      def apply(key: String, name: Any)
               (deferred: Deferred[IO, BigDecimal])
               (`>R`: Ref[IO, `><`])
               (using % : %, - : -)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         ^ : String): IO[BigDecimal] =
        for
          delta <- deferred.get
          _     <- discard(key) >>
                   Deferred[IO, Unit].flatMap { offerer =>
                     IO.uncancelable { poll => // `poll` used to embed cancelable code, i.e. the call to `offerer.get`
                       `>R`.modify {
                         case `><`(takers, offerers) if takers.nonEmpty =>
                           `><`(None, offerers) -> takers.get.complete(name)
                         case `><`(takers, _) =>
                           val cleanup = `>R`.update(_.copy(offerers = None))
                           `><`(takers, Some(name -> offerer)) -> poll(offerer.get).onCancel(cleanup)
                       }.flatten
                     }
                   } <* ready(key)
        yield
          delta

      def apply(key: String)
               (deferred: Deferred[IO, BigDecimal])
               (`<R`: Ref[IO, `><`])
               (using % : %, - : -)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         ^ : String): IO[(Any, BigDecimal)] =
        for
          delta <- deferred.get
          name  <- discard(key) >>
                   Deferred[IO, Any].flatMap { taker =>
                     IO.uncancelable { poll =>
                       `<R`.modify {
                         case `><`(takers, offerers) if offerers.nonEmpty =>
                           val (name, release) = offerers.get
                           `><`(takers, None) -> release.complete(()).as(name)
                         case `><`(_, offerers) =>
                           val cleanup = `<R`.update(_.copy(takers = None))
                           `><`(Some(taker), offerers) -> poll(taker.get).onCancel(cleanup)
                       }.flatten
                     }
                   } <* ready(key)
        yield
          name -> delta
