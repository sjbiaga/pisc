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

  import _root_.java.util.UUID

  import _root_.scala.collection.immutable.{ List, Map, Set }

  import _root_.cats.effect.{ Deferred, Ref, IO, IOLocal }
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.{ CyclicBarrier, Semaphore, Supervisor }

  import `Π-loop`._
  import `Π-magic`._
  export `Π-magic`.>*<
  import `Π-stats`.Rate


  type `Π-Map`[K, +V] = Map[K, V]

  type `Π-Set`[A] = Set[A]


  /**
    * Type of keys in [[`][`]].
    */
  type `)*(` = Set[`)(`]

  /**
    * Wraps either transaction names ([[Unit]]) or transaction keys ([[UUID]]).
    *
    * @param value either [[[Unit]] or [[UUID]]
    */
  final class `)(`(private val value: Any):
    override def hashCode: Int = value.##
    override def equals(any: Any): Boolean = any match
      case that: `)(` => this.value == that.value
      case _ => false
    override def toString: String = value.toString

  object `)(`:
    def apply(): `)(` = new `)(`(UUID.randomUUID)


  /**
    * Transactions' trees' nodes.
    */
  final case class `}{`(xa: `()`,
                        root: `)*(`,
                        children: Set[`)*(`])


  object `}{`:
    def apply(`)(`: IOLocal[`)(`], xa: `()`)
             (implicit `][`: `][`, `1`: Semaphore[IO]): IO[Unit] =
      for
        _    <- `1`.acquire
        root <- `)(`.get
        uuid  = sΠ.`)(`()
        node  = Set(uuid)
        _    <- `)(`.set(uuid)
        _    <- `][`.update { it =>
                              val key = it.keys.find(_.contains(root)).get
                              val tree @ `}{`(_, _, children) = it(key)
                              it + (node -> `}{`(xa, key, Set.empty))
                                 + (key  -> tree.copy(children = children + node))
                            }
        _    <- `1`.release
      yield
        ()

    private def update(temp: `}{`, root: `)*(`, join: `)*(`)
                      (implicit `][`: `][`): IO[Unit] =
      `][`.update { it =>
                    val tree @ `}{`(_, _, children) = it(temp.root)
                    it + (temp.root -> tree.copy(children = children - root + join))
                  }

    private def merge_(join: `)*(`, tail: Set[`)*(`])
                      (implicit `][`: `][`): IO[Unit] =
      if tail.isEmpty
      then
        IO.cede
      else
        `][`.update { it =>
                      val tree = it(tail.head)
                      it + (tail.head -> tree.copy(root = join))
                    } >> merge_(join, tail.tail)

    private def merge(tree: `}{`, join: `)*(`, node: `)*(`)
                     (implicit `][`: `][`): IO[Unit] =
      for
        _        <- `][`.update { it =>
                                  val temp @ `}{`(_, _, children) = it(join)
                                  it + (join -> temp.copy(children = children - node ++ tree.children))
                                }
        children <- `][`.modify { it => it -> it(join).children }
        _        <- merge_(join, children)
      yield
        ()

    def apply(xa: `()`)(`)(`: IOLocal[`)(`])
             (implicit `][`: `][`, `1`: Semaphore[IO]): IO[Unit] =
      for
        _                        <- `1`.acquire
        node                     <- `)(`.get
        (root, temp, node, tree) <- `][`.modify { it =>
                                                  val key = it.keys.find(_.contains(node)).get
                                                  val tree = it(key)
                                                  assert(tree.xa.xct eq xa.xct)
                                                  val root = tree.root
                                                  val temp = it(root)
                                                  it -> (root, temp, key, tree)
                                                }
        join                      = root ++ node
        _                        <- `][`.update(_ - root - node + (join -> temp))
        _                        <- update(temp, root, join)
        _                        <- merge(tree, join, node)
        _                        <- `1`.release
      yield
        ()

  /**
    * Type of transactions' trees.
    */
  type `][` = Ref[IO, Map[`)*(`, `}{`]]

  object `][`:
    def apply(): IO[(IOLocal[`)(`], `][`)] =
      val id  = `)(`()
      val key = Set(id)
      for
        lo <- IOLocal[`)(`](id)
        xa  = new `)(`(())
        map = Map[`)*(`, `}{`](key -> `}{`(xa, null, Set.empty))
        tr <- Ref.of[IO, Map[`)*(`, `}{`]](map)
      yield
        (lo, tr)

    /**
      * Return the transaction for this [[IOLocal]].
      */
    def apply(`)(`: IOLocal[`)(`])
             (implicit `][`: `][`): IO[`)(`] =
      for
        node <- `)(`.get
        xa   <- `][`.modify { it =>
                              val key = it.keys.find(_.contains(node)).get
                              it -> it(key).xa
                            }
      yield
        xa.xct


  /**
    * transaction
    */
  object χ:

    def map(f: `()` => Unit): IO[Unit] = flatMap(f andThen IO.pure)
    def flatMap(f: `()` => IO[Unit]): IO[Unit] = f(new `)(`(()))


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


  def `π-disable`(key: String, enabled: String*)
                 (using % : %)
                 (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    val (_, spell) = `π-wand`
    if spell.contains(key)
    then
      `π-release`(spell(key) -- Set.from(enabled))
    else
      IO.unit

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
    val deferred = if m.contains(^ + head)
                   then Some(m(^ + head).asInstanceOf[+]._1)
                   else None
    for
      _ <- deferred.map(_.complete(None)).getOrElse(IO.unit)
      _ <- if tail.isEmpty then IO.unit
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
    private[sΠ] def xct = as[`)(`]

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
    def apply(rate: Rate, value: `()`, `)(`: IOLocal[`)(`])(key: String)
             (using % : %, / : /)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String, `][`: `][`): IO[java.lang.Double] =
      for
        _        <- exclude(key)
        deferred <- Deferred[IO, Option[(Double, -)]]
        _        <- /.offer(^ -> key -> (deferred -> (ref, Some(false), rate)))
        _        <- IO { assert(!value.name.isInstanceOf[`)(`]) }
        delay    <- ><(key, value.name, `)(`)(deferred)(ref)
      yield
        delay

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`, `)(`: IOLocal[`)(`])(key: String)(code: => IO[Any])
             (using % : %, / : /)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String, `][`: `][`): IO[java.lang.Double] =
      for
        _        <- exclude(key)
        deferred <- Deferred[IO, Option[(Double, -)]]
        _        <- /.offer(^ -> key -> (deferred -> (ref, Some(false), rate)))
        _        <- IO { assert(!value.name.isInstanceOf[`)(`]) }
        delay    <- ><(key, value.name, `)(`)(code)(deferred)(ref)
      yield
        delay

    /**
      * positive prefix i.e. input
      */
    def apply(rate: Rate, `)(`: IOLocal[`)(`])(key: String)
             (using % : %, / : /)
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                       `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String, `][`: `][`): IO[(`()`, Double)] =
      for
        _          <- exclude(key)
        deferred   <- Deferred[IO, Option[(Double, -)]]
        _          <- /.offer(^ -> key -> (deferred -> (ref, Some(true), rate)))
        (r, delay) <- ><(key, `)(`)(deferred)(ref)
      yield
        `()`(r) -> delay

    /**
      * positive prefix i.e. input
      */
    def apply[T](rate: Rate, `)(`: IOLocal[`)(`])(key: String)(code: T => IO[T])
                (using % : %, / : /)
                (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                          `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String, `][`: `][`): IO[(`()`, Double)] =
      for
        _          <- exclude(key)
        deferred   <- Deferred[IO, Option[(Double, -)]]
        _          <- /.offer(^ -> key -> (deferred -> (ref, Some(true), rate)))
        (r, delay) <- ><(key, `)(`)(code)(deferred)(ref)
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

    final case class ><(takers: List[(`)(`, Deferred[IO, (Any, CyclicBarrier[IO])])],
                        offerers: List[(`)(`, (Any, Deferred[IO, (Unit, CyclicBarrier[IO])]))],
                        stop: Boolean)

    type >*< = Ref[IO, ><]

    object >< :

      inline def apply(): >< = ><(Nil, Nil, false)

      def apply(key: String, name: Any, `)(`: IOLocal[`)(`])
               (deferred: Deferred[IO, Option[(Double, -)]])
               (`>R`: >*<)
               (using % : %)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         ^ : String, `][`: `][`): IO[java.lang.Double] =
        for
          opt        <- deferred.get
          _          <- if opt eq None then IO.canceled else IO.unit
          xa         <- sΠ.`][`(`)(`)
          (delay, b) <- IO.pure(opt.get)
          _          <- discard(key)
          b2         <- CyclicBarrier[IO](2)
          offerer    <- Deferred[IO, (Unit, CyclicBarrier[IO])]
          (_, b2)    <- IO.uncancelable { poll =>
                          `>R`.modify {
                            case it @ ><(takers_, _, _) if takers_.filter(_._1 eq xa).nonEmpty =>
                              val takers = takers_.filter(_._1 eq xa)
                              val (taker, rest) = takers.head._2 -> takers.tail
                              it.copy(takers = rest) -> taker.complete(name -> b2).void.map(_ -> b2)
                            case it =>
                              val cleanup = `>R`.update { it => it.copy(offerers = it.offerers.filter(_._2._2 ne offerer)) }
                              it.copy(offerers = xa -> (name -> offerer) :: it.offerers) -> poll(offerer.get).onCancel(cleanup)
                          }.flatten
                        }
          _          <- b2.await
          stop       <- `>R`.modify { it => it -> it.stop }
          _          <- if stop then b.await else ready(key, b)
        yield
          if stop then null else delay

      def apply(key: String, name: Any, `)(`: IOLocal[`)(`])(code: => IO[Any])
               (deferred: Deferred[IO, Option[(Double, -)]])
               (`>R`: >*<)
               (using % : %)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         ^ : String, `][`: `][`): IO[java.lang.Double] =
        for
          opt        <- deferred.get
          _          <- if opt eq None then IO.canceled else IO.unit
          xa         <- sΠ.`][`(`)(`)
          (delay, b) <- IO.pure(opt.get)
          _          <- discard(key)
          b2         <- CyclicBarrier[IO](2)
          offerer    <- Deferred[IO, (Unit, CyclicBarrier[IO])]
          (_, b2)    <- IO.uncancelable { poll =>
                          `>R`.modify {
                            case it @ ><(takers_, _, _) if takers_.filter(_._1 eq xa).nonEmpty =>
                              val takers = takers_.filter(_._1 eq xa)
                              val (taker, rest) = takers.head._2 -> takers.tail
                              it.copy(takers = rest) -> taker.complete(name -> b2).void.map(_ -> b2)
                            case it =>
                              val cleanup = `>R`.update { it => it.copy(offerers = it.offerers.filter(_._2._2 ne offerer)) }
                              it.copy(offerers = xa -> (name -> offerer) :: it.offerers) -> poll(offerer.get).onCancel(cleanup)
                          }.flatten <* exec(code)
                        }
          _          <- b2.await
          stop       <- `>R`.modify { it => it -> it.stop }
          _          <- if stop then b.await else ready(key, b)
        yield
          if stop then null else delay

      def apply(key: String, `)(`: IOLocal[`)(`])
               (deferred: Deferred[IO, Option[(Double, -)]])
               (`<R`: >*<)
               (using % : %)
               (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                         ^ : String, `][`: `][`): IO[(Any, Double)] =
        for
          opt        <- deferred.get
          _          <- if opt eq None then IO.canceled else IO.unit
          xa         <- sΠ.`][`(`)(`)
          (delay, b) <- IO.pure(opt.get)
          _          <- discard(key)
          b2         <- CyclicBarrier[IO](2)
          (name, b2) <- Deferred[IO, (Any, CyclicBarrier[IO])].flatMap { taker =>
                          IO.uncancelable { poll =>
                            `<R`.modify {
                              case it @ ><(_, offerers_, _) if offerers_.filter(_._1 eq xa).nonEmpty =>
                                val offerers = offerers_.filter(_._1 eq xa)
                                val ((name, offerer), rest) = offerers.head._2 -> offerers.tail
                                it.copy(offerers = rest) -> offerer.complete(() -> b2).as(name).map(_ -> b2)
                              case it =>
                                val cleanup = `<R`.update { it => it.copy(takers = it.takers.filter(_._1 ne taker)) }
                                it.copy(takers = xa -> taker :: it.takers) -> poll(taker.get).onCancel(cleanup)
                            }.flatten
                          }
                        }
          _          <- b2.await
          _          <- ready(key, b)
        yield
          name -> delay

      def apply[T](key: String, `)(`: IOLocal[`)(`])(code: T => IO[T])
                  (deferred: Deferred[IO, Option[(Double, -)]])
                  (`<R`: >*<)
                  (using % : %)
                  (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]]),
                            ^ : String, `][`: `][`): IO[(Any, Double)] =
        for
          opt        <- deferred.get
          _          <- if opt eq None then IO.canceled else IO.unit
          xa         <- sΠ.`][`(`)(`)
          (delay, b) <- IO.pure(opt.get)
          _          <- discard(key)
          b2         <- CyclicBarrier[IO](2)
          (name, b2) <- Deferred[IO, (Any, CyclicBarrier[IO])].flatMap { taker =>
                          IO.uncancelable { poll =>
                            `<R`.modify {
                              case it @ ><(_, offerers_, _) if offerers_.filter(_._1 eq xa).nonEmpty =>
                                val offerers = offerers_.filter(_._1 eq xa)
                                val ((name, offerer), rest) = offerers.head._2 -> offerers.tail
                                it.copy(offerers = rest) -> offerer.complete(() -> b2).as(name).map(_ -> b2)
                              case it =>
                                val cleanup = `<R`.update { it => it.copy(takers = it.takers.filter(_._1 ne taker)) }
                                it.copy(takers = xa -> taker :: it.takers) -> poll(taker.get).onCancel(cleanup)
                            }.flatten.flatMap {
                              case it @ (null, _) => IO.pure(it)
                              case (it: T, b2) => (code andThen exec)(it)
                                                    .flatTap {
                                                      case null => `<R`.update(_.copy(stop = true))
                                                      case _ => IO.unit
                                                    }.map(_ -> b2)
                            }
                          }
                        }
          _          <- b2.await
          _          <- ready(key, b)
        yield
          name -> delay
