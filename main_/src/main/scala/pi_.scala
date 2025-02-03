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

package object Π:

  import _root_.java.util.UUID

  import _root_.scala.collection.immutable.{ List, Map, Set }

  import _root_.cats.data.NonEmptyList

  import _root_.cats.effect.{ Deferred, Ref, IO, IOLocal }
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.{ CyclicBarrier, Semaphore, Supervisor }

  import `Π-magic`.*


  type `Π-Map`[K, +V] = Map[K, V]

  type `Π-List`[+A] = List[A]


  /**
    * Type of keys in [[`][`]].
    */
  type `)*(` = Set[`)(`]

  /**
    * Wraps either transaction names ([[String]]) or transaction keys ([[UUID]]).
    *
    * @param value either [[String]] or [[UUID]]
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
        uuid  = Π.`)(`()
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

    private def update_(join: `)*(`, node: `)*(`)
                       (implicit `][`: `][`): IO[Unit] =
      `][`.update { it =>
                    val tree = it(node)
                    it + (node -> tree.copy(root = join))
                  }

    private def update(temp: `}{`, node: `)*(`, join: `)*(`)
                      (implicit `][`: `][`): IO[Unit] =
      for
        _ <- `][`.update { it =>
                           val root = temp.root
                           val tree = it(root)
                           it + (root -> tree.copy(children = tree.children - node + join))
                         }
        _ <- if (temp.children.isEmpty)
             then
               IO.cede
             else
               NonEmptyList
                 .fromList(temp.children.toList)
                 .get
                 .traverse(update_(join, _))
                 .void
      yield
        ()

    private def merge(tree: `}{`, join: `)*(`, node: `)*(`)
                     (implicit `][`: `][`): IO[Unit] =
      `][`.update { it =>
                    val temp @ `}{`(_, _, children) = it(join)
                    it + (join -> temp.copy(children = children - node ++ tree.children))
                  }

    def apply(id: String)(`)(`: IOLocal[`)(`])
             (implicit `π-kong`: `Π-Map`[String, `Π-List`[String]],
                       `][`: `][`, `1`: Semaphore[IO]): IO[`()`] =
      for
        _       <- `1`.acquire
        root    <- `)(`.get
        blocked <- `][`.modify { it =>
                                 val key = it.keys.find(_.contains(root)).get
                                 val tree = it(key)
                                 it -> !tree.children.exists { node =>
                                   val xa = it(node).xa.xct.toString
                                   `π-kong`(id).contains(xa)
                                 }
                               }
        xa      <- if blocked then `1`.release >> IO.cede >> this(id)(`)(`)
                   else
                     val uuid = Π.`)(`()
                     for
                       _                  <- `)(`.set(uuid)
                       (node, temp, join) <- `][`.modify { it =>
                                                           val key = it.keys.find(_.contains(root)).get
                                                           val tree = it(key)
                                                           val node = tree.children.find { node =>
                                                             val xa = it(node).xa.xct.toString
                                                             `π-kong`(id).contains(xa)
                                                           }.get
                                                           val temp = it(node)
                                                           val join = node + uuid
                                                           (it - node + (join -> temp)) -> (node, temp, join)
                                                         }
                       _                  <- update(temp, node, join)
                       _                  <- `1`.release
                     yield
                       temp.xa
      yield
        xa

    def apply(xa: `()`)(`)(`: IOLocal[`)(`])
             (implicit `][`: `][`, `1`: Semaphore[IO]): IO[Unit] =
      for
        _                        <- `1`.acquire
        node                     <- `)(`.get
        (root, join, node, tree) <- `][`.modify { it =>
                                                  var key = it.keys.find(_.contains(node)).get
                                                  var tree = it(key)
                                                  while tree.xa.xct ne xa.xct
                                                  do
                                                    key = tree.root
                                                    tree = it(key)
                                                  val root = tree.root
                                                  val temp = it(root)
                                                  val join = root ++ key
                                                  (it - root - key + (join -> temp)) -> (root, join, key, tree)
                                                }
        _                        <- merge(tree, join, node)
        temp                     <- `][`.modify { it => it -> it(join) }
        _                        <- update(temp, root, join)
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
      * Note that the semaphore is acquired and not yet released,
      * but its release delayed until input/output action.
      */
    def apply(`)(`: IOLocal[`)(`])
             (implicit `][`: `][`, `1`: Semaphore[IO]): IO[`)(`] =
      for
        _    <- `1`.acquire
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
  case class χ(uuid: String):

    def map[B](f: `()` => B): IO[B] = flatMap(f andThen IO.pure)
    def flatMap[B](f: `()` => IO[B]): IO[B] = f(new `)(`(uuid))


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


  /**
    * restriction aka new name
    */
  object ν:

    def map[B](f: `()` => B): IO[B] = flatMap(f andThen IO.pure)
    def flatMap[B](f: `()` => IO[B]): IO[B] =
      ( for
          ref <- Ref.of[IO, ><](><())
        yield
          f(ref)
      ).flatten


  /**
    * silent transition
    */
  val τ: IO[Option[Unit]] = IO(Some(()))


  /**
    * prefix
    */
  implicit final class `()`(private val name: Any) extends AnyVal:

    private def ref = as[>*<]
    private[Π] def xct = as[`)(`]

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
    def apply(value: `()`, `)(`: IOLocal[`)(`])
             (implicit `][`: `][`, `1`: Semaphore[IO]): IO[Option[Unit]] =
      for
        xa <- Π.`][`(`)(`)
        _  <- IO { assert(!value.name.isInstanceOf[`)(`]) }
        r  <- ><(value.name, xa)(ref)
      yield
        r

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`, `)(`: IOLocal[`)(`])(code: => IO[Any])
             (implicit `][`: `][`, `1`: Semaphore[IO]): IO[Option[Unit]] =
      for
        xa <- Π.`][`(`)(`)
        _  <- IO { assert(!value.name.isInstanceOf[`)(`]) }
        r  <- ><(value.name, xa)(code)(ref)
      yield
        r

    /**
      * positive prefix i.e. input
      */
    def apply(`)(`: IOLocal[`)(`])
             (implicit `][`: `][`, `1`: Semaphore[IO]): IO[`()`] =
      for
        xa <- Π.`][`(`)(`)
        r  <- ><(xa)(ref).map(`()`)
      yield
        r

    /**
      * positive prefix i.e. input
      */
    def apply[T](`)(`: IOLocal[`)(`])(code: T => IO[T])
                (implicit `][`: `][`, `1`: Semaphore[IO]): IO[`()`] =
      for
        xa <- Π.`][`(`)(`)
        r  <- ><(xa)(code)(ref).map(`()`)
      yield
        r

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

      import _root_.scala.util.Random

      private val random = Random()

      def apply(name: Any, xa: `)(`)(`>R`: >*<)
               (implicit `1`: Semaphore[IO]): IO[Option[Unit]] =
        for
          b2       <- CyclicBarrier[IO](2)
          (_, b2)  <- Deferred[IO, (Unit, CyclicBarrier[IO])].flatMap { offerer =>
                        IO.uncancelable { poll =>
                          `>R`.modify {
                            case it @ ><(takers_, _, _) if takers_.filter(_._1 eq xa).nonEmpty =>
                              val takers = takers_.filter(_._1 eq xa)
                              val i = random.nextInt(takers.size)
                              val (taker, rest) = takers(i)._2 -> (takers.take(i) ++ takers.drop(i+1))
                              it.copy(takers = rest) -> (taker.complete(name -> b2).void.map(_ -> b2) <* `1`.release)
                            case it =>
                              val cleanup = `>R`.update { it => it.copy(offerers = it.offerers.filter(_._2._2 ne offerer)) }
                              val cleanup2 = `1`.acquire >> cleanup >> `1`.release
                              it.copy(offerers = xa -> (name -> offerer) :: it.offerers) -> poll(`1`.release *> offerer.get).onCancel(cleanup2)
                          }.flatten
                        }
                      }
          _        <- b2.await
          stop     <- `1`.acquire *> `>R`.modify { it => it -> it.stop } <* `1`.release
        yield
          if stop then None else Some(())

      def apply(name: Any, xa: `)(`)(code: => IO[Any])(`>R`: >*<)
               (implicit `1`: Semaphore[IO]): IO[Option[Unit]] =
        for
          b2       <- CyclicBarrier[IO](2)
          (_, b2)  <- Deferred[IO, (Unit, CyclicBarrier[IO])].flatMap { offerer =>
                        IO.uncancelable { poll =>
                          `>R`.modify {
                            case it @ ><(takers_, _, _) if takers_.filter(_._1 eq xa).nonEmpty =>
                              val takers = takers_.filter(_._1 eq xa)
                              val i = random.nextInt(takers.size)
                              val (taker, rest) = takers(i)._2 -> (takers.take(i) ++ takers.drop(i+1))
                              it.copy(takers = rest) -> (taker.complete(name -> b2).void.map(_ -> b2) <* `1`.release)
                            case it =>
                              val cleanup = `>R`.update { it => it.copy(offerers = it.offerers.filter(_._2._2 ne offerer)) }
                              val cleanup2 = `1`.acquire >> cleanup >> `1`.release
                              it.copy(offerers = xa -> (name -> offerer) :: it.offerers) -> poll(`1`.release *> offerer.get).onCancel(cleanup2)
                          }.flatten <* exec(code)
                        }
                      }
          _        <- b2.await
          stop     <- `1`.acquire *> `>R`.modify { it => it -> it.stop } <* `1`.release
        yield
          if stop then None else Some(())

      def apply(xa: `)(`)(`<R`: >*<)
               (implicit `1`: Semaphore[IO]): IO[Any] =
        for
          b2         <- CyclicBarrier[IO](2)
          (name, b2) <- Deferred[IO, (Any, CyclicBarrier[IO])].flatMap { taker =>
                          IO.uncancelable { poll =>
                            `<R`.modify {
                              case it @ ><(_, offerers_, _) if offerers_.filter(_._1 eq xa).nonEmpty =>
                                val offerers = offerers_.filter(_._1 eq xa)
                                val i = random.nextInt(offerers.size)
                                val ((name, offerer), rest) = offerers(i)._2 -> (offerers.take(i) ++ offerers.drop(i+1))
                                it.copy(offerers = rest) -> (offerer.complete(() -> b2).as(name).map(_ -> b2) <* `1`.release)
                              case it =>
                                val cleanup = `<R`.update { it => it.copy(takers = it.takers.filter(_._2 ne taker)) }
                                val cleanup2 = `1`.acquire >> cleanup >> `1`.release
                                it.copy(takers = xa -> taker :: it.takers) -> poll(`1`.release *> taker.get).onCancel(cleanup2)
                            }.flatten
                          }
                        }
          _      <- b2.await
        yield
          name

      def apply[T](xa: `)(`)(code: T => IO[T])(`<R`: >*<)
                  (implicit `1`: Semaphore[IO]): IO[Any] =
        for
          b2         <- CyclicBarrier[IO](2)
          (name, b2) <- Deferred[IO, (Any, CyclicBarrier[IO])].flatMap { taker =>
                          IO.uncancelable { poll =>
                            `<R`.modify {
                              case it @ ><(_, offerers_, _) if offerers_.filter(_._1 eq xa).nonEmpty =>
                                val offerers = offerers_.filter(_._1 eq xa)
                                val i = random.nextInt(offerers.size)
                                val ((name, offerer), rest) = offerers(i)._2 -> (offerers.take(i) ++ offerers.drop(i+1))
                                it.copy(offerers = rest) -> (offerer.complete(() -> b2).as(name).map(_ -> b2) <* `1`.release)
                              case it =>
                                val cleanup = `<R`.update { it => it.copy(takers = it.takers.filter(_._2 ne taker)) }
                                val cleanup2 = `1`.acquire >> cleanup >> `1`.release
                                it.copy(takers = xa -> taker :: it.takers) -> poll(`1`.release *> taker.get).onCancel(cleanup2)
                            }.flatten.flatMap {
                              case it @ (null, _) => IO.pure(it)
                              case (it: T, b2) => (code andThen exec)(it)
                                                    .flatTap {
                                                      case null => `1`.acquire >> `<R`.update(_.copy(stop = true)) >> `1`.release
                                                      case _ => IO.unit
                                                    }.map(_ -> b2)
                            }
                          }
                        }
          _          <- b2.await
        yield
          name
