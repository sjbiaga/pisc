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

package object Π:

  import _root_.java.util.UUID

  import _root_.scala.collection.immutable.{ List, Set, Map }

  import _root_.cats.data.NonEmptyList

  import _root_.cats.effect.{ Deferred, IOLocal, IO, Ref }
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.{ Semaphore, Supervisor }

  import `Π-magic`._


  /**
    * Type of keys in [[`][`]].
    */
  type `)*(` = Set[`)(`]

  /**
    * Wraps either ambient names ([[Unit]]), ambient keys ([[UUID]]),
    * or capabilities path ([[ζ]]).
    * 
    * @param value either [[[Unit]] or [[UUID]] or [[ζ]]
    */
  implicit final class `)(`(private val value: Any):
    inline def unary_! : Boolean = value == null
    inline def ζ: ζ = value.asInstanceOf[ζ]
    override def hashCode(): Int = value.##
    override def toString(): String = if value == null then "null" else value.toString

  object `)(`:
    /**
      * Initial ambient unique key.
      */
    def apply(): `)(` = `)(`(UUID.randomUUID)


  enum `ζ-Op` { case in, out, open }

  final case class ζ(op: Option[`ζ-Op`], amb: `)(`, next: Option[ζ])

  object ζ:

    private def remove_(node: `)*(`, tail: Set[`)*(`])
                       (implicit `][`: `][`): IO[Unit] =
      if tail.isEmpty
      then
        IO.cede
      else
        `][`.update { it =>
                      val (tree, heth) = it(tail.head)
                      val `}{`(_, _, _, siblings) = tree
                      it + (tail.head -> (tree.copy(siblings = siblings - node), heth))
                    } >> remove_(node, tail.tail)

    private def remove(node: `)*(`, tree: `}{`)
                      (implicit `][`: `][`): IO[Unit] =
      val `}{`(_, root, _, siblings) = tree
      `][`.update { it =>
                    val (tree, reth) = it(root)
                    val `}{`(_, _, children, _) = tree
                    it + (root -> (tree.copy(children = children - node), reth))
                  } >> remove_(node, siblings)

    private def insert_(node: `)*(`, tail: Set[`)*(`])
                       (implicit `][`: `][`): IO[Unit] =
      if tail.isEmpty
      then
        IO.cede
      else
        `][`.update { it =>
                      val (tree, heth) = it(tail.head)
                      val `}{`(_, _, _, siblings) = tree
                      it + (tail.head -> (tree.copy(siblings = siblings + node), heth))
                    } >> insert_(node, tail.tail)

    private def insert(node: `)*(`, root: `)*(`)
                      (implicit `][`: `][`): IO[Unit] =
      for
        tree <- `][`.modify { it => it -> it(root)._1 }
        _    <- insert_(node, tree.children)
        _    <- `][`.update { it =>
                              val (ntree, neth) = it(node)
                              val (rtree, reth) = it(root)
                              val `}{`(_, _, children, _) = rtree
                              it + (root -> (rtree.copy(children = children + node), reth))
                                 + (node -> (ntree.copy(root = root, siblings = children), neth))
                            }
      yield
        ()

    private def update_(root: `)*(`, join: `)*(`, tail: Set[`)*(`])
                       (implicit `][`: `][`): IO[Unit] =
      if tail.isEmpty
      then
        IO.cede
      else
        `][`.update { it =>
                      val (tree, heth) = it(tail.head)
                      val `}{`(_, _, _, siblings) = tree
                      it + (tail.head -> (tree.copy(siblings = siblings - root + join), heth))
                    } >> update_(root, join, tail.tail)

    private def update(temp: `}{`, root: `)*(`, join: `)*(`)
                      (implicit `][`: `][`): IO[Unit] =
      `][`.update { it =>
                    val (tree, reth) = it(temp.root)
                    val `}{`(_, _, children, _) = tree
                    it + (temp.root -> (tree.copy(children = children - root + join), reth))
                  } >> update_(root, join, temp.siblings)

    private def merge_(join: `)*(`, tail: Set[`)*(`])
                      (implicit `][`: `][`): IO[Unit] =
      if tail.isEmpty
      then
        IO.cede
      else
        `][`.update { it =>
                      val (tree, heth) = it(tail.head)
                      it + (tail.head -> (tree.copy(root = join), heth))
                    } >> merge_(join, tail.tail)

    private def merge__(siblings: Set[`)*(`], tail: Set[`)*(`])
                       (implicit `][`: `][`): IO[Unit] =
      if tail.isEmpty
      then
        IO.cede
      else
        `][`.update { it =>
                      val (tree, heth) = it(tail.head)
                      it + (tail.head -> (tree.copy(siblings = siblings ++ tree.siblings), heth))
                    } >> merge__(siblings, tail.tail)

    private def merge(tree: `}{`, join: `)*(`)
                     (implicit `][`: `][`): IO[Unit] =
      `][`.update { it =>
                    val (temp, jeth) = it(join)
                    it + (join -> (temp.copy(children = tree.children ++ tree.siblings), jeth))
                  } >> merge_(join, tree.children ++ tree.siblings)
                    >> merge__(tree.children, tree.siblings)
                    >> merge__(tree.siblings, tree.children)

    private def ether(lhs: ><, rhs: ><): IO[><] =
      val min = lhs.takers.size min rhs.offerers.size
      if min == 0
      then
        IO.pure(><(lhs.takers, rhs.offerers))
      else
        NonEmptyList
          .fromList(lhs.takers.take(min) zip rhs.offerers.take(min))
          .get
          .traverse { (t, o) => t.complete(o._1).void >> o._2.complete(()).void }
          .as(><(lhs.takers.drop(min), rhs.offerers.drop(min)))

    def apply(`)(`: IOLocal[`)(`])(caps: ζ)
             (implicit `][`: `][`, `1`: Semaphore[IO]): IO[Unit] = caps match

      case ζ(Some(op), amb, next) => {

        assert(try { amb.ζ; false } catch _ => true)

        op match

          case `ζ-Op`.in =>
            for
              _       <- `1`.acquire
              node    <- `)(`.get
              blocked <- `][`.modify { it =>
                                       val key = it.keys.find(_.contains(node)).get
                                       val tree = it(key)._1
                                       it -> (1 != tree.siblings.count(it(_)._1.amb eq amb))
                                     }
              _       <- if blocked then `1`.release >> IO.cede >> this(`)(`)(caps)
                         else
                           for
                             (node, root, tree) <- `][`.modify { it =>
                                                                 val key = it.keys.find(_.contains(node)).get
                                                                 val tree = it(key)._1
                                                                 val root = tree.siblings.find(it(_)._1.amb eq amb).get
                                                                 it -> (key, root, tree)
                                                               }
                             _                  <- remove(node, tree)
                             _                  <- insert(node, root)
                             _                  <- `1`.release
                           yield
                             ()
            yield
              ()

          case `ζ-Op`.out =>
            for
              _       <- `1`.acquire
              node    <- `)(`.get
              blocked <- `][`.modify { it =>
                                       val key = it.keys.find(_.contains(node)).get
                                       val tree = it(key)._1
                                       it -> (it(tree.root)._1.amb ne amb)
                                     }
              _       <- if blocked then `1`.release >> IO.cede >> this(`)(`)(caps)
                         else
                           for
                             (node, root, tree) <- `][`.modify { it =>
                                                                 val key = it.keys.find(_.contains(node)).get
                                                                 val tree = it(key)._1
                                                                 it -> (key, it(tree.root)._1.root, tree)
                                                               }
                             _                  <- remove(node, tree)
                             _                  <- insert(node, root)
                             _                  <- `1`.release
                           yield
                             ()
            yield
              ()

          case `ζ-Op`.open =>
            for
              _       <- `1`.acquire
              root    <- `)(`.get
              blocked <- `][`.modify { it =>
                                       val key = it.keys.find(_.contains(root)).get
                                       val tree = it(key)._1
                                       it -> (1 != tree.children.count(it(_)._1.amb eq amb))
                                     }
              _       <- if blocked then `1`.release >> IO.cede >> this(`)(`)(caps)
                         else
                           for
                             (root, (temp, reth), node, (tree, neth)) <- `][`.modify { it =>
                                                                                       val key = it.keys.find(_.contains(root)).get
                                                                                       val v @ (tree, _) = it(key)
                                                                                       val node = tree.children.find(it(_)._1.amb eq amb).get
                                                                                       it -> (key, v, node, it(node))
                                                                                     }
                             _                                        <- remove(node, tree)
                             rstate                                   <- reth.get
                             nstate                                   <- neth.get
                             state1                                   <- ether(rstate, nstate)
                             state2                                   <- ether(nstate, rstate)
                             jeth                                     <- Ref.of[IO, ><](><(state1.takers ++ state2.takers,
                                                                                           state1.offerers ++ state2.offerers))
                             join                                      = root ++ node
                             _                                        <- `][`.update { it => ((it - root) - node)
                                                                                           + (join -> (temp, jeth)) }
                             _                                        <- update(temp, root, join)
                             _                                        <- merge(tree, join)
                             _                                        <- `1`.release
                           yield
                             ()
            yield
              ()

      } >> IO.cede >> next.map(this(`)(`)(_)).getOrElse(IO.cede)

      case ζ(_, name, next) =>

        this(`)(`)(name.ζ) >> IO.cede >> next.map(this(`)(`)(_)).getOrElse(IO.cede)


  /**
    * Ambients' trees' nodes.
    */
  final case class `}{`(amb: `)(`,
                        root: `)*(`,
                        children: Set[`)*(`],
                        siblings: Set[`)*(`])

  object `}{`:
    private def apply_(node: `)*(`, tail: Set[`)*(`])
                      (implicit `][`: `][`): IO[Unit] =
      if tail.isEmpty
      then
        IO.cede
      else
        `][`.update { it =>
                      val (tree, ceth) = it(tail.head)
                      val `}{`(_, _, _, siblings) = tree
                      it + (tail.head -> (tree.copy(siblings = siblings + node), ceth))
                    } >> apply_(node, tail.tail)

    def apply(`)(`: IOLocal[`)(`], amb: `)(`)
             (implicit `][`: `][`, `1`: Semaphore[IO]): IO[Unit] =
      for
        _        <- IO { assert(try { amb.ζ; false } catch _ => true) }.void
        _        <- `1`.acquire
        root     <- `)(`.get
        uuid      = Π.`)(`()
        node      = Set(uuid)
        _        <- `)(`.set(uuid)
        neth     <- Ref.of[IO, ><](><())
        children <- `][`.modify { it =>
                                  val key = it.keys.find(_.contains(root)).get
                                  val (tree, reth) = it(key)
                                  val `}{`(_, _, children, _) = tree
                                  (it + (node -> (`}{`(amb, key, Set.empty, children), neth))
                                      + (key  -> (tree.copy(children = children + node), reth))) -> children
                                }
        _        <- apply_(node, children)
        _        <- `1`.release
      yield
        ()


  /**
    * Type of ambients' trees.
    */
  type `][` = Ref[IO, Map[`)*(`, (`}{`, >*<)]]

  object `][`:
    def apply(): IO[(IOLocal[`)(`], `][`)] =
      for
        eth <- Ref.of[IO, ><](><())
        amb  = `)(`(())
        uuid = `)(`()
        root = Set(uuid)
        lo  <- IOLocal[`)(`](uuid)
        map  = Map[`)*(`, (`}{`, >*<)](root -> (`}{`(amb, null, Set.empty, Set.empty), eth))
        tr  <- Ref.of[IO, Map[`)*(`, (`}{`, >*<)]](map)
      yield
        (lo, tr)

    /**
      * Return the [[>*<]] ether for this [[IOLocal]].
      * Note that the semaphore is acquired and not yet released,
      * but its release delayed until input/output action.
      */
    def apply(`)(`: IOLocal[`)(`])
             (implicit `][`: `][`, `1`: Semaphore[IO]): IO[>*<] =
      for
        _    <- `1`.acquire
        node <- `)(`.get
        neth <- `][`.modify { it =>
                              val key = it.keys.find(_.contains(node)).get
                              it -> it(key)._2
                            }
      yield
        neth


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


  /**
    * restriction
    */
  object ν:

    def map(f: `)(` => Unit): IO[Unit] = flatMap(f andThen IO.pure)
    def flatMap(f: `)(` => IO[Unit]): IO[Unit] = f(`)(`(()))


  /**
    * silent transition
    */
  val τ = IO.unit


  /**
    * output
    */
  object <> :

     def apply(wrap: `)(`)(`)(`: IOLocal[`)(`])
              (implicit `][`: `][`, `1`: Semaphore[IO]): IO[Unit] =
       for
         `>R` <- Π.`][`(`)(`)
         _    <- ><(wrap)(`>R`)
       yield
         ()

     def apply(wrap: `)(`)(`)(`: IOLocal[`)(`])(code: => IO[Any])
              (implicit `][`: `][`, `1`: Semaphore[IO]): IO[Unit] =
       for
         `>R` <- Π.`][`(`)(`)
         _    <- ><(wrap)(code)(`>R`)
       yield
         ()

  /**
    * input
    */
  object `()`:

    def apply(`)(`: IOLocal[`)(`])
             (implicit `][`: `][`, `1`: Semaphore[IO]): IO[`)(`] =
       for
         `<R` <- Π.`][`(`)(`)
         name <- ><()(`<R`)
       yield
         name

    def apply[T](`)(`: IOLocal[`)(`])(code: T => IO[T])
                (implicit `][`: `][`, `1`: Semaphore[IO]): IO[`)(`] =
       for
         `<R` <- Π.`][`(`)(`)
         name <- ><()(code)(`<R`)
       yield
         name


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

    final case class ><(takers: List[Deferred[IO, `)(`]],
                        offerers: List[(`)(`, Deferred[IO, Unit])])

    /**
      * Type of ambients' ether.
      */
    type >*< = Ref[IO, ><]

    object >< :

      inline def apply(): >< = ><(Nil, Nil)

      import _root_.scala.util.Random

      private val random = Random()

      def apply(wrap: `)(`)(`>R`: Ref[IO, ><])
               (implicit `1`: Semaphore[IO]): IO[Unit] =
        Deferred[IO, Unit].flatMap { offerer =>
          IO.uncancelable { poll =>
            `>R`.modify {
              case it @ ><(takers, _) if takers.nonEmpty =>
                val i = random.nextInt(takers.size)
                val (taker, rest) = takers(i) -> (takers.take(i) ++ takers.drop(i+1))
                it.copy(takers = rest) -> (taker.complete(wrap).void <* `1`.release)
              case it =>
                val cleanup = `>R`.update { it => it.copy(offerers = it.offerers.filter(_._2 ne offerer)) }
                it.copy(offerers = wrap -> offerer :: it.offerers) -> poll(`1`.release *> offerer.get).onCancel(cleanup)
            }.flatten
          }
        }

      def apply(wrap: `)(`)(code: => IO[Any])(`>R`: Ref[IO, ><])
               (implicit `1`: Semaphore[IO]): IO[Unit] =
        Deferred[IO, Unit].flatMap { offerer =>
          IO.uncancelable { poll =>
            `>R`.modify {
              case it @ ><(takers, _) if takers.nonEmpty =>
                val i = random.nextInt(takers.size)
                val (taker, rest) = takers(i) -> (takers.take(i) ++ takers.drop(i+1))
                it.copy(takers = rest) -> (taker.complete(wrap).void <* `1`.release)
              case it =>
                val cleanup = `>R`.update { it => it.copy(offerers = it.offerers.filter(_._2 ne offerer)) }
                it.copy(offerers = wrap -> offerer :: it.offerers) -> poll(`1`.release *> offerer.get).onCancel(cleanup)
            }.flatten <* supervised(code)
          }
        }

      def apply()(`<R`: Ref[IO, ><])
                 (implicit `1`: Semaphore[IO]): IO[`)(`] =
        Deferred[IO, `)(`].flatMap { taker =>
          IO.uncancelable { poll =>
            `<R`.modify {
              case it @ ><(_, offerers) if offerers.nonEmpty =>
                val i = random.nextInt(offerers.size)
                val ((name, offerer), rest) = offerers(i) -> (offerers.take(i) ++ offerers.drop(i+1))
                it.copy(offerers = rest) -> (offerer.complete(()).as(name) <* `1`.release)
              case it =>
                val cleanup = `<R`.update { it => it.copy(takers = it.takers.filter(_ ne taker)) }
                it.copy(takers = taker :: it.takers) -> poll(`1`.release *> taker.get).onCancel(cleanup)
            }.flatten
          }
        }

      def apply[T]()(code: T => IO[T])(`<R`: Ref[IO, ><])
                    (implicit `1`: Semaphore[IO]): IO[`)(`] =
        Deferred[IO, `)(`].flatMap { taker =>
          IO.uncancelable { poll =>
            `<R`.modify {
              case it @ ><(_, offerers) if offerers.nonEmpty =>
                val i = random.nextInt(offerers.size)
                val ((name, offerer), rest) = offerers(i) -> (offerers.take(i) ++ offerers.drop(i+1))
                it.copy(offerers = rest) -> (offerer.complete(()).as(name) <* `1`.release)
              case it =>
                val cleanup = `<R`.update { it => it.copy(takers = it.takers.filter(_ ne taker)) }
                it.copy(takers = taker :: it.takers) -> poll(`1`.release *> taker.get).onCancel(cleanup)
            }.flatten.flatMap {
              case null => IO.pure(null)
              case it: T => (code andThen supervised)(it).asInstanceOf[IO[`)(`]]
            }
          }
        }
