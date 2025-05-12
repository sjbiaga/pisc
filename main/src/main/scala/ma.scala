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

  import _root_.scala.collection.immutable.{ List, Queue, Set, Map }

  import _root_.cats.data.NonEmptyList

  import _root_.cats.effect.{ Deferred, IOLocal, IO, Ref }
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.{ Semaphore, Supervisor }

  import `Π-magic`.*


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
    override def equals(any: Any): Boolean = any match
      case that: `)(` => this.value == that.value
      case _ => false
    override def toString(): String = if value == null then "null" else value.toString

  object `)(`:
    /**
      * Initial ambient unique key.
      */
    def apply(): `)(` = `)(`(UUID.randomUUID)
    /**
      * Discriminate names from capabilities.
      */
    def apply(`)(`: `)(`, next: Option[ζ]): ζ =
      try
        `)(`.ζ match
          case it @ ζ(None, Right(_), None) => // ambient name variable
            assert(next eq None)
            it
          case _ => // variable
            ζ(None, Left(`)(`), next)
      catch _ => // ambient name
        assert(next eq None)
        ζ(None, Right(`)(`), None)

  enum `ζ-Op` { case in, out, open }

  final case class ζ(op: Option[`ζ-Op`], amb: Either[`)(`, `)(`], next: Option[ζ])

  object ζ:

    private def remove_(node: `)*(`, sibling: `)*(`)
                       (implicit `][`: `][`): IO[Unit] =
    `][`.update { it =>
                  val (tree @ `}{`(_, _, _, siblings), heth) = it(sibling)
                  it + (sibling -> (tree.copy(siblings = siblings - node), heth))
                }

    private def remove(node: `)*(`, tree: `}{`)
                      (implicit `][`: `][`): IO[Unit] =
      val `}{`(_, root, _, siblings) = tree
      `][`.update { it =>
                    val (rtree, reth) = it(root)
                    it + (root -> (rtree.copy(children = siblings), reth))
                  } >> ( if siblings.isEmpty
                         then IO.cede
                         else NonEmptyList
                                .fromList(siblings.toList)
                                .get
                                .traverse(remove_(node, _))
                                .void
                       )

    private def insert_(node: `)*(`, child: `)*(`)
                       (implicit `][`: `][`): IO[Unit] =
      `][`.update { it =>
                    val (tree @ `}{`(_, _, _, siblings), heth) = it(child)
                    it + (child -> (tree.copy(siblings = siblings + node), heth))
                  }

    private def insert(node: `)*(`, root: `)*(`)
                      (implicit `][`: `][`): IO[Unit] =
      for
        tree <- `][`.modify { it => it -> it(root)._1 }
        _    <- if tree.children.isEmpty
                then IO.cede
                else NonEmptyList
                       .fromList(tree.children.toList).get
                       .traverse(insert_(node, _))
                       .void
        _    <- `][`.update { it =>
                              val (ntree, neth) = it(node)
                              val (rtree @ `}{`(_, _, children, _), reth) = it(root)
                              it + (root -> (rtree.copy(children = children + node), reth))
                                 + (node -> (ntree.copy(root = root, siblings = children), neth))
                            }
      yield
        ()

    private def update_(root: `)*(`, join: `)*(`, sibling: `)*(`)
                       (implicit `][`: `][`): IO[Unit] =
      `][`.update { it =>
                    val (tree @ `}{`(_, _, _, siblings), heth) = it(sibling)
                    it + (sibling -> (tree.copy(siblings = siblings - root + join), heth))
                  }

    private def update(temp: `}{`, root: `)*(`, join: `)*(`)
                      (implicit `][`: `][`): IO[Unit] =
      `][`.update { it =>
                    val (tree @ `}{`(_, _, children, _), reth) = it(temp.root)
                    it + (temp.root -> (tree.copy(children = children - root + join), reth))
                  } >> ( if temp.siblings.isEmpty
                         then IO.cede
                         else NonEmptyList
                                .fromList(temp.siblings.toList)
                                .get
                                .traverse(update_(root, join, _))
                                .void
                       )

    private def merge_(join: `)*(`, node: `)*(`)
                      (implicit `][`: `][`): IO[Unit] =
      `][`.update { it =>
                    val (tree, heth) = it(node)
                    it + (node -> (tree.copy(root = join), heth))
                  }

    private def merge__(siblings: Set[`)*(`], node: `)*(`)
                       (implicit `][`: `][`): IO[Unit] =
      `][`.update { it =>
                    val (tree, heth) = it(node)
                    it + (node -> (tree.copy(siblings = tree.siblings ++ siblings), heth))
                  }

    private def merge(tree: `}{`, join: `)*(`)
                     (implicit `][`: `][`): IO[Unit] =
      val children = tree.children ++ tree.siblings
      `][`.update { it =>
                    val (temp, jeth) = it(join)
                    it + (join -> (temp.copy(children = children), jeth))
                  } >> ( if children.isEmpty
                         then IO.cede
                         else NonEmptyList
                                .fromList(children.toList)
                                .get
                                .traverse(merge_(join, _))
                                .void
                       )
                    >> ( if tree.siblings.isEmpty
                         then IO.cede
                         else NonEmptyList
                                .fromList(tree.siblings.toList)
                                .get
                                .traverse(merge__(tree.children, _))
                                .void
                       )
                    >> ( if tree.children.isEmpty
                         then IO.cede
                         else NonEmptyList
                                .fromList(tree.children.toList).get
                                .traverse(merge__(tree.siblings, _))
                                .void
                       )

    private def ether(lhs: ><, rhs: ><): IO[><] =
      val min = lhs.queue.size min rhs.takers.size
      if min == 0
      then
        IO.pure(><(lhs.queue, rhs.takers))
      else
        NonEmptyList
          .fromList(lhs.queue.take(min).toList zip rhs.takers.take(min))
          .get
          .traverse { (n, t) => t.complete(n).void }
          .as(><(lhs.queue.drop(min), rhs.takers.drop(min)))

    def apply(`)(`: IOLocal[`)(`])(caps: ζ)
             (implicit `][`: `][`, `1`: Semaphore[IO]): IO[Unit] = caps match

      case ζ(Some(op), Left(_amb), next) =>

        val amb = try _amb.ζ.amb.right.get catch _ => _amb

        assert(try { amb.ζ; false } catch _ => true)

        op match

          case `ζ-Op`.in =>
            for
              _       <- `1`.acquire
              node    <- `)(`.get
              blocked <- `][`.modify { it =>
                                       val key = it.keys.find(_.contains(node)).get
                                       val tree = it(key)._1
                                       it -> !tree.siblings.exists(it(_)._1.amb eq amb)
                                     }
              _       <- if blocked then `1`.release >> IO.cede >> this(`)(`)(caps)
                         else {
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
                         } >> IO.cede >> next.map(this(`)(`)(_)).getOrElse(IO.cede)
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
                         else {
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
                         } >> IO.cede >> next.map(this(`)(`)(_)).getOrElse(IO.cede)
            yield
              ()

          case `ζ-Op`.open =>
            for
              _       <- `1`.acquire
              root    <- `)(`.get
              blocked <- `][`.modify { it =>
                                       val key = it.keys.find(_.contains(root)).get
                                       val tree = it(key)._1
                                       it -> !tree.children.exists(it(_)._1.amb eq amb)
                                     }
              _       <- if blocked then `1`.release >> IO.cede >> this(`)(`)(caps)
                         else {
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
                             jeth                                     <- Ref.of[IO, ><](><(state1.queue ++ state2.queue,
                                                                                           state1.takers ++ state2.takers))
                             join                                      = root ++ node
                             _                                        <- `][`.update { _ - root - node + (join -> (temp, jeth)) }
                             _                                        <- update(temp, root, join)
                             _                                        <- merge(tree, join)
                             _                                        <- `1`.release
                           yield
                             ()
                         } >> IO.cede >> next.map(this(`)(`)(_)).getOrElse(IO.cede)
            yield
              ()

      case ζ(Some(_), _, _) => ??? // impossible by syntax

      case ζ(_, Left(caps), next) =>

        IO.unit >> this(`)(`)(caps.ζ) >> IO.cede >> next.map(this(`)(`)(_)).getOrElse(IO.cede)

      case _ => ???

    def apply(`)(`: IOLocal[`)(`], _amb: `)(`)
             (implicit `][`: `][`, `1`: Semaphore[IO]): IO[Unit] =
      val amb = try _amb.ζ.amb.right.get catch _ => _amb
      for
        _       <- `1`.acquire
        root    <- `)(`.get
        blocked <- `][`.modify { it =>
                                 val key = it.keys.find(_.contains(root)).get
                                 val tree = it(key)._1
                                 it -> !tree.children.exists(it(_)._1.amb eq amb)
                               }
        _       <- if blocked then `1`.release >> IO.cede >> this(`)(`, amb)
                   else
                     val uuid = Π.`)(`()
                     for
                       _                     <- `)(`.set(uuid)
                       (node, v @ (tree, _)) <- `][`.modify { it =>
                                                              val key = it.keys.find(_.contains(root)).get
                                                              val node = it(key)._1.children.find(it(_)._1.amb eq amb).get
                                                              it -> (node, it(node))
                                                            }
                       _                     <- remove(node, tree)
                       join                   = node + uuid
                       _                     <- `][`.update(_ - node + (join -> v))
                       _                     <- insert(join, tree.root)
                       _                     <- `1`.release
                     yield
                       ()
      yield
        ()


  /**
    * Ambients' trees' nodes.
    */
  final case class `}{`(amb: `)(`,
                        root: `)*(`,
                        children: Set[`)*(`],
                        siblings: Set[`)*(`])

  object `}{`:
    private def apply_(node: `)*(`, child: `)*(`)
                      (implicit `][`: `][`): IO[Unit] =
      `][`.update { it =>
                    val (tree @ `}{`(_, _, _, siblings), ceth) = it(child)
                    it + (child -> (tree.copy(siblings = siblings + node), ceth))
                  }

    def apply(`)(`: IOLocal[`)(`], amb: `)(`)
             (implicit `][`: `][`, `1`: Semaphore[IO]): IO[Unit] =
      val uuid = Π.`)(`()
      val node = Set(uuid)
      for
        amb      <- IO { try amb.ζ.amb.right.get catch _ => amb }
        _        <- `1`.acquire
        root     <- `)(`.get
        _        <- `)(`.set(uuid)
        neth     <- Ref.of[IO, ><](><())
        children <- `][`.modify { it =>
                                  val key = it.keys.find(_.contains(root)).get
                                  val (tree @ `}{`(_, _, children, _), reth) = it(key)
                                  (it + (node -> (`}{`(amb, key, Set.empty, children), neth))
                                      + (key  -> (tree.copy(children = children + node), reth))) -> children
                                }
        _        <- if (children.isEmpty)
                    then
                      IO.cede
                    else
                      NonEmptyList
                        .fromList(children.toList)
                        .get
                        .traverse(apply_(node, _))
                        .void
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
        map  = Map(root -> (`}{`(amb, null, Set.empty, Set.empty), eth))
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
  private def exec[T](code: => IO[T]): IO[T] =
    Supervisor[IO](await = true)
      .use(_.supervise(code))
      .flatMap(_.join
                .flatMap
                { case Succeeded(it) => it
                  case _ => IO(null.asInstanceOf[T]) }
              )


  /**
    * restriction
    */
  object ν:

    def map[B](f: `)(` => B): IO[B] = flatMap(f andThen IO.pure)
    def flatMap[B](f: `)(` => IO[B]): IO[B] = f(`)(`(()))


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

    final case class ><(queue: Queue[`)(`],
                        takers: List[Deferred[IO, `)(`]])

    /**
      * Type of ambients' ether.
      */
    type >*< = Ref[IO, ><]

    object >< :

      inline def apply(): >< = ><(Queue.empty, Nil)

      import _root_.scala.util.Random

      private val random = Random()

      def apply(wrap: `)(`)(`>R`: Ref[IO, ><])
               (implicit `1`: Semaphore[IO]): IO[Unit] =
        `>R`.modify {
          case it @ ><(_, takers) if takers.nonEmpty =>
            val i = random.nextInt(takers.size)
            val (taker, rest) = takers(i) -> (takers.take(i) ++ takers.drop(i+1))
            it.copy(takers = rest) -> (taker.complete(wrap).void <* `1`.release)
          case it @ ><(queue, _) =>
            it.copy(queue = queue.enqueue(wrap)) -> `1`.release
        }.flatten

      def apply(wrap: `)(`)(code: => IO[Any])(`>R`: Ref[IO, ><])
               (implicit `1`: Semaphore[IO]): IO[Unit] =
        `>R`.modify {
          case it @ ><(_, takers) if takers.nonEmpty =>
            val i = random.nextInt(takers.size)
            val (taker, rest) = takers(i) -> (takers.take(i) ++ takers.drop(i+1))
            it.copy(takers = rest) -> (taker.complete(wrap).void <* `1`.release)
          case it @ ><(queue, _) =>
            it.copy(queue = queue.enqueue(wrap)) -> `1`.release
        }.flatten <* exec(code)

      def apply()(`<R`: Ref[IO, ><])
                 (implicit `1`: Semaphore[IO]): IO[`)(`] =
        Deferred[IO, `)(`].flatMap { taker =>
          IO.uncancelable { poll =>
            `<R`.modify {
              case it @ ><(queue, _) if queue.nonEmpty =>
                val (name, rest) = queue.dequeue
                it.copy(queue = rest) -> (IO.pure(name) <* `1`.release)
              case it =>
                val cleanup = `<R`.update { it => it.copy(takers = it.takers.filter(_ ne taker)) }
                val cleanupʹ = `1`.acquire >> cleanup >> `1`.release
                it.copy(takers = taker :: it.takers) -> poll(`1`.release *> taker.get).onCancel(cleanupʹ)
            }.flatten
          }
        }

      def apply[T]()(code: T => IO[T])(`<R`: Ref[IO, ><])
                    (implicit `1`: Semaphore[IO]): IO[`)(`] =
        Deferred[IO, `)(`].flatMap { taker =>
          IO.uncancelable { poll =>
            `<R`.modify {
              case it @ ><(queue, _) if queue.nonEmpty =>
                val (name, rest) = queue.dequeue
                it.copy(queue = rest) -> (IO.pure(name) <* `1`.release)
              case it =>
                val cleanup = `<R`.update { it => it.copy(takers = it.takers.filter(_ ne taker)) }
                val cleanupʹ = `1`.acquire >> cleanup >> `1`.release
                it.copy(takers = taker :: it.takers) -> poll(`1`.release *> taker.get).onCancel(cleanupʹ)
            }.flatten.flatMap {
              case null => IO.pure(null)
              case it: T => (code andThen exec)(it).asInstanceOf[IO[`)(`]]
            }
          }
        }
