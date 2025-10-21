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

  import _root_.scala.collection.immutable.{ Queue, Map, Set }

  import _root_.cats.instances.list.*
  import _root_.cats.syntax.traverse.*

  import _root_.cats.effect.{ Deferred, IOLocal, IO, Ref }
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.{ Supervisor, UUIDGen }

  import _root_.io.github.timwspence.cats.stm.STM


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
    def apply(): IO[`)(`] = UUIDGen.randomUUID[IO].map(new `)(`(_))
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

  final class `}{`(val stm: STM[IO]):

    import stm.*

    import `Π-magic`.*

    object ζ:

      private def remove(node: `)*(`, tree: `}{`)
                        (using `][`: `][`): Txn[Unit] =
        val `}{`(_, root, _, siblings) = tree
        `][`.modify { it =>
                      siblings.foldLeft {
                        val (rtree, reth) = it(root)
                        it + (root -> (rtree.copy(children = siblings), reth))
                      } { (it, sibling) =>
                        val (tree @ `}{`(_, _, _, siblings), heth) = it(sibling)
                        it + (sibling -> (tree.copy(siblings = siblings - node), heth))
                      }
                    }

      private def insert(node: `)*(`, root: `)*(`)
                        (using `][`: `][`): Txn[Unit] =
        for
          it  <- `][`.get
          tree = it(root)._1
          _   <- `][`.modify { tree.children.foldLeft(_) { (it, child) =>
                                 val (tree @ `}{`(_, _, _, siblings), heth) = it(child)
                                 it + (child -> (tree.copy(siblings = siblings + node), heth))
                               }
                             }
          _   <- `][`.modify { it =>
                               val (ntree, neth) = it(node)
                               val (rtree @ `}{`(_, _, children, _), reth) = it(root)
                               it + (root -> (rtree.copy(children = children + node), reth))
                                  + (node -> (ntree.copy(root = root, siblings = children), neth))
                             }
        yield
          ()

      private def update(temp: `}{`, root: `)*(`, join: `)*(`)
                        (using `][`: `][`): Txn[Unit] =
        `][`.modify { it =>
                      temp.siblings.foldLeft {
                        val (tree @ `}{`(_, _, children, _), reth) = it(temp.root)
                        it + (temp.root -> (tree.copy(children = children - root + join), reth))
                      } { (it, sibling) =>
                        val (tree @ `}{`(_, _, _, siblings), heth) = it(sibling)
                        it + (sibling -> (tree.copy(siblings = siblings - root + join), heth))
                      }
                    }

      private def merge(tree: `}{`, join: `)*(`)
                       (using `][`: `][`): Txn[Unit] =
        val children = tree.children
        val siblings = tree.siblings
        `][`.modify { it =>
                      children.foldLeft {
                        siblings.foldLeft {
                          (children ++ siblings).foldLeft {
                            val (temp, jeth) = it(join)
                            it + (join -> (temp.copy(children = children ++ siblings), jeth))
                          } { (it, node) =>
                            val (tree, heth) = it(node)
                            it + (node -> (tree.copy(root = join), heth))
                          }
                        } { (it, node) =>
                            val (tree, heth) = it(node)
                            it + (node -> (tree.copy(siblings = tree.siblings ++ children), heth))
                        }
                      } { (it, node) =>
                        val (tree, heth) = it(node)
                        it + (node -> (tree.copy(siblings = tree.siblings ++ siblings), heth))
                      }
                    }

      private def ether(lhs: ><, rhs: ><): IO[><] =
        val min = lhs.queue.size min rhs.takers.size
        if min == 0
        then
          IO.pure(><(lhs.queue, rhs.takers))
        else
          (lhs.queue.take(min).toList zip rhs.takers.take(min))
            .traverse { (n, t) => t.complete(n).void }
            .as(><(lhs.queue.drop(min), rhs.takers.drop(min)))

      def apply(`)(`: IOLocal[`)(`])(caps: ζ)
               (using `][`: `][`, `1`: TSemaphore): IO[Unit] =

        caps match

          case Π.ζ(Some(op), Left(_amb), next) =>

            val amb = try _amb.ζ.amb.right.get catch _ => _amb

            assert(try { amb.ζ; false } catch _ => true)

            op match

              case `ζ-Op`.in =>
                { for
                  key <- `)(`.get
                  _   <- stm.commit {
                    for
                      _   <- `1`.acquire
                      it  <- `][`.get
                      _   <- stm.check { val node = it.keys.find(_.contains(key)).get
                                         val tree = it(node)._1
                                         tree.siblings.exists(it(_)._1.amb eq amb)
                                       }
                      node = it.keys.find(_.contains(key)).get
                      tree = it(node)._1
                      root = tree.siblings.find(it(_)._1.amb eq amb).get
                      _   <- remove(node, tree)
                      _   <- insert(node, root)
                      _   <- `1`.release
                    yield
                      ()
                  }
                  yield
                    ()
                } >> IO.cede >> next.map(this(`)(`)(_)).getOrElse(IO.cede)

              case `ζ-Op`.out =>
                { for
                  key <- `)(`.get
                  _   <- stm.commit {
                    for
                      _   <- `1`.acquire
                      it  <- `][`.get
                      _   <- stm.check { val node = it.keys.find(_.contains(key)).get
                                         val tree = it(node)._1
                                         it(tree.root)._1.amb eq amb
                                       }
                      node = it.keys.find(_.contains(key)).get
                      tree = it(node)._1
                      root = it(tree.root)._1.root
                      _   <- remove(node, tree)
                      _   <- insert(node, root)
                      _   <- `1`.release
                    yield
                      ()
                  }
                  yield
                    ()
                } >> IO.cede >> next.map(this(`)(`)(_)).getOrElse(IO.cede)

              case `ζ-Op`.open =>
                { for
                  key    <- `)(`.get
                  (r, n) <- stm.commit {
                    for
                      _           <- `1`.acquire
                      it          <- `][`.get
                      _           <- stm.check { val root = it.keys.find(_.contains(key)).get
                                                 val tree = it(root)._1
                                                 tree.children.exists(it(_)._1.amb eq amb)
                                               }
                      root         = it.keys.find(_.contains(key)).get
                      (temp, reth) = it(root)
                      node         = temp.children.find(it(_)._1.amb eq amb).get
                      (tree, neth) = it(node)
                    yield
                      (root, temp, reth) ->
                      (node, tree, neth)
                  }
                  (root, temp, reth) = r
                  (node, tree, neth) = n
                  rstate <- reth.get
                  nstate <- neth.get
                  state1 <- ether(rstate, nstate)
                  state2 <- ether(nstate, rstate)
                  jeth   <- Ref.of[IO, ><](><(state1.queue ++ state2.queue,
                                              state1.takers ++ state2.takers))
                  join    = root ++ node
                  _      <- stm.commit {
                    for
                      _ <- remove(node, tree)
                      _ <- `][`.modify { _ - root - node + (join -> (temp, jeth)) }
                      _ <- update(temp, root, join)
                      _ <- merge(tree, join)
                      _ <- `1`.release
                    yield
                      ()
                  }
                  yield
                    ()
                } >> IO.cede >> next.map(this(`)(`)(_)).getOrElse(IO.cede)

          case Π.ζ(Some(_), _, _) => ??? // impossible by syntax

          case Π.ζ(_, Left(caps), next) =>

            IO.unit >> this(`)(`)(caps.ζ) >> IO.cede >> next.map(this(`)(`)(_)).getOrElse(IO.cede)

          case _ => ???

      def apply(`)(`: IOLocal[`)(`], _amb: `)(`)
               (using `][`: `][`, `1`: TSemaphore): IO[Unit] =
        val amb = try _amb.ζ.amb.right.get catch _ => _amb
        for
          key  <- `)(`.get
          uuid <- Π.`)(`()
          _    <- stm.commit {
            for
              _   <- `1`.acquire
              it  <- `][`.get
              _   <- stm.check { val root = it.keys.find(_.contains(key)).get
                                 val tree = it(root)._1
                                 tree.children.exists(it(_)._1.amb eq amb)
                               }
              root = it.keys.find(_.contains(key)).get
              temp = it(root)._1
              node = temp.children.find(it(_)._1.amb eq amb).get
              tree = it(node)._1
              _   <- remove(node, tree)
              join = node + uuid
              _   <- `][`.modify(_ - node + (join -> it(node)))
              _   <- insert(join, tree.root)
            yield
              ()
          }
          _    <- `)(`.set(uuid)
          _    <- stm.commit { `1`.release }
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

      def apply(`)(`: IOLocal[`)(`], _amb: `)(`)
               (using `][`: `][`, `1`: TSemaphore): IO[Unit] =
        val amb = try _amb.ζ.amb.right.get catch _ => _amb
        for
          uuid <- Π.`)(`()
          node  = Set(uuid)
          neth <- Ref.of[IO, ><](><())
          key  <- `)(`.get
          _    <- stm.commit {
            for
              _ <- `1`.acquire
              _ <- `][`.modify { it =>
                                 val root = it.keys.find(_.contains(key)).get
                                 val (tree @ `}{`(_, _, children, _), reth) = it(root)
                                 children.foldLeft {
                                   it + (node -> (`}{`(amb, root, Set.empty, children), neth))
                                      + (root -> (tree.copy(children = children + node), reth))
                                 } { (it, child) =>
                                   val (tree @ `}{`(_, _, _, siblings), ceth) = it(child)
                                   it + (child -> (tree.copy(siblings = siblings + node), ceth))
                                 }
                               }
            yield
              ()
          }
          _    <- `)(`.set(uuid)
          _    <- stm.commit { `1`.release }
        yield
          ()


    /**
      * Type of ambients' trees.
      */
    type `][` = TVar[Map[`)*(`, (`}{`, >*<)]]

    object `][`:
      def apply(): IO[(IOLocal[`)(`], `][`, TSemaphore)] =
        for
          eth  <- Ref.of[IO, ><](><())
          amb   = `)(`(())
          uuid <- `)(`()
          root  = Set(uuid)
          lo   <- IOLocal[`)(`](uuid)
          map   = Map(root -> (`}{`(amb, null, Set.empty, Set.empty), eth))
          tree <- stm.commit { TVar.of[Map[`)*(`, (`}{`, >*<)]](map) }
          sem  <- stm.commit { TSemaphore.make(1) }
        yield
          (lo, tree, sem)

      /**
        * Return the [[>*<]] ether for this [[IOLocal]].
        * Note that the semaphore is acquired and not yet released,
        * but its release delayed until input/output action.
        */
      def apply(`)(`: IOLocal[`)(`])
               (using `][`: `][`, `1`: TSemaphore): IO[>*<] =
        for
          key  <- `)(`.get
          neth <- stm.commit {
            for
              _   <- `1`.acquire
              it  <- `][`.get
              node = it.keys.find(_.contains(key)).get
            yield
              it(node)._2
          }
        yield
          neth


    /**
      * output
      */
    object <> :

       def apply(wrap: `)(`)(`)(`: IOLocal[`)(`])
                (using `][`, TSemaphore): IO[Unit] =
         for
           `>R` <- `][`(`)(`)
           _    <- ><(wrap)(`>R`)
         yield
           ()

       def apply(wrap: `)(`)(`)(`: IOLocal[`)(`])(code: => IO[Any])
                (using `][`, TSemaphore): IO[Unit] =
         for
           `>R` <- `][`(`)(`)
           _    <- ><(wrap)(code)(`>R`)
         yield
           ()

    /**
      * input
      */
    object `()`:

      def apply(`)(`: IOLocal[`)(`])
               (using `][`, TSemaphore): IO[`)(`] =
         for
           `<R` <- `][`(`)(`)
           name <- ><()(`<R`)
         yield
           name

      def apply[T](`)(`: IOLocal[`)(`])(code: T => IO[T])
                  (using `][`, TSemaphore): IO[`)(`] =
         for
           `<R` <- `][`(`)(`)
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
                          takers: Queue[Deferred[IO, `)(`]])

      /**
        * Type of ambients' ether.
        */
      type >*< = Ref[IO, ><]

      object >< :

        inline def apply(): >< = ><(Queue.empty, Queue.empty)

        def apply(wrap: `)(`)(`>R`: Ref[IO, ><])
                 (using `1`: TSemaphore): IO[Unit] =
          `>R`.flatModify { it =>
            it.takers.dequeueOption match
              case Some((taker, queue)) =>
                it.copy(takers = queue) -> (taker.complete(wrap).void <* stm.commit { `1`.release })
              case _ =>
                it.copy(queue = it.queue.enqueue(wrap)) -> stm.commit { `1`.release }
          }

        def apply(wrap: `)(`)(code: => IO[Any])(`>R`: Ref[IO, ><])
                 (using `1`: TSemaphore): IO[Unit] =
          `>R`.flatModify { it =>
            it.takers.dequeueOption match
              case Some((taker, queue)) =>
                it.copy(takers = queue) -> (taker.complete(wrap).void <* stm.commit { `1`.release })
              case _ =>
                it.copy(queue = it.queue.enqueue(wrap)) -> stm.commit { `1`.release }
          } <* exec(code)

        def apply()(`<R`: Ref[IO, ><])
                   (using `1`: TSemaphore): IO[`)(`] =
          Deferred[IO, `)(`].flatMap { taker =>
            `<R`.flatModifyFull { (poll, it) =>
              it.queue.dequeueOption match
                case Some((name, queue)) =>
                  it.copy(queue = queue) -> (IO.pure(name) <* stm.commit { `1`.release })
                case _ =>
                  val queue = it.takers.enqueue(taker)
                  it.copy(takers = queue) -> poll(stm.commit { `1`.release } *> taker.get)
            }
          }

        def apply[T]()(code: T => IO[T])(`<R`: Ref[IO, ><])
                      (using `1`: TSemaphore): IO[`)(`] =
          Deferred[IO, `)(`].flatMap { taker =>
            `<R`.flatModifyFull { (poll, it) =>
              it.queue.dequeueOption match
                case Some((name, queue)) =>
                  it.copy(queue = queue) -> (IO.pure(name) <* stm.commit { `1`.release })
                case _ =>
                  val queue = it.takers.enqueue(taker)
                  it.copy(takers = queue) -> poll(stm.commit { `1`.release } *> taker.get)
            }
          }.flatMap {
            case null => IO.pure(null)
            case it: T => (code andThen exec)(it).asInstanceOf[IO[`)(`]]
          }
