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

package object Π:

  import _root_.scala.collection.immutable.{ Queue, Map, Set }

  import _root_.zio.{ FiberRef, Promise, Random, Ref, Task, UIO, ZIO }
  import _root_.zio.stm.{ TRef, TSemaphore }
  import _root_.zio.stm.{ USTM, ZSTM }


  private def exec[T](code: => Task[T]): UIO[T] =
    code.absorb.either.map {
      case Right(it) => it
      case _         => null.asInstanceOf[T]
    }


  /**
    * restriction
    */
  object ν:

    def map[B](f: `)(` => B): UIO[B] = flatMap(f andThen ZIO.succeed)
    def flatMap[B](f: `)(` => UIO[B]): UIO[B] = f(`)(`(()))


  /**
    * silent transition
    */
  val τ = ZIO.unit


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
    def apply(): UIO[`)(`] = Random.nextUUID.map(new `)(`(_))
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

  object `}{`:

    import `Π-magic`.*

    object ζ:

      private def remove(node: `)*(`, tree: `}{`)
                        (using `][`: `][`): USTM[Unit] =
        val `}{`(_, root, _, siblings) = tree
        `][`.update { it =>
                      siblings.foldLeft {
                        val (rtree, reth) = it(root)
                        it + (root -> (rtree.copy(children = siblings), reth))
                      } { (it, sibling) =>
                        val (tree @ `}{`(_, _, _, siblings), heth) = it(sibling)
                        it + (sibling -> (tree.copy(siblings = siblings - node), heth))
                      }
                    }

      private def insert(node: `)*(`, root: `)*(`)
                        (using `][`: `][`): USTM[Unit] =
        for
          it  <- `][`.get
          tree = it(root)._1
          _   <- `][`.update { tree.children.foldLeft(_) { (it, child) =>
                                 val (tree @ `}{`(_, _, _, siblings), heth) = it(child)
                                 it + (child -> (tree.copy(siblings = siblings + node), heth))
                               }
                             }
          _   <- `][`.update { it =>
                               val (ntree, neth) = it(node)
                               val (rtree @ `}{`(_, _, children, _), reth) = it(root)
                               it + (root -> (rtree.copy(children = children + node), reth))
                                  + (node -> (ntree.copy(root = root, siblings = children), neth))
                             }
        yield
          ()

      private def update(temp: `}{`, root: `)*(`, join: `)*(`)
                        (using `][`: `][`): USTM[Unit] =
        `][`.update { it =>
                      temp.siblings.foldLeft {
                        val (tree @ `}{`(_, _, children, _), reth) = it(temp.root)
                        it + (temp.root -> (tree.copy(children = children - root + join), reth))
                      } { (it, sibling) =>
                        val (tree @ `}{`(_, _, _, siblings), heth) = it(sibling)
                        it + (sibling -> (tree.copy(siblings = siblings - root + join), heth))
                      }
                    }

      private def merge(tree: `}{`, join: `)*(`)
                       (using `][`: `][`): USTM[Unit] =
        val children = tree.children
        val siblings = tree.siblings
        `][`.update { it =>
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

      private def ether(lhs: ><, rhs: ><): UIO[><] =
        val min = lhs.takers.size min rhs.offerers.size
        if min == 0
        then
          ZIO.succeed(><(lhs.takers, rhs.offerers))
        else
          ZIO.collectAllDiscard {
            (lhs.takers.take(min) zip rhs.offerers.take(min))
              .map { (t, o) => t.succeed(o._1) *> o._2.succeed(()) }
          }.as(><(lhs.takers.drop(min), rhs.offerers.drop(min)))

      def apply(`)(`: FiberRef[`)(`])(caps: ζ)
               (using `][`: `][`, `1`: TSemaphore): UIO[Unit] =

        caps match

          case Π.ζ(Some(op), Left(_amb), next) =>

            val amb = try _amb.ζ.amb.right.get catch _ => _amb

            assert(try { amb.ζ; false } catch _ => true)

            op match

              case `ζ-Op`.in =>
                { for
                  key <- `)(`.get
                  _   <- {
                    for
                      _   <- `1`.acquire
                      it  <- `][`.get
                      _   <- ZSTM.check { val node = it.keys.find(_.contains(key)).get
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
                  }.commit
                  yield
                    ()
                } *> next.map(this(`)(`)(_)).getOrElse(ZIO.unit)

              case `ζ-Op`.out =>
                { for
                  key <- `)(`.get
                  _   <- {
                    for
                      _   <- `1`.acquire
                      it  <- `][`.get
                      _   <- ZSTM.check { val node = it.keys.find(_.contains(key)).get
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
                  }.commit
                  yield
                    ()
                } *> next.map(this(`)(`)(_)).getOrElse(ZIO.unit)

              case `ζ-Op`.open =>
                { for
                  key    <- `)(`.get
                  (r, n) <- {
                    for
                      _           <- `1`.acquire
                      it          <- `][`.get
                      _           <- ZSTM.check { val root = it.keys.find(_.contains(key)).get
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
                  }.commit
                  (root, temp, reth) = r
                  (node, tree, neth) = n
                  rstate <- reth.get
                  nstate <- neth.get
                  state1 <- ether(rstate, nstate)
                  state2 <- ether(nstate, rstate)
                  jeth   <- Ref.make[><](><(state1.takers ++ state2.takers,
                                            state1.offerers ++ state2.offerers))
                  join    = root ++ node
                  _      <- {
                    for
                      _ <- remove(node, tree)
                      _ <- `][`.update { _ - root - node + (join -> (temp, jeth)) }
                      _ <- update(temp, root, join)
                      _ <- merge(tree, join)
                      _ <- `1`.release
                    yield
                      ()
                  }.commit
                  yield
                    ()
                } *> next.map(this(`)(`)(_)).getOrElse(ZIO.unit)

          case Π.ζ(Some(_), _, _) => ??? // impossible by syntax

          case Π.ζ(_, Left(caps), next) =>

            ZIO.unit *> this(`)(`)(caps.ζ) *> next.map(this(`)(`)(_)).getOrElse(ZIO.unit)

          case _ => ???

      def apply(`)(`: FiberRef[`)(`], _amb: `)(`)
               (using `][`: `][`, `1`: TSemaphore): UIO[Unit] =
        val amb = try _amb.ζ.amb.right.get catch _ => _amb
        for
          key  <- `)(`.get
          uuid <- Π.`)(`()
          _    <- {
            for
              _   <- `1`.acquire
              it  <- `][`.get
              _   <- ZSTM.check { val root = it.keys.find(_.contains(key)).get
                                  val tree = it(root)._1
                                  tree.children.exists(it(_)._1.amb eq amb)
                                }
              root = it.keys.find(_.contains(key)).get
              temp = it(root)._1
              node = temp.children.find(it(_)._1.amb eq amb).get
              tree = it(node)._1
              _   <- remove(node, tree)
              join = node + uuid
              _   <- `][`.update(_ - node + (join -> it(node)))
              _   <- insert(join, tree.root)
            yield
              ()
          }.commit
          _    <- `)(`.set(uuid)
          _    <- `1`.release.commit
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

      def apply(`)(`: FiberRef[`)(`], _amb: `)(`)
               (using `][`: `][`, `1`: TSemaphore): UIO[Unit] =
        val amb = try _amb.ζ.amb.right.get catch _ => _amb
        for
          uuid <- Π.`)(`()
          node  = Set(uuid)
          neth <- Ref.make[><](><())
          key  <- `)(`.get
          _    <- {
            for
              _ <- `1`.acquire
              _ <- `][`.update { it =>
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
          }.commit
          _    <- `)(`.set(uuid)
          _    <- `1`.release.commit
        yield
          ()


    /**
      * Type of ambients' trees.
      */
    type `][` = TRef[Map[`)*(`, (`}{`, >*<)]]

    object `][`:
      def apply(): UIO[(FiberRef[`)(`], `][`, TSemaphore)] =
        for
          eth  <- Ref.make[><](><())
          amb   = `)(`(())
          uuid <- `)(`()
          root  = Set(uuid)
          lo   <- ZIO.scoped(FiberRef.make[`)(`](uuid))
          map   = Map(root -> (`}{`(amb, null, Set.empty, Set.empty), eth))
          tree <- TRef.make[Map[`)*(`, (`}{`, >*<)]](map).commit
          sem  <- TSemaphore.make(1).commit
        yield
          (lo, tree, sem)

      /**
        * Return the [[>*<]] ether for this [[FiberRef]].
        * Note that the semaphore is acquired and not yet released,
        * but its release delayed until input/output action.
        */
      def apply(`)(`: FiberRef[`)(`])
               (using `][`: `][`, `1`: TSemaphore): UIO[>*<] =
        for
          key  <- `)(`.get
          neth <- {
            for
              _   <- `1`.acquire
              it  <- `][`.get
              node = it.keys.find(_.contains(key)).get
            yield
              it(node)._2
          }.commit
        yield
          neth


    /**
      * output
      */
    object <> :

       def apply(wrap: `)(`)(`)(`: FiberRef[`)(`])
                (using `][`, TSemaphore): UIO[Unit] =
         for
           `>R` <- `][`(`)(`)
           _    <- ><(wrap)(`>R`)
         yield
           ()

       def apply(wrap: `)(`)(`)(`: FiberRef[`)(`])(code: => Task[Any])
                (using `][`, TSemaphore): UIO[Unit] =
         for
           `>R` <- `][`(`)(`)
           _    <- ><(wrap)(code)(`>R`)
         yield
           ()

    /**
      * input
      */
    object `()`:

      def apply(`)(`: FiberRef[`)(`])
               (using `][`, TSemaphore): UIO[`)(`] =
         for
           `<R` <- `][`(`)(`)
           name <- ><()(`<R`)
         yield
           name

      def apply[T](`)(`: FiberRef[`)(`])(code: T => Task[T])
                  (using `][`, TSemaphore): UIO[`)(`] =
         for
           `<R` <- `][`(`)(`)
           name <- ><()(code)(`<R`)
         yield
           name


    object `Π-magic`:

      final case class ><(takers: Queue[Promise[Nothing, `)(`]],
                          offerers: Queue[(`)(`, Promise[Nothing, Unit])])

      /**
        * Type of ambients' ether.
        */
      type >*< = Ref[><]

      object >< :

        inline def apply(): >< = ><(Queue.empty, Queue.empty)

        def apply(wrap: `)(`)(`>R`: Ref[><])
                 (using `1`: TSemaphore): UIO[Unit] =
          Promise.make[Nothing, Unit].flatMap { offerer =>
            ZIO.uninterruptibleMask { restore =>
              `>R`.modify { it =>
                it.takers.dequeueOption match
                  case Some((taker, queue)) =>
                    (taker.succeed(wrap).unit <* `1`.release.commit) -> it.copy(takers = queue)
                  case _ =>
                    val queue = it.offerers.enqueue(wrap -> offerer)
                    (`1`.release.commit *> restore(offerer.await)) -> it.copy(offerers = queue)
              }.flatten
            }
          }

        def apply(wrap: `)(`)(code: => Task[Any])(`>R`: Ref[><])
                 (using `1`: TSemaphore): UIO[Unit] =
          Promise.make[Nothing, Unit].flatMap { offerer =>
            ZIO.uninterruptibleMask { restore =>
              `>R`.modify { it =>
                it.takers.dequeueOption match
                  case Some((taker, queue)) =>
                    (taker.succeed(wrap).unit <* `1`.release.commit) -> it.copy(takers = queue)
                  case _ =>
                    val queue = it.offerers.enqueue(wrap -> offerer)
                    (`1`.release.commit *> restore(offerer.await)) -> it.copy(offerers = queue)
              }.flatten
            }
          } <* exec(code)

        def apply()(`<R`: Ref[><])
                   (using `1`: TSemaphore): UIO[`)(`] =
          Promise.make[Nothing, `)(`].flatMap { taker =>
            ZIO.uninterruptibleMask { restore =>
              `<R`.modify { it =>
                it.offerers.dequeueOption match
                  case Some(((name, offerer), queue)) =>
                    (offerer.succeed(()).as(name) <* `1`.release.commit) -> it.copy(offerers = queue)
                  case _ =>
                    val queue = it.takers.enqueue(taker)
                    (`1`.release.commit *> restore(taker.await)) -> it.copy(takers = queue)
              }.flatten
            }
          }

        def apply[T]()(code: T => Task[T])(`<R`: Ref[><])
                      (using `1`: TSemaphore): UIO[`)(`] =
          Promise.make[Nothing, `)(`].flatMap { taker =>
            ZIO.uninterruptibleMask { restore =>
              `<R`.modify { it =>
                it.offerers.dequeueOption match
                  case Some(((name, offerer), queue)) =>
                    (offerer.succeed(()).as(name) <* `1`.release.commit) -> it.copy(offerers = queue)
                  case _ =>
                    val queue = it.takers.enqueue(taker)
                    (`1`.release.commit *> restore(taker.await)) -> it.copy(takers = queue)
              }.flatten
            }
          }.flatMap {
            case null  => ZIO.succeed(null)
            case it: T => (code andThen exec)(it).map(`)(`)
          }
