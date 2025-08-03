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

  import _root_.scala.collection.immutable.{ List, Queue, Map, Set }

  import _root_.cats.instances.list.*
  import _root_.cats.syntax.traverse.*
  import _root_.cats.effect.{ IO, IOLocal, Deferred, Poll, Ref }
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.{ CyclicBarrier, Semaphore, Supervisor, UUIDGen }

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
    def apply(): IO[`)(`] = UUIDGen.randomUUID[IO].map(new `)(`(_))


  /**
    * Transactions' trees' nodes.
    */
  final case class `}{`(xa: `()`,
                        root: `)*(`,
                        children: Set[`)*(`])


  object `}{`:

    def apply(`)(`: IOLocal[`)(`], xa: `()`)
             (implicit `][`: `][`, `2`: Semaphore[IO]): IO[Unit] =
      for
        _    <- `2`.acquireN(2)
        root <- `)(`.get
        uuid <- Π.`)(`()
        node  = Set(uuid)
        _    <- `)(`.set(uuid)
        _    <- `][`.update { it =>
                              val key = it.keys.find(_.contains(root)).get
                              val tree @ `}{`(_, _, children) = it(key)
                              it + (node -> `}{`(xa, key, Set.empty))
                                 + (key  -> tree.copy(children = children + node))
                            }
        _    <- `2`.releaseN(2)
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
        _ <- temp.children.toList.traverse(update_(join, _)).void
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
                       `][`: `][`, `2`: Semaphore[IO]): IO[`()`] =
      for
        _       <- `2`.acquireN(2)
        root    <- `)(`.get
        blocked <- `][`.modify { it =>
                                 val key = it.keys.find(_.contains(root)).get
                                 val tree = it(key)
                                 it -> !tree.children.exists { node =>
                                   val xa = it(node).xa.xct.toString
                                   `π-kong`(id).contains(xa)
                                 }
                               }
        xa      <- if blocked then `2`.releaseN(2) >> IO.cede >> this(id)(`)(`)
                   else
                     for
                       uuid               <- Π.`)(`()
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
                       _                  <- `2`.releaseN(2)
                     yield
                       temp.xa
      yield
        xa

    def apply(xa: `()`)(`)(`: IOLocal[`)(`])
             (implicit `][`: `][`, `2`: Semaphore[IO]): IO[Unit] =
      for
        _                        <- `2`.acquireN(2)
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
        _                        <- `2`.releaseN(2)
      yield
        ()

  /**
    * Type of transactions' trees.
    */
  type `][` = Ref[IO, Map[`)*(`, `}{`]]

  object `][`:

    def apply(): IO[(IOLocal[`)(`], `][`)] =
      for
        id <- `)(`()
        key = Set(id)
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

    type > = (Deferred[IO, Any], (CyclicBarrier[IO], CyclicBarrier[IO], Ref[IO, Deferred[IO, `)(`]]))

    type < = ((Any, Deferred[IO, Unit]), (CyclicBarrier[IO], CyclicBarrier[IO], Ref[IO, Deferred[IO, `)(`]]))

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


  private def loop(`)(`: IOLocal[`)(`],
                   xaCB: CyclicBarrier[IO], xaCB2: CyclicBarrier[IO], xaR: Ref[IO, Deferred[IO, `)(`]])
                  (using `][`, Semaphore[IO]): IO[Unit] =
    xaCB.await >>
    `][`(`)(`).flatMap { xa =>
      for
        xaD <- xaR.get
        _   <- xaD.complete(xa)
        _   <- xaCB2.await
        xaD <- Deferred[IO, `)(`]
        _   <- xaR.set(xaD)
      yield
        ()
    } >> loop(`)(`, xaCB, xaCB2, xaR)

  /**
    * prefix
    */
  implicit final class `()`(private val name: Any) extends AnyVal:

    private def ref = `()`[>*<]
    private[Π] def xct = `()`[`)(`]

    def ====(that: `()`) =
      try
        this.ref eq that.ref
      catch
        case _ =>
          this.name == that.name

    inline def unary_! : Boolean = name == null
    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`, `)(`: IOLocal[`)(`])
             (using `][`, Semaphore[IO]): IO[Option[Unit]] =
      for
        _     <- IO { assert(!value.name.isInstanceOf[`)(`]) }
        xaCB  <- CyclicBarrier[IO](2)
        xaCB2 <- CyclicBarrier[IO](2)
        xaD   <- Deferred[IO, `)(`]
        xaR   <- Ref.of[IO, Deferred[IO, `)(`]](xaD)
        _     <- loop(`)(`, xaCB, xaCB2, xaR).background.use { _ =>
                   ><(value.name, `)(`)(xaCB, xaCB2, xaR)(ref)
                 }
      yield
        Some(())

    /**
      * negative prefix i.e. output
      */
    def apply(value: `()`, `)(`: IOLocal[`)(`])(code: => IO[Any])
             (using `][`, Semaphore[IO]): IO[Option[Unit]] =
      for
        _     <- IO { assert(!value.name.isInstanceOf[`)(`]) }
        xaCB  <- CyclicBarrier[IO](2)
        xaCB2 <- CyclicBarrier[IO](2)
        xaD   <- Deferred[IO, `)(`]
        xaR   <- Ref.of[IO, Deferred[IO, `)(`]](xaD)
        _     <- loop(`)(`, xaCB, xaCB2, xaR).background.use { _ =>
                   ><(value.name, `)(`)(xaCB, xaCB2, xaR)(code)(ref)
                 }
      yield
        Some(())

    /**
      * positive prefix i.e. input
      */
    def apply(`)(`: IOLocal[`)(`])
             (using `][`, Semaphore[IO]): IO[`()`] =
      for
        xaCB  <- CyclicBarrier[IO](2)
        xaCB2 <- CyclicBarrier[IO](2)
        xaD   <- Deferred[IO, `)(`]
        xaR   <- Ref.of[IO, Deferred[IO, `)(`]](xaD)
        name  <- loop(`)(`, xaCB, xaCB2, xaR).background.use { _ =>
                   ><(`)(`)(xaCB, xaCB2, xaR)(ref)
                 }
      yield
        new `()`(name)

    /**
      * positive prefix i.e. input
      */
    def apply[T](`)(`: IOLocal[`)(`])(code: T => IO[T])
                (using `][`, Semaphore[IO]): IO[`()`] =
      for
        xaCB  <- CyclicBarrier[IO](2)
        xaCB2 <- CyclicBarrier[IO](2)
        xaD   <- Deferred[IO, `)(`]
        xaR   <- Ref.of[IO, Deferred[IO, `)(`]](xaD)
        name  <- loop(`)(`, xaCB, xaCB2, xaR).background.use { _ =>
                   ><(`)(`)(xaCB, xaCB2, xaR)(code)(ref)
                 }
      yield
        new `()`(name)

    override def toString: String = if name == null then "null" else name.toString


  private object `Π-magic`:

    private val tsdga = "pisc.transactions.deferred.get.anytime"

    private lazy val anytime = sys.BooleanProp.keyExists(tsdga).value

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

    final case class ><(takers: Queue[ν.>], offerers: Queue[ν.<])

    type >*< = Ref[IO, ><]

    object >< :

      inline def apply(): >< = ><(Queue.empty, Queue.empty)

      def apply(name: Any, `)(`: IOLocal[`)(`])
               (xaCB: CyclicBarrier[IO], xaCB2: CyclicBarrier[IO], xaR: Ref[IO, Deferred[IO, `)(`]])
               (`>R`: >*<)
               (using `][`)
               (using `2`: Semaphore[IO]): IO[Unit] =
        for
          offerer <- Deferred[IO, Unit]
          _       <- {
                       def loop(firsttime: Boolean): IO[Unit] =
                         def or(poll: Poll[IO], xa: `)(`, head: Option[ν.>]): IO[(Option[ν.>], Boolean)] =
                           ( head match
                               case Some((_, (xaCBʹ, xaCB2ʹ, xaRʹ))) =>
                                 xaCBʹ.await
                                 *> xaRʹ.get.flatMap(_.get.map(_ eq xa)) <*
                                 xaCB2ʹ.await
                               case _ =>
                                 IO.pure(true)
                           ).ifM(
                             ( head match
                                 case Some((taker, _)) =>
                                   taker.complete(name).void
                                 case _ =>
                                   poll(offerer.get)
                             ).flatMap { _ => `2`.release.as(None -> false) },
                             `2`.releaseN(2).as(head -> true)
                           )
                         `>R`.flatModifyFull { (poll, it) =>
                           it.takers.dequeueOption match
                             case Some((head, queue)) =>
                               it.copy(takers = queue) ->
                               (`2`.acquireN(2) >> `][`(`)(`).flatMap(or(null, _, Some(head))))
                             case _ if firsttime =>
                               val queue = it.offerers.enqueue(name -> offerer -> (xaCB, xaCB2, xaR))
                               it.copy(offerers = queue) -> or(poll, null, None)
                             case _ =>
                               it -> IO.pure(None -> true)
                         }.flatTap {
                           case (Some(head), _) =>
                             `>R`.update { it =>
                               val queue = it.takers.enqueue(head)
                               it.copy(takers = queue)
                             }
                           case _ =>
                             IO.unit
                         }.flatMap {
                           case (_, true) => IO.cede >> loop(anytime)
                           case _ => IO.unit
                         }
                       loop(true)
                     }
        yield
          ()

      def apply(name: Any, `)(`: IOLocal[`)(`])
               (xaCB: CyclicBarrier[IO], xaCB2: CyclicBarrier[IO], xaR: Ref[IO, Deferred[IO, `)(`]])
               (code: => IO[Any])
               (`>R`: >*<)
               (using `][`)
               (using `2`: Semaphore[IO]): IO[Unit] =
        for
          offerer <- Deferred[IO, Unit]
          _       <- {
                       def loop(firsttime: Boolean): IO[Unit] =
                         def or(poll: Poll[IO], xa: `)(`, head: Option[ν.>]): IO[(Option[ν.>], Boolean)] =
                           ( head match
                               case Some((_, (xaCBʹ, xaCB2ʹ, xaRʹ))) =>
                                 xaCBʹ.await
                                 *> xaRʹ.get.flatMap(_.get.map(_ eq xa)) <*
                                 xaCB2ʹ.await
                               case _ =>
                                 IO.pure(true)
                           ).ifM(
                             ( head match
                                 case Some((taker, _)) =>
                                   taker.complete(name).void
                                 case _ =>
                                   poll(offerer.get)
                             ).flatMap { _ => `2`.release.as(None -> false) <* exec(code) },
                             `2`.releaseN(2).as(head -> true)
                           )
                         `>R`.flatModifyFull { (poll, it) =>
                           it.takers.dequeueOption match
                             case Some((head, queue)) =>
                               it.copy(takers = queue) ->
                               (`2`.acquireN(2) >> `][`(`)(`).flatMap(or(null, _, Some(head))))
                             case _ if firsttime =>
                               val queue = it.offerers.enqueue(name -> offerer -> (xaCB, xaCB2, xaR))
                               it.copy(offerers = queue) -> or(poll, null, None)
                             case _ =>
                               it -> IO.pure(None -> true)
                         }.flatTap {
                           case (Some(head), _) =>
                             `>R`.update { it =>
                               val queue = it.takers.enqueue(head)
                               it.copy(takers = queue)
                             }
                           case _ =>
                             IO.unit
                         }.flatMap {
                           case (_, true) => IO.cede >> loop(anytime)
                           case _ => IO.unit
                         }
                       loop(true)
                     }
        yield
          ()

      def apply(`)(`: IOLocal[`)(`])
               (xaCB: CyclicBarrier[IO], xaCB2: CyclicBarrier[IO], xaR: Ref[IO, Deferred[IO, `)(`]])
               (`<R`: >*<)
               (using `][`)
               (using `2`: Semaphore[IO]): IO[Any] =
        for
          taker <- Deferred[IO, Any]
          name  <- {
                     def loop(firsttime: Boolean): IO[Any] =
                       def or(poll: Poll[IO], xa: `)(`, head: Option[ν.<]): IO[((Option[ν.<], Boolean), Any)] =
                         ( head match
                             case Some((_, (xaCBʹ, xaCB2ʹ, xaRʹ))) =>
                               xaCBʹ.await
                               *> xaRʹ.get.flatMap(_.get.map(_ eq xa)) <*
                               xaCB2ʹ.await
                             case _ =>
                               IO.pure(true)
                         ).ifM(
                           ( head match
                               case Some(((name, offerer), _)) =>
                                 offerer.complete(()).as(name)
                               case _ =>
                                 poll(taker.get)
                           ).flatMap { it => `2`.release.as(None -> false -> it) },
                           `2`.releaseN(2).as(head -> true -> ())
                         )
                       `<R`.flatModifyFull { (poll, it) =>
                         it.offerers.dequeueOption match
                           case Some((head, queue)) =>
                             it.copy(offerers = queue) ->
                             (`2`.acquireN(2) >> `][`(`)(`).flatMap(or(null, _, Some(head))))
                           case _ if firsttime =>
                             val queue = it.takers.enqueue(taker -> (xaCB, xaCB2, xaR))
                             it.copy(takers = queue) -> or(poll, null, None)
                           case _ =>
                             it -> IO.pure(None -> true -> ())
                       }.flatTap {
                         case ((Some(head), _), _) =>
                           `<R`.update { it =>
                             val queue = it.offerers.enqueue(head)
                             it.copy(offerers = queue)
                           }
                         case _ =>
                           IO.unit
                       }.flatMap {
                         case ((_, true), _) => IO.cede >> loop(anytime)
                         case (_, it) => IO.pure(it)
                       }
                     loop(true)
                   }
        yield
          name

      def apply[T](`)(`: IOLocal[`)(`])
                  (xaCB: CyclicBarrier[IO], xaCB2: CyclicBarrier[IO], xaR: Ref[IO, Deferred[IO, `)(`]])
                  (code: T => IO[T])
                  (`<R`: >*<)
                  (using `][`)
                  (using `2`: Semaphore[IO]): IO[Any] =
        for
          taker <- Deferred[IO, Any]
          name  <- {
                     def loop(firsttime: Boolean): IO[Any] =
                       def or(poll: Poll[IO], xa: `)(`, head: Option[ν.<]): IO[((Option[ν.<], Boolean), Any)] =
                         ( head match
                             case Some((_, (xaCBʹ, xaCB2ʹ, xaRʹ))) =>
                               xaCBʹ.await
                               *> xaRʹ.get.flatMap(_.get.map(_ eq xa)) <*
                               xaCB2ʹ.await
                             case _ =>
                               IO.pure(true)
                         ).ifM(
                           ( head match
                               case Some(((name, offerer), _)) =>
                                 offerer.complete(()).as(name)
                               case _ =>
                                 poll(taker.get)
                           ).flatMap { case it: T => `2`.release.as(None -> false) product (code andThen exec)(it) },
                           `2`.releaseN(2).as(head -> true -> ())
                         )
                       `<R`.flatModifyFull { (poll, it) =>
                         it.offerers.dequeueOption match
                           case Some((head, queue)) =>
                             it.copy(offerers = queue) ->
                             (`2`.acquireN(2) >> `][`(`)(`).flatMap(or(null, _, Some(head))))
                           case _ if firsttime =>
                             val queue = it.takers.enqueue(taker -> (xaCB, xaCB2, xaR))
                             it.copy(takers = queue) -> or(poll, null, None)
                           case _ =>
                             it -> IO.pure(None -> true -> ())
                       }.flatTap {
                         case ((Some(head), _), _) =>
                           `<R`.update { it =>
                             val queue = it.offerers.enqueue(head)
                             it.copy(offerers = queue)
                           }
                         case _ =>
                           IO.unit
                       }.flatMap {
                         case ((_, true), _) => IO.cede >> loop(anytime)
                         case (_, it) => IO.pure(it)
                       }
                     loop(true)
                   }
        yield
          name
