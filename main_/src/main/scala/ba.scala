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

  import _root_.scala.collection.immutable.{ Queue, Map, Set }

  import _root_.cats.instances.list.*
  import _root_.cats.syntax.traverse.*
  import _root_.cats.effect.{ IO, IOLocal, Clock, Deferred, FiberIO, Ref }
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.{ CyclicBarrier, Semaphore, Supervisor, UUIDGen }

  import `Π-loop`.{ <>, %, /, \ }
  import `Π-magic`.><
  export `Π-magic`.>*<
  import `Π-stats`.Rate

  import `π-$`.*, `π-ζ`.*


  /**
    * Wraps ambient keys.
    *
    * @param value
    */
  final class `)(`(private val value: Any) extends AnyVal:
    override def toString: String = value.toString

  object `)(`:
    /**
      * Initial ambient unique key.
      */
    def apply(): IO[`)(`] =
      UUIDGen.randomUUID[IO].map(new `)(`(_))

  /**
    * Type of keys in [[`][`]].
    */
  type `)*(` = Set[`)(`]


  private abstract trait Ordʹ { val ord: Int }
  sealed abstract trait Ord(val ord: Int) extends Ordʹ

  /**
    * Type of directions.
    */
  enum `π-$` extends Ordʹ {
    case `π-local` extends `π-$` with Ord(0)
    case `π-s2s`   extends `π-$` with Ord(1)
    case `π-p2c`   extends `π-$` with Ord(2)
    case `π-c2p`   extends `π-$` with Ord(2)
  }

  /**
    * Type of capabilities.
    */
  enum `π-ζ` extends Ordʹ {
    case `π-enter`  extends `π-ζ` with Ord(3)
    case `π-accept` extends `π-ζ` with Ord(3)
    case `π-exit`   extends `π-ζ` with Ord(4)
    case `π-expel`  extends `π-ζ` with Ord(4)
    case `π-merge+` extends `π-ζ` with Ord(5)
    case `π-merge-` extends `π-ζ` with Ord(5)
  }


  type `Π-Map`[K, +V] = Map[K, V]

  type `Π-Set`[A] = Set[A]


  /**
    * Ambients' trees' nodes.
    */
  final case class `}{`(label: Option[String],
                        root: `)*(`,
                        children: Set[`)*(`],
                        siblings: Set[`)*(`])

  object `}{`:
    private def apply_(node: `)*(`, child: `)*(`)
                      (implicit `][`: `][`): IO[Unit] =
      `][`.update { m =>
                    val tree @ `}{`(_, _, _, siblings) = m(child)
                    m + (child -> tree.copy(siblings = siblings + node))
                  }

    def apply(`)(`: IOLocal[`)(`], label: Option[String])
             (implicit `][`: `][`, `2`: Semaphore[IO]): IO[Unit] =
      for
        _        <- `2`.acquireN(2)
        root     <- `)(`.get
        uuid     <- sΠ.`)(`()
        node      = Set(uuid)
        _        <- `)(`.set(uuid)
        children <- `][`.modify { m =>
                                  val key = m.keys.find(_.contains(root)).get
                                  val tree @ `}{`(_, _, children, _) = m(key)
                                  (m + (node -> `}{`(label, key, Set.empty, children))
                                     + (key  -> tree.copy(children = children + node))) -> children
                                }
        _        <- children.toList.traverse(apply_(node, _)).void
        _        <- `2`.releaseN(2)
      yield
        ()

    /**
      * Return the label and the snapshot for this [[IOLocal]].
      */
    def apply(`)(`: IOLocal[`)(`], snapshot: Boolean = false)
             (implicit `][`: `][`): IO[(String, String)] =
      for
        node  <- `)(`.get
        lab_s <- `][`.modify { m => m -> {
                               var root = m.keys.find(_.contains(node)).get
                               def label(node: `)*(`): String = m(node).label.getOrElse("")
                               label(root) -> (
                                 if !snapshot
                                 then ""
                                 else
                                   while m(root).root ne null do root = m(root).root
                                   var id = 0
                                   var tree = Map[`)*(`, Int](root -> id)
                                   def make(root: `)*(`): Unit =
                                     for
                                       node <- m(root).children
                                     do
                                       id += 1
                                       tree += node -> id
                                       make(node)
                                   make(root)
                                   def xml(root: `)*(`, count: Int, indent: String): StringBuilder =
                                     val pid = tree(root)
                                     def siblings(node: `)*(`, count: Int): StringBuilder =
                                       val sid = tree(node)
                                       val sb = StringBuilder()
                                       sb.append(s"$indent\t\t<siblings count=$count sibling=$sid>\n")
                                         .append {
                                           ( for
                                               nodeʹ <- m(node).siblings
                                               sidʹ   = tree(nodeʹ)
                                             yield
                                               StringBuilder(s"""$indent\t\t\t<node id=$sidʹ label="${label(nodeʹ)}" parent=$pid sibling=$sid/>""")
                                           ).reduce(_.append("\n").append(_)).append("\n")
                                         }
                                         .append(s"$indent\t\t</siblings>\n")
                                     def children: StringBuilder =
                                       val sb = StringBuilder()
                                       sb.append(s"$indent<children count=$count parent=$pid>\n")
                                         .append {
                                           ( for
                                               node <- m(root).children
                                               cid   = tree(node)
                                             yield
                                               val sbʹ = StringBuilder()
                                               val count = m(node).children.size
                                               if count == 0
                                               then
                                                 val count = m(node).siblings.size
                                                 if count == 0
                                                 then
                                                   sbʹ.append(s"""$indent\t<node id=$cid label="${label(node)}" parent=$pid/>""")
                                                 else
                                                   sbʹ.append(s"""$indent\t<node id=$cid label="${label(node)}" parent=$pid>\n""")
                                                      .append(siblings(node, count))
                                                      .append(s"$indent\t</node>")
                                               else
                                                 sbʹ.append(s"""$indent\t<node id=$cid label="${label(node)}" parent=$pid>\n""")
                                                    .append(xml(node, count, indent + "\t\t"))
                                                    .append("\n")
                                                    .append {
                                                      val count = m(node).siblings.size
                                                      if count == 0
                                                      then
                                                        StringBuilder()
                                                      else
                                                        siblings(node, count)
                                                    }
                                                    .append(s"$indent\t</node>")
                                           ).reduce(_.append("\n").append(_)).append("\n")
                                         }
                                         .append(s"$indent</children>")
                                     children
                                   val count = m(root).children.size
                                   val sb = StringBuilder()
                                   if count == 0
                                   then
                                     sb.append(s"""<root id=${tree(root)} label="${label(root)}"/>\n""")
                                       .toString
                                   else
                                     sb.append(s"""<root id=${tree(root)} label="${label(root)}">\n""")
                                       .append(xml(root, count, "\t"))
                                       .append("\n</root>")
                                       .toString
                               )
                            }
                }
      yield
        lab_s

  /**
    * Type of ambients' trees.
    */
  type `][` = Ref[IO, Map[`)*(`, `}{`]]

  object `][`:
    def apply(): IO[(IOLocal[`)(`], `][`)] =
      for
        _    <- IO.unit
        uuid <- `)(`()
        root  = Set(uuid)
        lo  <- IOLocal[`)(`](uuid)
        map   = Map(root -> `}{`(None, null, Set.empty, Set.empty))
        tree <- Ref.of[IO, Map[`)*(`, `}{`]](map)
      yield
        (lo, tree)

    /**
      * Return the [[`)*(`]] node for this [[IOLocal]].
      */
    def apply(`)(`: IOLocal[`)(`])
             (implicit `][`: `][`): IO[`)*(`] =
      for
        node <- `)(`.get
        node <- `][`.modify { m => m -> m.keys.find(_.contains(node)).get }
      yield
        node


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

    type > = ((Deferred[IO, Any], (CyclicBarrier[IO], CyclicBarrier[IO], Ref[IO, Deferred[IO, `)*(`]])), `π-$` | `π-ζ`)

    type < = (((Any, Deferred[IO, Unit]), (CyclicBarrier[IO], CyclicBarrier[IO], Ref[IO, Deferred[IO, `)*(`]])), `π-$` | `π-ζ`)

    def map[B](f: `()` => B): IO[B] = flatMap(f andThen IO.pure)
    def flatMap[B](f: `()` => IO[B]): IO[B] =
      ( for
          ref <- Ref.of[IO, Map[Int, ><]] {
                   Map(
                     `π-local`.ord  -> ><(),
                     `π-s2s`.ord    -> ><(),
                     `π-p2c`.ord    -> ><(),
                     `π-accept`.ord -> ><(),
                     `π-expel`.ord  -> ><(),
                     `π-merge+`.ord -> ><()
                   )
                 }
        yield
          f(ref)
      ).flatten


  /**
    * silent transition
    */
  object τ:

    def apply(rate: Rate)(key: String, `)(`: IOLocal[`)(`])
             (using % : %, / : /)
             (using `][`: `][`, `2`: Semaphore[IO])
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[Double] =
      for
        _            <- exclude(key)
        deferred     <- Deferred[IO, Option[<>]]
        dummy_ref    <- Ref.of[IO, Map[Int, ><]](Map.empty)
        _            <- `2`.acquireN(2)
        (s_label, _) <- `}{`(`)(`)
        _            <- `2`.releaseN(2)
        timestamp    <- Clock[IO].monotonic.map(_.toNanos)
        _            <- /.offer(^ -> key -> (deferred -> (timestamp, (dummy_ref -> -1, None, rate))))
        opt          <- deferred.get
        _            <- if opt eq None then IO.canceled else IO.unit
        (delay, _,
         s, b, f, d)  = opt.get
        _            <- `2`.acquireN(2)
        e_label      <- `}{`(`)(`, s)
        _            <- `2`.releaseN(2)
        _            <- d.complete(s_label -> e_label)
        _            <- b.await
        _            <- f.join
      yield
        delay

  private def loop(`)(`: IOLocal[`)(`],
                   nodeCB: CyclicBarrier[IO], nodeCB2: CyclicBarrier[IO], nodeR: Ref[IO, Deferred[IO, `)*(`]])
                  (using `][`): IO[Unit] =
    nodeCB.await >>
    `][`(`)(`).flatMap { node =>
      for
        nodeD <- nodeR.get
        _     <- nodeD.complete(node)
        _     <- nodeCB2.await
        nodeD <- Deferred[IO, `)*(`]
        _     <- nodeR.set(nodeD)
      yield
        ()
    } >> loop(`)(`, nodeCB, nodeCB2, nodeR)

  /**
    * prefix
    */
  final implicit class `()`(private val name: Any) extends AnyVal:

    private def ref = `()`[>*<]

    def ====(that: `()`) =
      try
        this.ref eq that.ref
      catch
        case _ =>
          this.name == that.name

    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

    /**
      * capability prefix
      */
    def apply(rate: Rate)(key: String, `)(`: IOLocal[`)(`], cap: `π-ζ`)
             (using % : %, / : /)
             (using `][`: `][`, `2`: Semaphore[IO])
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[Double] =
      for
        _            <- exclude(key)
        deferred     <- Deferred[IO, Option[<>]]
        polarity      = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
        _            <- `2`.acquireN(2)
        (s_label, _) <- `}{`(`)(`)
        _            <- `2`.releaseN(2)
        timestamp    <- Clock[IO].monotonic.map(_.toNanos)
        _            <- /.offer(^ -> key -> (deferred -> (timestamp, (ref -> cap.ord, Some(polarity), rate))))
        opt          <- deferred.get
        _            <- if opt eq None then IO.canceled else IO.unit
        (delay, b2,
         s, b, f, d)  = opt.get
        nodeCB       <- CyclicBarrier[IO](2)
        nodeCB2      <- CyclicBarrier[IO](2)
        nodeD        <- Deferred[IO, `)*(`]
        nodeR        <- Ref.of[IO, Deferred[IO, `)*(`]](nodeD)
        _            <- loop(`)(`, nodeCB, nodeCB2, nodeR).background.use { _ =>
                          if polarity
                          then ><.ζ.<(s_label, `)(`, cap, nodeCB, nodeCB2, nodeR)(b2, s, d)(ref)
                          else ><.ζ.>(s_label, `)(`, cap, nodeCB, nodeCB2, nodeR)(b2, s, d)(ref)
                        }
        _            <- b.await
        _            <- f.join
      yield
        delay

    /**
      * capability prefix
      */
    def apply(rate: Rate)(key: String, `)(`: IOLocal[`)(`], cap: `π-ζ`)(code: => IO[Any])
             (using % : %, / : /)
             (using `][`: `][`, `2`: Semaphore[IO])
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[Double] =
      for
        _            <- exclude(key)
        deferred     <- Deferred[IO, Option[<>]]
        polarity      = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
        _            <- `2`.acquireN(2)
        (s_label, _) <- `}{`(`)(`)
        _            <- `2`.releaseN(2)
        timestamp    <- Clock[IO].monotonic.map(_.toNanos)
        _            <- /.offer(^ -> key -> (deferred -> (timestamp, (ref -> cap.ord, Some(polarity), rate))))
        opt          <- deferred.get
        _            <- if opt eq None then IO.canceled else IO.unit
        (delay, b2,
         s, b, f, d)  = opt.get
        nodeCB       <- CyclicBarrier[IO](2)
        nodeCB2      <- CyclicBarrier[IO](2)
        nodeD        <- Deferred[IO, `)*(`]
        nodeR        <- Ref.of[IO, Deferred[IO, `)*(`]](nodeD)
        _            <- loop(`)(`, nodeCB, nodeCB2, nodeR).background.use { _ =>
                          if polarity
                          then ><.ζ.<(s_label, `)(`, cap, nodeCB, nodeCB2, nodeR)(b2, s, d)(code)(ref)
                          else ><.ζ.>(s_label, `)(`, cap, nodeCB, nodeCB2, nodeR)(b2, s, d)(code)(ref)
                        }
        _            <- b.await
        _            <- f.join
      yield
        delay

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)
             (using % : %, / : /)
             (using `][`: `][`, `2`: Semaphore[IO])
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[Double] =
      for
        _            <- exclude(key)
        deferred     <- Deferred[IO, Option[<>]]
        _            <- `2`.acquireN(2)
        (s_label, _) <- `}{`(`)(`)
        _            <- `2`.releaseN(2)
        timestamp    <- Clock[IO].monotonic.map(_.toNanos)
        _            <- /.offer(^ -> key -> (deferred -> (timestamp, (ref -> dir.ord, Some(false), rate))))
        opt          <- deferred.get
        _            <- if opt eq None then IO.canceled else IO.unit
        (delay, _,
         s, b, f, d)  = opt.get
        nodeCB       <- CyclicBarrier[IO](2)
        nodeCB2      <- CyclicBarrier[IO](2)
        nodeD        <- Deferred[IO, `)*(`]
        nodeR        <- Ref.of[IO, Deferred[IO, `)*(`]](nodeD)
        _            <- loop(`)(`, nodeCB, nodeCB2, nodeR).background.use { _ =>
                          ><.π(value.name, s_label, `)(`, dir, nodeCB, nodeCB2, nodeR)(s, d)(ref)
                        }
        _            <- b.await
        _            <- f.join
      yield
        delay

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: => IO[Any])
             (using % : %, / : /)
             (using `][`: `][`, `2`: Semaphore[IO])
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[Double] =
      for
        _            <- exclude(key)
        deferred     <- Deferred[IO, Option[<>]]
        _            <- `2`.acquireN(2)
        (s_label, _) <- `}{`(`)(`)
        _            <- `2`.releaseN(2)
        timestamp    <- Clock[IO].monotonic.map(_.toNanos)
        _            <- /.offer(^ -> key -> (deferred -> (timestamp, (ref -> dir.ord, Some(false), rate))))
        opt          <- deferred.get
        _            <- if opt eq None then IO.canceled else IO.unit
        (delay, _,
         s, b, f, d)  = opt.get
        nodeCB       <- CyclicBarrier[IO](2)
        nodeCB2      <- CyclicBarrier[IO](2)
        nodeD        <- Deferred[IO, `)*(`]
        nodeR        <- Ref.of[IO, Deferred[IO, `)*(`]](nodeD)
        _            <- loop(`)(`, nodeCB, nodeCB2, nodeR).background.use { _ =>
                          ><.π(value.name, s_label, `)(`, dir, nodeCB, nodeCB2, nodeR)(s, d)(code)(ref)
                        }
        _            <- b.await
        _            <- f.join
      yield
        delay

    /**
      * positive prefix i.e. input
      */
    def apply(rate: Rate)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)
             (using % : %, / : /)
             (using `][`: `][`, `2`: Semaphore[IO])
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[(`()`, Double)] =
      for
        _            <- exclude(key)
        deferred     <- Deferred[IO, Option[<>]]
        _            <- `2`.acquireN(2)
        (s_label, _) <- `}{`(`)(`)
        _            <- `2`.releaseN(2)
        timestamp    <- Clock[IO].monotonic.map(_.toNanos)
        _            <- /.offer(^ -> key -> (deferred -> (timestamp, (ref -> dir.ord, Some(true), rate))))
        opt          <- deferred.get
        _            <- if opt eq None then IO.canceled else IO.unit
        (delay, _,
         s, b, f, d)  = opt.get
        nodeCB       <- CyclicBarrier[IO](2)
        nodeCB2      <- CyclicBarrier[IO](2)
        nodeD        <- Deferred[IO, `)*(`]
        nodeR        <- Ref.of[IO, Deferred[IO, `)*(`]](nodeD)
        name         <- loop(`)(`, nodeCB, nodeCB2, nodeR).background.use { _ =>
                          ><.π(s_label, `)(`, dir, nodeCB, nodeCB2, nodeR)(s, d)(ref)
                        }
        _            <- b.await
        _            <- f.join
      yield
        new `()`(name) -> delay

    /**
      * positive prefix i.e. input
      */
    def apply[T](rate: Rate)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: T => IO[T])
                (using % : %, / : /)
                (using `][`: `][`, `2`: Semaphore[IO])
                (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): IO[(`()`, Double)] =
      for
        _            <- exclude(key)
        deferred     <- Deferred[IO, Option[<>]]
        _            <- `2`.acquireN(2)
        (s_label, _) <- `}{`(`)(`)
        _            <- `2`.releaseN(2)
        timestamp    <- Clock[IO].monotonic.map(_.toNanos)
        _            <- /.offer(^ -> key -> (deferred -> (timestamp, (ref -> dir.ord, Some(true), rate))))
        opt          <- deferred.get
        _            <- if opt eq None then IO.canceled else IO.unit
        (delay, _,
         s, b, f, d)  = opt.get
        nodeCB       <- CyclicBarrier[IO](2)
        nodeCB2      <- CyclicBarrier[IO](2)
        nodeD        <- Deferred[IO, `)*(`]
        nodeR        <- Ref.of[IO, Deferred[IO, `)*(`]](nodeD)
        name         <- loop(`)(`, nodeCB, nodeCB2, nodeR).background.use { _ =>
                          ><.π(s_label, `)(`, dir, nodeCB, nodeCB2, nodeR)(s, d)(code)(ref)
                        }
        _            <- b.await
        _            <- f.join
      yield
        new `()`(name) -> delay

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

    final case class ><(takers: Queue[ν.>], offerers: Queue[ν.<])

    type >*< = Ref[IO, Map[Int, ><]]

    object >< :

      inline def apply(): >< = ><(Queue.empty, Queue.empty)

      @annotation.tailrec
      private def check(node: `)*(`,
                        nodeʹ: `)*(`,
                        dir_cap: `π-$` | `π-ζ`,
                        dir_capʹ: `π-$` | `π-ζ`)
                       (using `][`: `][`): IO[Boolean] =
        (dir_cap, dir_capʹ) match
          case (`π-local`, `π-local`)   => IO.pure(node == nodeʹ)
          case (`π-s2s`, `π-s2s`)
             | (`π-enter`, `π-accept`)
             | (`π-merge+`, `π-merge-`) =>
            `][`.modify { m => m -> m(node).siblings.contains(nodeʹ) }
          case (`π-p2c`, `π-c2p`)
             | (`π-expel`, `π-exit`)    =>
            `][`.modify { m => m -> (m(nodeʹ).root == node) }
          case (`π-c2p`, `π-p2c`)       => check(nodeʹ, node, dir_capʹ, dir_cap)
          case (`π-accept`, `π-enter`)  => check(nodeʹ, node, dir_capʹ, dir_cap)
          case (`π-exit`, `π-expel`)    => check(nodeʹ, node, dir_capʹ, dir_cap)
          case (`π-merge-`, `π-merge+`) => check(nodeʹ, node, dir_capʹ, dir_cap)

      object π:

        def apply(name: Any, s_label: String, `)(`: IOLocal[`)(`], dir: `π-$`,
                  nodeCB: CyclicBarrier[IO], nodeCB2: CyclicBarrier[IO], nodeR: Ref[IO, Deferred[IO, `)*(`]])
                 (snapshot: Boolean, deferred: Deferred[IO, (String, (String, String))])
                 (using `][`)
                 (using `2`: Semaphore[IO])
                 (`>R`: >*<): IO[Unit] =
          for
            offerer <- Deferred[IO, Unit]
            ord      = dir.ord
            _       <- {
                         def loop: IO[Unit] =
                           def take(node: `)*(`, head: Option[ν.>]): IO[Unit] =
                             IO.uncancelable { poll =>
                               ( head match
                                   case Some(((_, (nodeCBʹ, nodeCB2ʹ, nodeRʹ)), dirʹ: `π-$`)) =>
                                     nodeCBʹ.await
                                     *> nodeRʹ.get.flatMap(_.get.flatMap(check(node, _, dir, dirʹ))) <*
                                     nodeCB2ʹ.await
                                   case _ =>
                                     IO.pure(true)
                               ).ifM(
                                 ( head match
                                     case Some(((taker, _), _)) =>
                                       taker.complete(name).void
                                     case _ =>
                                       poll(offerer.get)
                                 ).flatTap { _ =>
                                    for
                                      e_label <- `}{`(`)(`, snapshot)
                                      _       <- `2`.release
                                      _       <- deferred.complete(s_label -> e_label)
                                    yield
                                      ()
                                 },
                                 `>R`.flatModify { m =>
                                   val queue = m(ord).takers.enqueue(head.get)
                                   m + (ord -> m(ord).copy(takers = queue)) ->
                                   (`2`.releaseN(2) >> IO.cede >> loop)
                                 }
                               )
                             }
                           `>R`.flatModify { m =>
                             m(ord).takers.dequeueOption match
                               case Some((taker, queue)) =>
                                 m + (ord -> m(ord).copy(takers = queue)) ->
                                 (`2`.acquireN(2) >> `][`(`)(`).map(_ -> Some(taker)))
                               case _ =>
                                 val queue = m(ord).offerers.enqueue(name -> offerer -> (nodeCB, nodeCB2, nodeR) -> dir)
                                 m + (ord -> m(ord).copy(offerers = queue)) -> IO.pure((null: `)*(`) -> None)
                           }.flatMap(take.tupled)
                         loop
                     }
          yield
            ()

        def apply(name: Any, s_label: String, `)(`: IOLocal[`)(`], dir: `π-$`,
                  nodeCB: CyclicBarrier[IO], nodeCB2: CyclicBarrier[IO], nodeR: Ref[IO, Deferred[IO, `)*(`]])
                 (snapshot: Boolean, deferred: Deferred[IO, (String, (String, String))])
                 (code: => IO[Any])
                 (using `][`)
                 (using `2`: Semaphore[IO])
                 (`>R`: >*<): IO[Unit] =
          for
            offerer <- Deferred[IO, Unit]
            ord      = dir.ord
            _       <- {
                         def loop: IO[Unit] =
                           def take(node: `)*(`, head: Option[ν.>]): IO[Unit] =
                             IO.uncancelable { poll =>
                               ( head match
                                   case Some(((_, (nodeCBʹ, nodeCB2ʹ, nodeRʹ)), dirʹ: `π-$`)) =>
                                     nodeCBʹ.await
                                     *> nodeRʹ.get.flatMap(_.get.flatMap(check(node, _, dir, dirʹ))) <*
                                     nodeCB2ʹ.await
                                   case _ =>
                                     IO.pure(true)
                               ).ifM(
                                 ( head match
                                     case Some(((taker, _), _)) =>
                                       taker.complete(name).void
                                     case _ =>
                                       poll(offerer.get)
                                 ).flatTap { _ =>
                                    for
                                      e_label <- `}{`(`)(`, snapshot)
                                      _       <- `2`.release
                                      _       <- deferred.complete(s_label -> e_label)
                                      _       <- exec(code)
                                    yield
                                      ()
                                 },
                                 `>R`.flatModify { m =>
                                   val queue = m(ord).takers.enqueue(head.get)
                                   m + (ord -> m(ord).copy(takers = queue)) ->
                                   (`2`.releaseN(2) >> IO.cede >> loop)
                                 }
                               )
                             }
                           `>R`.flatModify { m =>
                             m(ord).takers.dequeueOption match
                               case Some((taker, queue)) =>
                                 m + (ord -> m(ord).copy(takers = queue)) ->
                                 (`2`.acquireN(2) >> `][`(`)(`).map(_ -> Some(taker)))
                               case _ =>
                                 val queue = m(ord).offerers.enqueue(name -> offerer -> (nodeCB, nodeCB2, nodeR) -> dir)
                                 m + (ord -> m(ord).copy(offerers = queue)) -> IO.pure((null: `)*(`) -> None)
                           }.flatMap(take.tupled)
                         loop
                     }
          yield
            ()

        def apply(s_label: String, `)(`: IOLocal[`)(`], dir: `π-$`,
                  nodeCB: CyclicBarrier[IO], nodeCB2: CyclicBarrier[IO], nodeR: Ref[IO, Deferred[IO, `)*(`]])
                 (snapshot: Boolean, deferred: Deferred[IO, (String, (String, String))])
                 (using `][`)
                 (using `2`: Semaphore[IO])
                 (`<R`: >*<): IO[Any] =
          for
            taker   <- Deferred[IO, Any]
            ord      = dir.ord
            name    <- {
                         def loop: IO[Any] =
                           def offer(node: `)*(`, head: Option[ν.<]): IO[Any] =
                             IO.uncancelable { poll =>
                               ( head match
                                  case Some(((_, (nodeCBʹ, nodeCB2ʹ, nodeRʹ)), dirʹ: `π-$`)) =>
                                    nodeCBʹ.await
                                    *> nodeRʹ.get.flatMap(_.get.flatMap(check(node, _, dir, dirʹ))) <*
                                    nodeCB2ʹ.await
                                  case _ =>
                                    IO.pure(true)
                               ).ifM(
                                 ( head match
                                     case Some((((name, offerer), _), _)) =>
                                       offerer.complete(()).as(name)
                                     case _ =>
                                       poll(taker.get)
                                 ).flatTap { _ =>
                                    for
                                      e_label <- `}{`(`)(`, snapshot)
                                      _       <- `2`.release
                                      _       <- deferred.complete(s_label -> e_label)
                                    yield
                                      ()
                                 },
                                 `<R`.flatModify { m =>
                                   val queue = m(ord).offerers.enqueue(head.get)
                                   m + (ord -> m(ord).copy(offerers = queue)) ->
                                   (`2`.releaseN(2) >> IO.cede >> loop)
                                 }
                               )
                             }
                           `<R`.flatModify { m =>
                             m(ord).offerers.dequeueOption match
                               case Some((offerer, queue)) =>
                                 m + (ord -> m(ord).copy(offerers = queue)) ->
                                 (`2`.acquireN(2) >> `][`(`)(`).map(_ -> Some(offerer)))
                               case _ =>
                                 val queue = m(ord).takers.enqueue(taker -> (nodeCB, nodeCB2, nodeR) -> dir)
                                 m + (ord -> m(ord).copy(takers = queue)) -> IO.pure((null: `)*(`) -> None)
                           }.flatMap(offer.tupled)
                         loop
                     }
          yield
            name

        def apply[T](s_label: String, `)(`: IOLocal[`)(`], dir: `π-$`,
                     nodeCB: CyclicBarrier[IO], nodeCB2: CyclicBarrier[IO], nodeR: Ref[IO, Deferred[IO, `)*(`]])
                    (snapshot: Boolean, deferred: Deferred[IO, (String, (String, String))])
                    (code: T => IO[T])
                    (using `][`)
                    (using `2`: Semaphore[IO])
                    (`<R`: >*<): IO[Any] =
          for
            taker   <- Deferred[IO, Any]
            ord      = dir.ord
            name    <- {
                         def loop: IO[Any] =
                           def offer(node: `)*(`, head: Option[ν.<]): IO[Any] =
                             IO.uncancelable { poll =>
                               ( head match
                                  case Some(((_, (nodeCBʹ, nodeCB2ʹ, nodeRʹ)), dirʹ: `π-$`)) =>
                                    nodeCBʹ.await
                                    *> nodeRʹ.get.flatMap(_.get.flatMap(check(node, _, dir, dirʹ))) <*
                                    nodeCB2ʹ.await
                                  case _ =>
                                    IO.pure(true)
                               ).ifM(
                                 ( head match
                                     case Some((((name, offerer), _), _)) =>
                                       offerer.complete(()).as(name)
                                     case _ =>
                                       poll(taker.get)
                                 ).flatMap { case it: T =>
                                    for
                                      e_label <- `}{`(`)(`, snapshot)
                                      _       <- `2`.release
                                      _       <- deferred.complete(s_label -> e_label)
                                      it      <- (code andThen exec)(it)
                                    yield
                                      it
                                 },
                                 `<R`.flatModify { m =>
                                   val queue = m(ord).offerers.enqueue(head.get)
                                   m + (ord -> m(ord).copy(offerers = queue)) ->
                                   (`2`.releaseN(2) >> IO.cede >> loop)
                                 }
                               )
                             }
                           `<R`.flatModify { m =>
                             m(ord).offerers.dequeueOption match
                               case Some((offerer, queue)) =>
                                 m + (ord -> m(ord).copy(offerers = queue)) ->
                                 (`2`.acquireN(2) >> `][`(`)(`).map(_ -> Some(offerer)))
                               case _ =>
                                 val queue = m(ord).takers.enqueue(taker -> (nodeCB, nodeCB2, nodeR) -> dir)
                                 m + (ord -> m(ord).copy(takers = queue)) -> IO.pure((null: `)*(`) -> None)
                           }.flatMap(offer.tupled)
                         loop
                     }
          yield
            name

      object ζ:

        private def remove_(node: `)*(`, sibling: `)*(`)
                           (implicit `][`: `][`): IO[Unit] =
          `][`.update { m =>
                        val tree @ `}{`(_, _, _, siblings) = m(sibling)
                        m + (sibling -> tree.copy(siblings = siblings - node))
                      }

        private def remove(node: `)*(`, tree: `}{`)
                          (implicit `][`: `][`): IO[Unit] =
          val `}{`(_, root, _, siblings) = tree
          `][`.update { m =>
                        val rtree = m(root)
                        m + (root -> rtree.copy(children = siblings))
                      } >> siblings.toList.traverse(remove_(node, _)).void

        private def insert_(node: `)*(`, child: `)*(`)
                           (implicit `][`: `][`): IO[Unit] =
          `][`.update { m =>
                        val tree @ `}{`(_, _, _, siblings) = m(child)
                        m + (child -> tree.copy(siblings = siblings + node))
                      }

        private def insert(node: `)*(`, root: `)*(`)
                          (implicit `][`: `][`): IO[Unit] =
          for
            tree <- `][`.modify { m => m -> m(root) }
            _    <- tree.children.toList.traverse(insert_(node, _)).void
            _    <- `][`.update { m =>
                                  val ntree = m(node)
                                  val rtree @ `}{`(_, _, children, _) = m(root)
                                  m + (root -> rtree.copy(children = children + node))
                                    + (node -> ntree.copy(root = root, siblings = children))
                                }
          yield
            ()

        private def update_(root: `)*(`, join: `)*(`, sibling: `)*(`)
                           (implicit `][`: `][`): IO[Unit] =
          `][`.update { m =>
                        val tree @ `}{`(_, _, _, siblings) = m(sibling)
                        m + (sibling -> tree.copy(siblings = siblings - root + join))
                      }

        private def update(temp: `}{`, root: `)*(`, join: `)*(`)
                          (implicit `][`: `][`): IO[Unit] =
          `][`.update { m =>
                        val tree @ `}{`(_, _, children, _) = m(temp.root)
                        m + (temp.root -> tree.copy(children = children - root + join))
                      } >> temp.siblings.toList.traverse(update_(root, join, _)).void

        private def merge_(join: `)*(`, node: `)*(`)
                          (implicit `][`: `][`): IO[Unit] =
          `][`.update { m =>
                        val tree = m(node)
                        m + (node -> tree.copy(root = join))
                      }

        private def merge__(siblings: Set[`)*(`], node: `)*(`)
                           (implicit `][`: `][`): IO[Unit] =
          `][`.update { m =>
                        val tree = m(node)
                        m + (node -> tree.copy(siblings = tree.siblings ++ siblings))
                      }

        private def merge(tree: `}{`, join: `)*(`)
                         (implicit `][`: `][`): IO[Unit] =
          tree.children.toList.traverse(merge_(join, _)).void >>
          `][`.modify { m =>
                        val temp @ `}{`(_, _, children, _) = m(join)
                        m + (join -> temp.copy(children = children ++ tree.children)) -> children
                      }.flatMap { children =>
                        tree.children.toList.traverse(merge__(children, _)).void
                      }

        private def apply(node: `)*(`, any: Any, cap: `π-ζ`)
                         (using `][`: `][`): IO[Unit] =
          any match
            case nodeʹ: `)*(` =>
              cap match
                case `π-enter` | `π-exit` =>
                  for
                    (root, tree) <- `][`.modify { m =>
                                                  cap match
                                                    case `π-enter` =>
                                                      m -> (nodeʹ, m(node))
                                                    case `π-exit` =>
                                                      m -> (m(nodeʹ).root, m(node))
                                                }
                    _            <- remove(node, tree)
                    _            <- insert(node, root)
                  yield
                    ()

                case `π-merge+` =>
                  for
                    tree <- `][`.modify { m => m -> m(nodeʹ) }
                    _    <- remove(nodeʹ, tree)
                    temp <- `][`.modify { m => m -> m(node) }
                    join  = node ++ nodeʹ
                    _    <- `][`.update { _ - node - nodeʹ + (join -> temp) }
                    _    <- update(temp, node, join)
                    _    <- merge(tree, join)
                  yield
                    ()

        object > :

          def apply(s_label: String, `)(`: IOLocal[`)(`], cap: `π-ζ`,
                    nodeCB: CyclicBarrier[IO], nodeCB2: CyclicBarrier[IO], nodeR: Ref[IO, Deferred[IO, `)*(`]])
                   (b2: CyclicBarrier[IO], snapshot: Boolean, deferred: Deferred[IO, (String, (String, String))])
                   (using `][`)
                   (using `2`: Semaphore[IO])
                   (`>R`: >*<): IO[Unit] =
            for
              offerer <- Deferred[IO, Unit]
              ord      = cap.ord
              _       <- {
                           def loop: IO[Unit] =
                             def take(node: `)*(`, head: Option[ν.>]): IO[Unit] =
                               IO.uncancelable { poll =>
                                 ( head match
                                     case Some(((_, (nodeCBʹ, nodeCB2ʹ, nodeRʹ)), capʹ: `π-ζ`)) =>
                                       nodeCBʹ.await
                                       *> nodeRʹ.get.flatMap(_.get.flatMap(check(node, _, cap, capʹ))) <*
                                       nodeCB2ʹ.await
                                     case _ =>
                                       IO.pure(true)
                                 ).ifM(
                                   ( head match
                                       case Some(((taker, _), _)) =>
                                         taker.complete(node).void
                                       case _ =>
                                         poll(offerer.get)
                                   ).flatTap { _ =>
                                      for
                                        _       <- b2.await
                                        e_label <- `}{`(`)(`, snapshot)
                                        _       <- `2`.release
                                        _       <- deferred.complete(s_label -> e_label)
                                      yield
                                        ()
                                   },
                                   `>R`.flatModify { m =>
                                     val queue = m(ord).takers.enqueue(head.get)
                                     m + (ord -> m(ord).copy(takers = queue)) ->
                                     (`2`.releaseN(2) >> IO.cede >> loop)
                                   }
                                 )
                               }
                             `>R`.flatModify { m =>
                               m(ord).takers.dequeueOption match
                                 case Some((taker, queue)) =>
                                   m + (ord -> m(ord).copy(takers = queue)) ->
                                   (`2`.acquireN(2) >> `][`(`)(`).map(_ -> Some(taker)))
                                 case _ =>
                                   val queue = m(ord).offerers.enqueue(() -> offerer -> (nodeCB, nodeCB2, nodeR) -> cap)
                                   m + (ord -> m(ord).copy(offerers = queue)) -> IO.pure((null: `)*(`) -> None)
                             }.flatMap(take.tupled)
                           loop
                       }
            yield
              ()

          def apply(s_label: String, `)(`: IOLocal[`)(`], cap: `π-ζ`,
                    nodeCB: CyclicBarrier[IO], nodeCB2: CyclicBarrier[IO], nodeR: Ref[IO, Deferred[IO, `)*(`]])
                   (b2: CyclicBarrier[IO], snapshot: Boolean, deferred: Deferred[IO, (String, (String, String))])
                   (code: => IO[Any])
                   (using `][`)
                   (using `2`: Semaphore[IO])
                   (`>R`: >*<): IO[Unit] =
            for
              offerer <- Deferred[IO, Unit]
              ord      = cap.ord
              _       <- {
                           def loop: IO[Unit] =
                             def take(node: `)*(`, head: Option[ν.>]): IO[Unit] =
                               IO.uncancelable { poll =>
                                 ( head match
                                     case Some(((_, (nodeCBʹ, nodeCB2ʹ, nodeRʹ)), capʹ: `π-ζ`)) =>
                                       nodeCBʹ.await
                                       *> nodeRʹ.get.flatMap(_.get.flatMap(check(node, _, cap, capʹ))) <*
                                       nodeCB2ʹ.await
                                     case _ =>
                                       IO.pure(true)
                                 ).ifM(
                                   ( head match
                                       case Some(((taker, _), _)) =>
                                         taker.complete(node).void
                                       case _ =>
                                         poll(offerer.get)
                                   ).flatTap { _ =>
                                      for
                                        _       <- b2.await
                                        e_label <- `}{`(`)(`, snapshot)
                                        _       <- `2`.release
                                        _       <- deferred.complete(s_label -> e_label)
                                        _       <- exec(code)
                                      yield
                                        ()
                                   },
                                   `>R`.flatModify { m =>
                                     val queue = m(ord).takers.enqueue(head.get)
                                     m + (ord -> m(ord).copy(takers = queue)) ->
                                     (`2`.releaseN(2) >> IO.cede >> loop)
                                   }
                                 )
                               }
                             `>R`.flatModify { m =>
                               m(ord).takers.dequeueOption match
                                 case Some((taker, queue)) =>
                                   m + (ord -> m(ord).copy(takers = queue)) ->
                                   (`2`.acquireN(2) >> `][`(`)(`).map(_ -> Some(taker)))
                                 case _ =>
                                   val queue = m(ord).offerers.enqueue(() -> offerer -> (nodeCB, nodeCB2, nodeR) -> cap)
                                   m + (ord -> m(ord).copy(offerers = queue)) -> IO.pure((null: `)*(`) -> None)
                             }.flatMap(take.tupled)
                           loop
                       }
            yield
              ()

        object < :

          def apply(s_label: String, `)(`: IOLocal[`)(`], cap: `π-ζ`,
                    nodeCB: CyclicBarrier[IO], nodeCB2: CyclicBarrier[IO], nodeR: Ref[IO, Deferred[IO, `)*(`]])
                   (b2: CyclicBarrier[IO], snapshot: Boolean, deferred: Deferred[IO, (String, (String, String))])
                   (using `][`)
                   (using `2`: Semaphore[IO])
                   (`<R`: >*<): IO[Unit] =
            for
              taker   <- Deferred[IO, Any]
              ord      = cap.ord
              _       <- {
                           def loop: IO[Unit] =
                             def offer(node: `)*(`, head: Option[ν.<]): IO[Unit] =
                               IO.uncancelable { poll =>
                                 ( head match
                                    case Some(((_, (nodeCBʹ, nodeCB2ʹ, nodeRʹ)), capʹ: `π-ζ`)) =>
                                      nodeCBʹ.await
                                      *> nodeRʹ.get.flatMap(_.get.flatMap(check(node, _, cap, capʹ))) <*
                                      nodeCB2ʹ.await
                                    case _ =>
                                      IO.pure(true)
                                 ).ifM(
                                   ( head match
                                       case Some((((_, offerer), (nodeCBʹ, _, nodeRʹ)), _)) =>
                                         nodeCBʹ.await
                                         *> nodeRʹ.get.flatMap(_.get.flatMap(offerer.complete(()).as(_)))
                                       case _ =>
                                         poll(taker.get)
                                   ).flatMap { nodeʹ =>
                                     if node eq null
                                     then
                                       `][`(`)(`).flatMap(ζ(_, nodeʹ, cap))
                                     else
                                       ζ(node, nodeʹ, cap)
                                   }.flatTap { _ =>
                                      for
                                        _       <- b2.await
                                        e_label <- `}{`(`)(`, snapshot)
                                        _       <- `2`.release
                                        _       <- deferred.complete(s_label -> e_label)
                                      yield
                                        ()
                                   },
                                   `<R`.flatModify { m =>
                                     val queue = m(ord).offerers.enqueue(head.get)
                                     m + (ord -> m(ord).copy(offerers = queue)) ->
                                     (`2`.releaseN(2) >> IO.cede >> loop)
                                   }
                                 )
                               }
                             `<R`.flatModify { m =>
                               m(ord).offerers.dequeueOption match
                                 case Some((offerer, queue)) =>
                                   m + (ord -> m(ord).copy(offerers = queue)) ->
                                   (`2`.acquireN(2) >> `][`(`)(`).map(_ -> Some(offerer)))
                                 case _ =>
                                   val queue = m(ord).takers.enqueue(taker -> (nodeCB, nodeCB2, nodeR) -> cap)
                                   m + (ord -> m(ord).copy(takers = queue)) -> IO.pure((null: `)*(`) -> None)
                             }.flatMap(offer.tupled)
                           loop
                       }
            yield
              ()

          def apply(s_label: String, `)(`: IOLocal[`)(`], cap: `π-ζ`,
                    nodeCB: CyclicBarrier[IO], nodeCB2: CyclicBarrier[IO], nodeR: Ref[IO, Deferred[IO, `)*(`]])
                   (b2: CyclicBarrier[IO], snapshot: Boolean, deferred: Deferred[IO, (String, (String, String))])
                   (code: => IO[Any])
                   (using `][`)
                   (using `2`: Semaphore[IO])
                   (`<R`: >*<): IO[Unit] =
            for
              taker   <- Deferred[IO, Any]
              ord      = cap.ord
              _       <- {
                           def loop: IO[Unit] =
                             def offer(node: `)*(`, head: Option[ν.<]): IO[Unit] =
                               IO.uncancelable { poll =>
                                 ( head match
                                    case Some(((_, (nodeCBʹ, nodeCB2ʹ, nodeRʹ)), capʹ: `π-ζ`)) =>
                                      nodeCBʹ.await
                                      *> nodeRʹ.get.flatMap(_.get.flatMap(check(node, _, cap, capʹ))) <*
                                      nodeCB2ʹ.await
                                    case _ =>
                                      IO.pure(true)
                                 ).ifM(
                                   ( head match
                                       case Some((((_, offerer), (nodeCBʹ, _, nodeRʹ)), _)) =>
                                         nodeCBʹ.await
                                         *> nodeRʹ.get.flatMap(_.get.flatMap(offerer.complete(()).as(_)))
                                       case _ =>
                                         poll(taker.get)
                                   ).flatMap { nodeʹ =>
                                     if node eq null
                                     then
                                       `][`(`)(`).flatMap(ζ(_, nodeʹ, cap))
                                     else
                                       ζ(node, nodeʹ, cap)
                                   }.flatTap { _ =>
                                      for
                                        _       <- b2.await
                                        e_label <- `}{`(`)(`, snapshot)
                                        _       <- `2`.release
                                        _       <- deferred.complete(s_label -> e_label)
                                        _       <- exec(code)
                                      yield
                                        ()
                                   },
                                   `<R`.flatModify { m =>
                                     val queue = m(ord).offerers.enqueue(head.get)
                                     m + (ord -> m(ord).copy(offerers = queue)) ->
                                     (`2`.releaseN(2) >> IO.cede >> loop)
                                   }
                                 )
                               }
                             `<R`.flatModify { m =>
                               m(ord).offerers.dequeueOption match
                                 case Some((offerer, queue)) =>
                                   m + (ord -> m(ord).copy(offerers = queue)) ->
                                   (`2`.acquireN(2) >> `][`(`)(`).map(_ -> Some(offerer)))
                                 case _ =>
                                   val queue = m(ord).takers.enqueue(taker -> (nodeCB, nodeCB2, nodeR) -> cap)
                                   m + (ord -> m(ord).copy(takers = queue)) -> IO.pure((null: `)*(`) -> None)
                             }.flatMap(offer.tupled)
                           loop
                       }
            yield
              ()
