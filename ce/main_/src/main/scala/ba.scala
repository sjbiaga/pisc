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

  import _root_.cats.effect.{ IO, IOLocal, Clock, Deferred, Ref }
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.{ CyclicBarrier, MapRef, Supervisor, UUIDGen }

  import _root_.io.github.timwspence.cats.stm.STM

  import `Π-loop`.{ <>, %, /, \ }
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

    def apply(ref: Ref[IO, Map[Int, ><]]): MapRef[IO, Int, ><] =
      { k => Ref.lens(ref)(_.get(k).get, m => v => m + (k -> v)) }

    type > = Deferred[IO, (Any, (`)(`, `π-$` | `π-ζ`))]

    type < = (Any, Deferred[IO, (`)(`, `π-$` | `π-ζ`)])

    def map[B](f: `()` => B): IO[B] = flatMap(f andThen IO.pure)
    def flatMap[B](f: `()` => IO[B]): IO[B] =
      ( for
          ref <- Ref.of[IO, Map[Int, ><]] {
                   Map(
                     `π-local`.ord  -> ><(Queue.empty, Queue.empty),
                     `π-s2s`.ord    -> ><(Queue.empty, Queue.empty),
                     `π-p2c`.ord    -> ><(Queue.empty, Queue.empty),
                     `π-accept`.ord -> ><(Queue.empty, Queue.empty),
                     `π-expel`.ord  -> ><(Queue.empty, Queue.empty),
                     `π-merge+`.ord -> ><(Queue.empty, Queue.empty)
                   )
                 }
        yield
          f(this(ref))
      ).flatten


  /**
    * silent transition
    */
  object τ:

    def apply(rate: Rate, `}{`: `}{`)(key: String, `)(`: IOLocal[`)(`])
             (using % : %, / : /)
             (using `}{`.`][`, `}{`.stm.TSemaphore)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[Double] =
      for
        _            <- exclude(key)
        deferred     <- Deferred[IO, Option[<>]]
        (s_label, _) <- `}{`.`}{`(`)(`)
        timestamp    <- Clock[IO].monotonic.map(_.toNanos)
        _            <- /.offer(^ -> key -> (deferred -> (timestamp, (new Object -> -1, None, rate))))
        opt          <- deferred.get
        _            <- if opt eq None then IO.canceled else IO.unit
        (delay, s,
         _, b, f, d)  = opt.get
        e_label      <- `}{`.`}{`(`)(`, s)
        _            <- b.await
        _            <- d.complete(s_label -> e_label)
        _            <- f.join
      yield
        delay

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
    def apply(rate: Rate, `}{`: `}{`)(key: String, `)(`: IOLocal[`)(`], cap: `π-ζ`)
             (using % : %, / : /)
             (using `}{`.`][`, `}{`.stm.TSemaphore)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[Double] =
      for
        _            <- exclude(key)
        deferred     <- Deferred[IO, Option[<>]]
        polarity      = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
        (s_label, _) <- `}{`.`}{`(`)(`)
        timestamp    <- Clock[IO].monotonic.map(_.toNanos)
        _            <- /.offer(^ -> key -> (deferred -> (timestamp, (ref -> cap.ord, Some(polarity), rate))))
        opt          <- deferred.get
        _            <- if opt eq None then IO.canceled else IO.unit
        (delay, s,
         sd, b, f, d) = opt.get
        key          <- `)(`.get
        e_label      <- if polarity
                        then `}{`.`Π-magic`.><.ζ.<(key, cap)(sd, s)(ref)
                        else `}{`.`Π-magic`.><.ζ.>(key, cap)(sd, s)(ref)
        _            <- b.await
        _            <- d.complete(s_label -> e_label)
        _            <- f.join
      yield
        delay

    /**
      * capability prefix
      */
    def apply(rate: Rate, `}{`: `}{`)(key: String, `)(`: IOLocal[`)(`], cap: `π-ζ`)(code: => IO[Any])
             (using % : %, / : /)
             (using `}{`.`][`, `}{`.stm.TSemaphore)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[Double] =
      for
        _            <- exclude(key)
        deferred     <- Deferred[IO, Option[<>]]
        polarity      = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
        (s_label, _) <- `}{`.`}{`(`)(`)
        timestamp    <- Clock[IO].monotonic.map(_.toNanos)
        _            <- /.offer(^ -> key -> (deferred -> (timestamp, (ref -> cap.ord, Some(polarity), rate))))
        opt          <- deferred.get
        _            <- if opt eq None then IO.canceled else IO.unit
        (delay, s,
         sd, b, f, d) = opt.get
        key          <- `)(`.get
        e_label      <- if polarity
                        then `}{`.`Π-magic`.><.ζ.<(key, cap)(sd, s)(code)(ref)
                        else `}{`.`Π-magic`.><.ζ.>(key, cap)(sd, s)(code)(ref)
        _            <- b.await
        _            <- d.complete(s_label -> e_label)
        _            <- f.join
      yield
        delay

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, `}{`: `}{`, value: `()`)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)
             (using % : %, / : /)
             (using `}{`.`][`, `}{`.stm.TSemaphore)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[Double] =
      for
        _            <- exclude(key)
        deferred     <- Deferred[IO, Option[<>]]
        (s_label, _) <- `}{`.`}{`(`)(`)
        timestamp    <- Clock[IO].monotonic.map(_.toNanos)
        _            <- /.offer(^ -> key -> (deferred -> (timestamp, (ref -> dir.ord, Some(false), rate))))
        opt          <- deferred.get
        _            <- if opt eq None then IO.canceled else IO.unit
        (delay, s,
         sd, b, f, d) = opt.get
        key          <- `)(`.get
        e_label      <- `}{`.`Π-magic`.><.π(value.name, key, dir)(sd, s)(ref)
        _            <- b.await
        _            <- d.complete(s_label -> e_label)
        _            <- f.join
      yield
        delay

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, `}{`: `}{`, value: `()`)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: => IO[Any])
             (using % : %, / : /)
             (using `}{`.`][`, `}{`.stm.TSemaphore)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[Double] =
      for
        _            <- exclude(key)
        deferred     <- Deferred[IO, Option[<>]]
        (s_label, _) <- `}{`.`}{`(`)(`)
        timestamp    <- Clock[IO].monotonic.map(_.toNanos)
        _            <- /.offer(^ -> key -> (deferred -> (timestamp, (ref -> dir.ord, Some(false), rate))))
        opt          <- deferred.get
        _            <- if opt eq None then IO.canceled else IO.unit
        (delay, s,
         sd, b, f, d) = opt.get
        key          <- `)(`.get
        e_label      <- `}{`.`Π-magic`.><.π(value.name, key, dir)(sd, s)(code)(ref)
        _            <- b.await
        _            <- d.complete(s_label -> e_label)
        _            <- f.join
      yield
        delay

    /**
      * positive prefix i.e. input
      */
    def apply(rate: Rate, `}{`: `}{`)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)
             (using % : %, / : /)
             (using `}{`.`][`, `}{`.stm.TSemaphore)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[(`()`, Double)] =
      for
        _            <- exclude(key)
        deferred     <- Deferred[IO, Option[<>]]
        (s_label, _) <- `}{`.`}{`(`)(`)
        timestamp    <- Clock[IO].monotonic.map(_.toNanos)
        _            <- /.offer(^ -> key -> (deferred -> (timestamp, (ref -> dir.ord, Some(true), rate))))
        opt          <- deferred.get
        _            <- if opt eq None then IO.canceled else IO.unit
        (delay, s,
         sd, b, f, d) = opt.get
        key          <- `)(`.get
        (name,
         e_label)    <- `}{`.`Π-magic`.><.π(key, dir)(sd, s)(ref)
        _            <- b.await
        _            <- d.complete(s_label -> e_label)
        _            <- f.join
      yield
        new `()`(name) -> delay

    /**
      * positive prefix i.e. input
      */
    def apply[T](rate: Rate, `}{`: `}{`)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: T => IO[T])
                (using % : %, / : /)
                (using `}{`.`][`, `}{`.stm.TSemaphore)
                (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): IO[(`()`, Double)] =
      for
        _            <- exclude(key)
        deferred     <- Deferred[IO, Option[<>]]
        (s_label, _) <- `}{`.`}{`(`)(`)
        timestamp    <- Clock[IO].monotonic.map(_.toNanos)
        _            <- /.offer(^ -> key -> (deferred -> (timestamp, (ref -> dir.ord, Some(true), rate))))
        opt          <- deferred.get
        _            <- if opt eq None then IO.canceled else IO.unit
        (delay, s,
         sd, b, f, d) = opt.get
        key          <- `)(`.get
        (name,
         e_label)    <- `}{`.`Π-magic`.><.π(key, dir)(sd, s)(code)(ref)
        _            <- b.await
        _            <- d.complete(s_label -> e_label)
        _            <- f.join
      yield
        new `()`(name) -> delay

    override def toString: String = if name == null then "null" else name.toString


  final case class ><(takers: Queue[ν.>], offerers: Queue[ν.<])

  type >*< = MapRef[IO, Int, ><]


  final class `}{`(val stm: STM[IO]):

    import stm.*

    /**
      * Ambients' trees' nodes.
      */
    final case class `}{`(label: Option[String],
                          root: `)*(`,
                          children: Set[`)*(`],
                          siblings: Set[`)*(`])

    object `}{`:
      def apply(`)(`: IOLocal[`)(`], label: Option[String])
               (using `][`: `][`, `1`: TSemaphore): IO[Unit] =
        for
          key  <- `)(`.get
          uuid <- sΠ.`)(`()
          node  = Set(uuid)
          _    <- `)(`.set(uuid)
          _    <- stm.commit {
            for
              _ <- `1`.acquire
              _ <- `][`.modify { m =>
                                 val root = m.keys.find(_.contains(key)).get
                                 val tree @ `}{`(_, _, children, _) = m(root)
                                 children.foldLeft {
                                   m + (node -> `}{`(label, root, Set.empty, children))
                                     + (root -> tree.copy(children = children + node))
                                 } { (m, child) =>
                                   val tree @ `}{`(_, _, _, siblings) = m(child)
                                   m + (child -> tree.copy(siblings = siblings + node))
                                 }
                               }
              _ <- `1`.release
            yield
              ()
          }
        yield
          ()

      /**
        * Return the label and the snapshot for this [[IOLocal]].
        */
      def apply(`)(`: IOLocal[`)(`], snapshot: Boolean = false)
               (using `][`: `][`): IO[(String, String)] =
        for
          key   <- `)(`.get
          lab_s <- stm.commit { apply(key, snapshot) }
        yield
          lab_s
      def apply(key: `)(`, snapshot: Boolean)
               (using `][`: `][`): Txn[(String, String)] =
        `][`.get.map { m =>
                       var root = m.keys.find(_.contains(key)).get
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

    /**
      * Type of ambients' trees.
      */
    type `][` = TVar[Map[`)*(`, `}{`]]

    object `][`:
      def apply(): IO[(IOLocal[`)(`], `][`, TSemaphore)] =
        for
          _    <- IO.unit
          uuid <- `)(`()
          root  = Set(uuid)
          lo   <- IOLocal[`)(`](uuid)
          map   = Map(root -> `}{`(None, null, Set.empty, Set.empty))
          tree <- stm.commit { TVar.of[Map[`)*(`, `}{`]](map) }
          sem  <- stm.commit { TSemaphore.make(1) }
        yield
          (lo, tree, sem)


    private[sΠ] object `Π-magic`:

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

      object >< :

        @annotation.tailrec
        private def check(node: `)*(`,
                          nodeʹ: `)*(`,
                          dir_cap: `π-$` | `π-ζ`,
                          dir_capʹ: `π-$` | `π-ζ`)
                         (using `][`: `][`): Txn[Boolean] =
          (dir_cap, dir_capʹ) match
            case (`π-local`, `π-local`)   =>
              stm.pure(node == nodeʹ)
            case (`π-s2s`, `π-s2s`)
               | (`π-enter`, `π-accept`)
               | (`π-merge+`, `π-merge-`) =>
              `][`.get.map(_(node).siblings.contains(nodeʹ))
            case (`π-p2c`, `π-c2p`)
               | (`π-expel`, `π-exit`)    =>
              `][`.get.map(_(nodeʹ).root == node)
            case (`π-c2p`, `π-p2c`)       => check(nodeʹ, node, dir_capʹ, dir_cap)
            case (`π-accept`, `π-enter`)  => check(nodeʹ, node, dir_capʹ, dir_cap)
            case (`π-exit`, `π-expel`)    => check(nodeʹ, node, dir_capʹ, dir_cap)
            case (`π-merge-`, `π-merge+`) => check(nodeʹ, node, dir_capʹ, dir_cap)

        object π:

          def apply(name: Any, key: `)(`, dir: `π-$`)
                   (deferred: Deferred[IO, String], snapshot: Boolean)
                   (using `][`: `][`, `1`: TSemaphore)
                   (`>R`: >*<): IO[(String, String)] =
            for
              offerer <- Deferred[IO, (`)(`, `π-$` | `π-ζ`)]
              ord      = dir.ord
              kd      <- `>R`(ord).flatModifyFull { (poll, it) =>
                           it.takers.dequeueOption match
                             case Some((taker, queue)) =>
                               it.copy(takers = queue) -> taker.complete(name -> (key -> dir)).as(None)
                             case _ =>
                               val queue = it.offerers.enqueue(name -> offerer)
                               it.copy(offerers = queue) -> poll(offerer.get).map(Some(_))
                         }
              label   <- kd match
                           case Some((keyʹ, dirʹ: `π-$`)) =>
                             for
                               label <- stm.commit {
                                 for
                                   _     <- `1`.acquire
                                   node  <- `][`.get.map(_.keys.find(_.contains(key)).get)
                                   nodeʹ <- `][`.get.map(_.keys.find(_.contains(keyʹ)).get)
                                   _     <- check(node, nodeʹ, dir, dirʹ).flatMap(stm.check(_))
                                   label <- `}{`(key, snapshot)
                                   _     <- `1`.release
                                 yield
                                   label
                               }
                               _     <- deferred.complete(label._2)
                             yield
                               label
                           case _ =>
                             for
                               label <- stm.commit { `}{`(key, false) }
                               snaps <- deferred.get
                             yield
                               label._1 -> snaps
            yield
              label

          def apply(name: Any, key: `)(`, dir: `π-$`)
                   (deferred: Deferred[IO, String], snapshot: Boolean)
                   (code: => IO[Any])
                   (using `][`: `][`, `1`: TSemaphore)
                   (`>R`: >*<): IO[(String, String)] =
            for
              offerer <- Deferred[IO, (`)(`, `π-$` | `π-ζ`)]
              ord      = dir.ord
              kd      <- `>R`(ord).flatModifyFull { (poll, it) =>
                           it.takers.dequeueOption match
                             case Some((taker, queue)) =>
                               it.copy(takers = queue) -> taker.complete(name -> (key -> dir)).as(None)
                             case _ =>
                               val queue = it.offerers.enqueue(name -> offerer)
                               it.copy(offerers = queue) -> poll(offerer.get).map(Some(_))
                         }
              label   <- kd match
                           case Some((keyʹ, dirʹ: `π-$`)) =>
                             for
                               label <- stm.commit {
                                 for
                                   _     <- `1`.acquire
                                   node  <- `][`.get.map(_.keys.find(_.contains(key)).get)
                                   nodeʹ <- `][`.get.map(_.keys.find(_.contains(keyʹ)).get)
                                   _     <- check(node, nodeʹ, dir, dirʹ).flatMap(stm.check(_))
                                   label <- `}{`(key, snapshot)
                                   _     <- `1`.release
                                 yield
                                   label
                               }
                               _     <- deferred.complete(label._2)
                             yield
                               label
                           case _ =>
                             for
                               label <- stm.commit { `}{`(key, false) }
                               snaps <- deferred.get
                             yield
                               label._1 -> snaps
              _       <- exec(code)
            yield
              label

          def apply(key: `)(`, dir: `π-$`)
                   (deferred: Deferred[IO, String], snapshot: Boolean)
                   (using `][`: `][`, `1`: TSemaphore)
                   (`<R`: >*<): IO[(Any, (String, String))] =
            for
              taker   <- Deferred[IO, (Any, (`)(`, `π-$` | `π-ζ`))]
              ord      = dir.ord
              akd     <- `<R`(ord).flatModifyFull { (poll, it) =>
                           it.offerers.dequeueOption match
                             case Some(((name, offerer), queue)) =>
                               it.copy(offerers = queue) -> offerer.complete(key -> dir).as(name -> None)
                             case _ =>
                               val queue = it.takers.enqueue(taker)
                               it.copy(takers = queue) -> poll(taker.get).map(() -> Some(_))
                         }
              n_label <- akd match
                           case (_, Some((name, (keyʹ, dirʹ: `π-$`)))) =>
                             for
                               label <- stm.commit {
                                 for
                                   _     <- `1`.acquire
                                   node  <- `][`.get.map(_.keys.find(_.contains(key)).get)
                                   nodeʹ <- `][`.get.map(_.keys.find(_.contains(keyʹ)).get)
                                   _     <- check(node, nodeʹ, dir, dirʹ).flatMap(stm.check(_))
                                   label <- `}{`(key, snapshot)
                                   _     <- `1`.release
                                 yield
                                   label
                               }
                               _     <- deferred.complete(label._2)
                             yield
                               name -> label
                           case (name, _) =>
                             for
                               label <- stm.commit { `}{`(key, false) }
                               snaps <- deferred.get
                             yield
                               name -> (label._1 -> snaps)
            yield
              n_label

          def apply[T](key: `)(`, dir: `π-$`)
                      (deferred: Deferred[IO, String], snapshot: Boolean)
                      (code: T => IO[T])
                      (using `][`: `][`, `1`: TSemaphore)
                      (`<R`: >*<): IO[(Any, (String, String))] =
            for
              taker   <- Deferred[IO, (Any, (`)(`, `π-$` | `π-ζ`))]
              ord      = dir.ord
              akd     <- `<R`(ord).flatModifyFull { (poll, it) =>
                           it.offerers.dequeueOption match
                             case Some(((name, offerer), queue)) =>
                               it.copy(offerers = queue) -> offerer.complete(key -> dir).as(name -> None)
                             case _ =>
                               val queue = it.takers.enqueue(taker)
                               it.copy(takers = queue) -> poll(taker.get).map(() -> Some(_))
                         }
              n_label <- akd match
                           case (_, Some((name, (keyʹ, dirʹ: `π-$`)))) =>
                             for
                               label <- stm.commit {
                                 for
                                   _     <- `1`.acquire
                                   node  <- `][`.get.map(_.keys.find(_.contains(key)).get)
                                   nodeʹ <- `][`.get.map(_.keys.find(_.contains(keyʹ)).get)
                                   _     <- check(node, nodeʹ, dir, dirʹ).flatMap(stm.check(_))
                                   label <- `}{`(key, snapshot)
                                   _     <- `1`.release
                                 yield
                                   label
                               }
                               _     <- deferred.complete(label._2)
                             yield
                               name -> label
                           case (name, _) =>
                             for
                               label <- stm.commit { `}{`(key, false) }
                               snaps <- deferred.get
                             yield
                               name -> (label._1 -> snaps)
              (name,
               label)  = n_label
              name    <- (code andThen exec)(name.asInstanceOf[T])
            yield
              name -> label

        object ζ:

          private def remove(node: `)*(`, tree: `}{`)
                            (using `][`: `][`): Txn[Unit] =
            val `}{`(_, root, _, siblings) = tree
            `][`.modify { m =>
                          val rtree = m(root)
                          siblings.foldLeft {
                            m + (root -> rtree.copy(children = siblings))
                          } { (m, sibling) =>
                            val tree @ `}{`(_, _, _, siblings) = m(sibling)
                            m + (sibling -> tree.copy(siblings = siblings - node))
                          }
                        }

          private def insert(node: `)*(`, root: `)*(`)
                            (using `][`: `][`): Txn[Unit] =
            for
              _ <- `][`.modify { m =>
                                 val tree = m(root)
                                 tree.children.foldLeft(m) { (m, child) =>
                                   val tree @ `}{`(_, _, _, siblings) = m(child)
                                   m + (child -> tree.copy(siblings = siblings + node))
                                 }
                               }
              _ <- `][`.modify { m =>
                                 val ntree = m(node)
                                 val rtree @ `}{`(_, _, children, _) = m(root)
                                 m + (root -> rtree.copy(children = children + node))
                                   + (node -> ntree.copy(root = root, siblings = children))
                               }
            yield
              ()

          private def update(temp: `}{`, root: `)*(`, join: `)*(`)
                            (using `][`: `][`): Txn[Unit] =
            `][`.modify { m =>
                          val tree @ `}{`(_, _, children, _) = m(temp.root)
                          temp.siblings.foldLeft {
                            m + (temp.root -> tree.copy(children = children - root + join))
                          } { (m, sibling) =>
                            val tree @ `}{`(_, _, _, siblings) = m(sibling)
                            m + (sibling -> tree.copy(siblings = siblings - root + join))
                          }
                        }

          private def merge(tree: `}{`, join: `)*(`)
                           (using `][`: `][`): Txn[Unit] =
            for
              _ <- `][`.modify { tree.children.foldLeft(_) { (m, node) =>
                                  val tree = m(node)
                                  m + (node -> tree.copy(root = join))
                                 }
                               }
              _ <- `][`.modify { m =>
                                 val temp @ `}{`(_, _, children, _) = m(join)
                                 tree.children.foldLeft {
                                   m + (join -> temp.copy(children = children ++ tree.children))
                                 } { (m, node) =>
                                   val tree = m(node)
                                   m + (node -> tree.copy(siblings = tree.siblings ++ children))
                                 }
                               }
            yield
              ()

          private def apply(node: `)*(`, nodeʹ: `)*(`, cap: `π-ζ`)
                           (using `][`: `][`): Txn[Unit] =
            cap match
              case `π-enter` | `π-exit` =>
                for
                  m            <- `][`.get
                  (root, tree)  = cap match
                                    case `π-enter` =>
                                      (nodeʹ, m(node))
                                    case `π-exit` =>
                                      (m(nodeʹ).root, m(node))
                  _            <- remove(node, tree)
                  _            <- insert(node, root)
                yield
                  ()

              case `π-merge+` =>
                for
                  m    <- `][`.get
                  tree  = m(nodeʹ)
                  _    <- remove(nodeʹ, tree)
                  m    <- `][`.get
                  temp  =  m(node)
                  join  = node ++ nodeʹ
                  _    <- `][`.modify { _ - node - nodeʹ + (join -> temp) }
                  _    <- update(temp, node, join)
                  _    <- merge(tree, join)
                yield
                  ()

          object > :

            def apply(key: `)(`, cap: `π-ζ`)
                     (deferred: Deferred[IO, String], snapshot: Boolean)
                     (using `][`: `][`, `1`: TSemaphore)
                     (`>R`: >*<): IO[(String, String)] =
              for
                offerer <- Deferred[IO, (`)(`, `π-$` | `π-ζ`)]
                ord      = cap.ord
                kc      <- `>R`(ord).flatModifyFull { (poll, it) =>
                             it.takers.dequeueOption match
                               case Some((taker, queue)) =>
                                 it.copy(takers = queue) -> taker.complete(() -> (key -> cap)).as(None)
                               case _ =>
                                 val queue = it.offerers.enqueue(() -> offerer)
                                 it.copy(offerers = queue) -> poll(offerer.get).map(Some(_))
                           }
                label   <- kc match
                             case Some((keyʹ, capʹ: `π-ζ`)) =>
                               for
                                 label <- stm.commit {
                                   for
                                     _     <- `1`.acquire
                                     node  <- `][`.get.map(_.keys.find(_.contains(key)).get)
                                     nodeʹ <- `][`.get.map(_.keys.find(_.contains(keyʹ)).get)
                                     _     <- check(node, nodeʹ, cap, capʹ).flatMap(stm.check(_))
                                     _     <- ζ(nodeʹ, node, capʹ)
                                     label <- `}{`(key, snapshot)
                                     _     <- `1`.release
                                   yield
                                     label
                                 }
                                 _     <- deferred.complete(label._2)
                               yield
                                 label
                             case _ =>
                               for
                                 label <- stm.commit { `}{`(key, false) }
                                 snaps <- deferred.get
                               yield
                                 label._1 -> snaps
              yield
                label

            def apply(key: `)(`, cap: `π-ζ`)
                     (deferred: Deferred[IO, String], snapshot: Boolean)
                     (code: => IO[Any])
                     (using `][`: `][`, `1`: TSemaphore)
                     (`>R`: >*<): IO[(String, String)] =
              for
                offerer <- Deferred[IO, (`)(`, `π-$` | `π-ζ`)]
                ord      = cap.ord
                kc      <- `>R`(ord).flatModifyFull { (poll, it) =>
                             it.takers.dequeueOption match
                               case Some((taker, queue)) =>
                                 it.copy(takers = queue) -> taker.complete(() -> (key -> cap)).as(None)
                               case _ =>
                                 val queue = it.offerers.enqueue(() -> offerer)
                                 it.copy(offerers = queue) -> poll(offerer.get).map(Some(_))
                           }
                label   <- kc match
                             case Some((keyʹ, capʹ: `π-ζ`)) =>
                               for
                                 label <- stm.commit {
                                   for
                                     _     <- `1`.acquire
                                     node  <- `][`.get.map(_.keys.find(_.contains(key)).get)
                                     nodeʹ <- `][`.get.map(_.keys.find(_.contains(keyʹ)).get)
                                     _     <- check(node, nodeʹ, cap, capʹ).flatMap(stm.check(_))
                                     _     <- ζ(nodeʹ, node, capʹ)
                                     label <- `}{`(key, snapshot)
                                     _     <- `1`.release
                                   yield
                                     label
                                 }
                                 _     <- deferred.complete(label._2)
                               yield
                                 label
                             case _ =>
                               for
                                 label <- stm.commit { `}{`(key, false) }
                                 snaps <- deferred.get
                               yield
                                 label._1 -> snaps
              yield
                label

          object < :

            def apply(key: `)(`, cap: `π-ζ`)
                     (deferred: Deferred[IO, String], snapshot: Boolean)
                     (using `][`: `][`, `1`: TSemaphore)
                     (`<R`: >*<): IO[(String, String)] =
              for
                taker <- Deferred[IO, (Any, (`)(`, `π-$` | `π-ζ`))]
                ord    = cap.ord
                ukc   <- `<R`(ord).flatModifyFull { (poll, it) =>
                           it.offerers.dequeueOption match
                             case Some(((_, offerer), queue)) =>
                               it.copy(offerers = queue) -> offerer.complete(key -> cap).as(None)
                             case _ =>
                               val queue = it.takers.enqueue(taker)
                               it.copy(takers = queue) -> poll(taker.get).map(Some(_))
                         }
                label <- ukc match
                           case Some((_, (keyʹ, capʹ: `π-ζ`))) =>
                             for
                               label <- stm.commit {
                                 for
                                   _     <- `1`.acquire
                                   node  <- `][`.get.map(_.keys.find(_.contains(key)).get)
                                   nodeʹ <- `][`.get.map(_.keys.find(_.contains(keyʹ)).get)
                                   _     <- check(node, nodeʹ, cap, capʹ).flatMap(stm.check(_))
                                   _     <- ζ(node, nodeʹ, cap)
                                   label <- `}{`(key, snapshot)
                                   _     <- `1`.release
                                 yield
                                   label
                               }
                               _     <- deferred.complete(label._2)
                             yield
                               label
                           case _ =>
                             for
                               label <- stm.commit { `}{`(key, false) }
                               snaps <- deferred.get
                             yield
                               label._1 -> snaps
              yield
                label

            def apply(key: `)(`, cap: `π-ζ`)
                     (deferred: Deferred[IO, String], snapshot: Boolean)
                     (code: => IO[Any])
                     (using `][`: `][`, `1`: TSemaphore)
                     (`<R`: >*<): IO[(String, String)] =
              for
                taker <- Deferred[IO, (Any, (`)(`, `π-$` | `π-ζ`))]
                ord    = cap.ord
                ukc   <- `<R`(ord).flatModifyFull { (poll, it) =>
                           it.offerers.dequeueOption match
                             case Some(((_, offerer), queue)) =>
                               it.copy(offerers = queue) -> offerer.complete(key -> cap).as(None)
                             case _ =>
                               val queue = it.takers.enqueue(taker)
                               it.copy(takers = queue) -> poll(taker.get).map(Some(_))
                         }
                label <- ukc match
                           case Some((_, (keyʹ, capʹ: `π-ζ`))) =>
                             for
                               label <- stm.commit {
                                 for
                                   _     <- `1`.acquire
                                   node  <- `][`.get.map(_.keys.find(_.contains(key)).get)
                                   nodeʹ <- `][`.get.map(_.keys.find(_.contains(keyʹ)).get)
                                   _     <- check(node, nodeʹ, cap, capʹ).flatMap(stm.check(_))
                                   _     <- ζ(node, nodeʹ, cap)
                                   label <- `}{`(key, snapshot)
                                   _     <- `1`.release
                                 yield
                                   label
                               }
                               _     <- deferred.complete(label._2)
                             yield
                               label
                           case _ =>
                             for
                               label <- stm.commit { `}{`(key, false) }
                               snaps <- deferred.get
                             yield
                               label._1 -> snaps
                _     <- code
              yield
                label
