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

  import _root_.java.util.UUID

  import _root_.scala.collection.immutable.{ List, Map, Set }

  import _root_.cats.instances.list.*
  import _root_.cats.syntax.traverse.*
  import _root_.cats.effect.{ IO, IOLocal, Clock, Deferred, Ref }
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.{ Semaphore, Supervisor }

  import `Π-loop`.{ <>, -, %, /, \ }
  import `Π-magic`.><
  export `Π-magic`.>*<
  import `Π-stats`.Rate

  import `π-$`.*, `π-ζ`.*


  /**
    * Wraps ambient keys ([[UUID]]).
    *
    * @param value [[UUID]]
    */
  final class `)(`(private val value: Any) extends AnyVal:
    override def toString: String = value.toString

  object `)(`:
    /**
      * Initial ambient unique key.
      */
    def apply(): `)(` = new `)(`(UUID.randomUUID)

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
             (implicit `][`: `][`, `1`: Semaphore[IO]): IO[Unit] =
      for
        _        <- `1`.acquire
        root     <- `)(`.get
        uuid      = sΠ.`)(`()
        node      = Set(uuid)
        _        <- `)(`.set(uuid)
        children <- `][`.modify { m =>
                                  val key = m.keys.find(_.contains(root)).get
                                  val tree @ `}{`(_, _, children, _) = m(key)
                                  (m + (node -> `}{`(label, key, Set.empty, children))
                                     + (key  -> tree.copy(children = children + node))) -> children
                                }
        _        <- children.toList.traverse(apply_(node, _)).void
        _        <- `1`.release
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
                                   var id = 0L
                                   var tree = Map[`)*(`, Long](root -> id)
                                   def make(root: `)*(`): Unit =
                                     for
                                       node <- m(root).children
                                     yield
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
        _   <- IO.unit
        uuid = `)(`()
        root = Set(uuid)
        lo  <- IOLocal[`)(`](uuid)
        map  = Map(root -> `}{`(None, null, Set.empty, Set.empty))
        tr  <- Ref.of[IO, Map[`)*(`, `}{`]](map)
      yield
        (lo, tr)

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
    `π-exclude`(Set.from(enabled)) >> \()

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
             (using `][`: `][`, `1`: Semaphore[IO])
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[Double] =
      for
        _            <- exclude(key)
        deferred     <- Deferred[IO, Option[(Double, (-, -), Boolean, Deferred[IO, (String, (String, String))])]]
        dummy_ref    <- Ref.of[IO, Map[Int, ><]](Map.empty)
        _            <- `1`.acquire
        (s_label, _) <- `}{`(`)(`)
        _            <- `1`.release
        timestamp    <- Clock[IO].monotonic.map(_.toNanos)
        _            <- /.offer(^ -> key -> (deferred -> (timestamp, (dummy_ref -> -1, None, rate))))
        opt          <- deferred.get
        _            <- if opt eq None then IO.canceled else IO.unit
        (delay,
        (b, b2),
        s, d)         = opt.get
        _            <- b.await
        e_label      <- `}{`(`)(`, s)
        _            <- d.complete(s_label -> e_label)
        _            <- b2.await
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
    def apply(rate: Rate)(key: String, `)(`: IOLocal[`)(`], cap: `π-ζ`)
             (using % : %, / : /)
             (using `][`: `][`, `1`: Semaphore[IO])
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[Double] =
      for
        _            <- exclude(key)
        deferred     <- Deferred[IO, Option[(Double, (-, -), Boolean, Deferred[IO, (String, (String, String))])]]
        polarity      = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
        _            <- `1`.acquire
        (s_label, _) <- `}{`(`)(`)
        _            <- `1`.release
        timestamp    <- Clock[IO].monotonic.map(_.toNanos)
        _            <- /.offer(^ -> key -> (deferred -> (timestamp, (ref -> cap.ord, Some(polarity), rate))))
        delay        <- if polarity
                        then ><.ζ.<(key, s_label, `)(`, cap)(deferred)(ref)
                        else ><.ζ.>(key, s_label, `)(`, cap)(deferred)(ref)
      yield
        delay

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)
             (using % : %, / : /)
             (using `][`: `][`, `1`: Semaphore[IO])
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[Double] =
      for
        _            <- exclude(key)
        deferred     <- Deferred[IO, Option[(Double, (-, -), Boolean, Deferred[IO, (String, (String, String))])]]
        _            <- `1`.acquire
        (s_label, _) <- `}{`(`)(`)
        _            <- `1`.release
        timestamp    <- Clock[IO].monotonic.map(_.toNanos)
        _            <- /.offer(^ -> key -> (deferred -> (timestamp, (ref -> dir.ord, Some(false), rate))))
        delay        <- ><.π(key, value.name, s_label, `)(`, dir)(deferred)(ref)
      yield
        delay

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: => IO[Any])
             (using % : %, / : /)
             (using `][`: `][`, `1`: Semaphore[IO])
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[Double] =
      for
        _            <- exclude(key)
        deferred     <- Deferred[IO, Option[(Double, (-, -), Boolean, Deferred[IO, (String, (String, String))])]]
        _            <- `1`.acquire
        (s_label, _) <- `}{`(`)(`)
        _            <- `1`.release
        timestamp    <- Clock[IO].monotonic.map(_.toNanos)
        _            <- /.offer(^ -> key -> (deferred -> (timestamp, (ref -> dir.ord, Some(false), rate))))
        delay        <- ><.π(key, value.name, s_label, `)(`, dir)(code)(deferred)(ref)
      yield
        delay

    /**
      * positive prefix i.e. input
      */
    def apply(rate: Rate)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)
             (using % : %, / : /)
             (using `][`: `][`, `1`: Semaphore[IO])
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[(`()`, Double)] =
      for
        _            <- exclude(key)
        deferred     <- Deferred[IO, Option[(Double, (-, -), Boolean, Deferred[IO, (String, (String, String))])]]
        _            <- `1`.acquire
        (s_label, _) <- `}{`(`)(`)
        _            <- `1`.release
        timestamp    <- Clock[IO].monotonic.map(_.toNanos)
        _            <- /.offer(^ -> key -> (deferred -> (timestamp, (ref -> dir.ord, Some(true), rate))))
        (r, delay)   <- ><.π(key, s_label, `)(`, dir)(deferred)(ref)
      yield
        new `()`(r) -> delay

    /**
      * positive prefix i.e. input
      */
    def apply[T](rate: Rate)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: T => IO[T])
                (using % : %, / : /)
                (using `][`: `][`, `1`: Semaphore[IO])
                (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): IO[(`()`, Double)] =
      for
        _            <- exclude(key)
        deferred     <- Deferred[IO, Option[(Double, (-, -), Boolean, Deferred[IO, (String, (String, String))])]]
        _            <- `1`.acquire
        (s_label, _) <- `}{`(`)(`)
        _            <- `1`.release
        timestamp    <- Clock[IO].monotonic.map(_.toNanos)
        _            <- /.offer(^ -> key -> (deferred -> (timestamp, (ref -> dir.ord, Some(true), rate))))
        (r, delay)   <- ><.π(key, s_label, `)(`, dir)(code)(deferred)(ref)
      yield
        new `()`(r) -> delay

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

    final case class ><(taker: Option[((Deferred[IO, (Any, Boolean)], `)*(`), `π-$` | `π-ζ`)],
                        offerer: Option[(((Any, Deferred[IO, Boolean]), `)*(`), `π-$` | `π-ζ`)])

    type >*< = Ref[IO, Map[Int, ><]]

    object >< :

      inline def apply(): >< = ><(None, None)

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

        def apply(key: String, name: Any, s_label: String, `)(`: IOLocal[`)(`], dir: `π-$`)
                 (deferred: <>)
                 (using `][`)
                 (`>R`: >*<): IO[Double] =
          for
            opt     <- deferred.get
            _       <- if opt eq None then IO.canceled else IO.unit
            (delay,
            (b, b2),
            s, d)    = opt.get
            ord      = dir.ord
            offerer <- Deferred[IO, Boolean]
            _       <- IO.uncancelable { poll =>
                         `][`(`)(`).flatMap { node =>
                           `>R`.modify { m =>
                             m(ord) match
                               case it @ ><(Some(((taker, nodeʹ), dirʹ: `π-$`)), _) =>
                                 (m + (ord -> it.copy(taker = None))) ->
                                 check(node, nodeʹ, dir, dirʹ).flatMap { ok => taker.complete(name -> ok).as(ok) }
                               case it =>
                                 val cleanup = `>R`.update { m => m + (ord -> it.copy(offerer = None)) }
                                 (m + (ord -> it.copy(offerer = Some(name -> offerer -> node -> dir)))) ->
                                 poll(offerer.get).onCancel(cleanup)
                           }.flatten
                         }.flatTap { _ =>
                           for
                             _       <- b.await
                             e_label <- `}{`(`)(`, s)
                             _       <- d.complete(s_label -> e_label)
                             _       <- b2.await
                           yield
                             ()
                         }
                       }.ifM(IO.unit, IO.never)
          yield
            delay

        def apply(key: String, name: Any, s_label: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: => IO[Any])
                 (deferred: <>)
                 (using `][`)
                 (`>R`: >*<): IO[Double] =
          for
            opt     <- deferred.get
            _       <- if opt eq None then IO.canceled else IO.unit
            (delay,
            (b, b2),
            s, d)    = opt.get
            ord      = dir.ord
            offerer <- Deferred[IO, Boolean]
            _       <- IO.uncancelable { poll =>
                         `][`(`)(`).flatMap { node =>
                           `>R`.modify { m =>
                             m(ord) match
                               case it @ ><(Some(((taker, nodeʹ), dirʹ: `π-$`)), _) =>
                                 (m + (ord -> it.copy(taker = None))) ->
                                 check(node, nodeʹ, dir, dirʹ).flatMap { ok => taker.complete(name -> ok).as(ok) }
                               case it =>
                                 val cleanup = `>R`.update { m => m + (ord -> it.copy(offerer = None)) }
                                 (m + (ord -> it.copy(offerer = Some(name -> offerer -> node -> dir)))) ->
                                 poll(offerer.get).onCancel(cleanup)
                           }.flatten <* exec(code)
                         }.flatTap { _ =>
                           for
                             _       <- b.await
                             e_label <- `}{`(`)(`, s)
                             _       <- d.complete(s_label -> e_label)
                             _       <- b2.await
                           yield
                             ()
                         }
                       }.ifM(IO.unit, IO.never)
          yield
            delay

        def apply(key: String, s_label: String, `)(`: IOLocal[`)(`], dir: `π-$`)
                 (deferred: <>)
                 (using `][`)
                 (`<R`: >*<): IO[(Any, Double)] =
          for
            opt     <- deferred.get
            _       <- if opt eq None then IO.canceled else IO.unit
            (delay,
            (b, b2),
            s, d)    = opt.get
            ord      = dir.ord
            taker   <- Deferred[IO, (Any, Boolean)]
            name    <- IO.uncancelable { poll =>
                         `][`(`)(`).flatMap { node =>
                           `<R`.modify { m =>
                             m(ord) match
                               case it @ ><(_, Some((((name, offerer), nodeʹ), dirʹ: `π-$`))) =>
                                 (m + (ord -> it.copy(offerer = None))) ->
                                 check(node, nodeʹ, dir, dirʹ).flatMap { ok => offerer.complete(ok).as(name -> ok) }
                               case it =>
                                 val cleanup = `<R`.update { m => m + (ord -> it.copy(taker = None)) }
                                 (m + (ord -> it.copy(taker = Some(taker -> node -> dir)))) ->
                                 poll(taker.get).onCancel(cleanup)
                           }.flatten
                         }.flatTap { _ =>
                           for
                             _       <- b.await
                             e_label <- `}{`(`)(`, s)
                             _       <- d.complete(s_label -> e_label)
                             _       <- b2.await
                           yield
                             ()
                         }
                       }.flatMap { (name, ok) => if ok then IO.pure(name) else IO.never }
          yield
            name -> delay

        def apply[T](key: String, s_label: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: T => IO[T])
                    (deferred: <>)
                    (using `][`)
                    (`<R`: >*<): IO[(Any, Double)] =
          for
            opt     <- deferred.get
            _       <- if opt eq None then IO.canceled else IO.unit
            (delay,
            (b, b2),
            s, d)    = opt.get
            ord      = dir.ord
            taker   <- Deferred[IO, (Any, Boolean)]
            name    <- IO.uncancelable { poll =>
                         `][`(`)(`).flatMap { node =>
                           `<R`.modify { m =>
                             m(ord) match
                               case it @ ><(_, Some((((name, offerer), nodeʹ), dirʹ: `π-$`))) =>
                                 (m + (ord -> it.copy(offerer = None))) ->
                                 check(node, nodeʹ, dir, dirʹ).flatMap { ok => offerer.complete(ok).as(name -> ok) }
                               case it =>
                                 val cleanup = `<R`.update { m => m + (ord -> it.copy(taker = None)) }
                                 (m + (ord -> it.copy(taker = Some(taker -> node -> dir)))) ->
                                 poll(taker.get).onCancel(cleanup)
                           }.flatten
                            .flatMap { case (it: T, ok) => (code andThen exec)(it).map(_ -> ok) }
                         }.flatTap { _ =>
                           for
                             _       <- b.await
                             e_label <- `}{`(`)(`, s)
                             _       <- d.complete(s_label -> e_label)
                             _       <- b2.await
                           yield
                             ()
                         }
                       }.flatMap { (name, ok) => if ok then IO.pure(name) else IO.never }
          yield
            name -> delay

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
                         (using `][`: `][`): IO[Boolean] =
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
                    true

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
                    true

        object > :

          def apply(key: String, s_label: String, `)(`: IOLocal[`)(`], cap: `π-ζ`)
                   (deferred: <>)
                   (using `][`)
                   (`>R`: >*<): IO[Double] =
            for
              opt     <- deferred.get
              _       <- if opt eq None then IO.canceled else IO.unit
              (delay,
              (b, b2),
              s, d)    = opt.get
              ord      = cap.ord
              offerer <- Deferred[IO, Boolean]
              _       <- IO.uncancelable { poll =>
                           `][`(`)(`).flatMap { node =>
                             `>R`.modify { m =>
                               m(ord) match
                                 case it @ ><(Some(((taker, nodeʹ), capʹ: `π-ζ`)), _) =>
                                   (m + (ord -> it.copy(taker = None))) ->
                                   check(node, nodeʹ, cap, capʹ).flatMap { ok => taker.complete(node -> ok).as(ok) }
                                 case it =>
                                   val cleanup = `>R`.update { m => m + (ord -> it.copy(offerer = None)) }
                                   (m + (ord -> it.copy(offerer = Some(() -> offerer -> node -> cap)))) ->
                                   poll(offerer.get).onCancel(cleanup)
                             }.flatten
                           }.flatTap { _ =>
                             for
                               _       <- b.await
                               e_label <- `}{`(`)(`, s)
                               _       <- d.complete(s_label -> e_label)
                               _       <- b2.await
                             yield
                               ()
                           }
                         }.ifM(IO.unit, IO.never)
            yield
              delay

        object < :

          def apply(key: String, s_label: String, `)(`: IOLocal[`)(`], cap: `π-ζ`)
                   (deferred: <>)
                   (using `][`)
                   (`<R`: >*<): IO[Double] =
            for
              opt     <- deferred.get
              _       <- if opt eq None then IO.canceled else IO.unit
              (delay,
              (b, b2),
              s, d)    = opt.get
              ord      = cap.ord
              taker   <- Deferred[IO, (Any, Boolean)]
              _       <- IO.uncancelable { poll =>
                           `][`(`)(`).flatMap { node =>
                             `<R`.modify { m =>
                               m(ord) match
                                 case it @ ><(_, Some((((_, offerer), nodeʹ), capʹ: `π-ζ`))) =>
                                   (m + (ord -> it.copy(offerer = None))) ->
                                   check(node, nodeʹ, cap, capʹ).flatMap { ok => offerer.complete(ok).as(nodeʹ -> ok) }
                                 case it =>
                                   val cleanup = `<R`.update { m => m + (ord -> it.copy(taker = None)) }
                                   (m + (ord -> it.copy(taker = Some(taker -> node -> cap)))) ->
                                   poll(taker.get).onCancel(cleanup)
                             }.flatten
                              .flatMap { (nodeʹ, ok) => if ok then ζ(node, nodeʹ, cap) else IO.pure(ok) }
                           }.flatTap { _ =>
                             for
                               _       <- b.await
                               e_label <- `}{`(`)(`, s)
                               _       <- d.complete(s_label -> e_label)
                               _       <- b2.await
                             yield
                               ()
                           }
                         }.ifM(IO.unit, IO.never)
            yield
              delay
