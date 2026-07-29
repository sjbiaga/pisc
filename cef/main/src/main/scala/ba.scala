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

package object sΠ:

  import _root_.scala.collection.immutable.{ Map, Set }

  import _root_.cats.instances.list.*
  import _root_.cats.syntax.applicative.*
  import _root_.cats.syntax.traverse.*

  import _root_.cats.effect.{ IO, IOLocal, Deferred }
  import _root_.cats.effect.kernel.Outcome.Succeeded
  import _root_.cats.effect.std.{ CyclicBarrier, Supervisor, UUIDGen }

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


  sealed abstract trait Ordʹ { val ord: Int }
  sealed abstract trait Ord(val ord: Int) extends Ordʹ

  val `π-τ` = new Ord(-1) {}

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
    `π-exclude`(`π-elvis`(key)).whenA(`π-elvis`.contains(key))


  /**
    * restriction aka new name
    */
  object ν:

    def map[B](f: `()` => B): IO[B] = flatMap(f andThen IO.pure)
    def flatMap[B](f: `()` => IO[B]): IO[B] =
      f {
        Map(
          `π-local`.ord  -> new {},
          `π-s2s`.ord    -> new {},
          `π-p2c`.ord    -> new {},
          `π-accept`.ord -> new {},
          `π-expel`.ord  -> new {},
          `π-merge+`.ord -> new {}
        )
      }


  /**
    * silent transition
    */
  object τ:

    def apply(rate: Rate)(key: String, `)(`: IOLocal[`)(`])
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[java.lang.Double] =
      for
        _        <- exclude(key)
        deferred <- Deferred[IO, Option[<>]]
        `)(`     <- `)(`.get
        _        <- /.offer(^ -> key -> (deferred -> (`)(` -> `π-τ`, (new {}, None, rate))))
        opt      <- deferred.get
        delay    <- ( if opt eq None
                      then
                        IO.pure(null: java.lang.Double)
                      else
                        val (delay, b, f, _) = opt.get
                        for
                          _ <- b.await
                          _ <- f.join
                        yield
                          java.lang.Double(delay)
                    )
      yield
        delay

  /**
    * prefix
    */
  final implicit class `()`(private val name: Any) extends AnyVal:

    private def map = `()`[Map[Int, {}]]

    def ====(that: `()`) =
      try
        this.map eq that.map
      catch _ =>
        this.name == that.name

    inline def `()`[T]: T = name.asInstanceOf[T]
    inline def `()`(using DummyImplicit): `()` = this

    /**
      * capability prefix
      */
    def apply(rate: Rate)(key: String, `)(`: IOLocal[`)(`], cap: `π-ζ`)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[java.lang.Double] =
      for
        _        <- exclude(key)
        deferred <- Deferred[IO, Option[<>]]
        polarity  = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
        `)(`     <- `)(`.get
        _        <- /.offer(^ -> key -> (deferred -> (`)(` -> cap, (map(cap.ord), Some(if polarity then Right(null) else Left(())), rate))))
        opt      <- deferred.get
        delay    <- ( if opt eq None
                      then
                        IO.pure(null: java.lang.Double)
                      else
                        val (delay, b, f, _) = opt.get
                        for
                          _ <- b.await
                          _ <- f.join
                        yield
                          java.lang.Double(delay)
                    )
      yield
        delay

    /**
      * capability prefix
      */
    def apply(rate: Rate)(key: String, `)(`: IOLocal[`)(`], cap: `π-ζ`)(code: => IO[Any])
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[java.lang.Double] =
      for
        _        <- exclude(key)
        deferred <- Deferred[IO, Option[<>]]
        polarity  = cap == `π-enter` || cap == `π-exit` || cap == `π-merge+`
        `)(`     <- `)(`.get
        _        <- /.offer(^ -> key -> (deferred -> (`)(` -> cap, (map(cap.ord), Some(if polarity then Right(null) else Left(())), rate))))
        opt      <- deferred.get
        delay    <- ( if opt eq None
                      then
                        IO.pure(null: java.lang.Double)
                      else
                        val (delay, b, f, _) = opt.get
                        for
                          _ <- b.await
                          _ <- f.join
                          _ <- exec(code)
                        yield
                          java.lang.Double(delay)
                    )
      yield
        delay

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S](_f: false)(rate: Rate, value: => S)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)
                (using DummyImplicit)
                (using %, /)
                (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): IO[java.lang.Double] =
      value match
        case it: `()` =>
          apply(rate, it)(key, `)(`, dir)
        case _ =>
          apply(false)(rate, IO.delay(value))(key, `)(`, dir)

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S](_t: true)(rate: Rate, value: => S)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: => IO[Any])
                (using DummyImplicit)
                (using %, /)
                (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): IO[java.lang.Double] =
      value match
        case it: `()` =>
          apply(rate, it)(key, `)(`, dir)(code)
        case _ =>
          apply(true)(rate, IO.delay(value))(key, `)(`, dir)(code)

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S](_f: false)(rate: Rate, value: => IO[S])(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)
                (using %, /)
                (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): IO[java.lang.Double] =
      value.map(new `()`(_)).flatMap(apply(rate, _)(key, `)(`, dir))

    /**
      * variable negative prefix i.e. variable output
      */
    def apply[S](_t: true)(rate: Rate, value: => IO[S])(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: => IO[Any])
                (using %, /)
                (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): IO[java.lang.Double] =
      value.map(new `()`(_)).flatMap(apply(rate, _)(key, `)(`, dir)(code))

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[java.lang.Double] =
      for
        _        <- exclude(key)
        deferred <- Deferred[IO, Option[<>]]
        `)(`     <- `)(`.get
        _        <- /.offer(^ -> key -> (deferred -> (`)(` -> dir, (map(dir.ord), Some(Left(())), rate))))
        opt      <- deferred.get
        delay    <- ( if opt eq None
                      then
                        IO.pure(null: java.lang.Double)
                      else
                        val (delay, b, f, i) = opt.get
                        for
                          _ <- i.set(value)
                          _ <- b.await
                          _ <- f.join
                        yield
                          java.lang.Double(delay)
                    )
      yield
        delay

    /**
      * negative prefix i.e. output
      */
    def apply(rate: Rate, value: `()`)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: => IO[Any])
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[java.lang.Double] =
      for
        _        <- exclude(key)
        deferred <- Deferred[IO, Option[<>]]
        `)(`     <- `)(`.get
        _        <- /.offer(^ -> key -> (deferred -> (`)(` -> dir, (map(dir.ord), Some(Left(())), rate))))
        opt      <- deferred.get
        delay    <- ( if opt eq None
                      then
                        IO.pure(null: java.lang.Double)
                      else
                        val (delay, b, f, i) = opt.get
                        for
                          _ <- i.set(value)
                          _ <- b.await
                          _ <- f.join
                          _ <- exec(code)
                        yield
                          java.lang.Double(delay)
                    )
      yield
        delay

    /**
      * positive prefix i.e. input
      */
    def apply(rate: Rate)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)
             (using % : %, / : /)
             (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                       ^ : String): IO[(`()`, java.lang.Double)] =
      for
        _        <- exclude(key)
        deferred <- Deferred[IO, Option[<>]]
        result   <- IO.ref[`()`](sΠ.`()`.`null`)
        `)(`     <- `)(`.get
        _        <- /.offer(^ -> key -> (deferred -> (`)(` -> dir, (map(dir.ord), Some(Right(result)), rate))))
        opt      <- deferred.get
        (name,
         delay)  <- ( if opt eq None
                      then
                        IO.pure((sΠ.`()`.`null`) -> (null: java.lang.Double))
                      else
                        val (delay, b, f, _) = opt.get
                        for
                          _    <- b.await
                          _    <- f.join
                          name <- result.get
                        yield
                          name -> java.lang.Double(delay)
                    )
      yield
        name -> delay

    /**
      * positive prefix i.e. input
      */
    def apply[T](rate: Rate)(key: String, `)(`: IOLocal[`)(`], dir: `π-$`)(code: T => IO[T])
                (using % : %, / : /)
                (implicit `π-elvis`: `Π-Map`[String, `Π-Set`[String]],
                          ^ : String): IO[(`()`, java.lang.Double)] =
      for
        _        <- exclude(key)
        deferred <- Deferred[IO, Option[<>]]
        result   <- IO.ref[`()`](sΠ.`()`.`null`)
        `)(`     <- `)(`.get
        _        <- /.offer(^ -> key -> (deferred -> (`)(` -> dir, (map(dir.ord), Some(Right(result)), rate))))
        opt      <- deferred.get
        (name,
         delay)  <- ( if opt eq None
                      then
                        IO.pure((null: Any) -> (null: java.lang.Double))
                      else
                        val (delay, b, f, _) = opt.get
                        for
                          _    <- b.await
                          _    <- f.join
                          name <- result.get.map(_.name).flatMap { case it: T => (code andThen exec)(it) }
                        yield
                          name -> java.lang.Double(delay)
                    )
      yield
        new `()`(name) -> delay

    override def toString: String = if name == null then "null" else name.toString


  private object `()`:

    val `null` = new `()`(null)


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
      * Type of ambients' trees.
      */
    type `][` = TVar[Map[`)*(`, `}{`]]

    object `][`:
      def apply(): IO[(IOLocal[`)(`], `][`, TSemaphore)] =
        for
          uuid <- `)(`()
          root  = Set(uuid)
          lo   <- IOLocal[`)(`](uuid)
          map   = Map(root -> `}{`(None, null, Set.empty, Set.empty))
          tree <- stm.commit { TVar.of[Map[`)*(`, `}{`]](map) }
          sem  <- stm.commit { TSemaphore.make(1) }
        yield
          (lo, tree, sem)

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

        def apply(key: `)(`, dir: `π-$`, keyʹ: `)(`, dirʹ: `π-$`)
                 (using `][`: `][`, `1`: TSemaphore): IO[Unit] =
          stm.commit {
            for
              _     <- `1`.acquire
              node  <- `][`.get.map(_.keys.find(_.contains(key)).get)
              nodeʹ <- `][`.get.map(_.keys.find(_.contains(keyʹ)).get)
              _     <- check(node, nodeʹ, dir, dirʹ).flatMap(stm.check(_))
              _     <- `1`.release
            yield
              ()
          }

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

        @annotation.tailrec
        private def apply(node: `)*(`, nodeʹ: `)*(`, cap: `π-ζ`, capʹ: `π-ζ`)
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

            case _ =>
              apply(nodeʹ, node, capʹ, cap)

        def apply(key: `)(`, cap: `π-ζ`, keyʹ: `)(`, capʹ: `π-ζ`)
                 (using `][`: `][`, `1`: TSemaphore): IO[Unit] =
          stm.commit {
            for
              _     <- `1`.acquire
              node  <- `][`.get.map(_.keys.find(_.contains(key)).get)
              nodeʹ <- `][`.get.map(_.keys.find(_.contains(keyʹ)).get)
              _     <- check(node, nodeʹ, cap, capʹ).flatMap(stm.check(_))
              _     <- this(node, nodeʹ, cap, capʹ)
              _     <- `1`.release
            yield
              ()
          }
