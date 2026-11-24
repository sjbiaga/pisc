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

import _root_.scala.collection.immutable.{ List, Map, Set }
import _root_.scala.concurrent.duration.*
import _root_.scala.Option.{ unless, when }

import _root_.cats.instances.list.*
import _root_.cats.syntax.applicative.*
import _root_.cats.syntax.flatMap.*
import _root_.cats.syntax.parallel.*
import _root_.cats.syntax.traverse.*

import _root_.cats.effect.{ IO, Deferred, ExitCode, FiberIO, Ref, Resource }
import _root_.cats.effect.kernel.Outcome.Succeeded
import _root_.cats.effect.std.{ AtomicCell, CyclicBarrier, Queue, Semaphore }

import `Π-dump`.*
import `Π-stats`.*


package object `Π-loop`:

  import sΠ.{ `Π-Map`, `Π-Set`, `{}`, `()` }

  type <> = (Double, CyclicBarrier[IO], FiberIO[Unit], Ref[IO, `()`])

  type ++ = (Deferred[IO, Option[<>]], Ref[IO, Deferred[IO, Option[<>]]])
  type + = (++, (`{}`, Option[Either[Unit, Ref[IO, `()`]]], Rate))

  type % = AtomicCell[IO, Map[String, Int | (Boolean, +)]]

  type !! = Deferred[IO, Unit]
  type ! = Deferred[IO, ExitCode]

  type &| = Ref[IO, Long]

  type / = Queue[IO, ((String, String), +)]

  type \ = IO[Unit] => IO[Unit]

  type ++++ = (Double, (`{}`, Ref[IO, `()`]), (++, ++))
  type ** = Queue[IO, List[((String, String), ++++)]]

  type * = Semaphore[IO]

  type ^ = Resource[IO, Unit]


  final case class `Π-Parameters`(address: String,
                                  parallelism: Int,
                                  threshold: Int,
                                  timeout: Int,
                                  exit: Boolean,
                                  plugins: Set[String],
                                  causal: Boolean = false)

  final case class Feedback(paramsRD: Ref[IO, Deferred[IO, `Π-Parameters`]],
                            paramsR: Ref[IO, `Π-Parameters`],
                            tracesR: Ref[IO, Boolean],
                            keyByR: Ref[IO, Boolean],
                            lastR: Ref[IO, (Long, Double)],
                            pauseRD_stopR_exitRD: AtomicCell[IO, ((Deferred[IO, Unit], Boolean), Deferred[IO, Unit])],
                            initR: Ref[IO, Boolean],
                            doneR: Ref[IO, Boolean])


  def `π-enable`(enabled: `Π-Set`[String])
                (using % : %): IO[Unit] =
    %.update(enabled.foldLeft(_) { (m, key) =>
                                    val n = if m.contains(key)
                                            then m(key).asInstanceOf[Int]
                                            else 0
                                    m + (key -> (n + 1))
                                 }
    )

  private def enable(key: String)
                    (using %)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    val (_, spell) = `π-wand`
    `π-enable`(spell(key))


  private def unblock(map: Map[String, Int | (Boolean, +)], key: String)
                     (implicit ^ : String): IO[Unit] =
    map(^ + key).asInstanceOf[(Boolean, +)]._2._1._1.complete(None).void.whenA(map.contains(^ + key))

  private def `π-discard`(map: Map[String, Int | (Boolean, +)], discarded: `Π-Set`[String])
                         (implicit ^ : String): IO[Set[String]] =
    discarded.toList.traverse(unblock(map, _)).as(discarded.map(^ + _))

  private def discard(key: String, map: Map[String, Int | (Boolean, +)])
                     (using String)
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Set[String]] =
    val (trick, _) = `π-wand`
    if trick.contains(key)
    then
      `π-discard`(map, trick(key))
    else
      IO.pure(Set.empty)


  def peek(_plugins: Set[String])
          (using % : %, ** : **)
          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    %.evalModify { m =>
      val it =
        if m.exists(_._2.isInstanceOf[Int])
        then Map.empty
        else m
             .filter(_._2.asInstanceOf[(Boolean, +)]._1)
             .map(_ -> _.asInstanceOf[(Boolean, +)]._2._2)
             .toMap
      if it.isEmpty
      then
        IO.pure(m -> ())
      else
        ∥(it)(`π-wand`._1)().map {
          case (key1, key2, ein, delay) =>
            val (dc1, _) = m(key1).asInstanceOf[(Boolean, +)]._2
            val (dc2, _) = m(key2).asInstanceOf[(Boolean, +)]._2
            (key1, key2) -> (delay, ein, (dc1, dc2))
        } match
          case Nil =>
            IO.pure(m -> ())
          case nel =>
            nel.traverse {
              case ((key1, key2), _) =>
                val k1 = key1.substring(36)
                val k2 = key2.substring(36)
                val  ^ = key1.substring(0, 36)
                val ^^ = key2.substring(0, 36)
                val ((d1, c1), _) = m(key1).asInstanceOf[(Boolean, +)]._2
                val ((d2, c2), _) = m(key2).asInstanceOf[(Boolean, +)]._2
                for
                  s1 <- d1.tryGet.map(_ eq None).flatMap { if _ then discard(k1, m)(using  ^) else IO.pure(Set.empty) }
                  s2 <- if k1 == k2
                        then IO.pure(Set.empty)
                        else d2.tryGet.map(_ eq None).flatMap { if _ then discard(k2, m)(using ^^) else IO.pure(Set.empty) }
                yield
                  (s1 ++ s2 ++ when(c1 eq null)(key1) ++ when(c2 eq null)(key2))
               -> (Nil ++ unless(c1 eq null)(key1) ++ unless(k1 == k2)(unless(c2 eq null)(key2)).flatten)
            }.map(_.foldRight(m) {
                    case ((ks, ls), map) =>
                      ls.map { key => key -> (false, map(key).asInstanceOf[(Boolean, +)]._2) }
                        .foldLeft(ks.foldLeft(map)(_ - _))(_ + _)
                  }
            ).flatMap(mʹ => **.offer(nel).map(mʹ -> _))
    }


  private def canExit(using % : %)
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Boolean] =
    def exit(m: Map[String, Int | (Boolean, +)]): Boolean =
      val (trick, _) = `π-wand`
      !m.exists(_._2.isInstanceOf[Int])
      && m.forall {
           case (_, (true, (_, (_, None, _)))) => false
           case (key1, (true, (_, (e1, Some(p1), _)))) =>
             val ^ = key1.substring(0, 36)
             val k1 = key1.substring(36)
             !m.exists {
               case (key2, (true, (_, (e2, Some(p2), _)))) if (e1 eq e2) && p1.isLeft == p2.isRight =>
                 val ^^ = key2.substring(0, 36)
                 ^ != ^^
                 || {
                   val k2 = key2.substring(36)
                   !trick.contains(k1) || !trick(k1).contains(k2)
                 }
               case _ => false
             }
           case _ => true
         }
    %.modify { m => m -> exit(m) }

  private def doExit(using % : %): IO[Unit] =
    %.get.flatMap { m =>
      val ks = m.filterNot(_._2.isInstanceOf[Int]).keys.toList
      ks.traverse(m(_).asInstanceOf[(Boolean, +)]._2._1._1.complete(None)) >>
      ks.traverse(m(_).asInstanceOf[(Boolean, +)]._2._1._2 match { case null => IO.unit
                                                                   case it => it.get.flatMap(_.complete(None).void) }).void
    }


  def loopʹ(parameters: `Π-Parameters`, started: Ref[IO, Long], batch: Ref[IO, Long], feedback: Feedback)
           (using % : %, &| : &|, - : -, * : *, ** : **, ^ : ^)
           (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    for
      _ <- batch.set(0L) >> *.acquire.guaranteeCase { case Succeeded(_) => batch.update(_ + 1) case _ => IO.unit }.replicateA_(parameters.threshold).timeoutTo(parameters.timeout.microseconds, IO.unit)
      m  =
        for
          nel <- **.tryTake.map(_.getOrElse(Nil))
          stp <-
            if nel.isEmpty
            then
              (started.get product batch.get).map(_ + _).flatMap {
                case 0L =>
                  canExit.ifM(doExit >> IO.pure(None), IO.pure(Some(false)))
                case _  =>
                  feedback.pauseRD_stopR_exitRD.get.map(_._1._2).ifM(doExit >> IO.pure(Some(true)), IO.pure(Some(false)))
              }
            else
              (feedback.pauseRD_stopR_exitRD.get.map(_._1._2) product Semaphore[IO](parameters.parallelism)).flatMap { (stop, sem) =>
                val fun = { (f: (((String, String), ++++)) => IO[Unit]) => if parameters.parallelism == 1 then nel.traverse(f) else nel.parTraverse(f) }
                fun { case ((key1, key2), (delay, (eth, in), ((d1, c1), (d2, c2)))) =>
                        val k1 = key1.substring(36)
                        val k2 = key2.substring(36)
                        if stop
                        then
                          for
                            _ <- d1.complete(None)
                            _ <- d2.complete(None).unlessA(k1 == k2)
                            _ <- c1.get.flatMap(_.complete(None)).unlessA(c1 eq null)
                            _ <- c2.get.flatMap(_.complete(None)).unlessA(c2 eq null).unlessA(k1 == k2)
                          yield
                            ()
                        else
                          for
                            cb <- CyclicBarrier[IO](if k1 == k2 then 2 else 3)
                            _  <- sem.acquire
                            _  <- eth.fold(IO.unit)(_.acquire)
                            _  <- started.update(_ + 1)
                            fb <- ( for
                                      _ <- cb.await
                                      _ <- enable(k1)
                                      _ <- enable(k2).unlessA(k1 == k2)
                                      _ <- eth.fold(IO.unit)(_.release)
                                      _ <- sem.release
                                      _ <- started.update(_ - 1)
                                    yield
                                      ()
                                  ).start
                            _  <- d1.complete(Some((delay, cb, fb, in)))
                            _  <- d2.complete(Some((delay, cb, fb, in))).unlessA(k1 == k2)
                            _  <- c1.get.flatMap(_.complete(Some((delay, cb, fb, in)))).unlessA(c1 eq null)
                            _  <- c2.get.flatMap(_.complete(Some((delay, cb, fb, in)))).unlessA(c2 eq null).unlessA(k1 == k2)
                          yield
                            ()
                    }
              } >> IO.pure(Some(false))
        yield
          stp
      s <- ^.use(_ => (*.available >>= *.acquireN) >> peek(parameters.plugins) >> m)
      _ <- s.fold(IO.never)(if _ then stopʹ(parameters)
                            else IO.cede >> loopʹ(parameters, started, batch, feedback))
    yield
      ()

  private def stopʹ(parameters: `Π-Parameters`)
                   (using % : %, * : *, ** : **, ^ : ^)
                   (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    for
      _ <- *.acquireN(parameters.threshold).timeoutTo(parameters.timeout.microseconds, IO.unit)
      m  =
        for
          nel  <- **.tryTake.map(_.getOrElse(Nil))
          _    <- nel.traverse {
                    case ((key1, key2), (_, _, ((d1, c1), (d2, c2)))) =>
                      val k1 = key1.substring(36)
                      val k2 = key2.substring(36)
                      for
                        _ <- d1.complete(None)
                        _ <- d2.complete(None).unlessA(k1 == k2)
                        _ <- c1.get.flatMap(_.complete(None)).unlessA(c1 eq null)
                        _ <- c2.get.flatMap(_.complete(None)).unlessA(c2 eq null).unlessA(k1 == k2)
                      yield
                        ()
                  }
        yield
          ()
      _ <- ^.use(_ => (*.available >>= *.acquireN) >> peek(parameters.plugins) >> m)
      _ <- IO.cede >> stopʹ(parameters)
    yield
      ()

  def loop0(parameters: `Π-Parameters`, started: Ref[IO, Long], feedback: Feedback)
           (using % : %, &| : &|, - : -, * : *, ** : **)
           (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    for
      nel  <- **.take
      opt  <- ( if nel.isEmpty
                then
                  (started.get product **.size.map(_.toLong)).map(_ + _).flatMap {
                    case 0L =>
                      **.take.map(Some(_)).timeoutTo(parameters.timeout.microseconds, IO.none)
                    case _  =>
                      IO.pure(Some(nel))
                  }
                else
                  IO.pure(Some(nel))
              )
      _    <- ( if opt eq None
                then
                  canExit.ifM(doExit >> IO.never, IO.unit)
                else if opt.get.isEmpty
                then
                  feedback.pauseRD_stopR_exitRD.get.map(_._1._2).ifM(doExit >> stop0, IO.unit)
                else
                  val nel = opt.get
                  (feedback.pauseRD_stopR_exitRD.get.map(_._1._2) product Semaphore[IO](parameters.parallelism)).flatMap { (stop, sem) =>
                    val fun = { (f: (((String, String), ++++)) => IO[Unit]) => if parameters.parallelism == 1 then nel.traverse(f) else nel.parTraverse(f) }
                    fun { case ((key1, key2), (delay, (eth, in), ((d1, c1), (d2, c2)))) =>
                            val k1 = key1.substring(36)
                            val k2 = key2.substring(36)
                            if stop
                            then
                              for
                                _ <- **.offer(Nil)
                                _ <- d1.complete(None)
                                _ <- d2.complete(None).unlessA(k1 == k2)
                                _ <- c1.get.flatMap(_.complete(None)).unlessA(c1 eq null)
                                _ <- c2.get.flatMap(_.complete(None)).unlessA(c2 eq null).unlessA(k1 == k2)
                              yield
                                ()
                            else
                              for
                                cb <- CyclicBarrier[IO](if k1 == k2 then 2 else 3)
                                _  <- sem.acquire
                                _  <- eth.fold(IO.unit)(_.acquire)
                                _  <- started.update(_ + 1)
                                fb <- ( for
                                          _ <- cb.await
                                          _ <- enable(k1)
                                          _ <- enable(k2).unlessA(k1 == k2)
                                          _ <- eth.fold(IO.unit)(_.release)
                                          _ <- sem.release
                                          _ <- started.updateAndGet(_ - 1).map(_ == 0) >>= **.offer(Nil).whenA
                                        yield
                                          ()
                                      ).start
                                _  <- d1.complete(Some((delay, cb, fb, in)))
                                _  <- d2.complete(Some((delay, cb, fb, in))).unlessA(k1 == k2)
                                _  <- c1.get.flatMap(_.complete(Some((delay, cb, fb, in)))).unlessA(c1 eq null)
                                _  <- c2.get.flatMap(_.complete(Some((delay, cb, fb, in)))).unlessA(c2 eq null).unlessA(k1 == k2)
                              yield
                                ()
                        }
                  } >> IO.pure(true)
              )
      _    <- IO.cede >> loop0(parameters, started, feedback)
    yield
      ()

  private def stop0(using ** : **): IO[Unit] =
    for
      nel  <- **.take
      _    <- nel.traverse {
                case ((key1, key2), (_, _, ((d1, c1), (d2, c2)))) =>
                  val k1 = key1.substring(36)
                  val k2 = key2.substring(36)
                  for
                    _ <- d1.complete(None)
                    _ <- d2.complete(None).unlessA(k1 == k2)
                    _ <- c1.get.flatMap(_.complete(None)).unlessA(c1 eq null)
                    _ <- c2.get.flatMap(_.complete(None)).unlessA(c2 eq null).unlessA(k1 == k2)
                  yield
                    ()
              }
      _    <- IO.cede >> stop0
    yield
      ()

  def poll(using % : %, / : /, \ : \): IO[Unit] =
    /.take.flatMap {
      case ((^ @ (_: String), key), it @ ((d, _), _)) =>
        d.tryGet.map(_ eq None).flatMap {
          if _
          then
            \(
              %.update { m =>
                         val n = m(key).asInstanceOf[Int] - 1
                         ( if n == 0
                           then
                             m - key
                           else
                             m + (key -> n)
                         ) + (^ + key -> (true, it))
              }
            )
          else
            %.update(_ + (^ + key -> (false, it)))
        } >> IO.cede >> poll
    }
