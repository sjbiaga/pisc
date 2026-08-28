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

import _root_.cats.Order
import _root_.cats.instances.list.*
import _root_.cats.syntax.applicative.*
import _root_.cats.syntax.flatMap.*
import _root_.cats.syntax.parallel.*
import _root_.cats.syntax.traverse.*

import _root_.cats.effect.{ IO, Deferred, ExitCode, FiberIO, Ref, Resource }
import _root_.cats.effect.kernel.Outcome.Succeeded
import _root_.cats.effect.std.{ AtomicCell, CyclicBarrier, PQueue, Queue, Semaphore }

import `Π-dump`.*
import `Π-stats`.*


package object `Π-loop`:

  private val barsx = "pisc.bioambients.replications.exitcode.ignore"


  import sΠ.{ `Π-Map`, `Π-Set`, Ordʹ, `π-$`, `π-ζ`, `)(`, `()` }

  type <> = (Double, CyclicBarrier[IO], FiberIO[Unit], Ref[IO, `()`])

  type ++ = ((Deferred[IO, Option[<>]], Ref[IO, Deferred[IO, Option[<>]]]), (`)(`, Ordʹ))
  type + = (++, ({}, Option[Either[Unit, Ref[IO, `()`]]], Rate))

  type % = AtomicCell[IO, Map[String, Int | (Boolean, +)]]

  type ! = Deferred[IO, ExitCode]

  type & = Ref[IO, (Long, Double)]

  type / = Queue[IO, ((String, String), +)]

  type \ = IO[Unit] => IO[Unit]

  type ++++ = (Double, Ref[IO, `()`], (++, ++))
  type ** = PQueue[IO, (Int, List[List[((String, String), ++++)]])]

  type * = Semaphore[IO]

  type ^ = Resource[IO, Unit]


  final case class `Π-Parameters`(address: String,
                                  parallelism: Int,
                                  threshold: Int,
                                  timeout: Int,
                                  exit: Boolean,
                                  snapshot: Boolean)

  final case class Feedback(paramsRD: Ref[IO, Deferred[IO, `Π-Parameters`]],
                            paramsR: Ref[IO, `Π-Parameters`],
                            tracesR: Ref[IO, Boolean],
                            lastR: Ref[IO, (Long, Double)],
                            pauseRD_stopR_exitRD: AtomicCell[IO, ((Deferred[IO, Unit], Boolean), Deferred[IO, Unit])],
                            doneR: Ref[IO, Boolean])


  given Order[(Int, List[List[((String, String), ++++)]])] = Order.fromLessThan(_._1 < _._1)


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
    map(^ + key).asInstanceOf[(Boolean, +)]._2._1._1._1.complete(None).void.whenA(map.contains(^ + key))

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


  def peek(using % : %, ** : **)
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
        **.size.flatMap(size => **.offer(size -> Nil)).map(m -> _)
      else
        val nel = ∥(it)(`π-wand`._1)()
        val nelʹ = nel.map {
          _.map {
            case (key1, key2, in, (delay, _)) =>
              val (dcko1, _) = m(key1).asInstanceOf[(Boolean, +)]._2
              val (dcko2, _) = m(key2).asInstanceOf[(Boolean, +)]._2
              (key1, key2) -> (delay, in, (dcko1, dcko2))
          }
        }
        nel.flatten.traverse {
          case (key1, key2, _, _) =>
            val k1 = key1.substring(36)
            val k2 = key2.substring(36)
            val  ^ = key1.substring(0, 36)
            val ^^ = key2.substring(0, 36)
            val (((d1, c1), _), _) = m(key1).asInstanceOf[(Boolean, +)]._2
            val (((d2, c2), _), _) = m(key2).asInstanceOf[(Boolean, +)]._2
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
        ).flatMap(mʹ => **.size.flatMap(size => **.offer(size -> nelʹ)).map(mʹ -> _))
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
             !m.exists {
               case (key2, (true, (_, (e2, Some(p2), _)))) if (e1 eq e2) && p1.isLeft == p2.isRight =>
                 val ^^ = key2.substring(0, 36)
                 ^ != ^^
                 || {
                   val k1 = key1.substring(36)
                   val k2 = key2.substring(36)
                   !trick.contains(k1) || !trick(k1).contains(k2)
                 }
               case _ => false
             }
           case _ => true
         }
    %.modify { m => m -> exit(m) }

  private def doExit(using % : %, ! : !): IO[Unit] =
    %.get.flatMap { m =>
      val ks = m.keys.toList
      val ec =
        if ks.isEmpty
        then
          ExitCode.Success
        else
          if !sys.BooleanProp.keyExists(barsx).value
          && ks.forall(_.charAt(36) == '!')
          then ExitCode.Success
          else ExitCode.Error
      ks.traverse(m(_).asInstanceOf[(Boolean, +)]._2._1._1._1.complete(None)) >>
      ks.traverse(m(_).asInstanceOf[(Boolean, +)]._2._1._1._2 match { case null => IO.unit
                                                                      case it => it.get.flatMap(_.complete(None).void) }) >>
      !.complete(ec).void
    }

  def loopʹ(parameters: `Π-Parameters`, started: Ref[IO, Long], batch: Ref[IO, Long], feedback: Feedback, `}{`: sΠ.`}{`)
           (using % : %, ! : !, & : &, - : -, * : *, ** : **, ^ : ^)
           (using `}{`.`][`, `}{`.stm.TSemaphore)
           (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    for
      _ <- batch.set(0L) >> *.acquire.guaranteeCase { case Succeeded(_) => batch.update(_ + 1) case _ => IO.unit }.replicateA_(parameters.threshold).timeoutTo(parameters.timeout.microseconds, IO.unit)
      m  =
        for
          (_, nel) <- **.take
          l        <-
            if nel.isEmpty
            then
              (started.get product batch.get).map(_ + _).flatMap {
                case 0L =>
                  canExit.ifM(doExit >> IO.pure(false), IO.pure(true))
                case _  =>
                  IO.pure(true)
              }
            else
              (feedback.pauseRD_stopR_exitRD.get.map(_._1._2) product Semaphore[IO](parameters.parallelism)).flatMap { (stop, sem) =>
                nel.traverse {
                  _.parTraverse { case ((key1, key2), (delay, in, (((d1, c1), (key, ord)), ((d2, c2), (keyʹ, ordʹ))))) =>
                                    val k1 = key1.substring(36)
                                    val k2 = key2.substring(36)
                                    for
                                      cb <- CyclicBarrier[IO](if k1 == k2 then 2 else 3)
                                      fb  = ( for
                                                _ <- { (ord, ordʹ) match
                                                         case (dir: `π-$`, dirʹ: `π-$`) =>
                                                           `}{`.><.π(key, dir, keyʹ, dirʹ)
                                                         case (cap: `π-ζ`, capʹ: `π-ζ`) =>
                                                           `}{`.><.ζ(key, cap, keyʹ, capʹ)
                                                     }.unlessA(k1 == k2)
                                                _ <- cb.await
                                                _ <- enable(k1)
                                                _ <- enable(k2).unlessA(k1 == k2)
                                                _ <- sem.release
                                                _ <- started.update(_ - 1)
                                              yield
                                                ()
                                            ).start
                                      _  <- ( if stop
                                              then
                                                for
                                                  _ <- **.offer(-1 -> Nil)
                                                  _ <- d1.complete(None)
                                                  _ <- d2.complete(None).unlessA(k1 == k2)
                                                  _ <- c1.get.flatMap(_.complete(None)).unlessA(c1 eq null)
                                                  _ <- c2.get.flatMap(_.complete(None)).unlessA(c2 eq null).unlessA(k1 == k2)
                                                yield
                                                  ()
                                              else
                                                for
                                                  _  <- sem.acquire
                                                  _  <- started.update(_ + 1)
                                                  fb <- fb
                                                  _  <- d1.complete(Some((delay, cb, fb, in)))
                                                  _  <- d2.complete(Some((delay, cb, fb, in))).unlessA(k1 == k2)
                                                  _  <- c1.get.flatMap(_.complete(Some((delay, cb, fb, in)))).unlessA(c1 eq null)
                                                  _  <- c2.get.flatMap(_.complete(Some((delay, cb, fb, in)))).unlessA(c2 eq null).unlessA(k1 == k2)
                                                yield
                                                  ()
                                            )
                                    yield
                                      ()
                                }
                }
              } >> IO.pure(true)
        yield
          l
      l <- ^.use(_ => (*.available >>= *.acquireN) >> peek >> m)
      _ <- IO.cede >> loopʹ(parameters, started, batch, feedback, `}{`).whenA(l)
    yield
      ()

  def loop0(parameters: `Π-Parameters`, started: Ref[IO, Long], feedback: Feedback, `}{`: sΠ.`}{`)
           (using % : %, ! : !, & : &, - : -, * : *, ** : **)
           (using `}{`.`][`, `}{`.stm.TSemaphore)
           (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    for
      (_, nel) <- **.take
      l        <-
        if nel.isEmpty
        then
          (started.get product **.size.map(_.toLong)).map(_ + _).flatMap {
            case 0L =>
              **.take.map(Some(_)).timeoutTo(parameters.timeout.microseconds, IO.none).flatMap {
                case Some((_, nel)) =>
                  **.offer(-1 -> nel) >> IO.pure(true)
                case _               =>
                  canExit.ifM(doExit >> IO.pure(false), IO.pure(true))
              }
            case _  =>
              IO.pure(true)
          }
        else
          (feedback.pauseRD_stopR_exitRD.get.map(_._1._2) product Semaphore[IO](parameters.parallelism)).flatMap { (stop, sem) =>
            nel.traverse {
              _.parTraverse { case ((key1, key2), (delay, in, (((d1, c1), (key, ord)), ((d2, c2), (keyʹ, ordʹ))))) =>
                                val k1 = key1.substring(36)
                                val k2 = key2.substring(36)
                                for
                                  cb <- CyclicBarrier[IO](if k1 == k2 then 2 else 3)
                                  fb  = ( for
                                            _ <- { (ord, ordʹ) match
                                                     case (dir: `π-$`, dirʹ: `π-$`) =>
                                                       `}{`.><.π(key, dir, keyʹ, dirʹ)
                                                     case (cap: `π-ζ`, capʹ: `π-ζ`) =>
                                                       `}{`.><.ζ(key, cap, keyʹ, capʹ)
                                                 }.unlessA(k1 == k2)
                                            _ <- cb.await
                                            _ <- enable(k1)
                                            _ <- enable(k2).unlessA(k1 == k2)
                                            _ <- sem.release
                                            _ <- started.updateAndGet(_ - 1).map(_ == 0) >>= peek.whenA
                                          yield
                                            ()
                                        ).start
                                  _  <- ( if stop
                                          then
                                            for
                                              _ <- **.offer(-1 -> Nil)
                                              _ <- d1.complete(None)
                                              _ <- d2.complete(None).unlessA(k1 == k2)
                                              _ <- c1.get.flatMap(_.complete(None)).unlessA(c1 eq null)
                                              _ <- c2.get.flatMap(_.complete(None)).unlessA(c2 eq null).unlessA(k1 == k2)
                                            yield
                                              ()
                                          else
                                            for
                                              _  <- sem.acquire
                                              _  <- started.update(_ + 1)
                                              fb <- fb
                                              _  <- d1.complete(Some((delay, cb, fb, in)))
                                              _  <- d2.complete(Some((delay, cb, fb, in))).unlessA(k1 == k2)
                                              _  <- c1.get.flatMap(_.complete(Some((delay, cb, fb, in)))).unlessA(c1 eq null)
                                              _  <- c2.get.flatMap(_.complete(Some((delay, cb, fb, in)))).unlessA(c2 eq null).unlessA(k1 == k2)
                                            yield
                                              ()
                                        )
                                yield
                                  ()
                            }
            }
          } >> IO.pure(true)
      _        <- IO.cede >> loop0(parameters, started, feedback, `}{`).whenA(l)
    yield
      ()

  def poll(using % : %, / : /, \ : \): IO[Unit] =
    /.take.flatMap {
      case ((^ @ (_: String), key), it @ (((d, _), _), _)) =>
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
