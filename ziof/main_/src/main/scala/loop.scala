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

import Double.NaN

import _root_.cats.effect.std.Semaphore
import _root_.zio.interop.catz.generic.*
import _root_.zio.{ durationInt, Cause, Clock, Exit, ExitCode, Fiber, Promise, Queue, Ref, Semaphore => SemaphoreZIO, UIO, ZIO }
import _root_.zio.concurrent.CyclicBarrier
import _root_.zio.stm.TPriorityQueue

import `Π-dump`.*
import `Π-stats`.*


package object `Π-loop`:

  import sΠ.{ `Π-Map`, `Π-Set`, `()` }


  type <> = (Double, CyclicBarrier, Fiber[Nothing, Unit], Ref[`()`])

  type ++ = (Promise[Nothing, Option[<>]], Long)
  type + = (++, ({}, Option[Either[Unit, Ref[`()`]]], Rate))

  type % = Ref.Synchronized[Map[String, Int | +]]

  type ! = Promise[Nothing, ExitCode]

  type & = Ref[(Long, Double)]

  type / = Queue[((String, String), +)]

  type \ = UIO[Unit] => UIO[Unit]

  type ++++ = ((Double, Double), Ref[`()`], (++, ++))
  type ** = TPriorityQueue[(Int, List[List[((String, String), ++++)]])]

  type * = Semaphore[UIO]

  type ^ = SemaphoreZIO


  final case class `Π-Parameters`(address: String,
                                  parallelism: Int,
                                  threshold: Int,
                                  timeout: Int,
                                  exit: Boolean)

  final case class Feedback(paramsRP: Ref[Promise[Nothing, `Π-Parameters`]],
                            paramsR: Ref[`Π-Parameters`],
                            tracesR: Ref[Boolean],
                            lastR: Ref[(Long, Double)],
                            pauseRP_stopR_exitRP: Ref.Synchronized[((Promise[Nothing, Unit], Boolean), Promise[Nothing, Unit])],
                            doneR: Ref[Boolean])


  given Ordering[(Int, List[List[((String, String), ++++)]])] = Ordering.fromLessThan(_._1 < _._1)


  def `π-enable`(enabled: `Π-Set`[String])
                (using % : %): UIO[Unit] =
    %.update(enabled.foldLeft(_) { (m, key) =>
                                    val n = if m.contains(key)
                                            then m(key).asInstanceOf[Int]
                                            else 0
                                    m + (key -> (n + 1))
                                 }
    )

  private def enable(key: String)
                    (using %)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): UIO[Unit] =
    val (_, spell) = `π-wand`
    `π-enable`(spell(key))


  private def unblock(map: Map[String, Int | +], key: String)
                     (implicit ^ : String): UIO[Unit] =
    ZIO.when(map.contains(^ + key))(map(^ + key).asInstanceOf[+]._1._1.succeed(None)).unit

  private def `π-discard`(map: Map[String, Int | +], discarded: `Π-Set`[String])
                         (implicit ^ : String): UIO[Set[String]] =
    ZIO.collectAllParDiscard(discarded.toList.map(unblock(map, _))).as(discarded.map(^ + _))

  private def discard(key: String, map: Map[String, Int | +])
                     (using String)
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): UIO[Set[String]] =
    val (trick, _) = `π-wand`
    if trick.contains(key)
    then
      `π-discard`(map, trick(key))
    else
      ZIO.succeed(Set.empty)


  def peek(using % : %, ** : **)
          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): UIO[Unit] =
    %.modifyZIO { m =>
      val it =
        if m.exists(_._2.isInstanceOf[Int])
        then Map.empty
        else m
             .map(_ -> _.asInstanceOf[+]._2)
             .toMap
      if it.isEmpty
      then
        (**.size.flatMap(size => **.offer(size -> Nil))).commit.map(_ -> m)
      else
        val nel = ∥(it)(`π-wand`._1)()
        val nelʹ = nel.map {
          _.map {
            case (key1, key2, in, dd) =>
              val (ps1, _) = m(key1).asInstanceOf[+]
              val (ps2, _) = m(key2).asInstanceOf[+]
              (key1, key2) -> (dd, in, (ps1, ps2))
          }
        }
        ZIO.collectAll {
          nel.flatten.map {
            case (key1, key2, _, _) =>
              val k1 = key1.substring(36)
              val k2 = key2.substring(36)
              val  ^ = key1.substring(0, 36)
              val ^^ = key2.substring(0, 36)
              for
                s1 <- discard(k1, m)(using  ^)
                s2 <- if k1 == k2
                      then ZIO.succeed(Set.empty)
                      else discard(k2, m)(using ^^)
              yield
                s1 ++ s2 + key1 + key2
          }
        }.map(_.foldRight(m)(_.foldLeft(_)(_ - _)))
         .flatMap(mʹ => (**.size.flatMap(size => **.offer(size -> nelʹ))).commit.map(_ -> mʹ))
    }


  private def canExit(using % : %)
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): UIO[Boolean] =
    def exit(m: Map[String, Int | +]): Boolean =
      val (trick, _) = `π-wand`
      !m.exists(_._2.isInstanceOf[Int])
      && m.forall {
           case (key1, (_, (e1, Some(p1), _))) =>
             val ^ = key1.substring(0, 36)
             !m.exists {
               case (key2, (_, (e2, Some(p2), _))) if (e1 eq e2) && p1.isLeft == p2.isRight =>
                 val ^^ = key2.substring(0, 36)
                 ^ != ^^
                 || {
                   val k1 = key1.substring(36)
                   val k2 = key2.substring(36)
                   !trick.contains(k1) || !trick(k1).contains(k2)
                 }
               case _ => false
             }
           case _ => false
         }
    %.modify { m => exit(m) -> m }

  def loopʹ(parameters: `Π-Parameters`, started: Ref[Long], batch: Ref[Long], restore: ZIO.InterruptibilityRestorer, feedback: Feedback)
           (using % : %, ! : !, & : &, - : -, * : *, ** : **, ^ : ^)
           (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): UIO[Unit] =
    for
      _ <- batch.set(0L) *> restore(*.acquire.onExit { case Exit.Success(_) => batch.update(_ + 1) case _ => ZIO.unit }.repeatN(parameters.threshold-1)).timeout(parameters.timeout.microseconds).exit
      m  =
        for
          (_, nel) <- **.take.commit
          l        <-
            if nel.isEmpty
            then
              (started.get <*> batch.get).map(_ + _).flatMap {
                case 0L =>
                  canExit.flatMap(if _ then feedback.doneR.set(true) *> feedback.pauseRP_stopR_exitRP.get.flatMap(_._2.await) *> -.offer(None) *> ZIO.succeed(false) else ZIO.succeed(true))
                case _  =>
                  ZIO.succeed(true)
              }
            else
              Semaphore[UIO](parameters.parallelism).flatMap { sem =>
                ZIO.collectAll {
                  nel.map { nel =>
                    ZIO.collectAllParDiscard {
                      nel.map { case ((key1, key2), ((delay, duration), in, ((p1, s1), (p2, s2)))) =>
                                  val k1 = key1.substring(36)
                                  val k2 = key2.substring(36)
                                  ZIO.uninterruptible {
                                    for
                                      cb <- CyclicBarrier.make(if k1 == k2 then 2 else 3)
                                      fb  = ( for
                                                _  <- cb.await.exit
                                                _  <- enable(k1)
                                                _  <- enable(k2).unless(k1 == k2)
                                                nc <- duration match { case 0.0 | NaN => &.updateAndGet { (no, cl) => (no + 1, cl) }
                                                                       case _         => &.updateAndGet { (no, cl) => (no + 1, cl + delay) }  }
                                                now <- Clock.nanoTime
                                                _  <- feedback.lastR.set(now -> nc._2)
                                                _  <- -.offer(Some((nc, ((s1, s2), now), (k1, k2), (delay, duration)))).whenZIO(feedback.tracesR.get)
                                                _  <- sem.release
                                                _  <- started.update(_ - 1)
                                              yield
                                                ()
                                            ).fork
                                      st <- feedback.pauseRP_stopR_exitRP.get.map(_._1._2)
                                      _  <- ( if st
                                              then
                                                for
                                                  _ <- **.offer(-1 -> Nil).commit
                                                  _ <- p1.succeed(None)
                                                  _ <- p2.succeed(None).unless(k1 == k2)
                                                yield
                                                  ()
                                              else
                                                for
                                                  _  <- sem.acquire
                                                  _  <- started.update(_ + 1)
                                                  fb <- fb
                                                  _  <- p1.succeed(Some((delay, cb, fb, in)))
                                                  _  <- p2.succeed(Some((delay, cb, fb, in))).unless(k1 == k2)
                                                yield
                                                  ()
                                            )
                                    yield
                                      ()
                                  }
                              }
                    }
                  }
                }
              } *> ZIO.succeed(true)
        yield
          l
      l <- ^.withPermit(*.available.flatMap(*.acquireN) *> peek *> m)
      _ <- feedback.pauseRP_stopR_exitRP.get.flatMap(_._1._1.await)
      _ <- feedback.paramsRP.get.flatMap(_.isDone).flatMap {
             if _
             then
               feedback.paramsRP.get.flatMap {
                 _.await.flatMap { params =>
                   feedback.paramsR.set(params) *>
                   Promise.make[Nothing, `Π-Parameters`].flatMap(feedback.paramsRP.set) *>
                   loopʹ(params, started, batch, restore, feedback)
                 }
               }
             else
               loopʹ(parameters, started, batch, restore, feedback)
           }.when(l)
    yield
      ()

  def loop0(parameters: `Π-Parameters`, started: Ref[Long], restore: ZIO.InterruptibilityRestorer, feedback: Feedback)
           (using % : %, ! : !, & : &, - : -, * : *, ** : **)
           (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): UIO[Unit] =
    for
      (_, nel) <- **.take.commit
      l        <-
        if nel.isEmpty
        then
          (started.get <*> **.size.commit.map(_.toLong)).map(_ + _).flatMap {
            case 0L =>
              restore(**.take.commit).timeout(parameters.timeout.microseconds).exit.flatMap {
                case Exit.Success(Some((_, nel)))        =>
                  **.offer(-1 -> nel).commit *> ZIO.succeed(true)
                case Exit.Success(_)
                   | Exit.Failure(Cause.Interrupt(_, _)) =>
                  canExit.flatMap(if _ then feedback.doneR.set(true) *> feedback.pauseRP_stopR_exitRP.get.flatMap(_._2.await) *> -.offer(None) *> ZIO.succeed(false) else ZIO.succeed(true))
                case Exit.Failure(cause)                 =>
                  ZIO.failCause(cause)
              }
            case _  =>
              ZIO.succeed(true)
          }
        else
          Semaphore[UIO](parameters.parallelism).flatMap { sem =>
            ZIO.collectAll {
              nel.map { nel =>
                ZIO.collectAllParDiscard {
                  nel.map { case ((key1, key2), ((delay, duration), in, ((p1, s1), (p2, s2)))) =>
                              val k1 = key1.substring(36)
                              val k2 = key2.substring(36)
                              ZIO.uninterruptible {
                                for
                                  cb <- CyclicBarrier.make(if k1 == k2 then 2 else 3)
                                  fb  = ( for
                                            _  <- cb.await.exit
                                            _  <- enable(k1)
                                            _  <- enable(k2).unless(k1 == k2)
                                            nc <- duration match { case 0.0 | NaN => &.updateAndGet { (no, cl) => (no + 1, cl) }
                                                                   case _         => &.updateAndGet { (no, cl) => (no + 1, cl + delay) }  }
                                            now <- Clock.nanoTime
                                            _  <- feedback.lastR.set(now -> nc._2)
                                            _  <- -.offer(Some((nc, ((s1, s2), now), (k1, k2), (delay, duration)))).whenZIO(feedback.tracesR.get)
                                            _  <- sem.release
                                            _  <- started.updateAndGet(_ - 1).map(_ == 0).flatMap(peek.when(_))
                                          yield
                                            ()
                                        ).fork
                                  st <- feedback.pauseRP_stopR_exitRP.get.map(_._1._2)
                                  _  <- ( if st
                                          then
                                            for
                                              _ <- **.offer(-1 -> Nil).commit
                                              _ <- p1.succeed(None)
                                              _ <- p2.succeed(None).unless(k1 == k2)
                                            yield
                                              ()
                                          else
                                            for
                                              _  <- sem.acquire
                                              _  <- started.update(_ + 1)
                                              fb <- fb
                                              _  <- p1.succeed(Some((delay, cb, fb, in)))
                                              _  <- p2.succeed(Some((delay, cb, fb, in))).unless(k1 == k2)
                                            yield
                                              ()
                                        )
                                yield
                                  ()
                              }
                          }
                }
              }
            }
          } *> ZIO.succeed(true)
      _        <- feedback.pauseRP_stopR_exitRP.get.flatMap(_._1._1.await)
      _        <- feedback.paramsRP.get.flatMap(_.isDone).flatMap {
                    if _
                    then
                      feedback.paramsRP.get.flatMap {
                        _.await.flatMap { params =>
                          feedback.paramsR.set(params) *>
                          Promise.make[Nothing, `Π-Parameters`].flatMap(feedback.paramsRP.set) *>
                          loop0(params, started, restore, feedback)
                        }
                      }
                    else
                      loop0(parameters, started, restore, feedback)
                  }.when(l)
    yield
      ()

  def poll(using % : %, / : /, \ : \): UIO[Unit] =
    for
      h <- /.take
      _ <- if h eq null
           then ZIO.unit
           else
             val ((_, key), it) = h
             val ^ = h._1._1
             \( %.update { m =>
                           val n = m(key).asInstanceOf[Int] - 1
                           ( if n == 0
                             then
                               m - key
                             else
                               m + (key -> n)
                           ) + (^ + key -> it)
                         }
             ) *> poll
    yield
      ()
