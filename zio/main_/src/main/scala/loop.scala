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
import _root_.scala.Option.{ unless, when }

import _root_.cats.effect.std.Semaphore
import _root_.zio.interop.catz.generic.*
import _root_.zio.{ durationInt, Cause, Clock, Exit, ExitCode, Fiber, Promise, Queue, Ref, Semaphore => SemaphoreZIO, UIO, ZIO }
import _root_.zio.concurrent.CyclicBarrier
import _root_.zio.stm.{ TPriorityQueue, TSemaphore }

import `Π-dump`.*
import `Π-stats`.*


package object `Π-loop`:

  import sΠ.{ `Π-Map`, `Π-Set`, Ordʹ, `π-$`, `π-ζ`, `)(`, `()`, `}{` }


  type <> = (Double, CyclicBarrier, Fiber[Nothing, Unit], Ref[`()`])

  type ++ = ((Promise[Nothing, Option[<>]], Ref[Promise[Nothing, Option[<>]]]), (`)(`, Ordʹ), Ref[Long])
  type + = (++, ({}, Option[Either[Unit, Ref[`()`]]], Rate))

  type % = Ref.Synchronized[Map[String, Int | (Boolean, +)]]

  type ! = Promise[Nothing, ExitCode]

  type &| = Ref[(Long, Double)]

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
                                  exit: Boolean,
                                  snapshot: Boolean)

  final case class Feedback(paramsRP: Ref[Promise[Nothing, `Π-Parameters`]],
                            paramsR: Ref[`Π-Parameters`],
                            tracesR: Ref[Boolean],
                            lastR: Ref[(Long, Double)],
                            pauseRP_stopR_exitRP: Ref.Synchronized[((Promise[Nothing, Unit], Boolean), Promise[Nothing, Unit])],
                            doneR: Ref[Boolean])


  given Ordering[(Int, List[List[((String, String), ++++)]])] = Ordering.fromLessThan(_._1 < _._1)

  val currentTimeMillis = Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)


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


  private def unblock(map: Map[String, Int | (Boolean, +)], key: String)
                     (implicit ^ : String): UIO[Unit] =
    ZIO.when(map.contains(^ + key))(map(^ + key).asInstanceOf[(Boolean, +)]._2._1._1._1.succeed(None)).unit

  private def `π-discard`(map: Map[String, Int | (Boolean, +)], discarded: `Π-Set`[String])
                         (implicit ^ : String): UIO[Set[String]] =
    ZIO.collectAllParDiscard(discarded.toList.map(unblock(map, _))).as(discarded.map(^ + _))

  private def discard(key: String, map: Map[String, Int | (Boolean, +)])
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
             .filter(_._2.asInstanceOf[(Boolean, +)]._1)
             .map(_ -> _.asInstanceOf[(Boolean, +)]._2._2)
             .toMap
      if it.isEmpty
      then
        (**.size.flatMap(size => **.offer(size -> Nil))).commit.map(_ -> m)
      else
        val nel = ∥(it)(`π-wand`._1)()
        val nelʹ = nel.map {
          _.map {
            case (key1, key2, in, dd) =>
              val (pckots1, _) = m(key1).asInstanceOf[(Boolean, +)]._2
              val (pckots2, _) = m(key2).asInstanceOf[(Boolean, +)]._2
              (key1, key2) -> (dd, in, (pckots1, pckots2))
          }
        }
        ZIO.collectAll {
          nel.flatten.map {
            case (key1, key2, _, _) =>
              val k1 = key1.substring(36)
              val k2 = key2.substring(36)
              val  ^ = key1.substring(0, 36)
              val ^^ = key2.substring(0, 36)
              val (((p1, c1), _, _), _) = m(key1).asInstanceOf[(Boolean, +)]._2
              val (((p2, c2), _, _), _) = m(key2).asInstanceOf[(Boolean, +)]._2
              for
                s1 <- p1.isDone.negate.flatMap { if _ then discard(k1, m)(using  ^) else ZIO.succeed(Set.empty) }
                s2 <- if k1 == k2
                      then ZIO.succeed(Set.empty)
                      else p2.isDone.negate.flatMap { if _ then discard(k2, m)(using ^^) else ZIO.succeed(Set.empty) }
              yield
                (s1 ++ s2 ++ when(c1 eq null)(key1) ++ when(c2 eq null)(key2))
             -> (Nil ++ unless(c1 eq null)(key1) ++ unless(k1 == k2)(unless(c2 eq null)(key2)).flatten)
          }
        }.map(_.foldRight(m) {
                case ((ks, ls), map) =>
                  ls.map { key => key -> (false, map(key).asInstanceOf[(Boolean, +)]._2) }
                    .foldLeft(ks.foldLeft(map)(_ - _))(_ + _)
              }
        ).flatMap(mʹ => (**.size.flatMap(size => **.offer(size -> nelʹ))).commit.map(_ -> mʹ))
    }


  private def canExit(using % : %)
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): UIO[Boolean] =
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
    %.modify { m => exit(m) -> m }

  def loopʹ(parameters: `Π-Parameters`, started: Ref[Long], batch: Ref[Long], restore: ZIO.InterruptibilityRestorer, feedback: Feedback)
           (using % : %, / : /, ! : !, &| : &|, - : -, * : *, ** : **, ^ : ^)
           (using `][`: `}{`.`][`, `1`: TSemaphore)
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
                  canExit.flatMap(if _ then feedback.doneR.set(true) *> feedback.pauseRP_stopR_exitRP.get.flatMap(_._2.await) *> -.offer(None) *> /.offer(null) *> ZIO.succeed(false) else ZIO.succeed(true))
                case _  =>
                  ZIO.succeed(true)
              }
            else
              (feedback.pauseRP_stopR_exitRP.get.map(_._1._2) <*> Semaphore[UIO](parameters.parallelism)).flatMap { (stop, sem) =>
                ZIO.collectAll {
                  nel.map { nel =>
                    ZIO.collectAllParDiscard {
                      nel.map { case ((key1, key2), ((delay, duration), in, (((p1, c1), (key, ord), ts1), ((p2, c2), (keyʹ, ordʹ), ts2)))) =>
                                  val k1 = key1.substring(36)
                                  val k2 = key2.substring(36)
                                  ZIO.uninterruptible {
                                    for
                                      cb <- CyclicBarrier.make(if k1 == k2 then 2 else 3)
                                      fb  = ( for
                                                (slabel, _)  <- `}{`.`}{`(key).commit
                                                (slabelʹ, _) <- `}{`.`}{`(keyʹ).commit
                                                _            <- `1`.acquire.commit.when(k1 == k2)
                                                _            <- ZIO.unless(k1 == k2) {
                                                                  (ord, ordʹ) match
                                                                     case (dir: `π-$`, dirʹ: `π-$`) =>
                                                                       `}{`.><.π(key, dir, keyʹ, dirʹ)
                                                                     case (cap: `π-ζ`, capʹ: `π-ζ`) =>
                                                                       `}{`.><.ζ(key, cap, keyʹ, capʹ)
                                                                }
                                                elabel       <- `}{`.`}{`(key, parameters.snapshot).commit
                                                (elabelʹ, _) <- `}{`.`}{`(keyʹ).commit
                                                _            <- `1`.release.commit
                                                _            <- cb.await.exit
                                                _            <- enable(k1)
                                                _            <- enable(k2).unless(k1 == k2)
                                                nc           <- if duration == 0.0 || duration.isNaN
                                                                then &|.updateAndGet { (no, cl) => (no + 1, cl) }
                                                                else &|.updateAndGet { (no, cl) => (no + 1, cl + delay) }
                                                ss           <- ts1.get <*> ts2.get
                                                now          <- currentTimeMillis
                                                _            <- feedback.lastR.set(now -> nc._2)
                                                _            <- -.offer(Some((nc, (ss, now), (k1, k2), (delay, duration), (slabel -> elabel, slabelʹ -> (elabelʹ -> elabel._2))))).whenZIO(feedback.tracesR.get)
                                                _            <- sem.release
                                                _            <- started.update(_ - 1)
                                              yield
                                                ()
                                            ).fork
                                      _  <- ( if stop
                                              then
                                                for
                                                  _ <- **.offer(-1 -> Nil).commit
                                                  _ <- p1.succeed(None)
                                                  _ <- p2.succeed(None).unless(k1 == k2)
                                                  _ <- ZIO.unless(c1 eq null)(c1.get.flatMap(_.succeed(None)))
                                                  _ <- ZIO.unless(c2 eq null)(c2.get.flatMap(_.succeed(None))).unless(k1 == k2)
                                                yield
                                                  ()
                                              else
                                                for
                                                  _  <- sem.acquire
                                                  _  <- started.update(_ + 1)
                                                  fb <- fb
                                                  _  <- p1.succeed(Some((delay, cb, fb, in)))
                                                  _  <- p2.succeed(Some((delay, cb, fb, in))).unless(k1 == k2)
                                                  _  <- ZIO.unless(c1 eq null)(c1.get.flatMap(_.succeed(Some((delay, cb, fb, in)))))
                                                  _  <- ZIO.unless(c2 eq null)(c2.get.flatMap(_.succeed(Some((delay, cb, fb, in))))).unless(k1 == k2)
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
                   ZIO.yieldNow *> loopʹ(params, started, batch, restore, feedback)
                 }
               }
             else
               ZIO.yieldNow *> loopʹ(parameters, started, batch, restore, feedback)
           }.when(l)
    yield
      ()

  def loop0(parameters: `Π-Parameters`, started: Ref[Long], restore: ZIO.InterruptibilityRestorer, feedback: Feedback)
           (using % : %, / : /, ! : !, &| : &|, - : -, * : *, ** : **)
           (using `][`: `}{`.`][`, `1`: TSemaphore)
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
                  canExit.flatMap(if _ then feedback.doneR.set(true) *> feedback.pauseRP_stopR_exitRP.get.flatMap(_._2.await) *> -.offer(None) *> /.offer(null) *> ZIO.succeed(false) else ZIO.succeed(true))
                case Exit.Failure(cause)                 =>
                  ZIO.failCause(cause)
              }
            case _  =>
              ZIO.succeed(true)
          }
        else
          (feedback.pauseRP_stopR_exitRP.get.map(_._1._2) <*> Semaphore[UIO](parameters.parallelism)).flatMap { (stop, sem) =>
            ZIO.collectAll {
              nel.map { nel =>
                ZIO.collectAllParDiscard {
                  nel.map { case ((key1, key2), ((delay, duration), in, (((p1, c1), (key, ord), ts1), ((p2, c2), (keyʹ, ordʹ), ts2)))) =>
                              val k1 = key1.substring(36)
                              val k2 = key2.substring(36)
                              ZIO.uninterruptible {
                                for
                                  cb <- CyclicBarrier.make(if k1 == k2 then 2 else 3)
                                  fb  = ( for
                                            (slabel, _)  <- `}{`.`}{`(key).commit
                                            (slabelʹ, _) <- `}{`.`}{`(keyʹ).commit
                                            _            <- `1`.acquire.commit.when(k1 == k2)
                                            _            <- ZIO.unless(k1 == k2) {
                                                              (ord, ordʹ) match
                                                                 case (dir: `π-$`, dirʹ: `π-$`) =>
                                                                   `}{`.><.π(key, dir, keyʹ, dirʹ)
                                                                 case (cap: `π-ζ`, capʹ: `π-ζ`) =>
                                                                   `}{`.><.ζ(key, cap, keyʹ, capʹ)
                                                            }
                                            elabel       <- `}{`.`}{`(key, parameters.snapshot).commit
                                            (elabelʹ, _) <- `}{`.`}{`(keyʹ).commit
                                            _            <- `1`.release.commit
                                            _            <- cb.await.exit
                                            _            <- enable(k1)
                                            _            <- enable(k2).unless(k1 == k2)
                                            nc           <- if duration == 0.0 || duration.isNaN
                                                            then &|.updateAndGet { (no, cl) => (no + 1, cl) }
                                                            else &|.updateAndGet { (no, cl) => (no + 1, cl + delay) }
                                            ss           <- ts1.get <*> ts2.get
                                            now          <- currentTimeMillis
                                            _            <- feedback.lastR.set(now -> nc._2)
                                            _            <- -.offer(Some((nc, (ss, now), (k1, k2), (delay, duration), (slabel -> elabel, slabelʹ -> (elabelʹ -> elabel._2))))).whenZIO(feedback.tracesR.get)
                                            _            <- sem.release
                                            _            <- started.updateAndGet(_ - 1).map(_ == 0).flatMap(peek.when(_))
                                          yield
                                            ()
                                        ).fork
                                  _  <- ( if stop
                                          then
                                            for
                                              _ <- **.offer(-1 -> Nil).commit
                                              _ <- p1.succeed(None)
                                              _ <- p2.succeed(None).unless(k1 == k2)
                                              _ <- ZIO.unless(c1 eq null)(c1.get.flatMap(_.succeed(None)))
                                              _ <- ZIO.unless(c2 eq null)(c2.get.flatMap(_.succeed(None))).unless(k1 == k2)
                                            yield
                                              ()
                                          else
                                            for
                                              _  <- sem.acquire
                                              _  <- started.update(_ + 1)
                                              fb <- fb
                                              _  <- p1.succeed(Some((delay, cb, fb, in)))
                                              _  <- p2.succeed(Some((delay, cb, fb, in))).unless(k1 == k2)
                                              _  <- ZIO.unless(c1 eq null)(c1.get.flatMap(_.succeed(Some((delay, cb, fb, in)))))
                                              _  <- ZIO.unless(c2 eq null)(c2.get.flatMap(_.succeed(Some((delay, cb, fb, in))))).unless(k1 == k2)
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
                          ZIO.yieldNow *> loop0(params, started, restore, feedback)
                        }
                      }
                    else
                      ZIO.yieldNow *> loop0(parameters, started, restore, feedback)
                  }.when(l)
    yield
      ()

  def poll(using % : %, / : /, \ : \)
          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): UIO[Unit] =
    /.take.flatMap {
      case null => ZIO.unit
      case ((^ @ (_: String), key), it @ (((p, _), _, _), _)) =>
        p.isDone.negate.flatMap {
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
        } *> ZIO.yieldNow *> poll
    }
