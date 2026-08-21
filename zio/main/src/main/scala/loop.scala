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

import _root_.cats.effect.std.Semaphore
import _root_.zio.interop.catz.generic.*
import _root_.zio.{ durationInt, Exit, ExitCode, Fiber, Promise, Queue, Ref, Semaphore => SemaphoreZIO, UIO, ZIO }
import _root_.zio.concurrent.CyclicBarrier
import _root_.zio.stm.{ TPriorityQueue, TSemaphore }

import `Π-dump`.*
import `Π-stats`.*


package object `Π-loop`:

  private val barsx = "pisc.bioambients.replications.exitcode.ignore"


  import sΠ.{ `Π-Map`, `Π-Set`, Ordʹ, `π-$`, `π-ζ`, `)(`, `()`, `}{` }


  type <> = (Double, CyclicBarrier, Fiber[Nothing, Unit], Ref[`()`])

  type ++ = (Promise[Nothing, Option[<>]], (`)(`, Ordʹ))
  type + = (++, ({}, Option[Either[Unit, Ref[`()`]]], Rate))

  type % = Ref.Synchronized[Map[String, Int | +]]

  type ! = Promise[Nothing, ExitCode]

  type & = Ref[Long]

  type / = Queue[((String, String), +)]

  type \ = UIO[Unit] => UIO[Unit]

  type ++++ = (Double, Ref[`()`], (++, ++))
  type ** = TPriorityQueue[(Int, List[((String, String), ++++)])]

  type * = Semaphore[UIO]

  type ^ = SemaphoreZIO


  final case class `Π-Parameters`(parallelism: Int,
                                  threshold: Int,
                                  timeout: Int,
                                  exit: Boolean,
                                  snapshot: Boolean)


  given Ordering[(Int, List[((String, String), ++++)])] = Ordering.fromLessThan(_._1 < _._1)


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
          case (key1, key2, in, delay) =>
            val (pko1, _) = m(key1).asInstanceOf[+]
            val (pko2, _) = m(key2).asInstanceOf[+]
            (key1, key2) -> (delay, in, (pko1, pko2))
        }
        ZIO.collectAll {
          nel.map {
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

  private def doExit(using % : %, ! : !): UIO[Unit] =
    %.get.flatMap { m =>
      val ks = m.keys.toList
      val ec =
        if ks.isEmpty
        then
          ExitCode.success
        else
          if !sys.BooleanProp.keyExists(barsx).value
          && ks.forall(_.charAt(36) == '!')
          then ExitCode.success
          else ExitCode.failure
      ZIO.collectAllParDiscard(ks.map(m(_).asInstanceOf[+]._1._1.succeed(None))) *>
      !.succeed(ec).unit
    }

  def loopʹ(parameters: `Π-Parameters`, started: Ref[Long], batch: Ref[Long])
           (using % : %, ! : !, & : &, - : -, * : *, ** : **, ^ : ^)
           (using `}{`.`][`, TSemaphore)
           (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): UIO[Unit] =
    for
      _ <- batch.set(0L) *> *.acquire.onExit { case Exit.Success(_) => batch.update(_ + 1) case _ => ZIO.unit }.repeatN(parameters.threshold-1).timeout(parameters.timeout.microseconds)
      m  =
        for
          (_, nel) <- **.take.commit
          l        <-
            if nel.isEmpty
            then
              (started.get <*> batch.get).map(_ + _).flatMap {
                case 0L =>
                  canExit.flatMap(if _ then doExit *> ZIO.succeed(false) else ZIO.succeed(true))
                case _  =>
                  ZIO.succeed(true)
              }
            else
              Semaphore[UIO](parameters.parallelism).flatMap { sem =>
                ZIO.collectAllParDiscard {
                  nel.map { case ((key1, key2), (delay, in, ((p1, (key, ord)), (p2, (keyʹ, ordʹ))))) =>
                              val k1 = key1.substring(36)
                              val k2 = key2.substring(36)
                              ZIO.uninterruptible {
                                for
                                  cb <- CyclicBarrier.make(if k1 == k2 then 2 else 3)
                                  _  <- sem.acquire
                                  _  <- started.update(_ + 1)
                                  fb <- ( for
                                            _ <- ZIO.unless(k1 == k2) {
                                                   (ord, ordʹ) match
                                                     case (dir: `π-$`, dirʹ: `π-$`) =>
                                                       `}{`.><.π(key, dir, keyʹ, dirʹ)
                                                     case (cap: `π-ζ`, capʹ: `π-ζ`) =>
                                                       `}{`.><.ζ(key, cap, keyʹ, capʹ)
                                                 }
                                            _ <- cb.await.exit
                                            _ <- enable(k1)
                                            _ <- enable(k2).unless(k1 == k2)
                                            _ <- sem.release
                                            _ <- started.update(_ - 1)
                                          yield
                                            ()
                                        ).fork
                                  _  <- p1.succeed(Some((delay, cb, fb, in)))
                                  _  <- p2.succeed(Some((delay, cb, fb, in))).unless(k1 == k2)
                                yield
                                  ()
                              }
                          }
                }
              } *> ZIO.succeed(true)
        yield
          l
      l <- ^.withPermit(*.available.flatMap(*.acquireN) *> peek *> m)
      _ <- loopʹ(parameters, started, batch).when(l)
    yield
      ()

  def loop0(parameters: `Π-Parameters`, started: Ref[Long])
           (using % : %, ! : !, & : &, - : -, * : *, ** : **)
           (using `}{`.`][`, TSemaphore)
           (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): UIO[Unit] =
    for
      (_, nel) <- **.take.commit
      l        <-
        if nel.isEmpty
        then
          (started.get <*> **.size.commit.map(_.toLong)).map(_ + _).flatMap {
            case 0L =>
              ZIO.sleep(parameters.timeout.microseconds).raceEither(**.take.commit).flatMap {
                case Right((_, nel)) =>
                  **.offer(-1 -> nel).commit *> ZIO.succeed(true)
                case _               =>
                  canExit.flatMap(if _ then doExit *> ZIO.succeed(false) else ZIO.succeed(true))
              }
            case _  =>
              ZIO.succeed(true)
          }
        else
          Semaphore[UIO](parameters.parallelism).flatMap { sem =>
            ZIO.collectAllParDiscard {
              nel.map { case ((key1, key2), (delay, in, ((p1, (key, ord)), (p2, (keyʹ, ordʹ))))) =>
                          val k1 = key1.substring(36)
                          val k2 = key2.substring(36)
                          ZIO.uninterruptible {
                            for
                              cb <- CyclicBarrier.make(if k1 == k2 then 2 else 3)
                              _  <- sem.acquire
                              _  <- started.update(_ + 1)
                              fb <- ( for
                                        _ <- ZIO.unless(k1 == k2) {
                                               (ord, ordʹ) match
                                                 case (dir: `π-$`, dirʹ: `π-$`) =>
                                                   `}{`.><.π(key, dir, keyʹ, dirʹ)
                                                 case (cap: `π-ζ`, capʹ: `π-ζ`) =>
                                                   `}{`.><.ζ(key, cap, keyʹ, capʹ)
                                             }
                                        _ <- cb.await.exit
                                        _ <- enable(k1)
                                        _ <- enable(k2).unless(k1 == k2)
                                        _ <- sem.release
                                        _ <- started.updateAndGet(_ - 1).map(_ == 0).flatMap(peek.when(_))
                                      yield
                                        ()
                                    ).fork
                              _  <- p1.succeed(Some((delay, cb, fb, in)))
                              _  <- p2.succeed(Some((delay, cb, fb, in))).unless(k1 == k2)
                            yield
                              ()
                          }
                      }
            }
          } *> ZIO.succeed(true)
      _        <- loop0(parameters, started).when(l)
    yield
      ()

  def poll(using % : %, / : /, \ : \): UIO[Unit] =
    for
      h <- /.take
      ((_, key), it) = h
      _ <- \( %.update { m =>
                         val ^ = h._1._1
                         val n = m(key).asInstanceOf[Int] - 1
                         ( if n == 0
                           then
                             m - key
                           else
                             m + (key -> n)
                         ) + (^ + key -> it)
              }
            )
      _ <- poll
    yield
      ()
