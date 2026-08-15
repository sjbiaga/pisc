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
import _root_.zio.{ durationInt, ExitCode, Fiber, Promise, Queue, Ref, Semaphore => SemaphoreZIO, UIO, ZIO }
import _root_.zio.concurrent.CyclicBarrier

import `Π-dump`.*
import `Π-stats`.*


package object `Π-loop`:

  private val spirsx = "pisc.stochastic.replications.exitcode.ignore"


  import sΠ.{ `Π-Map`, `Π-Set`, `()` }

  type <> = (CyclicBarrier, Fiber[Nothing, Unit], Ref[`()`])

  type ++ = (Promise[Nothing, Option[<>]], Ref[Promise[Nothing, Option[<>]]])
  type + = (++, ({}, Option[Either[Unit, Ref[`()`]]], Rate))

  type % = Ref.Synchronized[Map[String, Int | (Boolean, +)]]

  type ! = Promise[Nothing, ExitCode]

  type & = Ref[Long]

  type / = Queue[((String, String), +)]

  type \ = UIO[Unit] => UIO[Unit]

  type ++++ = (Double, Ref[`()`], (++, ++))
  type ** = Queue[(() => Boolean, List[((String, String), ++++)])]

  type * = Semaphore[UIO]

  type ^ = SemaphoreZIO


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
    map(^ + key).asInstanceOf[(Boolean, +)]._2._1._1.succeed(None).unit

  private def `π-discard`(map: Map[String, Int | (Boolean, +)], discarded: `Π-Set`[String])
                         (implicit ^ : String): UIO[Set[String]] =
    ZIO.collectAllParDiscard(discarded.toList.map(unblock(map, _))).as(discarded.map(^ + _))

  private def discard(key: String, map: Map[String, Int | (Boolean, +)])(using String)
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
      val (trick, _) = `π-wand`
      def exit(mʹ: Map[String, Int | (Boolean, +)]) =
        { () => !mʹ.exists(_._2.isInstanceOf[Int])
             && mʹ.forall {
                  case (key1, (true, (_, (e1, Some(p1), _)))) =>
                    val ^ = key1.substring(0, 36)
                    !mʹ.exists {
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
                  case _ => false
                }
        }
      if it.isEmpty
      then
        **.offer(exit(m) -> Nil).unit.map(_ -> m)
      else
        val nel = ∥(it)(trick)()
        val nelʹ = nel.map {
          case (key1, key2, in, delay) =>
            val (pc1, _) = m(key1).asInstanceOf[(Boolean, +)]._2
            val (pc2, _) = m(key2).asInstanceOf[(Boolean, +)]._2
            (key1, key2) -> (delay, in, (pc1, pc2))
        }
        ZIO.collectAll {
          nel.map {
                case (key1, key2, _, _) =>
                  val k1 = key1.substring(36)
                  val k2 = key2.substring(36)
                  val  ^ = key1.substring(0, 36)
                  val ^^ = key2.substring(0, 36)
                  val ((p1, c1), _) = m(key1).asInstanceOf[(Boolean, +)]._2
                  val ((p2, c2), _) = m(key2).asInstanceOf[(Boolean, +)]._2
                  for
                    s1 <- p1.isDone.flatMap { if _ then ZIO.succeed(Set.empty) else discard(k1, m)(using  ^) }
                    s2 <- if k1 == k2
                          then ZIO.succeed(Set.empty)
                          else p2.isDone.flatMap { if _ then ZIO.succeed(Set.empty) else discard(k2, m)(using ^^) }
                  yield
                    (s1 ++ s2 ++ when(c1 eq null)(key1) ++ when(c2 eq null)(key2))
                 -> (Nil ++ unless(c1 eq null)(key1) ++ unless(k1 == k2)(unless(c2 eq null)(key2)).flatten)
          }
        }.map(_.foldRight(m) {
                case ((ks, ls), map) =>
                  ls.map { key => key -> (false, map(key).asInstanceOf[(Boolean, +)]._2) }
                    .foldLeft(ks.foldLeft(map)(_ - _))(_ + _)
              }
        ).flatMap(mʹ => **.offer(exit(mʹ) -> nelʹ).unit.map(_ -> mʹ))
    }


  private def exit(using % : %, ! : !): UIO[Unit] =
    %.get.flatMap { m =>
      val ks = m.keys.toList
      val ec =
        if ks.isEmpty
        then
          ExitCode.success
        else
          if !sys.BooleanProp.keyExists(spirsx).value
          && ks.forall(_.charAt(36) == '!')
          then ExitCode.success
          else ExitCode.failure
      ZIO.collectAllParDiscard(ks.map(m(_).asInstanceOf[(Boolean, +)]._2._1._1.succeed(None))) *>
      ZIO.collectAllParDiscard(ks.map(m(_).asInstanceOf[(Boolean, +)]._2._1._2 match { case null => ZIO.unit
                                                                                       case it => it.get.flatMap(_.succeed(None).unit) })) *> !.succeed(ec).unit
    }

  def loop(parallelism: Int, threshold: Int, timeout: Int, started: Ref[Long], batch: Ref[Long])
          (using % : %, ! : !, & : &, - : -, * : *, ** : **, ^ : ^)
          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): UIO[Unit] =
    for
      _ <- (batch.set(0L) *> *.acquire.ensuring(batch.update(_ + 1)).repeatN(threshold-1).timeout(timeout.nanoseconds)).when(threshold > 0)
      m  =
        for
          (exit, nel) <- **.take
          l           <-
            if nel.isEmpty
            then
              if threshold > 0
              then
                (started.get <*> batch.get).map(_ + _).flatMap {
                  case 0L if exit() =>
                    this.exit *> ZIO.succeed(false)
                  case _ =>
                    ZIO.succeed(true)
                }
              else
                (started.get <*> **.size.map(_.toLong)).map(_ + _).flatMap {
                  case 0L if exit() =>
                    this.exit *> ZIO.succeed(false)
                  case _ =>
                    ZIO.succeed(true)
                }
            else
              Semaphore[UIO](parallelism).flatMap { sem =>
                ZIO.collectAllParDiscard {
                  nel.map { case ((key1, key2), (_delay, in, ((p1, c1), (p2, c2)))) =>
                              val k1 = key1.substring(36)
                              val k2 = key2.substring(36)
                              ZIO.uninterruptible {
                                for
                                  cb <- CyclicBarrier.make(if k1 == k2 then 2 else 3)
                                  _  <- sem.acquire
                                  _  <- started.update(_ + 1)
                                  fb <- ( for
                                            _ <- cb.await.exit
                                            e  = ( for
                                                     _ <- enable(k1)
                                                     _ <- enable(k2).unless(k1 == k2)
                                                   yield
                                                     ()
                                                 )
                                            _ <- if threshold > 0
                                                 then e
                                                 else ^.withPermit(e *> peek)
                                            _ <- sem.release
                                            _ <- started.update(_ - 1)
                                            _ <- peek.unless(threshold > 0)
                                          yield
                                            ()
                                        ).fork
                                  _  <- p1.succeed(Some((cb, fb, in)))
                                  _  <- p2.succeed(Some((cb, fb, in))).unless(k1 == k2)
                                  _  <- ZIO.unless(c1 eq null)(c1.get.flatMap(_.succeed(Some((cb, fb, in)))))
                                  _  <- ZIO.unless(c2 eq null)(c2.get.flatMap(_.succeed(Some((cb, fb, in))))).unless(k1 == k2)
                                yield
                                  ()
                              }
                          }
                }
              } *> ZIO.succeed(true)
        yield
          l
      l <- if threshold > 0
           then ^.withPermit(*.available.flatMap(*.acquireN) *> peek *> m)
           else m
      _ <- loop(parallelism, threshold, timeout, started, batch).when(l)
    yield
      ()

  def poll(using % : %, / : /, \ : \): UIO[Unit] =
    for
      h <- /.take
      ((_, key), it) = h
      ((d, _), _) = it
      _ <- d.isDone.flatMap {
        if _
        then
          %.update { m =>
                     val ^ = h._1._1
                     m + (^ + key -> (false, it))
          }
        else
         \(
            %.update { m =>
                       val ^ = h._1._1
                       val n = m(key).asInstanceOf[Int] - 1
                       ( if n == 0
                         then
                           m - key
                         else
                           m + (key -> n)
                       ) + (^ + key -> (true, it))
            }
          )
      }
      _ <- poll
    yield
      ()
