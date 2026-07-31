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

import _root_.scala.collection.immutable.{ List, Map }

import _root_.cats.instances.list.*
import _root_.cats.syntax.applicative.*
import _root_.cats.syntax.functor.*
import _root_.cats.syntax.flatMap.*
import _root_.cats.syntax.parallel.*
import _root_.cats.syntax.semigroupal.*
import _root_.cats.syntax.traverse.*

import _root_.cats.Parallel
import _root_.cats.effect.{ Deferred, ExitCode, Fiber, Ref, Temporal }
import _root_.cats.effect.std.{ CyclicBarrier, Queue, Semaphore }
import _root_.cats.effect.syntax.spawn.*

import `Π-dump`.*
import `Π-stats`.*


package object `Π-loop`:

  import sΠ.{ `Π-Map`, `Π-Set`, `()` }

  type <>[F[_]] = (CyclicBarrier[F], Fiber[F, Throwable, Unit], Ref[F, `()`[F]])

  type +[F[_]] = ((Deferred[F, Option[<>[F]]], Ref[F, Deferred[F, Option[<>[F]]]]), (Ref[F, Long], ({}, Option[Either[Unit, Ref[F, `()`[F]]]], Rate)))

  type %[F[_]] = Ref[F, Map[String, Int | (Boolean, +[F])]]

  type /[F[_]] = Queue[F, ((String, String), +[F])]

  type ![F[_]] = Deferred[F, ExitCode]

  type &[F[_]] = Ref[F, Long]

  type *[F[_]] = Semaphore[F]

  type \[F[_]] = F[Unit]


  def `π-enable`[F[_]](enabled: `Π-Set`[String])
                      (using % : %[F]): F[Unit] =
    %.update(enabled.foldLeft(_) { (m, key) =>
                                    val n = if m.contains(key)
                                            then m(key).asInstanceOf[Int]
                                            else 0
                                    m + (key -> (n + 1))
                                 }
    )


  final class πloop[F[_]: Parallel: Temporal]:

    private def enable(key: String)
                      (using %[F])
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): F[Unit] =
      val (_, spell) = `π-wand`
      `π-enable`[F](spell(key))

    private def unblock(m: Map[String, Int | (Boolean, +[F])], k: String)
                       (implicit ^ : String): F[Unit] =
      m(^ + k).asInstanceOf[(Boolean, +[F])]._2._1._1.complete(None).void.whenA(m.contains(^ + k))

    private def `π-discard`(discarded: `Π-Set`[String])
                           (using % : %[F])
                           (implicit ^ : String): F[Unit] =
      for
        m <- %.get
        _ <- discarded.toList.traverse(unblock(m, _)).void
        _ <- %.update(discarded.map(^ + _).foldLeft(_)(_ - _))
      yield
        ()

    private def discard(key: String)(using ^ : String)
                       (using %[F])
                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): F[Unit] =
      val (trick, _) = `π-wand`
      `π-discard`(trick(key)).whenA(trick.contains(key))


    def loop(parallelism: Int, started: Ref[F, Long])
            (using % : %[F], ! : ![F], & : &[F], - : -[F], * : *[F])
            (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): F[Unit] =
      %.flatModify { m =>
        m -> {
          { if m.exists(_._2.isInstanceOf[Int])
            then Map.empty -> { () => false }
            else m
                 .filter(_._2.asInstanceOf[(Boolean, +[F])]._1)
                 .map(_ -> _.asInstanceOf[(Boolean, +[F])]._2._2._2)
                 .toMap
              -> { () => m.isEmpty
                      || m.forall(_._1.charAt(36) == '!')
                      && m.forall(_._2.asInstanceOf[(Boolean, +[F])]._1)
                      && { val (trick, _) = `π-wand`
                           m.forall {
                             case (key1, (_, (_, (_, (e1, Some(p1), _))))) =>
                               val ^ = key1.substring(0, 36)
                               !m.exists {
                                 case (key2, (_, (_, (_, (e2, Some(p2), _))))) if (e1 eq e2) && p1.isLeft == p2.isRight =>
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
                 }
          } match
            case (it: Map[String, ({}, Option[Either[Unit, Ref[F, `()`[F]]]], Rate)], exit) =>
              if it.isEmpty && !exit()
              then
                *.acquire >> loop(parallelism, started)
              else
                ∥(it)(`π-wand`._1)() match
                  case Nil =>
                    (started.get product *.available).map(_ + _).flatMap { n =>
                      if n == 0L && exit()
                      then
                        -.offer(it.keys.toList)
                      else
                        *.acquire >> loop(parallelism, started)
                    }
                  case nel =>
                    Semaphore[F](parallelism).flatMap { sem =>
                      nel.parTraverse { case (key1, key2, in, (delay, duration)) =>
                                          val k1 = key1.substring(36)
                                          val k2 = key2.substring(36)
                                          val  ^ = key1.substring(0, 36)
                                          val ^^ = key2.substring(0, 36)
                                          Temporal[F].uncancelable { _ =>
                                            for
                                              -- <- CyclicBarrier[F](if k1 == k2 then 2 else 3)
                                              p1 <- %.modify { m => m -> m(key1).asInstanceOf[(Boolean, +[F])]._2 }
                                              p2 <- %.modify { m => m -> m(key2).asInstanceOf[(Boolean, +[F])]._2 }
                                              ((d1, c1), (ts1, _)) = p1
                                              ((d2, c2), (ts2, _)) = p2
                                              _  <- sem.acquire
                                              o1 <- d1.tryGet
                                              o2 <- d2.tryGet
                                              _  <- (discard(k1)(using  ^) >> %.update(_ - key1).whenA(c1 eq null)).whenA(o1 eq None)
                                              _  <- (discard(k2)(using ^^) >> %.update(_ - key2).whenA(c2 eq null)).whenA(o2 eq None).unlessA(k1 == k2)
                                              b1 <- CyclicBarrier[F](2)
                                              b2 <- CyclicBarrier[F](2)
                                              _  <- started.update(_ + 1)
                                              fb <- ( for
                                                        _  <- b1.await.unlessA(c1 eq null)
                                                        _  <- b2.await.unlessA(c2 eq null).unlessA(k1 == k2)
                                                        _  <- --.await
                                                        _  <- enable(k1)
                                                        _  <- enable(k2).unlessA(k1 == k2)
                                                        no <- &.updateAndGet(_ + 1)
                                                        ss <- ts1.get product ts2.get
                                                        now <- Temporal[F].monotonic.map(_.toNanos)
                                                        _  <- -.offer((no, (ss, now), (k1, k2), (delay, duration)))
                                                        _ <- sem.release
                                                        _ <- started.update(_ - 1)
                                                        _ <- *.release
                                                      yield
                                                        ()
                                                    ).start
                                              _  <- d1.complete(Some((--, fb, in))).whenA(o1 eq None)
                                              _  <- d2.complete(Some((--, fb, in))).whenA(o2 eq None).unlessA(k1 == k2)
                                              _  <- (c1.get.flatMap(_.complete(Some((--, fb, in))))
                                                  >> %.update { m => m + (key1 -> (false, m(key1).asInstanceOf[(Boolean, +[F])]._2)) }
                                                  >> b1.await).unlessA(c1 eq null)
                                              _  <- (c2.get.flatMap(_.complete(Some((--, fb, in))))
                                                  >> %.update { m => m + (key2 -> (false, m(key2).asInstanceOf[(Boolean, +[F])]._2)) }
                                                  >> b2.await).unlessA(c2 eq null).unlessA(k1 == k2)
                                            yield
                                              ()
                                          }
                                      }
                    } >> Temporal[F].cede >> loop(parallelism, started)
        }
      }

    def poll(using % : %[F], / : /[F], * : *[F]): F[Unit] =
      for
        h <- /.take
        ((_, key), it) = h
        ((d, _), _) = it
        _ <- d.tryGet.map(_ ne None).flatMap {
          if _
          then
            %.update { m =>
                       val ^ = h._1._1
                       m + (^ + key -> (false, it))
            }
          else
            %.update { m =>
                       val ^ = h._1._1
                       val n = m(key).asInstanceOf[Int] - 1
                       ( if n == 0
                         then
                           m - key
                         else
                           m + (key -> n)
                       ) + (^ + key -> (true, it))
            } >> *.release
        }
        _ <- Temporal[F].cede >> poll
      yield
        ()
