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

import _root_.scala.collection.immutable.{ List, Map }

import _root_.cats.instances.list.*
import _root_.cats.syntax.functor.*
import _root_.cats.syntax.flatMap.*
import _root_.cats.syntax.parallel.*
import _root_.cats.syntax.traverse.*

import _root_.cats.Parallel
import _root_.cats.effect.{ Deferred, ExitCode, Fiber, Ref, Temporal, Unique }
import _root_.cats.effect.std.{ CyclicBarrier, Queue, Semaphore }
import _root_.cats.effect.syntax.spawn.*

import `Π-dump`.*
import `Π-stats`.*


package object `Π-loop`:

  import sΠ.{ `Π-Map`, `Π-Set`, Ordʹ, `π-$`, `π-ζ`, `)(`, >*< }

  type <>[F[_]] = (CyclicBarrier[F], Fiber[F, Throwable, Unit], Unique.Token)

  type +[F[_]] = ((Deferred[F, Option[<>[F]]], Ref[F, Deferred[F, Option[<>[F]]]]), (Ref[F, Long], ((`)(`, Ordʹ), ((>*<[F] | Object, Int), Option[Boolean], Rate))))

  type %[F[_]] = Ref[F, Map[String, Int | (Boolean, +[F])]]

  type /[F[_]] = Queue[F, ((String, String), +[F])]

  type ![F[_]] = Deferred[F, ExitCode]

  type &[F[_]] = Ref[F, Long]

  type ~[F[_]] = Semaphore[F]

  type *[F[_]] = Queue[F, Unit]

  type \[F[_]] = () => F[Unit]


  final class πloop[F[_]: Parallel: Temporal]:

    private def unblock(m: Map[String, Int | (Boolean, +[F])], k: String)
                       (implicit ^ : String): F[Unit] =
      if m.contains(^ + k)
      then m(^ + k).asInstanceOf[(Boolean, +[F])]._2._1._1.complete(None).void
      else Temporal[F].unit

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
      if trick.contains(key)
      then
        `π-discard`(trick(key))
      else
        Temporal[F].unit


    def loop(snapshot: Boolean, `}{`: sΠ.`}{`[F])
            (using % : %[F], ! : ![F], & : &[F], ~ : ~[F], - : -[F], * : *[F])
            (using `][`: `}{`.`][`, `2`: `}{`.stm.TSemaphore)
            (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): F[Unit] =
      %.flatModify { m =>
        m -> {
          val (it: Map[String, ((>*<[F] | Object, Int), Option[Boolean], Rate)], exit) =
            if m.exists(_._2.isInstanceOf[Int])
            then Map.empty -> { () => false }
            else m
                 .filter(_._2.asInstanceOf[(Boolean, +[F])]._1)
                 .map(_ -> _.asInstanceOf[(Boolean, +[F])]._2._2._2._2)
                 .toMap
              -> { () => m.isEmpty
                      || m.forall(_._1.charAt(36) == '!')
                      && m.forall(_._2.asInstanceOf[(Boolean, +[F])]._1)
                      && { val (trick, _) = `π-wand`
                           m.forall {
                             case (key1, (_, (_, (_, (_, (e1, Some(p1), _)))))) =>
                               val ^ = key1.substring(0, 36)
                               !m.exists {
                                 case (key2, (_, (_, (_, (_, (e2, Some(p2), _)))))) if e1 == e2 && p1 != p2 =>
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
          if it.isEmpty && !exit()
          then
            *.take >> loop(snapshot, `}{`)
          else
            ∥[F](it)(`π-wand`._1)() match
              case Nil =>
                *.size.flatMap { n =>
                  if n == 0 && exit()
                  then
                    -.offer(it.keys.toList)
                  else
                    *.take >> loop(snapshot, `}{`)
                }
              case nel =>
                nel.parTraverse { case (key1, key2, (delay, duration)) =>
                                  Temporal[F].uncancelable { _ =>
                                    val k1 = key1.substring(36)
                                    val k2 = key2.substring(36)
                                    val ^  = key1.substring(0, 36)
                                    val ^^ = key2.substring(0, 36)
                                    for
                                      cb <- CyclicBarrier[F](if k1 == k2 then 2 else 3)
                                      _  <- ~.acquire
                                      tk <- if k1 == k2 then Temporal[F].pure(null) else Temporal[F].unique
                                      sd1 <- Deferred[F, (String, (String, String))]
                                      sd2 <- Deferred[F, (String, (String, String))]
                                      sD <- Deferred[F, String]
                                      p1 <- %.modify { m => m -> m(key1).asInstanceOf[(Boolean, +[F])]._2 }
                                      p2 <- %.modify { m => m -> m(key2).asInstanceOf[(Boolean, +[F])]._2 }
                                      ((d1, c1), (ts1, ((key, ord), _))) = p1
                                      ((d2, c2), (ts2, ((keyʹ, ordʹ), _))) = p2
                                      o1 <- d1.tryGet
                                      o2 <- d2.tryGet
                                      fb <- ( for
                                                (slabel, _)  <- `}{`.stm.commit { `}{`.`}{`(key) }
                                                (slabelʹ, _) <- `}{`.stm.commit { `}{`.`}{`(keyʹ) }
                                                _            <- ( if k1 == k2 then
                                                                    `}{`.stm.commit {
                                                                      for
                                                                        _ <- `2`.acquire
                                                                        _ <- `2`.acquire
                                                                      yield
                                                                        ()
                                                                    }
                                                                  else
                                                                    (ord, ordʹ) match
                                                                      case (dir: `π-$`, dirʹ: `π-$`) =>
                                                                        `}{`.><.π(key, dir, keyʹ, dirʹ)
                                                                      case (cap: `π-ζ`, capʹ: `π-ζ`) =>
                                                                        `}{`.><.ζ(key, cap, keyʹ, capʹ)
                                                                )
                                                elabel       <- `}{`.stm.commit { `}{`.`}{`(key, snapshot) }
                                                (elabelʹ, _) <- `}{`.stm.commit { `}{`.`}{`(keyʹ) }
                                                _            <- sd1.complete(slabel -> elabel)
                                                _            <- if k1 == k2 then Temporal[F].unit
                                                                else sd2.complete(slabelʹ -> (elabelʹ -> elabel._2)).void
                                                _            <- ( if k1 == k2 then
                                                                    `}{`.stm.commit {
                                                                      for
                                                                        _ <- `2`.release
                                                                        _ <- `2`.release
                                                                      yield
                                                                        ()
                                                                    }
                                                                  else
                                                                    Temporal[F].unit
                                                                )
                                              yield
                                                ()
                                            ).start
                                      _  <- if o1 eq None then discard(k1)(using ^) >> (if k1.charAt(0) != '!' || (c1 eq null) then %.update(_ - key1) else Temporal[F].unit) >> d1.complete(Some((cb, fb, tk))).void
                                            else Temporal[F].unit
                                      _  <- if k1 == k2 then Temporal[F].unit
                                            else if o2 eq None then discard(k2)(using ^^) >> (if k2.charAt(0) != '!' || (c2 eq null) then %.update(_ - key2) else Temporal[F].unit) >> d2.complete(Some((cb, fb, tk))).void
                                            else Temporal[F].unit
                                      _  <- if k1.charAt(0) == '!' && (c1 ne null) then c1.get.flatTap(_.complete(Some((cb, fb, tk)))).void
                                            else Temporal[F].unit
                                      _  <- if k1 == k2 then Temporal[F].unit
                                            else if k2.charAt(0) == '!' && (c2 ne null) then c2.get.flatTap(_.complete(Some((cb, fb, tk)))).void
                                            else Temporal[F].unit
                                      _  <- ~.release
                                      _  <- cb.await
                                      no <- &.updateAndGet(_ + 1)
                                      now <- Temporal[F].monotonic.map(_.toNanos)
                                      s1 <- ts1.get
                                      s2 <- ts2.get
                                      _  <- -.offer((no, ((s1, s2), now), (k1, k2), (delay, duration), (sd1, sd2)))
                                    yield
                                      ()
                                  }
                                } >> loop(snapshot, `}{`)
        }
      }

    def poll(using % : %[F], / : /[F], * : *[F]): F[Unit] =
      for
        h <- /.take
        ((_, key), it) = h
        ((d, _), _) = it
        o <- d.tryGet
        _ <- ( if o eq None
               then
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
               else
                 %.update { m =>
                            val ^ = h._1._1
                            m + (^ + key -> (false, it))
                 }
             )
        _ <- *.offer(())
        _ <- Temporal[F].cede >> poll
      yield
        ()
