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

import _root_.cats.effect.std.Semaphore

import _root_.zio.{ Clock, ExitCode, Fiber, Promise, Queue, Ref, Semaphore => SemaphoreZIO, Task, UIO, ZIO }
import _root_.zio.concurrent.CyclicBarrier
import _root_.zio.stm.TSemaphore

import `Π-dump`.*
import `Π-stats`.*


package object `Π-loop`:

  private val barsx = "pisc.bioambients.replications.exitcode.ignore"


  import sΠ.{ `Π-Map`, `Π-Set`, Ordʹ, `π-$`, `π-ζ`, `)(`, `}{`, `()` }

  type <> = (CyclicBarrier, Fiber[Throwable, Unit], Ref[`()`])

  type + = ((Promise[Throwable, Option[<>]], Ref[Promise[Throwable, Option[<>]]]), (Ref[Long], ((`)(`, Ordʹ), ({}, Option[Either[Unit, Ref[`()`]]], Rate))))

  type % = Ref[Map[String, Int | (Boolean, +)]]

  type / = Queue[((String, String), +)]

  type ! = Promise[Throwable, ExitCode]

  type & = Ref[Long]

  type ~ = SemaphoreZIO

  type * = Semaphore[Task]

  type \ = () => Task[Unit]


  private def unblock(m: Map[String, Int | (Boolean, +)], k: String)
                     (implicit ^ : String): UIO[Unit] =
    m(^ + k).asInstanceOf[(Boolean, +)]._2._1._1.succeed(None).when(m.contains(^ + k)).unit

  private def `π-discard`(discarded: `Π-Set`[String])
                         (using % : %)
                         (implicit ^ : String): UIO[Unit] =
    for
      m <- %.get
      _ <- ZIO.collectAllParDiscard(discarded.toList.map(unblock(m, _)))
      _ <- %.update(discarded.map(^ + _).foldLeft(_)(_ - _))
    yield
      ()

  private def discard(key: String)(using ^ : String)
                     (using %)
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): UIO[Unit] =
    val (trick, _) = `π-wand`
    ZIO.when(trick.contains(key))(`π-discard`(trick(key))).unit


  def loop(snapshot: Boolean)
          (using % : %, ! : !, & : &, ~ : ~, - : -, * : *)
          (using `][`: `}{`.`][`, `2`: TSemaphore)
          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): Task[Unit] =
    %.modify { m =>
      {
        { if m.exists(_._2.isInstanceOf[Int])
          then Map.empty -> { () => false }
          else m
               .filter(_._2.asInstanceOf[(Boolean, +)]._1)
               .map(_ -> _.asInstanceOf[(Boolean, +)]._2._2._2._2)
               .toMap
            -> { () => m.isEmpty
                    || m.forall(_._1.charAt(36) == '!')
                    && m.forall(_._2.asInstanceOf[(Boolean, +)]._1)
                    && { val (trick, _) = `π-wand`
                         m.forall {
                           case (key1, (_, (_, (_, (_, (e1, Some(p1), _)))))) =>
                             val ^ = key1.substring(0, 36)
                             !m.exists {
                               case (key2, (_, (_, (_, (_, (e2, Some(p2), _)))))) if (e1 eq e2) && p1.isLeft == p2.isRight =>
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
          case (it: Map[String, ({}, Option[Either[Unit, Ref[`()`]]], Rate)], exit) =>
            if it.isEmpty && !exit()
            then
              *.acquire *> loop(snapshot)
            else
              ∥(it)(`π-wand`._1)() match
                case Nil =>
                  *.available.flatMap { n =>
                    if n == 0L && exit()
                    then
                      -.offer(it.keys.toList).unit
                    else
                      *.acquire *> loop(snapshot)
                  }
                case nel =>
                  ZIO.collectAllParDiscard {
                    nel.map { case (key1, key2, in, (delay, duration)) =>
                              val k1 = key1.substring(36)
                              val k2 = key2.substring(36)
                              val  ^ = key1.substring(0, 36)
                              val ^^ = key2.substring(0, 36)
                              ZIO.uninterruptible {
                                for
                                  cb <- CyclicBarrier.make(if k1 == k2 then 2 else 3)
                                  sp1 <- Promise.make[Throwable, (String, (String, String))]
                                  sp2 <- Promise.make[Throwable, (String, (String, String))]
                                  p1 <- %.modify { m => m(key1).asInstanceOf[(Boolean, +)]._2 -> m }
                                  p2 <- %.modify { m => m(key2).asInstanceOf[(Boolean, +)]._2 -> m}
                                  ((d1, c1), (ts1, ((key, ord), _))) = p1
                                  ((d2, c2), (ts2, ((keyʹ, ordʹ), _))) = p2
                                  _  <- ~.withPermit {
                                    for
                                      b1 <- d1.isDone
                                      b2 <- d2.isDone
                                      fb <- ( for
                                                (slabel, _)  <- `}{`.`}{`(key).commit
                                                (slabelʹ, _) <- `}{`.`}{`(keyʹ).commit
                                                _            <- `2`.acquireN(2).commit.when(k1 == k2)
                                                _            <- ZIO.unless(k1 == k2) { (ord, ordʹ) match
                                                                                         case (dir: `π-$`, dirʹ: `π-$`) =>
                                                                                           `}{`.><.π(key, dir, keyʹ, dirʹ)
                                                                                         case (cap: `π-ζ`, capʹ: `π-ζ`) =>
                                                                                           `}{`.><.ζ(key, cap, keyʹ, capʹ)
                                                                                     }
                                                elabel       <- `}{`.`}{`(key, snapshot).commit
                                                (elabelʹ, _) <- `}{`.`}{`(keyʹ).commit
                                                _            <- sp1.succeed(slabel -> elabel)
                                                _            <- sp2.succeed(slabelʹ -> (elabelʹ -> elabel._2)).unless(k1 == k2)
                                                _            <- `2`.releaseN(2).commit.when(k1 == k2)
                                              yield
                                                ()
                                            ).fork
                                      _  <- (discard(k1)(using  ^) *> %.update(_ - key1).when(c1 eq null) *> d1.succeed(Some((cb, fb, in)))).unless(b1)
                                      _  <- (discard(k2)(using ^^) *> %.update(_ - key2).when(c2 eq null) *> d2.succeed(Some((cb, fb, in)))).unless(b2).unless(k1 == k2)
                                      _  <- ZIO.unless(c1 eq null)(c1.get.tap(_.succeed(Some((cb, fb, in)))))
                                      _  <- ZIO.unless(c2 eq null)(c2.get.tap(_.succeed(Some((cb, fb, in))))).unless(k1 == k2)
                                    yield
                                      ()
                                  }
                                  _  <- cb.await.exit
                                  no <- &.updateAndGet(_ + 1)
                                  ss <- ts1.get <*> ts2.get
                                  now <- Clock.nanoTime
                                  _  <- -.offer((no, (ss, now), (k1, k2), (delay, duration), (sp1, sp2)))
                                yield
                                  ()
                              }
                    }
                  } *> loop(snapshot)
      } -> m
    }.flatten

  def poll(using % : %, / : /, * : *): Task[Unit] =
    for
      h <- /.take
      ((_, key), it) = h
      ((p, _), _) = it
      b <- p.isDone
      _ <- ( if !b
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
      _ <- *.release
      _ <- poll
    yield
      ()
