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

import _root_.scala.collection.immutable.Map

import _root_.cats.effect.std.Semaphore
import _root_.zio.interop.catz.generic.*
import _root_.zio.{ Clock, ExitCode, Fiber, Promise, Queue, Ref, UIO, ZIO }
import _root_.zio.concurrent.CyclicBarrier

import `Π-dump`.*
import `Π-stats`.*


package object `Π-loop`:

  import sΠ.{ `Π-Map`, `Π-Set`, `()` }

  type <> = (Double, CyclicBarrier, Fiber[Nothing, Unit], Ref[`()`])

  type + = (Promise[Nothing, Option[<>]], (Long, ({}, Option[Either[Unit, Ref[`()`]]], Rate)))

  type % = Ref[Map[String, Int | +]]

  type ! = Promise[Nothing, ExitCode]

  type & = Ref[Long]

  type * = Semaphore[UIO]

  type / = Queue[((String, String), +)]

  type \ = UIO[Unit]


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


  private def unblock(m: Map[String, Int | +], k: String)
                     (implicit ^ : String): UIO[Unit] =
    ZIO.when(m.contains(^ + k))(m(^ + k).asInstanceOf[+]._1.succeed(None)).unit

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


  def loop(parallelism: Int, started: Ref[Long])
          (using % : %, ! : !, & : &, - : -, * : *)
          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): UIO[Unit] =
    %.modify { m =>
      {
        { if m.exists(_._2.isInstanceOf[Int])
          then Map.empty -> { () => false }
          else m
               .map(_ -> _.asInstanceOf[+]._2._2)
               .toMap
            -> { () => m.isEmpty
                    || m.keys.forall(_.charAt(36) == '!')
                    && { val (trick, _) = `π-wand`
                         m.forall {
                           case (key1, (_, (_, (e1, Some(p1), _)))) =>
                             val ^ = key1.substring(0, 36)
                             !m.exists {
                               case (key2, (_, (_, (e2, Some(p2), _)))) if (e1 eq e2) && p1.isLeft == p2.isRight =>
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
              *.acquire *> loop(parallelism, started)
            else
              ∥(it)(`π-wand`._1)() match
                case Nil =>
                  (started.get <*> *.available).map(_ + _).flatMap { n =>
                    if n == 0L && exit()
                    then
                      -.offer(it.keys.toList).unit
                    else
                      *.acquire *> loop(parallelism, started)
                  }
                case nel =>
                  Semaphore[UIO](parallelism).flatMap { sem =>
                    ZIO.collectAllParDiscard {
                      nel.map { case (key1, key2, in, (delay, duration)) =>
                                  val k1 = key1.substring(36)
                                  val k2 = key2.substring(36)
                                  val  ^ = key1.substring(0, 36)
                                  val ^^ = key2.substring(0, 36)
                                  ZIO.uninterruptible {
                                    for
                                      cb <- CyclicBarrier.make(if k1 == k2 then 2 else 3)
                                      p1 <- %.modify { m => m(key1).asInstanceOf[+] -> m }
                                      p2 <- %.modify { m => m(key2).asInstanceOf[+] -> m }
                                      (d1, (s1, _)) = p1
                                      (d2, (s2, _)) = p2
                                      _  <- sem.acquire
                                      _  <- discard(k1)(using  ^)
                                      _  <- discard(k2)(using ^^).unless(k1 == k2)
                                      _  <- %.update(_ - key1 - key2)
                                      _  <- started.update(_ + 1)
                                      fb <- ( for
                                                _  <- cb.await.exit
                                                _  <- enable(k1)
                                                _  <- enable(k2).unless(k1 == k2)
                                                no <- &.updateAndGet(_ + 1)
                                                now <- Clock.nanoTime
                                                _  <- -.offer((no, ((s1, s2), now), (k1, k2), (delay, duration)))
                                                _  <- sem.release
                                                _  <- started.update(_ - 1)
                                                _  <- *.release
                                              yield
                                                ()
                                            ).fork
                                      _  <- d1.succeed(Some((delay, cb, fb, in)))
                                      _  <- d2.succeed(Some((delay, cb, fb, in))).unless(k1 == k2)
                                    yield
                                      ()
                                  }
                      }
                    }
                  } *> loop(parallelism, started)
      } -> m
    }.flatten

  def poll(using % : %, / : /, * : *): UIO[Unit] =
    for
      h <- /.take
      ((_, key), it) = h
      _ <- %.update { m =>
                      val ^ = h._1._1
                      val n = m(key).asInstanceOf[Int] - 1
                      ( if n == 0
                        then
                          m - key
                        else
                          m + (key -> n)
                      ) + (^ + key -> it)
           }
      _ <- *.release
      _ <- poll
    yield
      ()
