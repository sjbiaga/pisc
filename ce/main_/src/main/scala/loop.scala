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

import _root_.cats.instances.list.*
import _root_.cats.syntax.applicative.*
import _root_.cats.syntax.flatMap.*
import _root_.cats.syntax.parallel.*
import _root_.cats.syntax.traverse.*

import _root_.cats.effect.{ IO, Clock, Deferred, ExitCode, FiberIO, Ref }
import _root_.cats.effect.std.{ CyclicBarrier, Queue, Semaphore }

import `Π-dump`.*
import `Π-stats`.*


package object `Π-loop`:

  import sΠ.{ `Π-Map`, `Π-Set`, Ordʹ, `π-$`, `π-ζ`, `)(`, `()` }

  type <> = (Double, CyclicBarrier[IO], FiberIO[Unit], Ref[IO, `()`])

  type + = (Deferred[IO, Option[<>]], (Long, ((`)(`, Ordʹ), ({}, Option[Either[Unit, Ref[IO, `()`]]], Rate))))

  type % = Ref[IO, Map[String, Int | +]]

  type ! = Deferred[IO, ExitCode]

  type & = Ref[IO, Long]

  type * = Semaphore[IO]

  type / = Queue[IO, ((String, String), +)]

  type \ = IO[Unit]


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


  private def unblock(m: Map[String, Int | +], k: String)
                     (implicit ^ : String): IO[Unit] =
    if m.contains(^ + k)
    then m(^ + k).asInstanceOf[+]._1.complete(None).void
    else IO.unit

  private def `π-discard`(discarded: `Π-Set`[String])
                         (using % : %)
                         (implicit ^ : String): IO[Unit] =
    for
      m <- %.get
      _ <- discarded.toList.traverse(unblock(m, _)).void
      _ <- %.update(discarded.map(^ + _).foldLeft(_)(_ - _))
    yield
      ()

  private def discard(key: String)(using ^ : String)
                     (using %)
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    val (trick, _) = `π-wand`
    `π-discard`(trick(key)).whenA(trick.contains(key))


  def loop(parallelism: Int, snapshot: Boolean, started: Ref[IO, Long], `}{`: sΠ.`}{`)
          (using % : %, / : /, ! : !, & : &, - : -, * : *)
          (using `][`: `}{`.`][`, `1`: `}{`.stm.TSemaphore)
          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    %.flatModify { m =>
      m -> {
        { if m.exists(_._2.isInstanceOf[Int])
          then Map.empty -> { () => false }
          else m
               .map(_ -> _.asInstanceOf[+]._2._2._2)
               .toMap
            -> { () => m.isEmpty
                    || m.keys.forall(_.charAt(36) == '!')
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
          case (it: Map[String, ({}, Option[Either[Unit, Ref[IO, `()`]]], Rate)], exit) =>
            if it.isEmpty && !exit()
            then
              *.acquire >> loop(parallelism, snapshot, started, `}{`)
            else
              ∥(it)(`π-wand`._1)() match
                case Nil =>
                  (started.get product *.available).map(_ + _).flatMap { n =>
                    if n == 0L && exit()
                    then
                      -.offer(it.keys.toList)
                    else
                      *.acquire >> loop(parallelism, snapshot, started, `}{`)
                  }
                case nel =>
                  Semaphore[IO](parallelism).flatMap { sem =>
                    nel.parTraverse { case (key1, key2, in, (delay, duration)) =>
                                      val k1 = key1.substring(36)
                                      val k2 = key2.substring(36)
                                      val  ^ = key1.substring(0, 36)
                                      val ^^ = key2.substring(0, 36)
                                      IO.uncancelable { _ =>
                                        for
                                          cb <- CyclicBarrier[IO](if k1 == k2 then 2 else 3)
                                          p1 <- %.modify { m => m -> m(key1).asInstanceOf[+] }
                                          p2 <- %.modify { m => m -> m(key2).asInstanceOf[+] }
                                          (d1, (ts1, ((key, ord), _))) = p1
                                          (d2, (ts2, ((keyʹ, ordʹ), _))) = p2
                                          _  <- sem.acquire
                                          _  <- discard(k1)(using  ^)
                                          _  <- discard(k2)(using ^^).unlessA(k1 == k2)
                                          _  <- %.update(_ - key1 - key2)
                                          _  <- started.update(_ + 1)
                                          fb <- ( for
                                                    (slabel, _)  <- `}{`.stm.commit { `}{`.`}{`(key) }
                                                    (slabelʹ, _) <- `}{`.stm.commit { `}{`.`}{`(keyʹ) }
                                                    _            <- `}{`.stm.commit { `1`.acquire }.whenA(k1 == k2)
                                                    _            <- { (ord, ordʹ) match
                                                                        case (dir: `π-$`, dirʹ: `π-$`) =>
                                                                          `}{`.><.π(key, dir, keyʹ, dirʹ)
                                                                        case (cap: `π-ζ`, capʹ: `π-ζ`) =>
                                                                          `}{`.><.ζ(key, cap, keyʹ, capʹ)
                                                                    }.unlessA(k1 == k2)
                                                    elabel       <- `}{`.stm.commit { `}{`.`}{`(key, snapshot) }
                                                    (elabelʹ, _) <- `}{`.stm.commit { `}{`.`}{`(keyʹ) }
                                                    _            <- `}{`.stm.commit { `1`.release }
                                                    _            <- cb.await
                                                    _            <- enable(k1)
                                                    _            <- enable(k2).unlessA(k1 == k2)
                                                    no           <- &.updateAndGet(_ + 1)
                                                    ts           <- Clock[IO].monotonic.map(_.toNanos)
                                                    _            <- -.offer((no, ((ts1, ts2), ts), (k1, k2), (delay, duration), (slabel -> elabel, slabelʹ -> (elabelʹ -> elabel._2))))
                                                    _            <- sem.release
                                                    _            <- started.update(_ - 1)
                                                    _            <- *.release
                                                  yield
                                                    ()
                                                ).start
                                          _  <- d1.complete(Some((delay, cb, fb, in)))
                                          _  <- d2.complete(Some((delay, cb, fb, in))).unlessA(k1 == k2)
                                        yield
                                          ()
                                      }
                                    }
                  } >> IO.cede >> loop(parallelism, snapshot, started, `}{`)
      }
    }

  def poll(using % : %, / : /, * : *): IO[Unit] =
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
      _ <- IO.cede >> poll
    yield
      ()
