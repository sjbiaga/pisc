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


import _root_.scala.collection.immutable.Map

import _root_.cats.instances.list.*
import _root_.cats.syntax.flatMap.*
import _root_.cats.syntax.parallel.*
import _root_.cats.syntax.traverse.*

import _root_.cats.effect.{ IO, Clock, Deferred, ExitCode, FiberIO, Ref }
import _root_.cats.effect.std.{ CyclicBarrier, Queue, Semaphore }

import `Π-dump`.*
import `Π-stats`.*


package object `Π-loop`:

  import sΠ.{ `Π-Map`, `Π-Set`, >< }

  type <> = (Double, CyclicBarrier[IO], FiberIO[Unit])

  type + = (Deferred[IO, Option[<>]], (Long, (><, Option[Boolean], Rate)))

  type % = Ref[IO, Map[String, Int | +]]

  type ! = Deferred[IO, ExitCode]

  type & = Ref[IO, Long]

  type * = Queue[IO, Unit]

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
    if trick.contains(key)
    then
      `π-discard`(trick(key))
    else
      IO.unit


  def loop(parallelism: Int, started: Ref[IO, Long])
          (using % : %, ! : !, & : &, - : -, * : *)
          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    %.flatModify { m =>
      m -> started.get.map { n =>
        if n > 0
        || m.exists(_._2.isInstanceOf[Int])
        then Map.empty -> false
        else m
             .map(_ -> _.asInstanceOf[+]._2._2)
             .toMap
          -> m.forall(_._1.charAt(36) == '!')
      }
    } >>= {
      case (it, exit) =>
        if !exit && it.isEmpty
        then
          *.take >> loop(parallelism, started)
        else
          ∥(it)(`π-wand`._1)() match
            case Nil =>
              (started.get product *.size).flatMap { (n, m) =>
                if n + m == 0
                then
                  -.offer(it.keys.toList)
                else
                  *.take >> loop(parallelism, started)
              }
            case nel =>
              Semaphore[IO](parallelism).flatMap { sem =>
                nel.parTraverse { case (key1, key2, (delay, duration)) =>
                                  IO.uncancelable { _ =>
                                    val k1 = key1.substring(36)
                                    val k2 = key2.substring(36)
                                    val ^  = key1.substring(0, 36)
                                    val ^^ = key2.substring(0, 36)
                                    for
                                      -- <- CyclicBarrier[IO](if k1 == k2 then 2 else 3)
                                      p1 <- %.modify { m => m -> m(key1).asInstanceOf[+] }
                                      p2 <- %.modify { m => m -> m(key2).asInstanceOf[+] }
                                      (d1, (ts1, _)) = p1
                                      (d2, (ts2, _)) = p2
                                      _  <- discard(k1)(using ^)
                                      _  <- if k1 == k2 then IO.unit else discard(k2)(using ^^)
                                      _  <- %.update(_ - key1 - key2)
                                      tD <- Deferred[IO, Long]
                                      nD <- Deferred[IO, Long]
                                      _  <- started.update(_ + 1)
                                      fb <- ( for
                                                _  <- --.await
                                                ts <- tD.get
                                                no <- nD.get
                                                _  <- -.offer((no, ((ts1, ts2), ts), (k1, k2), (delay, duration)))
                                                _  <- enable(k1)
                                                _  <- if k1 == k2 then IO.unit else enable(k2)
                                                _  <- started.update(_ - 1)
                                                _  <- *.offer(())
                                              yield
                                                ()
                                            ).start
                                      _  <- sem.acquire
                                      _  <- d1.complete(Some((delay, --, fb)))
                                      _  <- if k1 == k2 then IO.unit else d2.complete(Some((delay, --, fb)))
                                      ts <- Clock[IO].monotonic.map(_.toNanos)
                                      no <- &.updateAndGet(_ + 1)
                                      _  <- tD.complete(ts)
                                      _  <- nD.complete(no)
                                      _  <- sem.release
                                    yield
                                      ()
                                  }
                                } >> loop(parallelism, started)
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
      _ <- *.offer(())
      _ <- IO.cede >> poll
    yield
      ()
