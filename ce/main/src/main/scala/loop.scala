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
import _root_.cats.syntax.flatMap.*
import _root_.cats.syntax.parallel.*
import _root_.cats.syntax.traverse.*

import _root_.cats.effect.{ IO, Deferred, ExitCode, FiberIO, Ref }
import _root_.cats.effect.std.{ CyclicBarrier, Queue, Semaphore }

import `Π-dump`.*
import `Π-stats`.*


package object `Π-loop`:

  private val spirsx = "pisc.stochastic.replications.exitcode.ignore"


  import sΠ.{ `Π-Map`, `Π-Set`, `()` }

  type <> = (Double, CyclicBarrier[IO], FiberIO[Unit], Ref[IO, `()`])

  type + = ((Deferred[IO, Option[<>]], Ref[IO, Deferred[IO, Option[<>]]]), ({}, Option[Either[Unit, Ref[IO, `()`]]], Rate))

  type % = Ref[IO, Map[String, Int | (Boolean, +)]]

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


  private def unblock(m: Map[String, Int | (Boolean, +)], k: String)
                     (implicit ^ : String): IO[Unit] =
    m(^ + k).asInstanceOf[(Boolean, +)]._2._1._1.complete(None).whenA(m.contains(^ + k))

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


  private def exit(ks: List[String])
                  (using % : %, ! : !): IO[Unit] =
    if ks.isEmpty
    then
      !.complete(ExitCode.Success).void
    else
      %.flatModify { m =>
          m -> (ks.traverse(m(_).asInstanceOf[(Boolean, +)]._2._1._1.complete(None)) >>
                ks.traverse(m(_).asInstanceOf[(Boolean, +)]._2._1._2 match { case null => IO.unit
                                                                             case it => it.get.flatMap(_.complete(None).void) }))
      }.as {
        if !sys.BooleanProp.keyExists(spirsx).value
        && ks.forall(_.charAt(36) == '!')
        then ExitCode.Success
        else ExitCode.Error
      } >>= (!.complete(_).void)


  def loop(parallelism: Int, started: Ref[IO, Long])
          (using % : %, ! : !, & : &, - : -, * : *)
          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    %.flatModify { m =>
      m -> {
        { if m.exists(_._2.isInstanceOf[Int])
          then Map.empty -> { () => false }
          else m
               .filter(_._2.asInstanceOf[(Boolean, +)]._1)
               .map(_ -> _.asInstanceOf[(Boolean, +)]._2._2)
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
          case (it: Map[String, ({}, Option[Either[Unit, Ref[IO, `()`]]], Rate)], exit) =>
            if it.isEmpty && !exit()
            then
              *.acquire >> loop(parallelism, started)
            else
              ∥(it)(`π-wand`._1)() match
                case Nil =>
                  (started.get product *.available).map(_ + _).flatMap { n =>
                    if n == 0L && exit()
                    then
                      this.exit(it.keys.toList)
                    else
                      *.acquire >> loop(parallelism, started)
                  }
                case nel =>
                  Semaphore[IO](parallelism).flatMap { sem =>
                    nel.parTraverse { case (key1, key2, in, delay) =>
                                        val k1 = key1.substring(36)
                                        val k2 = key2.substring(36)
                                        val  ^ = key1.substring(0, 36)
                                        val ^^ = key2.substring(0, 36)
                                        IO.uncancelable { _ =>
                                          for
                                            cb <- CyclicBarrier[IO](if k1 == k2 then 2 else 3)
                                            p1 <- %.modify { m => m -> m(key1).asInstanceOf[(Boolean, +)]._2 }
                                            p2 <- %.modify { m => m -> m(key2).asInstanceOf[(Boolean, +)]._2 }
                                            ((d1, c1), _) = p1
                                            ((d2, c2), _) = p2
                                            _  <- sem.acquire
                                            o1 <- d1.tryGet
                                            o2 <- d2.tryGet
                                            _  <- (discard(k1)(using  ^) >> %.update(_ - key1).whenA(c1 eq null)).whenA(o1 eq None)
                                            _  <- (discard(k2)(using ^^) >> %.update(_ - key2).whenA(c2 eq null)).whenA(o2 eq None).unlessA(k1 == k2)
                                            -- <- CyclicBarrier[IO](2)
                                            _  <- started.update(_ + 1)
                                            fb <- ( for
                                                      _ <- --.await.unlessA(c1 eq null)
                                                      _ <- --.await.unlessA(c2 eq null).unlessA(k1 == k2)
                                                      _ <- cb.await
                                                      _ <- enable(k1)
                                                      _ <- enable(k2).unlessA(k1 == k2)
                                                      _ <- sem.release
                                                      _ <- started.update(_ - 1)
                                                      _ <- *.release
                                                    yield
                                                      ()
                                                  ).start
                                            _  <- d1.complete(Some((delay, cb, fb, in))).whenA(o1 eq None)
                                            _  <- d2.complete(Some((delay, cb, fb, in))).whenA(o2 eq None).unlessA(k1 == k2)
                                            _  <- (c1.get.flatMap(_.complete(Some((delay, cb, fb, in))))
                                                >> %.update { m => m + (key1 -> (false, m(key1).asInstanceOf[(Boolean, +)]._2)) }
                                                >> --.await).unlessA(c1 eq null)
                                            _  <- (c2.get.flatMap(_.complete(Some((delay, cb, fb, in))))
                                                >> %.update { m => m + (key2 -> (false, m(key2).asInstanceOf[(Boolean, +)]._2)) }
                                                >> --.await).unlessA(c2 eq null).unlessA(k1 == k2)
                                          yield
                                            ()
                                        }
                                    }
                  } >> IO.cede >> loop(parallelism, started)
      }
    }

  def poll(using % : %, / : /, * : *): IO[Unit] =
    for
      h <- /.take
      ((_, key), it) = h
      ((d, _), _) = it
      _ <- d.tryGet.map(_ eq None).flatMap {
        if _
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
          } >> *.release
        else
          %.update { m =>
                     val ^ = h._1._1
                     m + (^ + key -> (false, it))
          }
      }
      _ <- IO.cede >> poll
    yield
      ()
