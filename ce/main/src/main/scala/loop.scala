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
import _root_.scala.concurrent.duration.*
import _root_.scala.Option.{ unless, when }

import _root_.cats.instances.list.*
import _root_.cats.syntax.applicative.*
import _root_.cats.syntax.flatMap.*
import _root_.cats.syntax.parallel.*
import _root_.cats.syntax.traverse.*

import _root_.cats.effect.{ IO, Deferred, ExitCode, FiberIO, Ref, Resource }
import _root_.cats.effect.std.{ AtomicCell, CyclicBarrier, Queue, Semaphore }

import `Π-dump`.*
import `Π-stats`.*


package object `Π-loop`:

  private val spirsx = "pisc.stochastic.replications.exitcode.ignore"


  import sΠ.{ `Π-Map`, `Π-Set`, `()` }

  type <> = (Double, CyclicBarrier[IO], FiberIO[Unit], Ref[IO, `()`])

  type ++ = (Deferred[IO, Option[<>]], Ref[IO, Deferred[IO, Option[<>]]])
  type + = (++, ({}, Option[Either[Unit, Ref[IO, `()`]]], Rate))

  type % = AtomicCell[IO, Map[String, Int | (Boolean, +)]]

  type ! = Deferred[IO, ExitCode]

  type & = Ref[IO, Long]

  type / = Queue[IO, ((String, String), +)]

  type \ = IO[Unit] => IO[Unit]

  type ++++ = (Double, Ref[IO, `()`], (++, ++))
  type ** = Queue[IO, ((Set[String], () => Boolean), List[((String, String), ++++)])]

  type * = Semaphore[IO]

  type ^ = Resource[IO, Unit]


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


  private def unblock(map: Map[String, Int | (Boolean, +)], key: String)
                     (implicit ^ : String): IO[Unit] =
    map(^ + key).asInstanceOf[(Boolean, +)]._2._1._1.complete(None).void

  private def `π-discard`(map: Map[String, Int | (Boolean, +)], discarded: `Π-Set`[String])
                         (implicit ^ : String): IO[Set[String]] =
    discarded.toList.traverse(unblock(map, _)).as(discarded.map(^ + _))

  private def discard(key: String, map: Map[String, Int | (Boolean, +)])(using String)
                     (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Set[String]] =
    val (trick, _) = `π-wand`
    if trick.contains(key)
    then
      `π-discard`(map, trick(key))
    else
      IO.pure(Set.empty)


  def peek(using % : %, ** : **)
          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    %.evalModify { m =>
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
          if it.isEmpty
          then
            **.offer((Set.empty -> exit, Nil)).map(m -> _)
          else
            val nel = ∥(it)(`π-wand`._1)()
            val nelʹ = nel.map {
              case (key1, key2, in, delay) =>
                val (dc1, _) = m(key1).asInstanceOf[(Boolean, +)]._2
                val (dc2, _) = m(key2).asInstanceOf[(Boolean, +)]._2
                (key1, key2) -> (delay, in, (dc1, dc2))
            }
            nel.traverse {
              case (key1, key2, _, _) =>
                val k1 = key1.substring(36)
                val k2 = key2.substring(36)
                val  ^ = key1.substring(0, 36)
                val ^^ = key2.substring(0, 36)
                val ((d1, c1), _) = m(key1).asInstanceOf[(Boolean, +)]._2
                val ((d2, c2), _) = m(key2).asInstanceOf[(Boolean, +)]._2
                for
                  s1 <- d1.tryGet.map(_ eq None).flatMap { if _ then discard(k1, m)(using  ^) else IO.pure(Set.empty) }
                  s2 <- d2.tryGet.map(_ eq None).flatMap { if _ then discard(k2, m)(using ^^) else IO.pure(Set.empty) }
                yield
                  (s1 ++ s2 ++ when(c1 eq null)(key1) ++ when(c2 eq null)(key2))
               -> (Nil ++ unless(c1 eq null)(key1) ++ unless(k1 == k2)(unless(c2 eq null)(key2)).flatten)
            }.map(_.foldRight(m) {
                    case ((ks, ls), map) =>
                      ls.map { key => key -> (false, map(key).asInstanceOf[(Boolean, +)]._2) }
                        .foldLeft(ks.foldLeft(map)(_ - _))(_ + _)
                  }
            ).flatMap(mʹ => **.offer((it.keySet -> exit, nelʹ)).map(mʹ -> _))
    }


  private def exit(ks: List[String])
                  (using % : %, ! : !): IO[Unit] =
    if ks.isEmpty
    then
      !.complete(ExitCode.Success).void
    else
      %.get.flatMap { m =>
        ks.traverse(m(_).asInstanceOf[(Boolean, +)]._2._1._1.complete(None)) >>
        ks.traverse(m(_).asInstanceOf[(Boolean, +)]._2._1._2 match { case null => IO.unit
                                                                     case it => it.get.flatMap(_.complete(None).void) })
      }.as {
        if !sys.BooleanProp.keyExists(spirsx).value
        && ks.forall(_.charAt(36) == '!')
        then ExitCode.Success
        else ExitCode.Error
      } >>= (!.complete(_).void)

  def loop(parallelism: Int, threshold: Int, timeout: Int, started: Ref[IO, Long], batch: Ref[IO, Boolean])
          (using % : %, ! : !, & : &, - : -, * : *, ** : **, ^ : ^)
          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): IO[Unit] =
    for
      _ <- (batch.set(false) >> *.acquire.guarantee(batch.set(true)).replicateA_(threshold).timeout(timeout.nanoseconds).orElse(IO.unit)).whenA(threshold > 0)
      m  =
        for
          ((keys, exit), nel) <- **.take
          l                   <-
            if nel.isEmpty
            then
              (started.get product (if threshold > 0 then *.available else **.size.map(_.toLong))).map(_ + _).flatMap {
                case 0L if exit() =>
                  this.exit(keys.toList) >> IO.pure(false)
                case _ =>
                  IO.pure(true)
              }
            else
              Semaphore[IO](parallelism).flatMap { sem =>
                nel.parTraverse { case ((key1, key2), (delay, in, ((d1, c1), (d2, c2)))) =>
                                    val k1 = key1.substring(36)
                                    val k2 = key2.substring(36)
                                    IO.uncancelable { _ =>
                                      for
                                        cb <- CyclicBarrier[IO](if k1 == k2 then 2 else 3)
                                        _  <- sem.acquire
                                        _  <- started.update(_ + 1)
                                        fb <- ( for
                                                  _ <- cb.await
                                                  e  = ( for
                                                           _ <- enable(k1)
                                                           _ <- enable(k2).unlessA(k1 == k2)
                                                         yield
                                                           ()
                                                       )
                                                  _ <- if threshold > 0
                                                       then e
                                                       else ^.use(_ => e >> peek)
                                                  _ <- sem.release
                                                  _ <- started.update(_ - 1)
                                                yield
                                                  ()
                                              ).start
                                        _  <- d1.complete(Some((delay, cb, fb, in)))
                                        _  <- d2.complete(Some((delay, cb, fb, in))).unlessA(k1 == k2)
                                        _  <- c1.get.flatMap(_.complete(Some((delay, cb, fb, in)))).unlessA(c1 eq null)
                                        _  <- c2.get.flatMap(_.complete(Some((delay, cb, fb, in)))).unlessA(c2 eq null).unlessA(k1 == k2)
                                      yield
                                        ()
                                    }
                                }
              } >> IO.pure(true)
        yield
          l
      l <- if threshold > 0
           then (batch.get product started.get)
                .map(_ || _ == 0L)
                .ifM(^.use(_ => peek *> m <* (*.available >>= *.acquireN)), IO.pure(true))
           else m
      _ <- loop(parallelism, threshold, timeout, started, batch).whenA(l)
    yield
      ()

  def poll(using % : %, / : /, \ : \): IO[Unit] =
    for
      h <- /.take
      ((_, key), it) = h
      ((d, _), _) = it
      _ <- d.tryGet.map(_ eq None).flatMap {
        if _
        then
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
        else
          %.update { m =>
                     val ^ = h._1._1
                     m + (^ + key -> (false, it))
          }
      }
      _ <- IO.cede >> poll
    yield
      ()
