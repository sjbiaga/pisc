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
import _root_.cats.syntax.applicativeError.*
import _root_.cats.syntax.functor.*
import _root_.cats.syntax.flatMap.*
import _root_.cats.syntax.parallel.*
import _root_.cats.syntax.semigroupal.*
import _root_.cats.syntax.traverse.*

import _root_.cats.Parallel
import _root_.cats.effect.{ Deferred, Fiber, ExitCode, Ref, Resource, Temporal }
import _root_.cats.effect.std.{ AtomicCell, CyclicBarrier, Queue, Semaphore }
import _root_.cats.effect.syntax.monadCancel.*
import _root_.cats.effect.syntax.spawn.*
import _root_.cats.effect.syntax.temporal.*

import `Π-dump`.*
import `Π-stats`.*


package object `Π-loop`:

  private val spirsx = "pisc.stochastic.replications.exitcode.ignore"

  import sΠ.{ `Π-Map`, `Π-Set`, `()` }

  type <>[F[_]] = (CyclicBarrier[F], Fiber[F, Throwable, Unit], Ref[F, `()`[F]])

  type ++[F[_]] = (Deferred[F, Option[<>[F]]], Ref[F, Deferred[F, Option[<>[F]]]])
  type +[F[_]] = (++[F], ({}, Option[Either[Unit, Ref[F, `()`[F]]]], Rate))

  type %[F[_]] = AtomicCell[F, Map[String, Int | (Boolean, +[F])]]

  type ![F[_]] = Deferred[F, ExitCode]

  type &[F[_]] = Ref[F, Long]

  type /[F[_]] = Queue[F, ((String, String), +[F])]

  type \[F[_]] = F[Unit] => F[Unit]

  type ++++[F[_]] = (Double, Ref[F, `()`[F]], (++[F], ++[F]))
  type **[F[_]] = Queue[F, (() => Boolean, List[((String, String), ++++[F])])]

  type *[F[_]] = Semaphore[F]

  type ^[F[_]] = Resource[F, Unit]


  def `π-enable`[F[_]](enabled: `Π-Set`[String])
                      (using % : %[F]): F[Unit] =
    %.update(enabled.foldLeft(_) { (m, key) =>
                                    val n = if m.contains(key)
                                            then m(key).asInstanceOf[Int]
                                            else 0
                                    m + (key -> (n + 1))
                                 }
    )

  final class πloop[F[_]: Temporal: Parallel]:

    private def enable(key: String)
                      (using %[F])
                      (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): F[Unit] =
      val (_, spell) = `π-wand`
      `π-enable`(spell(key))


    private def unblock(map: Map[String, Int | (Boolean, +[F])], key: String)
                       (implicit ^ : String): F[Unit] =
      map(^ + key).asInstanceOf[(Boolean, +[F])]._2._1._1.complete(None).void

    private def `π-discard`(map: Map[String, Int | (Boolean, +[F])], discarded: `Π-Set`[String])
                           (implicit ^ : String): F[Set[String]] =
      discarded.toList.traverse(unblock(map, _)).as(discarded.map(^ + _))

    private def discard(key: String, map: Map[String, Int | (Boolean, +[F])])(using String)
                       (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): F[Set[String]] =
      val (trick, _) = `π-wand`
      if trick.contains(key)
      then
        `π-discard`(map, trick(key))
      else
        Temporal[F].pure(Set.empty)


    def peek(using % : %[F], ** : **[F])
            (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): F[Unit] =
      %.evalModify { m =>
        val it =
          if m.exists(_._2.isInstanceOf[Int])
          then Map.empty
          else m
               .filter(_._2.asInstanceOf[(Boolean, +[F])]._1)
               .map(_ -> _.asInstanceOf[(Boolean, +[F])]._2._2)
               .toMap
        val (trick, _) = `π-wand`
        def exit(mʹ: Map[String, Int | (Boolean, +[F])]) =
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
          **.offer(exit(m) -> Nil).map(m -> _)
        else
          val nel = ∥(it)(trick)()
          val nelʹ = nel.map {
            case (key1, key2, in, delay) =>
              val (dc1, _) = m(key1).asInstanceOf[(Boolean, +[F])]._2
              val (dc2, _) = m(key2).asInstanceOf[(Boolean, +[F])]._2
              (key1, key2) -> (delay, in, (dc1, dc2))
          }
          nel.traverse {
            case (key1, key2, _, _) =>
              val k1 = key1.substring(36)
              val k2 = key2.substring(36)
              val  ^ = key1.substring(0, 36)
              val ^^ = key2.substring(0, 36)
              val ((d1, c1), _) = m(key1).asInstanceOf[(Boolean, +[F])]._2
              val ((d2, c2), _) = m(key2).asInstanceOf[(Boolean, +[F])]._2
              for
                s1 <- d1.tryGet.map(_ eq None).flatMap { if _ then discard(k1, m)(using  ^) else Temporal[F].pure(Set.empty) }
                s2 <- if k1 == k2
                      then Temporal[F].pure(Set.empty)
                      else d2.tryGet.map(_ eq None).flatMap { if _ then discard(k2, m)(using ^^) else Temporal[F].pure(Set.empty) }
              yield
                (s1 ++ s2 ++ when(c1 eq null)(key1) ++ when(c2 eq null)(key2))
             -> (Nil ++ unless(c1 eq null)(key1) ++ unless(k1 == k2)(unless(c2 eq null)(key2)).flatten)
          }.map(_.foldRight(m) {
                  case ((ks, ls), map) =>
                    ls.map { key => key -> (false, map(key).asInstanceOf[(Boolean, +[F])]._2) }
                      .foldLeft(ks.foldLeft(map)(_ - _))(_ + _)
                }
          ).flatMap(mʹ => **.offer(exit(mʹ) -> nelʹ).map(mʹ -> _))
      }


    private def exit(using % : %[F], ! : ![F]): F[Unit] =
      %.get.flatMap { m =>
        val ks = m.keys.toList
        val ec =
          if ks.isEmpty
          then
            ExitCode.Success
          else
            if !sys.BooleanProp.keyExists(spirsx).value
            && ks.forall(_.charAt(36) == '!')
            then ExitCode.Success
            else ExitCode.Error
        ks.traverse(m(_).asInstanceOf[(Boolean, +[F])]._2._1._1.complete(None)) >>
        ks.traverse(m(_).asInstanceOf[(Boolean, +[F])]._2._1._2 match { case null => Temporal[F].unit
                                                                        case it => it.get.flatMap(_.complete(None).void) }) >> !.complete(ec).void
      }

    def loop(parallelism: Int, threshold: Int, timeout: Int, started: Ref[F, Long], batch: Ref[F, Long])
            (using % : %[F], ! : ![F], & : &[F], - : -[F], * : *[F], ** : **[F], ^ : ^[F])
            (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): F[Unit] =
      for
        _ <- (batch.set(0L) >> *.acquire.guarantee(batch.update(_ + 1)).replicateA_(threshold).timeout(timeout.nanoseconds).orElse(Temporal[F].unit)).whenA(threshold > 0)
        m  =
          for
            (exit, nel) <- **.take
            l                   <-
              if nel.isEmpty
              then
                if threshold > 0
                then
                  (started.get product batch.get).map(_ + _).flatMap {
                    case 0L if exit() =>
                      this.exit >> Temporal[F].pure(false)
                    case _ =>
                      Temporal[F].pure(true)
                  }
                else
                  (started.get product **.size.map(_.toLong)).map(_ + _).flatMap {
                    case 0L if exit() =>
                      this.exit >> Temporal[F].pure(false)
                    case _ =>
                      Temporal[F].pure(true)
                  }
              else
                Semaphore[F](parallelism).flatMap { sem =>
                  nel.parTraverse { case ((key1, key2), (delay, in, ((d1, c1), (d2, c2)))) =>
                                      val k1 = key1.substring(36)
                                      val k2 = key2.substring(36)
                                      Temporal[F].uncancelable { _ =>
                                        for
                                          cb <- CyclicBarrier[F](if k1 == k2 then 2 else 3)
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
                                                    _ <- peek.unlessA(threshold > 0)
                                                  yield
                                                    ()
                                                ).start
                                          _  <- d1.complete(Some((cb, fb, in)))
                                          _  <- d2.complete(Some((cb, fb, in))).unlessA(k1 == k2)
                                          _  <- c1.get.flatMap(_.complete(Some((cb, fb, in)))).unlessA(c1 eq null)
                                          _  <- c2.get.flatMap(_.complete(Some((cb, fb, in)))).unlessA(c2 eq null).unlessA(k1 == k2)
                                        yield
                                          ()
                                      }
                                  }
                } >> Temporal[F].pure(true)
          yield
            l
        l <- if threshold > 0
             then ^.use(_ => (*.available >>= *.acquireN) >> peek >> m)
             else m
        _ <- Temporal[F].cede >> loop(parallelism, threshold, timeout, started, batch).whenA(l)
      yield
        ()

    def poll(using % : %[F], / : /[F], \ : \[F]): F[Unit] =
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
        _ <- Temporal[F].cede >> poll
      yield
        ()
