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
import _root_.zio.interop.catz.generic.*
import _root_.zio.{ ExitCode, Fiber, Promise, Queue, Ref, UIO, ZIO }
import _root_.zio.concurrent.CyclicBarrier
import _root_.zio.stm.TSemaphore

import `Π-dump`.*
import `Π-stats`.*


package object `Π-loop`:

  private val barsx = "pisc.bioambients.replications.exitcode.ignore"


  import sΠ.{ `Π-Map`, `Π-Set`, Ordʹ, `π-$`, `π-ζ`, `)(`, `}{`, `()` }

  type <> = (CyclicBarrier, Fiber[Nothing, Unit], Ref[`()`])

  type + = ((Promise[Nothing, Option[<>]], Ref[Promise[Nothing, Option[<>]]]), ((`)(`, Ordʹ), ({}, Option[Either[Unit, Ref[`()`]]], Rate)))

  type % = Ref[Map[String, Int | (Boolean, +)]]

  type / = Queue[((String, String), +)]

  type ! = Promise[Nothing, ExitCode]

  type & = Ref[Long]

  type * = Semaphore[UIO]

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
                    (using % : %)
                    (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): UIO[Unit] =
    val (_, spell) = `π-wand`
    `π-enable`(spell(key))


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


  private def exit(ks: List[String])
                  (using % : %, ! : !): UIO[Unit] =
    if ks.isEmpty
    then
      !.succeed(ExitCode.success).unit
    else
      %.modify { m =>
        (ZIO.collectAllParDiscard(ks.map(m(_).asInstanceOf[(Boolean, +)]._2._1._1.succeed(None))) *>
         ZIO.collectAllParDiscard(ks.map(m(_).asInstanceOf[(Boolean, +)]._2._1._2 match { case null => ZIO.unit
                                                                                          case it => it.get.flatMap(_.succeed(None).unit) }))) -> m
      }.flatten.as {
        if !sys.BooleanProp.keyExists(barsx).value
        && ks.forall(_.charAt(36) == '!')
        then ExitCode.success
        else ExitCode.failure
      }.flatMap(!.succeed(_).unit)

  def loop(parallelism: Int, started: Ref[Long], _snapshot: Boolean)
          (using % : %, ! : !, & : &, - : -, * : *)
          (using `}{`.`][`, TSemaphore)
          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): UIO[Unit] =
    %.modify { m =>
      {
        { if m.exists(_._2.isInstanceOf[Int])
          then Map.empty -> { () => false }
          else m
               .filter(_._2.asInstanceOf[(Boolean, +)]._1)
               .map(_ -> _.asInstanceOf[(Boolean, +)]._2._2._2)
               .toMap
            -> { () => m.isEmpty
                    || m.forall(_._1.charAt(36) == '!')
                    && m.forall(_._2.asInstanceOf[(Boolean, +)]._1)
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
          case (it: Map[String, ({}, Option[Either[Unit, Ref[`()`]]], Rate)], exit) =>
            if it.isEmpty && !exit()
            then
              *.acquire *> loop(parallelism, started, _snapshot)
            else
              ∥(it)(`π-wand`._1)() match
                case Nil =>
                  (started.get <*> *.available).map(_ + _).flatMap { n =>
                    if n == 0L && exit()
                    then
                      this.exit(it.keys.toList)
                    else
                      *.acquire *> loop(parallelism, started, _snapshot)
                  }
                case nel =>
                  Semaphore[UIO](parallelism).flatMap { sem =>
                    ZIO.collectAllParDiscard {
                      nel.map { case (key1, key2, in, _delay) =>
                                  val k1 = key1.substring(36)
                                  val k2 = key2.substring(36)
                                  val  ^ = key1.substring(0, 36)
                                  val ^^ = key2.substring(0, 36)
                                  ZIO.uninterruptible {
                                    for
                                      cb <- CyclicBarrier.make(if k1 == k2 then 2 else 3)
                                      p1 <- %.modify { m => m(key1).asInstanceOf[(Boolean, +)]._2 -> m }
                                      p2 <- %.modify { m => m(key2).asInstanceOf[(Boolean, +)]._2 -> m}
                                      ((d1, c1), ((key, ord), _)) = p1
                                      ((d2, c2), ((keyʹ, ordʹ), _)) = p2
                                      _  <- sem.acquire
                                      f1 <- d1.isDone
                                      f2 <- d2.isDone
                                      _  <- (discard(k1)(using  ^) *> %.update(_ - key1).when(c1 eq null)).unless(f1)
                                      _  <- (discard(k2)(using ^^) *> %.update(_ - key2).when(c2 eq null)).unless(f2).unless(k1 == k2)
                                      -- <- CyclicBarrier.make(2)
                                      _  <- started.update(_ + 1)
                                      fb <- ( for
                                                _ <- --.await.exit.unless(c1 eq null)
                                                _ <- --.await.exit.unless(c2 eq null).unless(k1 == k2)
                                                _ <- ZIO.unless(k1 == k2) { (ord, ordʹ) match
                                                                              case (dir: `π-$`, dirʹ: `π-$`) =>
                                                                                `}{`.><.π(key, dir, keyʹ, dirʹ)
                                                                              case (cap: `π-ζ`, capʹ: `π-ζ`) =>
                                                                                `}{`.><.ζ(key, cap, keyʹ, capʹ)
                                                                          }
                                                _ <- cb.await.exit
                                                _ <- enable(k1)
                                                _ <- enable(k2).unless(k1 == k2)
                                                _ <- sem.release
                                                _ <- started.update(_ - 1)
                                                _ <- *.release
                                              yield
                                                ()
                                            ).fork
                                      _  <- d1.succeed(Some((cb, fb, in))).unless(f1)
                                      _  <- d2.succeed(Some((cb, fb, in))).unless(f2).unless(k1 == k2)
                                      _  <- ZIO.unless(c1 eq null)(c1.get.flatMap(_.succeed(Some((cb, fb, in))))
                                                                *> %.update { m => m + (key1 -> (false, m(key1).asInstanceOf[(Boolean, +)]._2)) }
                                                                *> --.await.exit)
                                      _  <- ZIO.unless(c2 eq null)(c2.get.flatMap(_.succeed(Some((cb, fb, in))))
                                                                *> %.update { m => m + (key2 -> (false, m(key2).asInstanceOf[(Boolean, +)]._2)) }
                                                                *> --.await.exit).unless(k1 == k2)
                                    yield
                                      ()
                                  }
                      }
                    }
                  } *> loop(parallelism, started, _snapshot)
      } -> m
    }.flatten

  def poll(using % : %, / : /, * : *): UIO[Unit] =
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
          %.update { m =>
                     val ^ = h._1._1
                     val n = m(key).asInstanceOf[Int] - 1
                     ( if n == 0
                       then
                         m - key
                       else
                         m + (key -> n)
                     ) + (^ + key -> (true, it))
          } *> *.release
      }
      _ <- poll
    yield
      ()
