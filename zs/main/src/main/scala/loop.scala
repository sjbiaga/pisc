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

import _root_.zio.{ ExitCode, Fiber, Promise, Queue, Ref, Semaphore, UIO, ZIO }
import _root_.zio.concurrent.CyclicBarrier
import _root_.zio.stm.TSemaphore

import `Π-dump`.*
import `Π-stats`.*


package object `Π-loop`:

  private val barsx = "pisc.bioambients.replications.exitcode.ignore"


  import sΠ.{ `Π-Map`, `Π-Set`, Ordʹ, `π-$`, `π-ζ`, `)(`, `}{`, >*< }

  type <> = (CyclicBarrier, Fiber[Throwable, Unit], Object)

  type + = ((Promise[Throwable, Option[<>]], Ref[Promise[Throwable, Option[<>]]]), ((`)(`, Ordʹ), ((>*< | Object, Int), Option[Boolean], Rate)))

  type % = Ref[Map[String, Int | (Boolean, +)]]

  type / = Queue[((String, String), +)]

  type ! = Promise[Throwable, ExitCode]

  type & = Ref[Long]

  type ~ = Semaphore

  type * = Queue[Unit]

  type \ = () => UIO[Unit]


  private def unblock(m: Map[String, Int | (Boolean, +)], k: String)
                     (implicit ^ : String): UIO[Unit] =
    if m.contains(^ + k)
    then m(^ + k).asInstanceOf[(Boolean, +)]._2._1._1.succeed(None).unit
    else ZIO.unit

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
    if trick.contains(key)
    then
      `π-discard`(trick(key))
    else
      ZIO.unit


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

  def loop(_snapshot: Boolean)
          (using % : %, ! : !, & : &, ~ : ~, - : -, * : *)
          (using `}{`.`][`, TSemaphore)
          (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): UIO[Unit] =
    %.modify { m =>
      {
        val (it: Map[String, ((>*< | Object, Int), Option[Boolean], Rate)], exit) =
          if m.exists(_._2.isInstanceOf[Int])
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
                               case (key2, (_, (_, (_, (e2, Some(p2), _))))) if e1 == e2 && p1 != p2 =>
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
          ZIO.debug(-200) *> *.take *> ZIO.debug(200) *> loop(_snapshot)
        else
          ∥(it)(`π-wand`._1)() match
            case Nil =>
              *.size.flatMap { n =>
                if n == 0 && exit()
                then
                  ZIO.debug(-9) *> this.exit(it.keys.toList)
                else
                  ZIO.debug(-20) *> *.take *> ZIO.debug(20) *> loop(_snapshot)
              }
            case nel =>
              ZIO.collectAllParDiscard {
                nel.map { case (key1, key2, _delay) =>
                            ZIO.uninterruptible {
                              val k1 = key1.substring(36)
                              val k2 = key2.substring(36)
                              val ^  = key1.substring(0, 36)
                              val ^^ = key2.substring(0, 36)
                              for
                                cb <- CyclicBarrier.make(if k1 == k2 then 2 else 3)
                                _  <- ~.withPermit {
                                  for
                                    tk <- if k1 == k2 then ZIO.succeed(null) else ZIO.succeed(new Object)
                                    p1 <- %.modify { m => m(key1).asInstanceOf[(Boolean, +)]._2 -> m }
                                    p2 <- %.modify { m => m(key2).asInstanceOf[(Boolean, +)]._2 -> m}
                                    ((d1, c1), ((key, ord), _)) = p1
                                    ((d2, c2), ((keyʹ, ordʹ), _)) = p2
                                    b1 <- d1.isDone
                                    b2 <- d2.isDone
                                    fb <- ( if k1 == k2 then ZIO.unit
                                            else
                                              (ord, ordʹ) match
                                                case (dir: `π-$`, dirʹ: `π-$`) =>
                                                  `}{`.><.π(key, dir, keyʹ, dirʹ)
                                                case (cap: `π-ζ`, capʹ: `π-ζ`) =>
                                                  `}{`.><.ζ(key, cap, keyʹ, capʹ)
                                          ).fork
                                    _  <- if !b1 then discard(k1)(using ^) *> (if k1.charAt(0) != '!' || (c1 eq null) then %.update(_ - key1) else ZIO.unit) *> d1.succeed(Some((cb, fb, tk))).unit
                                          else ZIO.unit
                                    _  <- if k1 == k2 then ZIO.unit
                                          else if !b2 then discard(k2)(using ^^) *> (if k2.charAt(0) != '!' || (c2 eq null) then %.update(_ - key2) else ZIO.unit) *> d2.succeed(Some((cb, fb, tk))).unit
                                          else ZIO.unit
                                    _  <- if k1.charAt(0) == '!' && (c1 ne null) then c1.get.tap(_.succeed(Some((cb, fb, tk)))).unit
                                          else ZIO.unit
                                    _  <- if k1 == k2 then ZIO.unit
                                          else if k2.charAt(0) == '!' && (c2 ne null) then c2.get.tap(_.succeed(Some((cb, fb, tk)))).unit
                                          else ZIO.unit
                                  yield
                                    ()
                                }
                                _  <- ZIO.debug(-10)
                                _  <- cb.await.exit
                                _  <- ZIO.debug(10)
                              yield
                                ()
                            }
                }
              } *> ZIO.debug(-60) *> loop(_snapshot)
      } -> m
    }.flatten

  def poll(using % : %, / : /, * : *): UIO[Unit] =
    for
      _ <- ZIO.debug(-50)
      h <- /.take
      _ <- ZIO.debug(50)
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
      _ <- *.offer(())
      _ <- poll
    yield
      ()
