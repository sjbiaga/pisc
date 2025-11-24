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

import _root_.scala.concurrent.Promise
import _root_.org.apache.pekko.actor.typed.scaladsl.Behaviors
import _root_.org.apache.pekko.actor.typed.{ ActorRef, Behavior }

import `Π-dump`.*
import `Π-stats`.*


package object `Π-loop`:

  import sΠ.{ `Π-Map`, `Π-Set`, >< }

  type + = (Promise[Option[Double]], (>< | Object, Option[Boolean], Rate))

  type % = ActorRef[Loop]

  enum Loop:

    private case Trigger
    case Enqueue(^ : String, key: String, it: +)
    case Exclude(enabled: `Π-Set`[String])

  object Loop:

    def apply(parallelism: Int)
             (dump: ActorRef[-])
             (% : Map[String, Int | +])
             (implicit `π-wand`: (`Π-Map`[String, `Π-Set`[String]], `Π-Map`[String, `Π-Set`[String]])): Behavior[Loop] =

      Behaviors.receive[Loop] {

        case (context, Trigger) =>

          if %.exists(_._2.isInstanceOf[Int])
          then
            Behaviors.same

          else
            val it = %.map(_ -> _.asInstanceOf[+]._2).toMap

            ∥(it)(`π-wand`._1)() match

              case Nil =>

                if %.isEmpty ||
                   %.keys.forall(_.charAt(36) == '!')
                && { val (trick, _) = `π-wand`
                     %.forall {
                       case (key1, (_, (e1, Some(p1), _))) =>
                         val ^ = key1.substring(0, 36)
                         ! %.exists {
                           case (key2, (_, (e2, Some(p2), _))) if (e1 eq e2) && p1 != p2 =>
                             val ^^ = key2.substring(0, 36)
                             ^ != ^^
                             || {
                               val k1 = key1.substring(36)
                               val k2 = key2.substring(36)
                               !trick.contains(k1) || !trick(k1).contains(k2)
                             }
                           case _ => false
                         }
                       case _ => true
                     }
                   }
                then
                  %.keys.foreach(%(_).asInstanceOf[+]._1.success(None))
                  context.system.unsafeUpcast[Either[Unit, Unit]] ! Right(())
                  Behaviors.stopped

                else
                  Behaviors.same

              case nel =>

                var m = %

                def `π-enable`(enabled: `Π-Set`[String]) =
                  m = enabled.foldLeft(m) { (m, key) =>
                                            val n = if m.contains(key)
                                                    then m(key).asInstanceOf[Int]
                                                    else 0
                                            m + (key -> (n + 1))
                                          }

                def enable(key: String) =
                  val (_, spell) = `π-wand`
                  `π-enable`(spell(key))

                def unblock(k: String)
                           (implicit ^ : String) =
                  if m.contains(^ + k)
                  then m(^ + k).asInstanceOf[+]._1.success(None)

                def `π-discard`(discarded: `Π-Set`[String])
                               (implicit ^ : String) =
                  discarded.foreach(unblock(_))
                  m = discarded.map(^ + _).foldLeft(m)(_ - _)

                def discard(key: String)(using ^ : String) =
                  val (trick, _) = `π-wand`
                  if trick.contains(key)
                  then
                    `π-discard`(trick(key))

                nel
                  .sliding(parallelism, parallelism)
                  .toList
                  .foreach {
                    _.foreach { case (key1, key2, delay) =>
                                val k1 = key1.substring(36)
                                val k2 = key2.substring(36)
                                val ^  = key1.substring(0, 36)
                                val ^^ = key2.substring(0, 36)
                                val (p1, _) = m(key1).asInstanceOf[+]
                                val (p2, _) = m(key2).asInstanceOf[+]
                                discard(k1)(using ^)
                                if k1 != k2 then discard(k2)(using ^^)
                                m -= key1
                                m -= key2
                                enable(k1)
                                if k1 != k2 then enable(k2)
                                p1.success(Some((delay)))
                                if k1 != k2 then p2.success(Some((delay)))
                              }
                  }

                context.self ! Trigger

                Loop(parallelism)(dump)(m)

        case (context, Enqueue(^ : String, key: String, it: +)) =>

          context.self ! Trigger

          Loop(parallelism)(dump) {
            val n = %(key).asInstanceOf[Int] - 1
            ( if n == 0
              then
                % - key
              else
                % + (key -> n)
            ) + (^ + key -> it)
          }

        case (context, Exclude(enabled)) =>

          context.self ! Trigger

          Loop(parallelism)(dump) {
            enabled.foldLeft(%) { (m, key) =>
                                  val n = m(key).asInstanceOf[Int] - 1
                                  if n == 0
                                  then
                                    m - key
                                  else
                                    m + (key -> n)
                                }
          }

      }
