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
import _root_.scala.Option.unless

import _root_.org.apache.pekko.actor.typed.scaladsl.Behaviors
import _root_.org.apache.pekko.actor.typed.Behavior

import `Π-loop`.*
import `Π-traces`.*


package object `Π-dump`:

  type - = Map[String, Int | +] | (Long, ((Long, Long), (Long, Double)), (String, String), (Double, Double))

  private def record(number: Long, clock: Double, started: Long, ended: Long, delay: Double, duration: Double): String => Unit =
    _.split(",") match
      case Array(key, name, polarity, label, rate, agent) =>
        `π-traces`(number, clock, started, ended,
                   agent, name, unless(polarity.isEmpty)(java.lang.Boolean.parseBoolean(polarity)),
                   key.stripPrefix("!"), key.startsWith("!"), label,
                   rate, delay, duration)
      case _ =>

  object Dump:

    def apply(): Behavior[-] =

      Behaviors.receive[-] {

        case (_, (no, ((ts1, ts2), (ts, cl)), (k1, k2), (delay, duration))) =>
          if `π-traces` ne null
          then
            record(no, cl, ts1, ts, delay, duration)(k1)
            if k1 != k2 then record(no, cl, ts2, ts, delay, duration)(k2)
          Behaviors.same

        case (context, it: Map[String, Int | +]) =>
          if `π-traces` ne null
          then
            `π-traces`.close
          it.keys.foreach(it(_).asInstanceOf[+]._1.success(None))
          context.system.unsafeUpcast[Either[Unit, Unit]] ! Right(())
          Behaviors.stopped

      }
