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

import _root_.java.io.{ PrintStream, FileOutputStream }

import _root_.scala.collection.immutable.Map

import _root_.akka.actor.typed.scaladsl.Behaviors
import _root_.akka.actor.typed.Behavior

import `Π-loop`.*


package object `Π-dump`:

  type - = Map[String, Int | +] | (Long, ((Long, Long), Long), (String, String), (Double, Double))

  private def record(number: Long, started: Long, ended: Long, delay: Double, duration: Double): String => Unit =
    _.split(",") match
      case Array(key, name, polarity, label, rate, agent) =>
        printf("%d,%d,%d,%s,%s,%s,%s,%s,%s,%s,%s,%s,\n",
               number, started, ended, name, polarity,
               key.stripPrefix("!"), key.startsWith("!"),
               label, rate, delay, duration, agent)
      case Array(key, name, polarity, label, rate, agent, filename*) =>
        var ps: PrintStream = null
        try
          val fn = filename.mkString(",")
          ps = PrintStream(FileOutputStream(fn + ".csv", true), true)
          ps.printf("%d,%d,%d,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n",
                    number, started, ended, name, polarity,
                    key.stripPrefix("!"), key.startsWith("!"),
                    label, rate, delay, duration, agent, fn)
        finally
          if ps != null then ps.close
      case _ =>

  object Dump:

    def apply(): Behavior[-] =

      Behaviors.receiveMessage[-] {

        case (no, ((ts1, ts2), ts), (k1, k2), (delay, duration)) =>
          record(no, ts1, ts, delay, duration)(k1)
          if k1 != k2 then record(no, ts2, ts, delay, duration)(k2)
          Behaviors.same

        case it: Map[String, Int | +] =>
          it.keys.foreach(it(_).asInstanceOf[+]._1.success(None))
          Behaviors.stopped

      }
