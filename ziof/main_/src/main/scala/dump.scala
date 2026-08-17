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

import _root_.java.io.{ PrintStream, FileOutputStream }

import _root_.scala.collection.immutable.List

import _root_.zio.{ ExitCode, Queue, UIO, ZIO }

import `Π-loop`.*


package object `Π-dump`:

  private val spirsx = "pisc.stochastic.replications.exitcode.ignore"


  type - = Queue[Option[(Long, ((Long, Long), Long), (String, String), (Double, Double))]]


  private def record(number: Long, started: Long, ended: Long, delay: Double, duration: Double): String => UIO[Unit] =
    _.split(",") match
      case Array(key, name, polarity, label, rate, agent) =>
        ZIO.attemptBlocking {
          printf("%d,%d,%d,%s,%s,%s,%s,%s,%s,%s,%s,%s,\n",
                 number, started, ended, name, polarity,
                 key.stripPrefix("!"), key.startsWith("!"),
                 label, rate, delay, duration, agent)
        }.either.unit
      case Array(key, name, polarity, label, rate, agent, filename*) =>
        var ps: PrintStream = null
        ZIO.attemptBlocking {
          val fn = filename.mkString(",")
          ps = PrintStream(FileOutputStream(fn + ".csv", true), true)
          ps.printf("%d,%d,%d,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n",
                    number, started, ended, name, polarity,
                    key.stripPrefix("!"), key.startsWith("!"),
                    label, rate, delay, duration, agent, fn)
        }.either.unit.tap { _ => ZIO.attemptBlocking(ps.close).either.unless(ps eq null) }
      case _ =>
        ZIO.unit

  private def doExit(using % : %, ! : !): UIO[Unit] =
    %.get.flatMap { m =>
      val ks = m.keys.toList
      val ec =
        if ks.isEmpty
        then
          ExitCode.success
        else
          if !sys.BooleanProp.keyExists(spirsx).value
          && ks.forall(_.charAt(36) == '!')
          then ExitCode.success
          else ExitCode.failure
      ZIO.collectAllParDiscard(ks.map(m(_).asInstanceOf[+]._1._1.succeed(None))) *>
      !.succeed(ec).unit
    }

  def dump(using % : %, ! : !, - : -): UIO[Unit] =
    for
      h <- -.take
      _ <- h match
             case Some((no, ((s1, s2), e), (k1, k2), (delay, duration))) =>
               for
                 _ <- record(no, s1, e, delay, duration)(k1)
                 _ <- record(no, s2, e, delay, duration)(k2).unless(k1 == k2)
                 _ <- dump
               yield
                 ()
             case _ =>
               doExit
    yield
      ()
