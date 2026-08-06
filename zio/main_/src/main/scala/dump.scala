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

import sΠ.given


package object `Π-dump`:

  private val barsx = "pisc.bioambients.replications.exitcode.ignore"


  type - = Queue[List[String] | (Long, ((Long, Long), Long), (String, String), (Double, Double), ((String, (String, String)), (String, (String, String))))]


  private def record(number: Long, started: Long, ended: Long, delay: Double, duration: Double, ambient: (String, (String, String))): String => UIO[String] =
    _.split(",") match
      case Array(key, name, polarity, label, rate, agent, dir_cap) =>
        ZIO.attemptBlocking {
          printf("%d,%d,%d,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,\n",
                 number, started, ended, name, polarity,
                 key.stripPrefix("!"), key.startsWith("!"),
                 label, rate, delay, duration, agent, dir_cap, ambient._1, ambient._2._1)
          polarity
        }
      case Array(key, name, polarity, label, rate, agent, dir_cap, filename*) =>
        var ps: PrintStream = null
        ZIO.attemptBlocking {
          val fn = filename.mkString(",")
          ps = PrintStream(FileOutputStream(fn + ".csv", true), true)
          ps.printf("%d,%d,%d,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n",
                    number, started, ended, name, polarity,
                    key.stripPrefix("!"), key.startsWith("!"),
                    label, rate, delay, duration, agent, dir_cap, ambient._1, ambient._2._1, fn)
          polarity
        }.tap { _ => ZIO.attemptBlocking(ps.close).either.unless(ps eq null) }
      case _ =>
        ZIO.succeed(null)

  private def record(number: Long, polarity: String, snapshot: String): UIO[Unit] =
    if polarity eq null
    then
      ZIO.unit
    else
      var ps: PrintStream = null
      ZIO.attemptBlocking {
        ps = PrintStream(FileOutputStream("" + number + "-" + polarity + ".xml", false), true)
        ps.println("""<?xml version="1.0" ?>""")
        ps.println(snapshot)
      }.either.unit.tap { _ => ZIO.attemptBlocking(ps.close).either.unless(ps eq null) }


  private def exit(ks: List[String])
                  (using % : %, ! : !): UIO[Unit] =
    if ks.isEmpty
    then
      !.succeed(ExitCode.success).unit
    else
      %.modify { m =>
        ZIO.collectAllParDiscard(ks.map(m(_).asInstanceOf[+]._1.succeed(None))) -> m
      }.flatten.as {
        if !sys.BooleanProp.keyExists(barsx).value
        && ks.forall(_.charAt(36) == '!')
        then ExitCode.success
        else ExitCode.failure
      }.flatMap(!.succeed(_).unit)


  def dump(snapshot: Boolean)
          (using % : %, ! : !, - : -): UIO[Unit] =
    for
      h <- -.take
      _ <- h match
             case (no, ((ts1, ts2), ts), (k1, k2), (delay, duration), (l1, l2)) =>
               for
                 p  <- record(no, ts1, ts, delay, duration, l1)(k1)
                 _  <- record(no, p, l1._2._2).when(snapshot)
                 _  <- ( for
                           p <- record(no, ts2, ts, delay, duration, l2)(k2)
                           _ <- record(no, p, l2._2._2).when(snapshot)
                         yield
                           ()
                       ).unless(k1 == k2)
                 _  <- dump(snapshot)
               yield
                 ()
             case ks: List[String] =>
               exit(ks)
    yield
      ()
