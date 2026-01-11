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

import _root_.scala.collection.immutable.List

import _root_.zio.{ ExitCode, Promise, Queue, Task, UIO, ZIO }

import `Π-loop`.*


package object `Π-dump`:

  private val barsx = "pisc.bioambients.replications.exitcode.ignore"


  type - = Queue[List[String] | (Long, ((Long, Long), Long), (String, String), (Double, Double), (Promise[Throwable, (String, (String, String))], Promise[Throwable, (String, (String, String))]))]


  private def record(number: Long, started: Long, ended: Long, delay: Double, duration: Double, ambient: (String, (String, String))): String => Task[String] =
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
        }.exit.tap { _ => if ps == null then ZIO.unit else ZIO.attemptBlocking { ps.close } }.unexit
      case _ =>
        ZIO.succeed(null)

  private def record(number: Long, polarity: String, snapshot: String): Task[Unit] =
    if polarity eq null
    then
      ZIO.unit
    else
      var ps: PrintStream = null
      ZIO.attemptBlocking {
        ps = PrintStream(FileOutputStream("" + number + "-" + polarity + ".xml", false), true)
        ps.println("""<?xml version="1.0" ?>""")
        ps.println(snapshot)
      }.exit.tap { _ => if ps == null then ZIO.unit else ZIO.attemptBlocking { ps.close } }.unexit

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

  def dump(snapshot: Boolean)(using % : %, ! : !, - : -): Task[Unit] =
    for
      h <- -.take
      _ <- h match
             case (no, ((s1, s2), e), (k1, k2), (delay, duration), (p1, p2)) =>
               for
                 l1 <- p1.await
                 l2 <- if k1 == k2 then ZIO.succeed(l1) else p2.await
                 p  <- record(no, s1, e, delay, duration, l1)(k1)
                 _  <- if snapshot then record(no, p, l1._2._2) else ZIO.unit
                 _  <- if k1 == k2 then ZIO.unit
                       else
                         for
                           p <- record(no, s2, e, delay, duration, l2)(k2)
                           _ <- if snapshot then record(no, p, l2._2._2) else ZIO.unit
                         yield
                           ()
                 _ <- dump(snapshot)
               yield
                 ()
             case ks: List[String] =>
               exit(ks)
    yield
      ()
