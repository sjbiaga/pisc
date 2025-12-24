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

import _root_.cats.instances.list.*
import _root_.cats.syntax.flatMap.*
import _root_.cats.syntax.traverse.*

import _root_.cats.effect.{ IO, Deferred, ExitCode }
import _root_.cats.effect.std.Queue

import `Π-loop`.*


package object `Π-dump`:

  private val barsx = "pisc.bioambients.replications.exitcode.ignore"


  type - = Queue[IO, List[String] | (Long, ((Long, Long), Long), (String, String), (Double, Double), (Deferred[IO, (String, (String, String))], Deferred[IO, (String, (String, String))]))]


  private def record(number: Long, started: Long, ended: Long, delay: Double, duration: Double, ambient: (String, (String, String))): String => IO[String] =
    _.split(",") match
      case Array(key, name, polarity, label, rate, agent, dir_cap) =>
        IO.blocking {
          printf("%d,%d,%d,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,\n",
                 number, started, ended, name, polarity,
                 key.stripPrefix("!"), key.startsWith("!"),
                 label, rate, delay, duration, agent, dir_cap, ambient._1, ambient._2._1)
          polarity
        }
      case Array(key, name, polarity, label, rate, agent, dir_cap, filename*) =>
        var ps: PrintStream = null
        IO.blocking {
          val fn = filename.mkString(",")
          ps = PrintStream(FileOutputStream(fn + ".csv", true), true)
          ps.printf("%d,%d,%d,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n",
                    number, started, ended, name, polarity,
                    key.stripPrefix("!"), key.startsWith("!"),
                    label, rate, delay, duration, agent, dir_cap, ambient._1, ambient._2._1, fn)
          polarity
        }.attemptTap { _ => if ps == null then IO.unit else IO.blocking { ps.close } }
      case _ =>
        IO.pure(null)

  private def record(number: Long, polarity: String, snapshot: String): IO[Unit] =
    if polarity eq null
    then
      IO.unit
    else
      var ps: PrintStream = null
      IO.blocking {
        ps = PrintStream(FileOutputStream("" + number + "-" + polarity + ".xml", false), true)
        ps.println("""<?xml version="1.0" ?>""")
        ps.println(snapshot)
      }.void.attemptTap { _ => if ps == null then IO.unit else IO.blocking { ps.close } }


  private def exit(ks: List[String])
                  (using % : %, ! : !): IO[Unit] =
    if ks.isEmpty
    then
      !.complete(ExitCode.Success).void
    else
      %.flatModify { m =>
        m -> ks.traverse(m(_).asInstanceOf[+]._1.complete(None))
      }.as {
        if !sys.BooleanProp.keyExists(barsx).value
        && ks.forall(_.charAt(36) == '!')
        then ExitCode.Success
        else ExitCode.Error
      } >>= (!.complete(_).void)


  def dump(snapshot: Boolean)
          (using % : %, ! : !, - : -): IO[Unit] =
    for
      h <- -.take
      _ <- h match
             case (no, ((ts1, ts2), ts), (k1, k2), (delay, duration), (d1, d2)) =>
               for
                 l1 <- d1.get
                 l2 <- if k1 == k2 then IO.pure(l1) else d2.get
                 p  <- record(no, ts1, ts, delay, duration, l1)(k1)
                 _  <- if snapshot then record(no, p, l1._2._2) else IO.unit
                 _  <- if (k1 == k2)
                       then
                         IO.unit
                       else
                         for
                           p <- record(no, ts2, ts, delay, duration, l2)(k2)
                           _ <- if snapshot then record(no, p, l2._2._2) else IO.unit
                         yield
                           ()
                 _  <- IO.cede >> dump(snapshot)
               yield
                 ()
             case ks: List[String] =>
               exit(ks)
    yield
      ()
