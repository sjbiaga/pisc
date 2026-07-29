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

import _root_.cats.instances.list.*
import _root_.cats.syntax.applicative.*
import _root_.cats.syntax.functor.*
import _root_.cats.syntax.flatMap.*
import _root_.cats.syntax.monadError.*
import _root_.cats.syntax.traverse.*

import _root_.cats.effect.{ Async, ExitCode }
import _root_.cats.effect.std.Queue

import `Π-loop`.*


package object `Π-dump`:

  private val barsx = "pisc.bioambients.replications.exitcode.ignore"


  type -[F[_]] = Queue[F, List[String] | (Long, ((Long, Long), Long), (String, String), (Double, Double), ((String, (String, String)), (String, (String, String))))]


  final class πdump[F[_]: Async]:

    private def record(number: Long, started: Long, ended: Long, delay: Double, duration: Double, ambient: (String, (String, String))): String => F[String] =
      _.split(",") match
        case Array(key, name, polarity, label, rate, agent, dir_cap) =>
          Async[F].blocking {
            printf("%d,%d,%d,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,\n",
                   number, started, ended, name, polarity,
                   key.stripPrefix("!"), key.startsWith("!"),
                   label, rate, delay, duration, agent, dir_cap, ambient._1, ambient._2._1)
            polarity
          }
        case Array(key, name, polarity, label, rate, agent, dir_cap, filename*) =>
          var ps: PrintStream = null
          Async[F].blocking {
            val fn = filename.mkString(",")
            ps = PrintStream(FileOutputStream(fn + ".csv", true), true)
            ps.printf("%d,%d,%d,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n",
                      number, started, ended, name, polarity,
                      key.stripPrefix("!"), key.startsWith("!"),
                      label, rate, delay, duration, agent, dir_cap, ambient._1, ambient._2._1, fn)
            polarity
          }.attemptTap { _ => Async[F].blocking(ps.close).unlessA(ps eq null) }
        case _ =>
          Async[F].pure(null)

    private def record(number: Long, polarity: String, snapshot: String): F[Unit] =
      if polarity eq null
      then
        Async[F].unit
      else
        var ps: PrintStream = null
        Async[F].blocking {
          ps = PrintStream(FileOutputStream("" + number + "-" + polarity + ".xml", false), true)
          ps.println("""<?xml version="1.0" ?>""")
          ps.println(snapshot)
        }.void.attemptTap { _ => Async[F].blocking(ps.close).unlessA(ps eq null) }


    private def exit(ks: List[String])
                    (using % : %[F], ! : ![F]): F[Unit] =
      if ks.isEmpty
      then
        !.complete(ExitCode.Success).void
      else
        %.flatModify { m =>
          m -> (ks.traverse(m(_).asInstanceOf[(Boolean, +[F])]._2._1._1.complete(None)) >>
                ks.traverse(m(_).asInstanceOf[(Boolean, +[F])]._2._1._2 match { case null => Async[F].unit
                                                                                case it => it.get.flatMap(_.complete(None).void) }))
        }.as {
          if !sys.BooleanProp.keyExists(barsx).value
          && ks.forall(_.charAt(36) == '!')
          then ExitCode.Success
          else ExitCode.Error
        } >>= (!.complete(_).void)


    def dump(snapshot: Boolean)
            (using % : %[F], ! : ![F], - : -[F]): F[Unit] =
    for
      h <- -.take
      _ <- h match
             case (no, ((s1, s2), e), (k1, k2), (delay, duration), (l1, l2)) =>
               for
                 p  <- record(no, s1, e, delay, duration, l1)(k1)
                 _  <- record(no, p, l1._2._2).whenA(snapshot)
                 _  <- { for
                           p <- record(no, s2, e, delay, duration, l2)(k2)
                           _ <- record(no, p, l2._2._2).whenA(snapshot)
                         yield
                           ()
                       }.unlessA(k1 == k2)
                 _  <- Async[F].cede >> dump(snapshot)
               yield
                 ()
             case ks: List[String] =>
               exit(ks)
    yield
      ()
