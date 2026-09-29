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

import _root_.scala.collection.immutable.Seq
import _root_.scala.Option.unless

import _root_.cats.Order
import _root_.cats.instances.list.*
import _root_.cats.syntax.applicative.*
import _root_.cats.syntax.traverse.*

import _root_.cats.effect.{ IO, ExitCode, Ref }
import _root_.cats.effect.std.PQueue

import `Π-loop`.*
import `Π-traces`.*


package object `Π-dump`:

  private val spirsx = "pisc.stochastic.replications.exitcode.ignore"


  type - = PQueue[IO, Option[(Long, ((Long, Long), Long), (String, String, KeyBy), ((Long, Int), (Double, Seq[Plugin])))]]

  given Order[Option[(Long, ((Long, Long), Long), (String, String, KeyBy), ((Long, Int), (Double, Seq[Plugin])))]] =
    Order.fromLessThan { (o1, o2) => (o1 zip o2).map(_._4._1 -> _._4._1).map { case ((i1, j1), (i2, j2)) => i1 < i2 || i1 == i2 && j1 < j2 }.getOrElse(o1.isDefined) }


  private def record(number: Long, clock: Double, started: Long, ended: Long,
                     keyBy: KeyBy,
                     delay: Double, plugins: Seq[Plugin]): String => IO[Unit] =
    _.split(",") match
      case Array(key, name, polarity, label, rate, agent) =>
        IO.blocking {
          `π-traces`(number, clock, started, ended,
                     agent, name, unless(polarity.isEmpty)(polarity.toBoolean),
                     key.stripPrefix("!"), key.startsWith("!"), label, keyBy,
                     rate, plugins, delay)
        }
      case _ =>
        IO.unit

  private def doExit(using % : %, ! : !): IO[Unit] =
    %.get.flatMap { m =>
      val ks = m.keys.toList
      val ec =
        if ks.isEmpty
        then
          ExitCode.Success
        else
          if !sys.BooleanProp.keyExists(spirsx).value
          && ks.forall(_.charAt(36) == '!')
          then ExitCode.Success
          else ExitCode.Error
      ks.traverse(m(_).asInstanceOf[(Boolean, +)]._2._1._1._1.complete(None)) >>
      ks.traverse(m(_).asInstanceOf[(Boolean, +)]._2._1._1._2 match { case null => IO.unit
                                                                      case it => it.get.flatMap(_.complete(None).void) }) >>
      !.complete(ec).void
    }

  def dump(clock: Ref[IO, Double], feedback: Feedback)
          (using % : %, ! : !, - : -): IO[Unit] =
    -.take.flatMap {
      case Some(_) if `π-traces` eq null =>
        dump(clock, feedback)
      case Some((no, ((ts1, ts2), ts), (k1, k2, kb), (_, (delay, plugins)))) =>
        for
          cl <- if delay.isPosInfinity
                then clock.get
                else clock.updateAndGet(_ + delay)
          _  <- feedback.lastR.set(ts -> cl)
          _  <- record(no, cl, ts1, ts, kb, delay, plugins)(k1)
          _  <- record(no, cl, ts2, ts, kb, delay, plugins)(k2).unlessA(k1 == k2)
          _  <- IO.cede >> dump(clock, feedback)
        yield
          ()
      case _ =>
        IO.blocking(`π-traces`.close).whenA(`π-traces` ne null) >> doExit
    }
