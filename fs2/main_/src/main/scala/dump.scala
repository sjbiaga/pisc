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

import _root_.scala.collection.immutable.{ List, Set }
import _root_.scala.Option.unless

import _root_.cats.Order
import _root_.cats.instances.list.*
import _root_.cats.syntax.applicative.*
import _root_.cats.syntax.functor.*
import _root_.cats.syntax.flatMap.*
import _root_.cats.syntax.traverse.*

import _root_.cats.effect.{ Async, ExitCode, Ref }
import _root_.cats.effect.std.PQueue

import `Π-loop`.*
import `Π-traces`.*


package object `Π-dump`:

  private val barsx = "pisc.bioambients.replications.exitcode.ignore"


  type -[F[_]] = PQueue[F, Option[(Long, ((Long, Long), Long), (String, String, KeyBy), (Long, (Double, Seq[Plugin])), Set[Long], ((String, (String, String)), (String, (String, String))))]]

  given Order[Option[(Long, ((Long, Long), Long), (String, String, KeyBy), (Long, (Double, Seq[Plugin])), Set[Long], ((String, (String, String)), (String, (String, String))))]] =
    Order
      .fromLessThan { (o1, o2) =>
        (o1 zip o2).map {
          case ((_, _, _, (id1, (delay1, _)), _, _), (_, _, _, (id2, (delay2, _)), _, _)) =>
            if id1 == id2
            then
              if delay1.isPosInfinity || delay2.isPosInfinity
              then
                !delay1.isPosInfinity
              else
                delay1 < delay2
            else
              id1 < id2
        }.getOrElse(true)
    }


  final class πdump[F[_]: Async]:

    private def record(number: Long,
                       clock: Double, started: Long, ended: Long,
                       keyBy: KeyBy,
                       delay: Double, plugins: Seq[Plugin],
                       causes: Set[Long],
                       ambient: (String, (String, String))): String => F[Unit] =
      _.split(",") match
        case Array(key, name, polarity, label, rate, agent, dir_cap) =>
          Async[F].blocking {
            val snapshot = if ambient._2._2.isEmpty then null else """<?xml version="1.0" ?>\n""" + ambient._2._2
            `π-traces`(number, causes,
                       clock, started, ended,
                       agent, name, unless(polarity.isEmpty)(polarity.toBoolean),
                       key.stripPrefix("!"), key.startsWith("!"), label, keyBy,
                       rate, Seq.empty, delay,
                       dir_cap, ambient._1, ambient._2._1, Option(snapshot))
          }
        case _ =>
          Async[F].unit

    private def doExit(using % : %[F], ! : ![F]): F[Unit] =
      %.get.flatMap { m =>
        val ks = m.keys.toList
        val ec =
          if ks.isEmpty
          then
            ExitCode.Success
          else
            if !sys.BooleanProp.keyExists(barsx).value
            && ks.forall(_.charAt(36) == '!')
            then ExitCode.Success
            else ExitCode.Error
        ks.traverse(m(_).asInstanceOf[(Boolean, +[F])]._2._1._1._1.complete(None)) >>
        ks.traverse(m(_).asInstanceOf[(Boolean, +[F])]._2._1._1._2 match { case null => Async[F].unit
                                                                           case it => it.get.flatMap(_.complete(None).void) }) >>
        !.complete(ec).void
      }

    def dump(clock: Ref[F, Double], feedback: Feedback[F])
            (using % : %[F], ! : ![F], - : -[F]): F[Unit] =
      -.take.flatMap {
        case Some(_) if `π-traces` eq null =>
          dump(clock, feedback)
        case Some((no, ((ts1, ts2), ts), (k1, k2, kb), (_, (delay, plugins)), causes, (l1, l2))) =>
          for
            cl <- if delay.isPosInfinity
                  then clock.get
                  else clock.updateAndGet(_ + delay)
            _  <- feedback.lastR.set(ts -> cl)
            _  <- record(no, cl, ts1, ts, kb, delay, plugins, causes, l1)(k1)
            _  <- record(no, cl, ts2, ts, kb, delay, plugins, causes, l2)(k2).unlessA(k1 == k2)
            _  <- Async[F].cede >> dump(clock, feedback)
          yield
            ()
        case _ =>
          Async[F].blocking(`π-traces`.close).whenA(`π-traces` ne null) >> doExit
      }
