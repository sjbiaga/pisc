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

import _root_.scala.collection.immutable.List
import _root_.scala.Option.unless

import _root_.zio.{ ExitCode, Queue, UIO, ZIO }

import `Π-loop`.*
import `Π-traces`.*


package object `Π-dump`:

  private val barsx = "pisc.bioambients.replications.exitcode.ignore"


  type - = Queue[Option[((Long, Double), ((Long, Long), Long), (String, String), (Double, Double), ((String, (String, String)), (String, (String, String))))]]


  private def record(number: Long, clock: Double, started: Long, ended: Long, delay: Double, duration: Double, ambient: (String, (String, String))): String => UIO[Unit] =
    _.split(",") match
      case Array(key, name, polarity, label, rate, agent, dir_cap) =>
        ZIO.attemptBlocking {
          val snapshot = if ambient._2._2.isEmpty then null else """<?xml version="1.0" ?>""" + "\n" + ambient._2._2
          `π-traces`(number, clock, started, ended,
                     agent, name, unless(polarity.isEmpty)(java.lang.Boolean.parseBoolean(polarity)),
                     key.stripPrefix("!"), key.startsWith("!"), label,
                     rate, delay, duration,
                     dir_cap, ambient._1, ambient._2._1, Option(snapshot))
        }.either.unit
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
          if !sys.BooleanProp.keyExists(barsx).value
          && ks.forall(_.charAt(36) == '!')
          then ExitCode.success
          else ExitCode.failure
      ZIO.collectAllParDiscard(ks.map(m(_).asInstanceOf[(Boolean, +)]._2._1._1._1.succeed(None))) *>
      ZIO.collectAllParDiscard(ks.map(m(_).asInstanceOf[(Boolean, +)]._2._1._1._2 match { case null => ZIO.unit
                                                                                          case it => it.get.flatMap(_.succeed(None).unit) })) *>
      !.succeed(ec).unit
    }

  def dump(using % : %, ! : !, - : -): UIO[Unit] =
    for
      h <- -.take
      _ <- h match
             case Some(_) if `π-traces` eq null =>
               dump
             case Some(((no, cl), ((ts1, ts2), ts), (k1, k2), (delay, duration), (l1, l2))) =>
               for
                 _ <- record(no, cl, ts1, ts, delay, duration, l1)(k1)
                 _ <- record(no, cl, ts2, ts, delay, duration, l2)(k2).unless(k1 == k2)
                 _ <- ZIO.yieldNow *> dump
               yield
                 ()
             case _ =>
               ZIO.attemptBlocking(`π-traces`.close).when(`π-traces` ne null).either *> doExit
    yield
      ()
