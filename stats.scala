/*
 * Copyright (c) 2023-2024 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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

import _root_.scala.collection.mutable.{ LinkedHashMap => Map3 }
import _root_.scala.concurrent.duration._

import _root_.scala.util.control.NonLocalReturns.{ returning, throwReturn => thr }

import _root_.breeze.stats.distributions.{ Exponential, Rand }
import Rand.VariableSeed._

import _root_.cats.effect.{ IO, Ref }

import _root_.com.github.blemale.scaffeine.{ Scaffeine, Cache }


package object `Π-stats`:

  import sΠ.{ `Π-magic`, `Π-Map`, `Π-Set` }
  import `Π-magic`.`><`

  sealed trait Rate extends AnyRef
  case object ∞ extends Rate
  case class `@`(rate: BigDecimal) extends Rate

  private val distributionCache: Cache[Double, Exponential] =
    Scaffeine()
      .recordStats()
      .expireAfterWrite(1.hour)
      .maximumSize(500)
      .build[Double, Exponential]()

  private def distrib(r: Double) =
    distributionCache.getIfPresent(r).getOrElse {
      val it = Exponential(r)
      distributionCache.put(r, it)
      it
    }

  private val deltaCache: Cache[String, BigDecimal] =
    Scaffeine()
      .recordStats()
      .expireAfterWrite(1.hour)
      .maximumSize(1000)
      .build[String, BigDecimal]()

  private def delta(key: String, rate: BigDecimal) =
    deltaCache.getIfPresent(key).getOrElse {
      val it = BigDecimal(distrib(rate.toDouble).draw())
      deltaCache.put(key, it)
      it
    }

  def |(% : Map[String, (Ref[IO, `><`], Option[Boolean], Rate)])
       (`π-trick`: `Π-Map`[String, `Π-Set`[String]]): Option[((String, BigDecimal), (String, BigDecimal))] =
    returning {
      val `0` = Map3.from(% // immediate
        .filter(_._2._3 eq ∞)
        .map { case (k, (n, p, _)) => k -> (n, p, BigDecimal(0)) }
      )
      val `0+` = Map3.from(% // timed
        .filter(_._2._3.isInstanceOf[`@`])
        .map { case (k, (n, p, r)) => k -> (n, p, r.asInstanceOf[`@`].rate) }
        .map { case (k, (n, p, r)) => k -> (n, p, delta(k, r)) }
        .toList
        .sortBy(_._2._3)
        .reverse
      )
      val `-1` = Map3.from(% // passive
        .filter(_._2._3 eq null)
        .map { case (k, (n, p, _)) => k -> (n, p, BigDecimal(-1)) }
      )

      val χ = (`0` ++ `0+` ++ `-1`).zipWithIndex

      for
        (kv, i) <- χ
        (key1, (name1, polarity1, delta1)) = kv
        k1 = key1.substring(key1.length/2)
      do
        if polarity1 eq None then thr(Some((key1 -> delta1, key1 -> delta1)))
        for
          (kv, _) <- χ.drop(i+1)
          (key2, (name2, polarity2, delta2)) = kv
          k2 = key2.substring(key2.length/2)
          if !`π-trick`.contains(k1) || !`π-trick`(k1).contains(k2)
        do
          if name1 eq name2
          then
            (polarity1, polarity2) match
              case (Some(true), Some(false)) | (Some(false), Some(true)) =>
                thr(Some((key1 -> delta1, key2 -> delta2)))
              case _ =>

      None
    }
