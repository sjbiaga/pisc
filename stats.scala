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

import _root_.scala.util.Random

import _root_.scala.concurrent.duration._

import _root_.breeze.stats.distributions.{ Exponential, Rand }
import Rand.VariableSeed._

import _root_.com.github.blemale.scaffeine.{ Scaffeine, Cache }


package object `Π-stats`:

  sealed trait Rate extends AnyRef
  case object ∞ extends Rate
  case class `@`(rate: BigDecimal) extends Rate

  private val random = new Random

  private val cache: Cache[Double, Exponential] =
    Scaffeine()
      .recordStats()
      .expireAfterWrite(1.hour)
      .maximumSize(500)
      .build[Double, Exponential]()

  private def distrib(r: Double) =
    cache.getIfPresent(r).getOrElse {
      val it = Exponential(r)
      cache.put(r, it)
      it
    }

  def |(% : Map[String, Rate]): (String, BigDecimal) =
    require(%.nonEmpty)
    val `0` = %.filter(_._2 eq ∞)
    if `0`.nonEmpty
    then // immediate
      `0`.drop(random.nextInt(`0`.size)).head._1 -> BigDecimal(0)
    else
      val `0+` = %.filter(_._2 ne null)
      if `0+`.nonEmpty
      then // timed
        val (key, delta) = `0+`
          .map(_ -> _.asInstanceOf[`@`].rate)
          .map(_ -> _.toDouble)
          .map(_ -> distrib(_))
          .map(_ -> _.draw())
          .minBy(_._2)
        key -> BigDecimal(delta)
      else // passive
        val `-1` = %.filter(_._2 eq null)
        `-1`.drop(random.nextInt(`-1`.size)).head._1 -> BigDecimal(-1)
