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

import _root_.scala.util.control.NonLocalReturns.{ returning, throwReturn => thr }
import _root_.scala.util.Random

import _root_.scala.collection.immutable.{ List, Map, Set }
import _root_.scala.collection.mutable.HashMap
import _root_.scala.concurrent.duration._

import _root_.breeze.stats.distributions.Exponential
import _root_.breeze.stats.distributions.Rand.VariableSeed._

import _root_.cats.data.NonEmptyList
import _root_.cats.effect.{ IO, Ref }

import _root_.com.github.blemale.scaffeine.{ Scaffeine, Cache }


package object `Π-stats`:

  import sΠ.{ `Π-Map`, `Π-Set`, >*< }

  sealed trait Rate extends Any
  case class ∞(weight: Long) extends AnyVal with Rate
  case class `ℝ⁺`(rate: BigDecimal) extends AnyVal with Rate
  case class ⊤(weight: Long) extends AnyVal with Rate

  private val random = new Random

  private val distributionCache: Cache[Double, Exponential] =
    Scaffeine()
      .recordStats()
      .expireAfterWrite(1.hour)
      .maximumSize(500)
      .build[Double, Exponential]()

  private inline def distribution(rate: Double) =
    distributionCache.getIfPresent(rate).getOrElse {
      val it = Exponential(rate)
      distributionCache.put(rate, it)
      it
    }

  private inline def delta(rate: BigDecimal): Double =
    distribution(rate.toDouble).draw()

  class StatisticsException(msg: String, cause: Throwable = null)
      extends RuntimeException(msg, cause)

  case class CombinedActivitiesException(how: String)
      extends StatisticsException("The immediate and/or timed and/or passive activities must not be " + how)

  def |(% : Map[String, (>*<, Option[Boolean], Rate)])
       (`π-trick`: `Π-Map`[String, `Π-Set`[String]])
       (parallelism: Int): Option[NonEmptyList[(String, String, Double)]] =
                                             // ^^^^^^  ^^^^^^  ^^^^^^
                                             // key1    key1|2  duration

    val mr = HashMap[(String, (>*<, Option[Boolean])), Rate]() // rates

    val mls = HashMap[(>*<, Option[Boolean]), List[Either[Long, Either[BigDecimal, Long]]]]() // lists

    %
      .foreach {
        case (k, (n, p, r)) =>
          mr(k -> (n -> p)) = r
          mls(n -> p) = Nil
      }

    mr
      .foreach {
        case ((_, np), r: ∞) => // immediate
          mls(np) :+= Left(r.weight)
        case ((_, np), r: `ℝ⁺`) => // timed
          mls(np) :+= Right(Left(r.rate))
        case ((_, np), r: ⊤) => // passive
          mls(np) :+= Right(Right(r.weight))
      }

    val msrt = HashMap[(>*<, Option[Boolean]), BigDecimal]() // [timed] sums of rates

    mls // timed
      .foreach {
        case (np, ls) =>
          val rs = ls
            .filter(_.isRight)
            .filter(_.right.get.isLeft)
            .map(_.right.get.left.get)
          if rs.nonEmpty
          then
            msrt(np) = rs.sum
      }

    val mswi = HashMap[(>*<, Option[Boolean]), Long]() // [immediate] sums of weights

    mls // immediate
      .foreach {
        case (np, ls) =>
          val ws = ls
            .filter(_.isLeft)
            .map(_.left.get)
          if ws.nonEmpty
          then
            mswi(np) = ws.sum
      }

    val mswp = HashMap[(>*<, Option[Boolean]), Long]() // [passive] sums of weights

    mls // passive
      .foreach {
        case (np, ls) =>
          val ws = ls
            .filter(_.isRight)
            .filter(_.right.get.isRight)
            .map(_.right.get.right.get)
          if ws.nonEmpty
          then
            mswp(np) = ws.sum
      }

    if (msrt.keySet & mswi.keySet).nonEmpty
    || (msrt.keySet & mswp.keySet).nonEmpty
    || (mswi.keySet & mswp.keySet).nonEmpty
    then
      throw CombinedActivitiesException("mixed")

    val `0` = Map.from(% // immediate
      .filter(_._2._3.isInstanceOf[∞])
      .map { case (k, (n, p, r: ∞)) => k -> (n, p, Double.NaN -> r.weight) }
    )
    val `0+` = Map.from(% // timed
      .filter(_._2._3.isInstanceOf[`ℝ⁺`])
      .map { case (k, (n, p, r: `ℝ⁺`)) => k -> (n, p, r.rate.toDouble -> 0L) }
    )
    val `-1` = Map.from(% // passive
      .filter(_._2._3.isInstanceOf[⊤])
      .map { case (k, (n, p, r: ⊤)) => k -> (n, p, Double.NaN -> r.weight) }
    )

    var p = parallelism

    var r = List[((String, String, Double), ((Int, Double), >*<))]()
    //             ^^^^^^  ^^^^^^  ^^^^^^     ^^^  ^^^^^^
    //             key1    key1|2  duration   pri  delay

    var s = Set[String]()

    val χ = (`0` ++ `0+` ++ `-1`).zipWithIndex

    returning {
      for
        ((key1, (name1, polarity1, (rate1, weight1))), i) <- χ
      do
        if p == 0 then thr(())
        if !s.contains(key1)
        then
          if polarity1 eq None
          then
            val (rate, (priority, duration)) =
              if msrt.contains(name1 -> polarity1)
              then
                BigDecimal(1) * rate1 -> (2 -> Double.PositiveInfinity)
              else if mswi.contains(name1 -> polarity1)
              then
                BigDecimal(1) * weight1 -> (1 -> 0.0)
              else if mswp.contains(name1 -> polarity1)
              then
                BigDecimal(1) * weight1 -> (3 -> Double.NaN)
              else
                ???
            val delay = delta(rate)
            s += key1
            r :+= (key1, key1, if priority == 2 then delay else duration) -> (priority -> delay, name1)
            p -= 1
          else returning {
            for
              ((key2, (name2, polarity2, (rate2, weight2))), _) <- χ.drop(i+1)
            do
              if !s.contains(key2) && name1 == name2 && polarity1.get != polarity2.get
              then
                val k1 = key1.substring(key1.length/2)
                val k2 = key2.substring(key2.length/2)
                if !`π-trick`.contains(k1) || !`π-trick`(k1).contains(k2)
                then
                  val (rate, (priority, duration)) =
                    if msrt.contains(name1 -> polarity1)
                    && msrt.contains(name2 -> polarity2)
                    then
                      val apr1 = msrt(name1 -> polarity1)
                      val apr2 = msrt(name2 -> polarity2)
                      ((rate1 / apr1) * (rate2 / apr2) * (apr1 min apr2)) -> (2 -> Double.PositiveInfinity)
                    else if mswi.contains(name1 -> polarity1)
                         && mswi.contains(name2 -> polarity2)
                    then
                      val apr1 = mswi(name1 -> polarity1)
                      val apr2 = mswi(name2 -> polarity2)
                      ((BigDecimal(1) * weight1 / apr1) * (BigDecimal(1) * weight2 / apr2) * (apr1 min apr2)) -> (1 -> 0.0)
                    else if mswp.contains(name1 -> polarity1)
                         && mswp.contains(name2 -> polarity2)
                    then
                      val apr1 = mswp(name1 -> polarity1)
                      val apr2 = mswp(name2 -> polarity2)
                      ((BigDecimal(1) * weight1 / apr1) * (BigDecimal(1) * weight2 / apr2) * (apr1 min apr2)) -> (3 -> Double.NaN)
                    else
                      throw CombinedActivitiesException("crossed")
                  val delay = delta(rate)
                  s = s + key1 + key2
                  r :+= (key1, key2, if priority == 2 then delay else duration) -> (priority -> delay, name2)
                  p -= 1
                  thr(())
          }
    }

    NonEmptyList.fromList {
      ( for
          (((key1, key2, _), (_, name)), i) <- r.sortBy(_._2._1).reverse.zipWithIndex
          k1 = key1.substring(key1.length/2)
          k2 = key2.substring(key2.length/2)
        yield
          r(i)._1 -> {
            0 > r.indexWhere(
              {
                case (_, (_, `name`)) => true
                case ((key, _, _), _)
                    if {
                      val k = key.substring(key.length/2)
                      `π-trick`.contains(k) && (`π-trick`(k).contains(k1) || `π-trick`(k).contains(k2))
                    } => true
                case ((_, key, _), _)
                    if {
                      val k = key.substring(key.length/2)
                      `π-trick`.contains(k) && (`π-trick`(k).contains(k1) || `π-trick`(k).contains(k2))
                    } => true
                case _ => false
              }
              , i + 1
            )
          }
      )
      .reverse
      .takeWhile(_._2)
      .map(_._1)
    }
