package basc
package spark4traces
package isd

import scala.collection.mutable.ArrayBuffer
import scala.math._

import org.apache.spark.sql.expressions.Aggregator
import org.apache.spark.sql.{ Encoder, Encoders }

import EssAggregator._


class EssAggregator extends Aggregator[Double, EssBuffer, EssMetrics] {

  override def zero: EssBuffer = EssBuffer(ArrayBuffer.empty)

  override def reduce(b: EssBuffer, a: Double): EssBuffer = {
    b.logWeights += a
    b
  }

  override def merge(b1: EssBuffer, b2: EssBuffer): EssBuffer = {
    b1.logWeights ++= b2.logWeights
    b1
  }

  override def finish(b: EssBuffer): EssMetrics = {
    val weights = b.logWeights
    if (weights.isEmpty) return EssMetrics(0L, .0, .0, .0)

    val n = weights.size
    val maxW = weights.max
    val minW = weights.min

    // 1. Compute LSE(w) = maxW + ln( sum( exp(w_i - maxW) ) )
    val sumExpW = weights.foldLeft(.0) { (acc, w) => acc + exp(w - maxW) }
    val lseW = maxW + log(sumExpW)

    // 2. Compute LSE(2w) = 2*maxW + ln( sum( exp(2*(w_i - maxW)) ) )
    val sumExp2W = weights.foldLeft(.0) { (acc, w) => acc + exp(2 * (w - maxW)) }
    val lse2W = (2 * maxW) + log(sumExp2W)

    // 3. ESS = exp(2 * LSE(w) - LSE(2w))
    val logEss = (2 * lseW) - lse2W
    val ess = exp(logEss)

    EssMetrics(count = n.toLong,
               ess = ess,
               maxLogWeight = maxW,
               minLogWeight = minW)
  }

  override def bufferEncoder: Encoder[EssBuffer] = Encoders.kryo[EssBuffer]

  override def outputEncoder: Encoder[EssMetrics] = Encoders.product[EssMetrics]

}


object EssAggregator {

  case class EssBuffer(var logWeights: ArrayBuffer[Double])

}
