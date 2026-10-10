package basc
package beam4traces
package isd

import scala.jdk.CollectionConverters._
import scala.math.{ exp, log }

import org.apache.beam.sdk.transforms.DoFn
import org.apache.beam.sdk.transforms.DoFn.ProcessElement
import org.apache.beam.sdk.transforms.windowing.{ BoundedWindow, IntervalWindow }
import org.apache.beam.sdk.values.KV


class ESSDiagnosticsFn
    extends DoFn[KV[String, java.lang.Iterable[java.lang.Double]], ESSWindowDiagnostics] {

  @ProcessElement
  def processElement(
    c: DoFn[KV[String, java.lang.Iterable[java.lang.Double]], ESSWindowDiagnostics]#ProcessContext,
    window: BoundedWindow
  ): Unit = {

    val hid = c.element.getKey
    val logWeightsJava = c.element.getValue

    val weights = logWeightsJava.asScala.map(_.toDouble).toVector

    val count = weights.length

    if (count == 0) return

    val maxW = weights.max

    // 1. Compute LSE(w) = maxW + ln( sum( exp(w_i - maxW) ) )
    val sumExpW = weights.foldLeft(.0) { (acc, w) => acc + exp(w - maxW) }
    val lseW = maxW + log(sumExpW)

    // 2. Compute LSE(2w) = 2*maxW + ln( sum( exp(2*(w_i - maxW)) ) )
    val sumExp2W = weights.foldLeft(.0) { (acc, w) => acc + exp(2 * (w - maxW)) }
    val lse2W = (2 * maxW) + log(sumExp2W)

    // 3. ESS = exp(2 * LSE(w) - LSE(2w))
    val logEss = (2 * lseW) - lse2W
    val ess = exp(logEss)

    // Clamp degeneracy ratio between [0.0, 1.0] to guard against tiny floating-point drift
    val degeneracyRatio = math.min(1, math.max(0, ess / count))

    val intervalWindow = window.asInstanceOf[IntervalWindow]

    c.output {
      ESSWindowDiagnostics(hid,
                           intervalWindow.start.toString,
                           intervalWindow.end.toString,
                           ess,
                           degeneracyRatio,
                           count.toLong)
    }

  }

}
