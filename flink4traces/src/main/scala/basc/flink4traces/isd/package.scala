package basc
package flink4traces

import org.apache.flink.api.common.typeinfo.TypeInformation
import org.apache.flink.util.OutputTag


package object isd:

  case class CausalSubTreeSummary(hid: String,
                                  totalLogWeight: Double,
                                  totalEvents: Int,
                                  totalDuration: Double,
                                  windowEndTime: Long)

  case class ESSWindowDiagnostics(hid: String,
                                  windowStart: Long,
                                  windowEnd: Long,
                                  sampleCount: Long,             // Total trace sub-trees in window (M)
                                  effectiveSampleSize: Double,   // Computed ESS
                                  efficiencyRatio: Double,       // ESS / M (0.0 to 1.0)
                                  maxLogWeight: Double,          // Tracks weight extremes
                                  weightedMeanDuration: Double): // Self-Normalized Importance Sampling latency
    def label = hid.substring(hid.indexOf('-') + 1)
    def toJson: String =
      s"""{
          |"label":"$label",
          |"windowStart":$windowStart,
          |"windowEnd":$windowEnd,
          |"sampleCount":$sampleCount,
          |"effectiveSampleSize":$effectiveSampleSize,
          |"efficiencyRatio":$efficiencyRatio,
          |"maxLogWeight":$maxLogWeight,
          |"weightedMeanDuration":$weightedMeanDuration
          |}""".stripMargin.replaceAll("\n", "").trim

  case class ESSMixedDiagnostics(essDiagnostics: Either[ESSWindowDiagnostics, ESSWindowDiagnostics])
      extends websocket.WebSocketSink.SinkableElement:
    override def uuid: String =
      val hid = essDiagnostics.toOption.orElse(essDiagnostics.swap.toOption).get.hid
      hid.substring(0, hid.indexOf('-'))
    override def toJson: String =
      val json = essDiagnostics.toOption.orElse(essDiagnostics.swap.toOption).get.toJson
      s"""{
          |"type":"${if essDiagnostics.isLeft then "ESS_DIAGNOSTICS" else "DEGENERACY_ALERT"}",
          |"payload":$json
          |}""".stripMargin.replaceAll("\n", "").trim


  object rca:

    // Single step inside the critical causal path
    case class PathStep(number: Long,
                        name: String,
                        label: String,
                        stepLogWeight: Double,
                        delay: Double)

    // Diagnostic report for Root Cause Analysis
    case class RootCauseReport(hid: String,
                               totalTreeLogWeight: Double,
                               criticalPathLogWeight: Double,
                               criticalPathLength: Int,
                               pathWeightContributionRatio: Double, // Percentage of total tree weight driven by this single path
                               criticalPathSteps: List[PathStep],
                               windowEndTime: Long)


  object StreamTags:
    implicit val rcaTypeInfo: TypeInformation[rca.RootCauseReport] = TypeInformation.of(classOf[rca.RootCauseReport])
    // OutputTag defining the side-output for root cause analysis
    val rcaTag = new OutputTag[rca.RootCauseReport]("root-cause-analysis", rcaTypeInfo)
