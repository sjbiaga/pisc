package basc
package flink4traces

import java.util.{ HashMap => Map, LinkedList => List }


package object sweepline:

  def toJson(histogram: Map[Int, Double]): String =
    val result = List[String]()
    histogram.forEach { (count, percentage) =>
      result.add(s""""$count":$percentage""")
    }
    java.lang.String.join(",", result)

  def toJsonʹ(histograms: Map[String, SweepLine]): String =
    val result = List[String]()
    histograms.forEach { (label, element) =>
      result.add(s""""$label":${element.toJson}""")
    }
    java.lang.String.join(",", result)

  case class SweepLine(timestamp: Long,
                       label: String,
                       clock: Double,
                       histogram: Map[Int, Double],
                       perPIDSweepLine: Map[Long, SweepLine]):
    def toJson: String =
      s"""{
          |"timestamp":$timestamp,
          |"label":"$label",
          |"clock":$clock,
          |"histogram":{${sweepline.toJson(histogram)}}
          |}""".stripMargin.replaceAll("\n", "").trim

  case class SweepLine1msBurst(timestamp: Long,
                               clock: Double,
                               histograms: Map[String, SweepLine],
                               perPIDSweepLine1msBurst: Map[Long, SweepLine1msBurst]):
    def toJson: String =
      s"""{
          |"timestamp":$timestamp,
          |"clock":$clock,
          |"histograms":{${sweepline.toJsonʹ(histograms)}}
          |}""".stripMargin.replaceAll("\n", "").trim
