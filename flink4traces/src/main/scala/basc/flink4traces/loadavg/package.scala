package basc
package flink4traces

import java.util.{ HashMap => Map, LinkedList => List }


package object loadavg:

  def toJsonʹ(averages: Map[String, LoadAvg]): String =
    val result = List[String]()
    averages.forEach { (label, element) =>
      result.add(s""""$label":${element.toJson}""")
    }
    java.lang.String.join(",", result)

  case class LoadAvg(timestamp: Long,
                     label: String,
                     clock: Double,
                     oneMinuteLoad: Double,
                     tenMinutesLoad: Double,
                     fifteenMinutesLoad: Double,
                     perPIDLoadAvg: Map[Long, LoadAvg]):
    def toJson: String =
      s"""{
          |"timestamp":$timestamp,
          |"clock":$clock,
          |"oneMinuteLoad":$oneMinuteLoad,
          |"tenMinutesLoad":$tenMinutesLoad,
          |"fifteenMinutesLoad":$fifteenMinutesLoad
          |}""".stripMargin.replaceAll("\n", "").trim

  case class LoadAvg1msBurst(timestamp: Long,
                             clock: Double,
                             averages: Map[String, LoadAvg],
                             perPIDLoadAvg1msBurst: Map[Long, LoadAvg1msBurst]):
    def toJson: String =
      s"""{
          |"timestamp":$timestamp,
          |"clock":$clock,
          |"averages":{${loadavg.toJsonʹ(averages)}}
          |}""".stripMargin.replaceAll("\n", "").trim
