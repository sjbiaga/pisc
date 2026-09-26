package basc
package flink4traces

import org.apache.flink.api.common.typeinfo.TypeInformation
import org.apache.flink.util.OutputTag


package object cdf:

  case class PathTrace(hid: String,
                       root: Long,
                       totalEndToEndDelay: Double,
                       count: Int,
                       pathProbability: Double)

  case class CDFPercentiles(hid: String,
                            timestamp: Long,
                            totalPathCount: Long,
                            p50_Median_ms: Double,
                            p90_ms: Double,
                            p95_ms: Double,
                            p99_ms: Double,
                            maxDelay_ms: Double)

  object StreamTags:
    implicit val pathTraceTypeInfo: TypeInformation[PathTrace] = TypeInformation.of(classOf[PathTrace])
    // OutputTag defining the side-output for low-probability stochastic paths
    val lowProbabilityTag = new OutputTag[PathTrace]("low-probability-paths", pathTraceTypeInfo)
