package basc
package beam4traces

import java.sql.Timestamp

import org.apache.beam.sdk.coders.DefaultCoder
import org.apache.beam.sdk.coders.SerializableCoder


package object isd {

  @DefaultCoder(classOf[SerializableCoder[TraceNode]])
  case class TraceNode(parents: Set[Long],
                       name: String,
                       label: String,
                       stepLogWeight: Double,
                       delay: Option[Double])

  case class CounterfactualWeightResult(hid: String,
                                        number: Long,
                                        clock: Double,
                                        logWeight: Double,         // w(T) = ln W(T)
                                        importanceWeight: Double,  // W(T) = exp(w(T))
                                        stepCount: Long,
                                        timestamp: Timestamp,
                                        criticalPath: List[rca.PathStep],
                                        isDegeneracyAlert: Boolean,
                                        isBlowoutAlert: Boolean)

  case class ESSWindowDiagnostics(hid: String,
                                  windowStart: String,
                                  windowEnd: String,
                                  effectiveSampleSize: Double,
                                  degeneracyRatio: Double,  // ESS / N
                                  count: Long)

  object rca {

    // {
    //   "mappings": {
    //     "properties": {
    //       "hid": { "type": "keyword" },
    //       "number": { "type": "long" },
    //       "clock": { "type": "double" },
    //       "timestamp": { "type": "date" },
    //       "logWeight": { "type": "double" },
    //       "importanceWeight": { "type": "double" },
    //       "anomalyType": { "type": "keyword" },
    //       "activeTrajectories": { "type": "long" },
    //       "effectiveSampleSize": { "type": "double" },
    //       "degeneracyRatio": { "type": "double" },
    //       "criticalPath": {
    //         "type": "nested",
    //         "properties": {
    //           "number": { "type": "long" },
    //           "name": { "type": "keyword" },
    //           "label": { "type": "keyword" },
    //           "stepLogWeight": { "type": "double" },
    //           "delay": { "type": "double" }
    //         }
    //       }
    //     }
    //   }
    // }

    case class PathStep(number: Long,
                        name: String,
                        label: String,
                        stepLogWeight: Double,
                        delay: Double)

    case class RootCauseReport(hid: String,
                               number: Long,
                               clock: Double,
                               timestamp: String,
                               logWeight: Double,
                               importanceWeight: Double,
                               anomalyType: String,
                               activeTrajectories: Long,
                               effectiveSampleSize: Double,
                               degeneracyRatio: Double,
                               criticalPath: List[rca.PathStep]) {
      import io.circe.generic.auto._
      import io.circe.syntax._

      def toJson: String = this.asJson.noSpaces
    }

    // object RootCauseReport {
    //   import com.fasterxml.jackson.databind.JsonNode
    //   import org.apache.beam.sdk.io.elasticsearch.ElasticsearchIO.Write.FieldValueExtractFn

    //   val docIdFn = new FieldValueExtractFn {
    //     override def apply(input: JsonNode): String =
    //       input.at("hid").textValue + '_' + input.at("number").asLong
    //   }
    // }

  }

}
