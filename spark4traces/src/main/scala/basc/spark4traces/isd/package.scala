package basc
package spark4traces

import java.sql.Timestamp


package object isd {

  case class TraceNode(parents: Set[Long],
                       name: String,
                       label: String,
                       stepLogWeight: Double,
                       delay: Option[Double])

  // Cumulative trajectory state preserved across micro-batches per hierarchyId
  case class TraceState(hid: String,
                        cumulativeLogWeight: Double,
                        stepCount: Long,
                        lastUpdated: Long,
                        dagNodes: Map[Long, TraceNode],
                        bestPath: List[rca.PathStep],
                        maxPathWeight: Double,
                        observed: Option[Long] = None)

  case class CounterfactualWeightResult(hid: String,
                                        number: Long,
                                        logWeight: Double,         // w(T) = ln W(T)
                                        importanceWeight: Double,  // W(T) = exp(w(T))
                                        stepCount: Long,
                                        timestamp: Timestamp,
                                        criticalPath: List[rca.PathStep],
                                        isDegeneracyAlert: Boolean,
                                        isBlowoutAlert: Boolean)

  case class EssMetrics(count: Long,
                        ess: Double,
                        maxLogWeight: Double,
                        minLogWeight: Double)

  case class EssWindowDiagnostics(hid: String,
                                  windowStart: Timestamp,
                                  windowEnd: Timestamp,
                                  activeTrajectories: Long,
                                  effectiveSampleSize: Double,
                                  degeneracyRatio: Double,  // ESS / N
                                  maxLogWeight: Double,
                                  minLogWeight: Double)

  object rca {

    // {
    //   "mappings": {
    //     "properties": {
    //       "hid": { "type": "keyword" },
    //       "number": { "type": "long" },
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
                               timestamp: Timestamp,
                               logWeight: Double,
                               importanceWeight: Double,
                               anomalyType: String,
                               activeTrajectories: Long,
                               effectiveSampleSize: Double,
                               degeneracyRatio: Double,
                               criticalPath: List[rca.PathStep])

  }

}
