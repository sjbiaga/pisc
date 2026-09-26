package basc
package flink4traces


package object velocityreport:

  case class DepthTrace(name: String,
                        hid: String,
                        uuid: String,
                        label: String,
                        dir_cap: String,
                        clock: Double,
                        depth: Long,
                        parentClock: Double)

  case class AttributionReport(name: String,
                               uuid: String,
                               label: String,
                               dir_cap: String,
                               clockStep: Double,
                               microscopicThreshold: Double,
                               isMicroscopicStep: Boolean):
    def toJson: String =
      s"""{
          |"name":"$name",
          |"label":"$label",
          |"dir_cap":"$dir_cap",
          |"clockStep":$clockStep,
          |"microscopicThreshold":$microscopicThreshold,
          |"isMicroscopicStep":$isMicroscopicStep
          |}""".stripMargin.replaceAll("\n", "").trim


  case class WindowVelocityReport(name: String,
                                  uuid: String,
                                  label: String,
                                  dir_cap: String,
                                  clock: Double,
                                  deltaDepth: Long,
                                  deltaClock: Double,
                                  structuralVelocity: java.lang.Double, // ΔDepth / ΔClock
                                  eventDensity: Long):
    def toJson: String =
      s"""{
          |"name":"$name",
          |"label":"$label",
          |"dir_cap":"$dir_cap",
          |"clock":$clock,
          |"deltaDepth":$deltaDepth,
          |"deltaClock":$deltaClock,
          |"structuralVelocity":$structuralVelocity,
          |"eventDensity":$eventDensity
          |}""".stripMargin.replaceAll("\n", "").trim

  case class StructuralVelocityAlert(name: String,
                                     uuid: String,
                                     clock: Double,
                                     observedVelocity: Double,
                                     runningMean: Double,
                                     runningStdDev: Double,
                                     zScore: Double):
    def toJson: String =
      s"""{
          |"name":"$name",
          |"clock":$clock,
          |"observedVelocity":$observedVelocity,
          |"runningMean":$runningMean,
          |"runningStdDev":$runningStdDev,
          |"zScore":$zScore
          |}""".stripMargin.replaceAll("\n", "").trim

  case class MixedVelocityReport(attributionReport: Option[AttributionReport],
                                 windowVelocityReport: Option[WindowVelocityReport],
                                 structuralVelocityAlert: Option[StructuralVelocityAlert]):
    def uuid = attributionReport.map(_.uuid)
      .orElse(windowVelocityReport.map(_.uuid))
      .orElse(structuralVelocityAlert.map(_.uuid))
      .get
    def toJson: String =
      attributionReport.map { it =>
        s"""{
            |"type":"CLOCK_ATTRIBUTION",
            |"payload":${it.toJson}
            |}""".stripMargin.replaceAll("\n", "").trim
      }.orElse {
        windowVelocityReport.map { it =>
        s"""{
            |"type":"WINDOW_VELOCITY",
            |"payload":${it.toJson}
            |}""".stripMargin.replaceAll("\n", "").trim
        }
      }.orElse {
        structuralVelocityAlert.map { it =>
        s"""{
            |"type":"STRUCTURAL_VELOCITY",
            |"payload":${it.toJson}
            |}""".stripMargin.replaceAll("\n", "").trim
        }
      }.get
