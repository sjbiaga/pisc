package basc
package flink4traces
package functions

import org.apache.flink.streaming.api.functions.KeyedProcessFunction
import org.apache.flink.util.Collector

import velocityreport.{ AttributionReport, DepthTrace }


class ClockStepAttributor(microscopicThreshold: Double)
  extends KeyedProcessFunction[String, DepthTrace, AttributionReport]:

  override def processElement(value: DepthTrace,
                              ctx: KeyedProcessFunction[String, DepthTrace, AttributionReport]#Context,
                              out: Collector[AttributionReport]): Unit =

      val clockStep = value.clock - value.parentClock

      // Only attribute positive updates to prevent duplicate tracking from sibling polarities
      if clockStep >= .0
      then
        out.collect:
          AttributionReport(value.name,
                            value.uuid,
                            value.label,
                            value.dir_cap,
                            clockStep,
                            microscopicThreshold,
                            clockStep < microscopicThreshold)
