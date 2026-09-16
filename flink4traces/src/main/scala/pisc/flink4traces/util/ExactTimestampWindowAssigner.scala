package pisc
package flink4traces
package util

import java.util.{ Collection, Collections }

import org.apache.flink.api.common.ExecutionConfig
import org.apache.flink.api.common.typeutils.TypeSerializer
import org.apache.flink.streaming.api.windowing.triggers.{ EventTimeTrigger, Trigger }
import org.apache.flink.streaming.api.windowing.assigners.WindowAssigner
import org.apache.flink.streaming.api.windowing.windows.TimeWindow


object ExactTimestampWindowAssigner extends WindowAssigner[Object, TimeWindow]:

  override def assignWindows(element: Object, timestamp: Long, context: WindowAssigner.WindowAssignerContext): Collection[TimeWindow] =
    // Create a 1ms window for this precise timestamp: [timestamp, timestamp + 1)
    Collections.singletonList(new TimeWindow(timestamp, timestamp + 1))

  override def getDefaultTrigger: Trigger[Object, TimeWindow] =
    // Standard event-time trigger fires automatically when watermark >= window.getEnd
    EventTimeTrigger.create

  override def getWindowSerializer(executionConfig: ExecutionConfig): TypeSerializer[TimeWindow] =
    new TimeWindow.Serializer()

  override def isEventTime: Boolean = true
