package basc
package flink4traces
package velocityreport

import java.io.IOException

import scala.Option.when

import org.apache.flink.api.connector.sink2.{ Sink, SinkWriter, WriterInitContext }

import websocket.EmbeddedWebSocketServer


class WebSocketSink(port: Int, path: String) extends Sink[MixedVelocityReport]:

  @throws[IOException]
  override def createWriter(context: WriterInitContext): SinkWriter[MixedVelocityReport] =
    WebSocketSink.Writer(port, path)


object WebSocketSink:

  class Writer(port: Int, path: String) extends SinkWriter[MixedVelocityReport]:

    @transient private var server: EmbeddedWebSocketServer = null

    private def initServer: Unit =
      if server eq null
      then
        try
          server = EmbeddedWebSocketServer(port, path)
          server.start()
        catch e =>
          System.err.println(s"Failed to bind Embedded WebSocket Server on port $port: ${e.getMessage}")
          throw IOException(e)

    @throws[IOException]
    override def write(element: MixedVelocityReport, context: SinkWriter.Context): Unit =
      initServer
      server.broadcastMessage {
        case null => Some(element.toJson)
        case uuid if element.uuid == uuid => Some(element.toJson)
        case _ => None
      }

    @throws[IOException]
    override def flush(endOfInput: Boolean): Unit = {}

    @throws[Exception]
    override def close(): Unit =
      if server ne null
      then
        println(s"Stopping embedded WebSocket server running on port $port...")
        server.stop(2000)
        server = null
