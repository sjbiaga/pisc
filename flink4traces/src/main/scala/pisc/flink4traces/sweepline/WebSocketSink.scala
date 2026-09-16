package pisc
package flink4traces
package sweepline

import java.io.IOException

import org.apache.flink.api.connector.sink2.{ Sink, SinkWriter, WriterInitContext }

import websocket.EmbeddedWebSocketServer


class WebSocketSink(port: Int, path: String) extends Sink[SweepLine1msBurst]:

  @throws[IOException]
  override def createWriter(context: WriterInitContext): SinkWriter[SweepLine1msBurst] =
    WebSocketSink.Writer(port, path)


object WebSocketSink:

  class Writer(port: Int, path: String) extends SinkWriter[SweepLine1msBurst]:

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
    override def write(element: SweepLine1msBurst, context: SinkWriter.Context): Unit =
      initServer
      val perPIDJson: Long => Option[String] = {
        case pid if element.perPIDSweepLine1msBurst.containsKey(pid) =>
          Some(element.perPIDSweepLine1msBurst.get(pid).toJson)
        case _ =>
          None
      }
      server.broadcastMessage({ case 0L => Some(element.toJson) case pid => perPIDJson(pid) })

    @throws[IOException]
    override def flush(endOfInput: Boolean): Unit = {}

    @throws[Exception]
    override def close(): Unit =
      if server ne null
      then
        println(s"Stopping embedded WebSocket server running on port $port...")
        server.stop(2000)
        server = null
