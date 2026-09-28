package basc
package flink4traces
package websocket

import java.io.IOException

import org.apache.flink.api.connector.sink2.{ Sink, SinkWriter, WriterInitContext }

import websocket.EmbeddedWebSocketServer
import WebSocketSink.SinkableElement


class WebSocketSink[E <: SinkableElement](port: Int,
                                          path: String,
                                          perUUIDJson: E => String => Option[String] =
                                            { (element: E) => {
                                                case null                         => Some(element.toJson)
                                                case uuid if element.uuid == uuid => Some(element.toJson)
                                                case _                            => None
                                              }
                                            }
                                         )
    extends Sink[E]:

  @throws[IOException]
  override def createWriter(context: WriterInitContext): SinkWriter[E] =
    WebSocketSink.Writer(port, path, perUUIDJson)


object WebSocketSink:

  abstract trait SinkableElement:
    def uuid: String = ???
    def toJson: String

  class Writer[E <: SinkableElement](port: Int, path: String, perUUIDJson: E => String => Option[String])
      extends SinkWriter[E]:

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
    override def write(element: E, context: SinkWriter.Context): Unit =
      initServer
      server.broadcastMessage { case null => Some(element.toJson) case uuid => perUUIDJson(element)(uuid) }

    @throws[IOException]
    override def flush(endOfInput: Boolean): Unit = {}

    @throws[Exception]
    override def close(): Unit =
      if server ne null
      then
        println(s"Stopping embedded WebSocket server running on port $port...")
        server.stop(2000)
        server = null
