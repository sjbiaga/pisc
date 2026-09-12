package basc
package flink4traces
package websocket

import java.io.IOException

import org.apache.flink.api.connector.sink2.{ Sink, SinkWriter, WriterInitContext }


class WebSocketSink(port: Int, path: String) extends Sink[LoadAvg]:

  @throws[IOException]
  override def createWriter(context: WriterInitContext): SinkWriter[LoadAvg] =
    WebSocketSink.Writer(port, path)


object WebSocketSink:

  def toJson(element: LoadAvg): String =
    s"""{
        |"timestamp":${element.timestamp},
        |"clock":${element.clock},
        |"oneMinuteLoad":${element.oneMinuteLoad},
        |"tenMinutesLoad":${element.tenMinutesLoad},
        |"fifteenMinutesLoad":${element.fifteenMinutesLoad}
        |}""".stripMargin.replaceAll("\n", "").trim

  class Writer(port: Int, path: String) extends SinkWriter[LoadAvg]:

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
    override def write(element: LoadAvg, context: SinkWriter.Context): Unit =
      initServer
      val perPIDJson: Long => Option[String] = {
        case pid if element.perPIDLoadAvg.containsKey(pid) =>
          Some(toJson(element.perPIDLoadAvg.get(pid)))
        case _ =>
          None
      }
      server.broadcastMessage({ case 0L => Some(toJson(element)) case pid => perPIDJson(pid) })

    @throws[IOException]
    override def flush(endOfInput: Boolean): Unit = {}

    @throws[Exception]
    override def close(): Unit =
      if server ne null
      then
        println(s"Stopping embedded WebSocket server running on port $port...")
        server.stop(2000)
        server = null
