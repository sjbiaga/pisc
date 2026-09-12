package basc
package flink4traces
package websocket

import java.net.{ InetSocketAddress, URI }

import java.util.concurrent.ConcurrentHashMap

import org.java_websocket.server.WebSocketServer
import org.java_websocket.handshake.ClientHandshake
import org.java_websocket.WebSocket


class EmbeddedWebSocketServer(port: Int, path: String)
    extends WebSocketServer(InetSocketAddress(port)):

  private val connections = ConcurrentHashMap[WebSocket, Long]

  override def onOpen(conn: WebSocket, handshake: ClientHandshake): Unit =
    val uri = URI(handshake.getResourceDescriptor)
    val path = uri.getPath.stripPrefix("/")
    if path.toLowerCase == this.path.toLowerCase
    then
      val query = uri.getQuery
      val pid =
        val i = query.indexOf('=')
        if i < 0 || query.substring(0, i).toLowerCase != "pid"
        then
          None
        else
          try
            Some(query.substring(i+1).toLong).filter(_ >= 0)
          catch _ =>
            None
      connections.put(conn, pid.getOrElse(0L))
      println(s"[WS Server] Browser connected: ${conn.getRemoteSocketAddress}")
    else
      conn.close(400, "Bad Request: Incorrect path inside the URL.")
      println(s"[WS Server] Rejected connection from ${conn.getRemoteSocketAddress}: Incorrect path parameter")

  override def onClose(conn: WebSocket, code: Int, reason: String, remote: Boolean): Unit =
    connections.remove(conn)
    println(s"[WS Server] Browser disconnected: ${conn.getRemoteSocketAddress}")

  override def onMessage(conn: WebSocket, message: String): Unit = {}
    // Left empty: The UI only consumes load metrics, it does not send data upstream

  override def onError(conn: WebSocket, ex: Exception): Unit =
    println(s"[WS Server] Error on connection ${if conn ne null then conn.getRemoteSocketAddress else "global"}: ${ex.getMessage}")

  override def onStart(): Unit =
    println(s"[WS Server] Embedded server successfully started on port $port")

  def broadcastMessage(text: Long => Option[String]): Unit =
    connections.forEach {
      case (conn, pid) if conn.isOpen =>
        conn.synchronized:
          try
            text(pid).map(conn.send)
          catch _.printStackTrace
      case _ =>
    }
