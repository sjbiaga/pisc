package pisc
package beam4traces
package isd

import java.io.{ IOException, StringReader }

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

import org.apache.beam.sdk.transforms.DoFn
import org.apache.beam.sdk.transforms.DoFn._

import org.apache.hc.core5.http.HttpHost

import jakarta.json.stream.JsonParser
import org.opensearch.client.json.JsonData
import org.opensearch.client.json.jackson.JacksonJsonpMapper

import org.opensearch.client.opensearch.OpenSearchClient
import org.opensearch.client.opensearch.core.BulkRequest
import org.opensearch.client.opensearch.core.bulk.{ BulkOperation, IndexOperation }
import org.opensearch.client.transport.httpclient5.ApacheHttpClient5TransportBuilder
import org.opensearch.client.transport.OpenSearchTransport

import org.slf4j.LoggerFactory


class OpenSearchWriteFn(
  host: String,
  port: Int,
  user: String,
  pass: String,
  indexName: String,
  batchSize: Int
) extends DoFn[(String, String), Void] {

  private val logger = LoggerFactory.getLogger(classOf[OpenSearchWriteFn])

  @transient private var transport: OpenSearchTransport = _
  @transient private var openSearchClient: OpenSearchClient = _
  @transient private var jsonMapper: JacksonJsonpMapper = _
  @transient private var buffer: ListBuffer[(String, String)] = _

  @Setup
  def setup(): Unit = {
    jsonMapper = new JacksonJsonpMapper()

    if (pass.isEmpty) {
      val httpHost = new HttpHost("http", host, port)
      transport = ApacheHttpClient5TransportBuilder
        .builder(httpHost)
        .setMapper(jsonMapper)
        .build()
    } else {
      val httpHost = new HttpHost("https", host, port)
      transport = ApacheHttpClient5TransportBuilder
        .builder(httpHost)
        .setMapper(jsonMapper)
        .setHttpClientConfigCallback { httpClientBuilder =>

          import org.apache.hc.client5.http.auth._
          import org.apache.hc.client5.http.impl.auth._

          val credentialsProvider = new BasicCredentialsProvider()
          credentialsProvider.setCredentials(
            new AuthScope(httpHost),
            new UsernamePasswordCredentials(user, pass.toCharArray()))
          httpClientBuilder.setDefaultCredentialsProvider(credentialsProvider)

          if (host == "localhost") {

            import org.apache.hc.client5.http.impl.nio._
            import org.apache.hc.client5.http.ssl._
            import org.apache.hc.core5.ssl._

            val sslContext = SSLContextBuilder
              .create()
              .loadTrustMaterial(null, { (_, _) => true })
              .build()
            val tlsStrategy = ClientTlsStrategyBuilder.create()
              .setSslContext(sslContext)
              .setHostnameVerifier(NoopHostnameVerifier.INSTANCE)
              .buildAsync()
            val connectionManager = PoolingAsyncClientConnectionManagerBuilder.create()
              .setTlsStrategy(tlsStrategy)
              .build()

            httpClientBuilder.setConnectionManager(connectionManager)
          } else
            httpClientBuilder
        }
        .build()
    }

    openSearchClient = new OpenSearchClient(transport)
  }

  @StartBundle
  def startBundle(): Unit =
    buffer = ListBuffer[(String, String)]()

  @ProcessElement
  def processElement(@Element element: (String, String)): Unit = {
    buffer += element
    if (buffer.size >= batchSize)
      flush()
  }

  @FinishBundle
  def finishBundle(): Unit =
    if (buffer.nonEmpty)
      flush()

  private def flush(): Unit = {
    val bulkBuilder = new BulkRequest.Builder()

    val operationsList = buffer.map { case (docId, jsonStr) =>
      val reader = new StringReader(jsonStr)

      val parser = jsonMapper.jsonProvider().createParser(reader)

      val jsonData = JsonData.from(parser, jsonMapper)

      val indexOp = new IndexOperation.Builder[JsonData]()
        .index(indexName)
        .id(docId)
        .document(jsonData)
        .build()

      new BulkOperation.Builder().index(indexOp).build()
    }.asJava

    val bulkRequest = new BulkRequest.Builder()
      .operations(operationsList)
      .build()

    try {
      val response = openSearchClient.bulk(bulkRequest)
      if (response.errors()) {
        response.items().asScala.foreach { item =>
          if (item.error() != null) {
            logger.error(s"Document write failure: ${item.error().reason()}")
          }
        }
        throw new IOException("Aborting bundle processing due to failures during OpenSearch bulk indexing.")
      }
    } catch {
      case e: IOException =>
        logger.error("Failed to post bulk request to OpenSearch Cluster", e)
        throw e
    } finally
      buffer.clear()
  }

  @Teardown
  def teardown(): Unit =
    if (transport != null)
      transport.close()

}
