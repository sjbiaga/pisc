package basc
package beam4traces

import java.util.Map

import org.apache.avro.generic.GenericRecord
import org.apache.kafka.common.serialization.Deserializer


final class KafkaAvroDeserializer extends Deserializer[GenericRecord] {
  private val inner = new io.confluent.kafka.serializers.KafkaAvroDeserializer()

  override def configure(configs: Map[String, _], isKey: Boolean): Unit = {
    inner.configure(configs, isKey)
  }

  override def deserialize(topic: String, data: Array[Byte]): GenericRecord = {
    inner.deserialize(topic, data).asInstanceOf[GenericRecord]
  }

  override def close(): Unit = inner.close()
}
