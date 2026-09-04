package basc
package feedback

import io.circe.{ Codec, Json }


package object kafka:

  case class CreateConsumerConfig(name: String, format: String,
                                  `auto.offset.reset`: String = "latest",
                                  `auto.commit.enable`: String = "false") derives Codec.AsObject
  case class CreateConsumerResponse(instance_id: String, base_uri: String) derives Codec.AsObject
  case class SubscriptionPayload(topics: List[String]) derives Codec.AsObject

  abstract class AbstractKafkaRecord[K, V]:
    val topic: String
    val key: K
    val value: V
    val partition: Int
    val offset: Long

  case class AvroKafkaRecord(topic: String, key: Json, value: Json, partition: Int, offset: Long) extends AbstractKafkaRecord[Json, Json] derives Codec.AsObject
