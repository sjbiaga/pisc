package pisc
package feedback

import io.circe.{ Codec, Decoder, DecodingFailure, Encoder, Json }


package object ast:

  export Pre.*, AST.*

  type Bind = (`(*)`, +)

  object Bind:

    given Decoder[List[Bind]] = Decoder.instance { c =>
      c.focus.flatMap(_.asArray).map {
        _.flatMap {
          _.asObject.flatMap { json =>
            json("agent").flatMap(_.as[AST].toOption.map(_.asInstanceOf[`(*)`])) zip
            json("body").flatMap(_.as[AST].toOption.map(_.asInstanceOf[+]))
          }.toVector
        }.toList
      }.toRight(DecodingFailure("decode List[Bind] error", c.history))
    }

  enum `$` derives Codec.AsObject { case local, s2s, p2c, c2p }

  enum Cap derives Codec.AsObject { case enter, accept, exit, expel, `merge+`, `merge-` }

  enum Rate derives Codec.AsObject:
    case ∞(weight: Long)
    case `ℝ⁺`(rate: BigDecimal)
    case ⊤(weight: Long)

  type Names = Set[String]

  trait Positional:
    val line: Int
    val column: Int

  trait Bound:
    val bound: Names

  trait Free:
    val free: Names

  trait Act:
    val agent: Option[String]
    val label: Option[String]
    val key: String
    val guard: Boolean
    val rate: Rate

  object Act:

    def unapply(self: Pre): Some[Boolean] =
      self match
        case _: Act => Some(true)
        case _ => Some(false)

  type λ = Symbol | BigDecimal | Boolean | String | Unit

  type μ = π | τ
