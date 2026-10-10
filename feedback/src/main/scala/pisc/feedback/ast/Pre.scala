package pisc
package feedback
package ast

import io.circe.{ Codec, Json }


enum Pre extends Positional with Bound with Free derives Codec.AsObject:

  case ν(override val line: Int,
         override val column: Int,
         override val bound: Names,
         override val free: Names,
         names: String*) // forcibly

  case τ(override val line: Int,
         override val column: Int,
         override val bound: Names,
         override val free: Names,
         override val agent: Option[String],
         override val label: Option[String],
         override val key: String,
         override val guard: Boolean,
         override val rate: Rate)
      extends Pre with Act

  case π(override val line: Int,
         override val column: Int,
         override val bound: Names,
         override val free: Names,
         channel: String,
         argument: Option[Json],
         parameter: Option[String],
         polarity: Boolean,
         override val agent: Option[String],
         override val label: Option[String],
         override val key: String,
         override val guard: Boolean,
         override val rate: Rate)
      extends Pre with Act

  case ::(override val line: Int,
          override val column: Int,
          override val bound: Names,
          override val free: Names,
          channel: String,
          operator: String,
          operands: List[String])

  override def toString: String = this match
    case ν(_, _, _, _, names*) => names.mkString("ν(", ", ", ")")
    case π(_, _, _, _, channel, Some(_), Some(parameter), false, _, _, _, _, _) =>
      channel + " ! {ν" + parameter + "}."
    case π(_, _, _, _, channel, Some(argument), None, false, _, _, _, _, _) =>
      import Pre.given
      val name: String = argument
      channel + " ! {" + name + "}."
    case π(_, _, _, _, channel, _, Some(name), true, _, _, _, _, _) =>
      channel + " ? {" + name + "}."
    case ::(_, _, _, _, channel, operator, operands) =>
      channel + s"$operator ? {" + operands.mkString(",") + "}."
    case _ => "τ."


object Pre:

  given Conversion[Json, String] = given_Conversion_Json_λ(_) match
    case Symbol(it) => it
    case () => "/*…*/"
    case it => it.toString

  given Conversion[Json, λ] = { it =>
    None
      .orElse(it.asString.flatMap { s =>
        if s.startsWith("\"") && s.endsWith("\"")
        then Some(s.stripPrefix("\"").stripSuffix("\""))
        else Some(Symbol(s))
      })
      .orElse(it.asNumber.flatMap(_.toBigDecimal))
      .orElse(it.asBoolean)
      .orElse(it.asNull)
      .get
  }
