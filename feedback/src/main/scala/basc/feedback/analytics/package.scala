package basc
package feedback

import _root_.io.circe.{ Codec, Decoder, DecodingFailure, Encoder, Json }
import _root_.io.circe.generic.auto.*


package object analytics:

  enum Plugin derives Codec.AsObject:
    case causes(causes: Set[Long])
    case parents(numbers: Set[Long])
    case probability(probability: BigDecimal)
    case syncRate(rate: Option[BigDecimal])
    case whatIf(fraction: (BigDecimal, BigDecimal), difference: BigDecimal)

  def time(milliseconds: Long): Option[String] =
    val seconds = milliseconds / 1000
    val minutes = seconds / 60
    val hours = minutes / 60
    val days = hours / 24

    if seconds > 0
    then
      if minutes > 0
      then
        if hours > 0
        then
          if days > 0
          then
            Some(s"$days DAYS ${hours%24}:${minutes%60}:${seconds%60}")
          else
            Some(s"${hours%24}:${minutes%60}:${seconds%60}")
        else
          Some(s"${minutes%60}:${seconds%60}")
      else
        Some(s"${seconds%60}")
    else
      None

  object ast:

    export Pre.*
    export AST.*

    type Bind = (`(*)`, +)

    object Bind:
      given Decoder[List[Bind]] = Decoder.instance { c =>
        c.focus.flatMap(_.asObject).map { json =>
          json.keys.flatMap { json(_)
            .flatMap(_.asObject).flatMap { bind =>
              bind("agent").flatMap(_.as[AST].toOption.map(_.asInstanceOf[`(*)`])) zip
              bind("body").flatMap(_.as[AST].toOption.map(_.asInstanceOf[+]))
            }
          }.toList
        }.toRight(DecodingFailure("decode List[Bind] error", c.history))
      }

    enum `$` derives Codec.AsObject { case local, s2s, p2c, c2p }

    enum Cap derives Codec.AsObject { case enter, accept, exit, expel, `merge+`, `merge-` }

    sealed trait Rate extends Any
    case class ∞(weight: Long) extends AnyVal with Rate
    case class `ℝ⁺`(rate: BigDecimal) extends AnyVal with Rate
    case class ⊤(weight: Long) extends AnyVal with Rate

    object Rate:

      given Decoder[Rate] = Decoder.decodeString.map { rate =>
        rate.charAt(0) match
          case '∞' => ∞(rate.substring(2, rate.length - 1).toLong)
          case '⊤' => ⊤(rate.substring(2, rate.length - 1).toLong)
          case _   => `ℝ⁺`(BigDecimal(rate.substring(3, rate.length - 1)))
      }
      given Encoder[Rate] = Encoder.encodeString.contramap(_.toString)

    type Names = Set[String]

    trait Positional:
      val line: Int
      val column: Int

    trait Bound:
      val bound: Names

    trait Free:
      val free: Names

    trait Act:
      val key: String
      val rate: Option[Rate]

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
             override val key: String,
             override val rate: Option[Rate])
          extends Pre with Act

      case π(direction: `$`,
             channel: String,
             argument: Option[Json],
             parameter: Option[String],
             polarity: Boolean,
             override val line: Int,
             override val column: Int,
             override val bound: Names,
             override val free: Names,
             override val key: String,
             override val rate: Option[Rate],
             cons: Option[String])
          extends Pre with Act

      private case πʹ(channel: String,
                      operator: String,
                      operands: Json,
                      override val line: Int,
                      override val column: Int,
                      override val bound: Names,
                      override val free: Names)

      case ζ(capability: Cap,
             channel: String,
             polarity: Boolean,
             override val line: Int,
             override val column: Int,
             override val bound: Names,
             override val free: Names,
             override val key: String,
             override val rate: Option[Rate])
          extends Pre with Act

      override def toString: String = this match
        case ν(_, _, _, _, names*) => names.mkString("ν(", ", ", ")")
        case π(dir, channel, Some(argument), _, false, _, _, _, _, _, _, Some("ν")) =>
          import Pre.given
          val Symbol(name) = argument: λ
          (if dir == `$`.local then "" else "" + dir + " ") + channel + " ! {ν" + name + "}."
        case π(dir, channel, Some(argument), _, false, _, _, _, _, _, _, _) =>
          import Pre.given
          val name: String = argument
          (if dir == `$`.local then "" else "" + dir + " ") + channel + " ! {" + name + "}."
        case π(dir, channel, _, Some(name), true, _, _, _, _, _, _, None) =>
          (if dir == `$`.local then "" else "" + dir + " ") + channel + " ? {" + name + "}."
        case π(_, channel, Some(argument), _, true, _, _, _, _, _, _, Some(cons)) =>
          import Pre.given
          channel + s"$cons ? {" + argument.asArray.get.map(it => it: String).mkString(",") + "}."
        case ζ(cap, name, _, _, _, _, _, _, _) =>
          "" + cap + " " + name + "."
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

      given Decoder[Pre] = Decoder.instance { c =>
        c.focus.flatMap(_.asObject).flatMap { json =>
          json.keys.headOption.flatMap {
            case "ν"  => json("ν").flatMap(_.as[ν].toOption)
            case "τ"  => json("τ").flatMap(_.as[τ].toOption)
            case "πν" => json("πν").flatMap(_.as[π].toOption.map(_.copy(cons = Some("ν"))))
            case "π"  => json("π").flatMap(_.as[π].toOption)
            case "ζ"  => json("ζ").flatMap(_.as[ζ].toOption)
            case cons =>
              json(cons)
                .flatMap(_.as[πʹ].toOption)
                .map { it =>
                  π(`$`.local,
                    it.channel,
                    Some(it.operands),
                    None,
                    true,
                    it.line,
                    it.column,
                    it.bound,
                    it.free,
                    null,
                    None,
                    Some(cons))
                  }
          }
        }.toRight(DecodingFailure("decode Pre error", c.history))
      }

    enum AST extends Positional with Free:

      case +(override val line: Int,
             override val column: Int,
             override val free: Names,
             scaling: Int,
             operands: AST*)

      case ∥(override val line: Int,
             override val column: Int,
             override val free: Names,
             scaling: Int,
             operands: AST*)

      case `.`(override val line: Int,
               override val column: Int,
               override val free: Names,
               leaf: AST,
               prefixes: Pre*)

      case ?:(override val line: Int,
              override val column: Int,
              override val free: Names,
              lhs: Json,
              rhs: Json,
              mismatch: Boolean,
              t: AST,
              f: Option[AST])

      case !(override val line: Int,
             override val column: Int,
             override val free: Names,
             parallelism: Int,
             pace: Option[Json],
             guard: Option[Pre],
             operand: AST)

      case `[]`(override val line: Int,
                override val column: Int,
                override val free: Names,
                label: Option[String],
                operand: AST)

      case `(*)`(override val line: Int,
                 override val column: Int,
                 override val free: Names,
                 identifier: String,
                 parameters: Json*)

      override def toString: String = this match
        case ∅() => "()"
        case +(_, _, _, -1, choices*) => choices.mkString(" + ")
        case +(_, _, _, sc, choices*) => "" + sc + " * " + choices.mkString(" + ")

        case ∥(_, _, _, -1, components*) => components.mkString(" | ")
        case ∥(_, _, _, sc, components*) => "" + sc + " * " + components.mkString(" | ")

        case `.`(_, _, _, ∅()) => "()"
        case `.`(_, _, _, ∅(), prefixes*) => prefixes.mkString(" ") + " ()"
        case `.`(_, _, _, end: +, prefixes*) =>
          prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + "(" + end + ")"
        case `.`(_, _, _, end, prefixes*) =>
          prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + end

        case ?:(_, _, _, lhs, rhs, mismatch, t, f) =>
          import Pre.given
          val test = "" + (lhs: String) + (if mismatch then " ≠ " else " = ") + (rhs: String)
          if f.isEmpty
          then
            "[ " + test + " ] " + t
          else
            "if " + test + " then " + t + " else " + f.get

        case !(_, _, _, -1, _, guard, sum) =>
          "!" + guard.map("." + _).getOrElse("") + sum

        case !(_, _, _, parallelism, _, guard, sum) if parallelism < -1 =>
          s"¡${-(parallelism%Int.MaxValue)}*" + guard.map("." + _).getOrElse("") + sum

        case !(_, _, _, parallelism, _, guard, sum) =>
          s"!$parallelism*" + guard.map("." + _).getOrElse("") + sum

        case `[]`(_, _, _, label, sum) =>
          label.getOrElse("") + "[ " + sum + " ]"

        case `(*)`(_, _, _, identifier, params*) =>
          import Pre.given
          params.map(it => it: String).toList.mkString(s"$identifier(", ",", ")")

    object AST:

      given Decoder[AST] = Decoder.instance { c =>
        c.focus.flatMap(_.asObject).flatMap { json =>
          json.keys.headOption.flatMap {
            case "+"   => json("+").flatMap(_.as[+].toOption)
            case "∥"   => json("∥").flatMap(_.as[∥].toOption)
            case "."   => json(".").flatMap(_.as[`.`].toOption)
            case "?:"  => json("?:").flatMap(_.as[`?:`].toOption)
            case "¡"   => json("¡").flatMap(_.as[!].toOption).map { it => it.copy(parallelism = if it.parallelism == 1 then Int.MinValue else -it.parallelism) }
            case "!"   => json("!").flatMap(_.as[!].toOption)
            case "[]"  => json("[]").flatMap(_.as[`[]`].toOption)
            case "(*)" => json("(*)").flatMap(_.as[`(*)`].toOption)
            case _     => None
          }
        }.toRight(DecodingFailure("decode AST error", c.history))
      }

    type λ = Symbol | BigDecimal | Boolean | String | Unit

    object ∅ :
      def unapply(self: AST): Boolean = self match
        case +(_, _, _, _) => true
        case _             => false
