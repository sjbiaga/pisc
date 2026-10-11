package basc
package feedback
package ast

import scala.Option.when
import scala.util.Try

import io.circe.{ Codec, Json }


enum AST extends Positional with Free derives Codec.AsObject:

  case +(override val line: Int,
         override val column: Int,
         override val free: Names,
         operands: AST*)

  case ∥(override val line: Int,
         override val column: Int,
         override val free: Names,
         operands: AST*)

  case `.`(override val line: Int,
           override val column: Int,
           override val free: Names,
           operand: AST,
           prefixes: Pre*)

  case ?:(override val line: Int,
          override val column: Int,
          override val free: Names,
          lhs: Json,
          rhs: Json,
          mismatch: Boolean,
          t: AST,
          f: Option[AST])

  case `¡`(override val line: Int,
           override val column: Int,
           override val free: Names,
           parallelism: Int,
           pace: Option[(Long, String)],
           guard: Pre,
           operand: AST)

  case !(override val line: Int,
         override val column: Int,
         override val free: Names,
         parallelism: Int,
         pace: Option[(Long, String)],
         guard: Pre,
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
    case +(_, _, _, choices*) => choices.mkString(" + ")

    case ∥(_, _, _, components*) => components.mkString(" | ")

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

    case `¡`(_, _, _, parallelism, _, guard, sum) =>
      s"¡ $parallelism* " + "." + guard + sum

    case !(_, _, _, -1, _, guard, sum) =>
      "!" + "." + guard + sum

    case !(_, _, _, parallelism, _, guard, sum) =>
      s"! $parallelism* " + "." + guard + sum

    case `[]`(_, _, _, label, sum) =>
      label.getOrElse("") + "[ " + sum + " ]"

    case `(*)`(_, _, _, identifier, params*) =>
      import Pre.given
      params.map(it => it: String).toList.mkString(s"$identifier(", ",", ")")


object AST:

  object ∅ :
    def unapply(self: AST): Boolean = self match
      case +(_, _, _) => true
      case _          => false

  object Searched:

    def unapply(self: (AST, String)): Option[(List[AST], Seq[Pre])] =
      self._1.unlabelʹ(using self._2)

  extension [T](self: List[T])
    def <->(it: Seq[Pre]): (List[T], Seq[Pre]) =
      self -> it

  extension (ast: AST)

    def unlabelʹ(using l: String): Option[(List[AST], Seq[Pre])] =

      ast match

        case sum @ +(_, _, _, par @ ∥(_, _, _, seq @ `.`(_, _, _, rep @ `¡`(_, _, _, _, _, guard, _sum), it*))) if !it.exists { case Act(it) => it } =>
          l.substring(4) match
            case "" =>
              Some(sum :: par :: seq :: rep :: Nil <-> Seq(guard))
            case given String =>
              _sum.unlabel.map(sum :: par :: seq :: rep :: _ <-> _)

        case sum @ +(_, _, _, par @ ∥(_, _, _, seq @ `.`(_, _, _, rep @ !(_, _, _, _, _, guard, _sum), it*))) if !it.exists { case Act(it) => it } =>
          l.substring(4) match
            case "" =>
              Some(sum :: par :: seq :: rep :: Nil <-> Seq(guard))
            case given String =>
              _sum.unlabel.map(sum :: par :: seq :: rep :: _ <-> _)

        case _ =>
          ast.unlabel

    def unlabel(using l: String): Option[(List[AST], Seq[Pre])] =
      val j = l.indexOf('/')

      ast match

        case ∅() => None

        case sum @ +(_, _, _, par @ ∥(_, _, _, it: `.`)) if !it.prefixes.exists { case Act(it) => it } =>
          it.unlabel.map(sum :: par :: _ <-> _)

        case sum @ +(_, _, _, it) =>
          it.unlabel.map(sum :: _ <-> _)

        case sum @ +(_, _, _, it*) =>
          when(l.charAt(0) == '+') {
            Try {
              val i = l.substring(1, j).toInt
              val k = it.size.toString.size
              when(l.substring(1+j, 1+j+k).toInt == it.size) {
                it(i) -> l.substring(1+j+k)
              }
            }.toOption.flatten.flatMap {
              case (o, given String) =>
                o.unlabel.map(sum :: o :: _ <-> _)
            }
          }.flatten

        case par @ ∥(_, _, _, it*) =>
          when(l.charAt(0) == '∥') {
            Try {
              val i = l.substring(1, j).toInt
              val k = it.size.toString.size
              when(l.substring(1+j, 1+j+k).toInt == it.size) {
                it(i) -> l.substring(1+j+k)
              }
            }.toOption.flatten.flatMap {
              case (o, given String) =>
                o.unlabel.map(par :: o :: _ <-> _)
            }
          }.flatten

        case it: `.` if l.isEmpty =>
          Some(it :: Nil <-> it.prefixes)

        case it @ `.`(_, _, _, end, _*) =>
          end.unlabel.map(it :: _ <-> _)

        case it @ ?:(_, _, _, _, _, _, t, f) =>
          when(l.charAt(0) == '+') {
            Try {
              val i = l.substring(1, j).toInt
              when(l.substring(1+j, 1+j+1).toInt == 1) {
                when(i == 0)(t).orElse(when(i == 1)(f).flatten) -> l.substring(1+j+1)
              }
            }.toOption.flatten.flatMap {
              case (Some(o), given String) =>
                o.unlabel.map(it :: o :: _ <-> _)
              case _ =>
                None
            }
          }.flatten

        case it @ `¡`(_, _, _, _, _, guard, sum) =>
          when(l.substring(0, 4) == "+0/0") {
            l.substring(4) match
              case "" =>
                Some(it :: Nil <-> Seq(guard))
              case given String =>
                sum.unlabel.map(it :: _ <-> _)
          }.flatten

        case it @ !(_, _, _, _, _, guard, sum) =>
          when(l.substring(0, 4) == "+0/0") {
            l.substring(4) match
              case "" =>
                Some(it :: Nil <-> Seq(guard))
              case given String =>
                sum.unlabel.map(it :: _ <-> _)
          }.flatten

        case it @ `[]`(_, _, _, _, sum) =>
          sum.unlabel.map(it :: _ <-> _)

        case _ => None
