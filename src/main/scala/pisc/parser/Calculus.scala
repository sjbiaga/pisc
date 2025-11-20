/*
 * Copyright (c) 2023-2025 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * [Except as contained in this notice, the name of Sebastian I. Gliţa-Catina
 * shall not be used in advertising or otherwise to promote the sale, use
 * or other dealings in this Software without prior written authorization
 * from Sebastian I. Gliţa-Catina.]
 */

package pisc
package parser

import scala.collection.mutable.{ LinkedHashSet => Set }

import scala.meta.{ Term, Type }

import Expression.Code
import Pi.*
import Calculus.*
import Encoding.*
import scala.util.parsing.combinator.pisc.parser.Expansion.Duplications


abstract class Calculus extends Pi:

  def equation(using Duplications): Parser[Bind] =
    invocation(true)<~"=" >> {
      case (bind, bound) =>
        _code = -1
        _dir = None
        given Bindings = Bindings() ++ bound.map(_ -> Occurrence(None, pos()))
        given Int = 1
        choice ^^ {
          case (_sum, _free) =>
            val sum = _sum.flatten
            val free = _free ++ sum.capitals
            if (free &~ bound).nonEmpty
            then
              throw EquationFreeNamesException(bind.identifier, free &~ bound)
            bind -> sum
        }
    }

  def choice(using Bindings, Duplications, Int): Parser[(+, Names)] =
    scale >> { scaling =>
      val scalingʹ = scaling.abs
      given Int = if scalingʹ == 1 then summon[Int] else scalingʹ
      rep1sep(parallel, "+") ^^ { _.unzip match
        case (it, ns) =>
          if scalingʹ == 0
          then
            `+`(-1) -> Names()
          else if _scaling && emitter != Emitter.ce
          then
            `+`(scaling, it*) -> ns.reduce(_ ++ _)
          else
            `+`(-1, it*) -> ns.reduce(_ ++ _) match
              case (+(_, it*), names) =>
                (0 until scalingʹ).foldLeft(`+`(-1): +) { case (+(_, itʹ*), _) => `+`(-1, (itʹ ++ it)*) } match
                  case sum => sum -> names
      }
    }

  def parallel(using Bindings, Duplications, Int): Parser[(∥, Names)] =
    scale >> { scaling =>
      val scalingʹ = scaling.abs
      given Int = if scalingʹ == 1 then summon[Int] else scalingʹ
      rep1sep(sequential, "|") ^^ { _.unzip match
        case (it, ns) =>
          if scalingʹ == 0
          then
            ∥(-1, `.`(`+`(-1))) -> Names()
          else if _scaling && emitter != Emitter.ce
          then
            ∥(scaling, it*) -> ns.reduce(_ ++ _)
          else
            ∥(-1, it*) -> ns.reduce(_ ++ _) match
              case (∥(_, it*), names) =>
                (0 until scalingʹ).foldLeft(∥(-1): ∥) { case (∥(_, itʹ*), _) => ∥(-1, (itʹ ++ it)*) } match
                  case par => par -> names
      }
    }

  def sequential(using bindings: Bindings, _ds: Duplications, _sc: Int): Parser[(`.`, Names)] =
    given Bindings = Bindings(bindings)
    prefixes ~ ( leaf | choiceʹ ) ^^ {
      case (it, (bound, free)) ~ (end, freeʹ) =>
        bindings ++= binders
        `.`(end, it*) -> (free ++ (freeʹ &~ bound))
    }

  def choiceʹ(using Bindings, Duplications, Int): Parser[(+, Names)] =
    opt( "("~>choice<~")" ) ^^ { _.getOrElse(`+`(-1) -> Names()) }

  def leaf(using bindings: Bindings, _ds: Duplications, _sc: Int): Parser[(-, Names)] =
    "["~condition~"]"~choice ^^ { // (mis)match
      case _ ~ cond ~ _ ~ t =>
        ?:(cond._1, t._1, None) -> (cond._2 ++ t._2)
    } |
    "if"~condition~"then"~choice~"else"~choice ^^ { // if then else
      case _ ~ cond ~ _ ~ t ~ _ ~ f =>
        ?:(cond._1, t._1, Some(f._1)) -> (cond._2 ++ (t._2 ++ f._2))
    } |
    condition~"?"~choice~":"~choice ^^ { // Elvis operator
      case cond ~ _ ~ t ~ _ ~ f =>
        ?:(cond._1, t._1, Some(f._1)) -> (cond._2 ++ (t._2 ++ f._2))
    } |
    "!"~> scale ~ opt( pace ) ~ opt( "."~>μ<~"." ) >> { // [guarded] replication
      case _ ~ _ ~ Some((π(λ(ch: Symbol), _, Some(cons), _), _)) if cons.nonEmpty && cons != "ν" =>
        throw ConsGuardParsingException(cons, ch.name)
      case parallelism ~ pace ~ Some(π @ (π(λ(ch: Symbol), λ(par: Symbol), Some(cons), _), _)) =>
        if ch == par && cons != "ν"
        then
          emitter match
            case Emitter.kk =>
            case _ => warn(throw GuardParsingException(ch.name))
        val bound = π._2._1
        BindingOccurrence(bound)
        choice ^^ {
          case (sum, free) =>
            `!`(parallelism, pace, Some(π._1), sum) -> ((free &~ bound) ++ π._2._2)
        }
      case parallelism ~ pace ~ Some(μ) =>
        choice ^^ {
          case (sum, free) =>
            `!`(parallelism, pace, Some(μ._1), sum) -> (free ++ μ._2._2)
        }
      case parallelism ~ pace ~ _ =>
        choice ^^ {
          case (sum, free) =>
            `!`(parallelism, pace, None, sum) -> free
        }
    } |
    capital |
    invocation() |
    instantiation

  def instantiation(using Bindings, Duplications, Int): Parser[(`⟦⟧`, Names)]

  def capital: Parser[(`{}`, Names)]

  def prefixes(using Bindings, Int): Parser[(List[Pre], (Names, Names))] =
    rep(prefix) ^^ { _.unzip match
      case (it, _2) => _2.unzip match
        case (bs, names) =>
          val free = Names()
          names
            .zipWithIndex
            .foreach { (ns, i) =>
              val bound = bs
                .take(i)
                .reduceOption(_ ++ _)
                .getOrElse(Names())
              free ++= ns -- bound
            }
          val bound = bs.reduceOption(_ ++ _).getOrElse(Names())
          it -> (bound, free)
    }

  def prefix(using Bindings, Int): Parser[(Pre, (Names, Names))] =
    "ν"~>"("~>names<~")" ^^ { // restriction
      case it if !it.forall(_._1.isSymbol) =>
        throw PrefixChannelsParsingException(it.filterNot(_._1.isSymbol).map(_._1)*)
      case it => it.unzip match
        case (λs, bs) =>
          val bound = bs.reduce(_ ++ _)
          BindingOccurrence(bound)
          ν(λs.map(_.asSymbol.name)*) -> (bound, Names())
    } |
    μ<~"." ^^ {
      case it @ (_, (bound, _)) =>
        BindingOccurrence(bound)
        it
    }

  def condition: Parser[(((λ, λ), Boolean), Names)] = "("~>condition<~")" |
    name~("="|"≠")~name ^^ {
      case (lhs, free_lhs) ~ mismatch ~ (rhs, free_rhs) =>
        (lhs -> rhs -> (mismatch != "=")) -> (free_lhs ++ free_rhs)
    }

  def invocation(equation: Boolean = false): Parser[(`(*)`, Names)] =
    qual ~ IDENT ~ opt( "("~> names ~ opt(if equation then "*" else "") <~")" ) ^^ {
      case qual ~ identifier ~ _ if equation && qual.nonEmpty =>
        throw EquationQualifiedException(identifier, qual)
      case _ ~ identifier ~ Some(params ~ _) if equation && !params.forall(_._1.isSymbol) =>
        throw EquationParamsException(identifier, params.filterNot(_._1.isSymbol).map(_._1)*)
      case qual ~ "Self" ~ Some(params ~ init) =>
        val paramsʹ = if equation && init.isDefined
                      then params.map(_._1).init
                      else params.map(_._1)
        self += _code
        `(*)`("Self_" + _code, qual, paramsʹ*) -> params.map(_._2).reduce(_ ++ _)
      case qual ~ "Self" ~ _ =>
        self += _code
        `(*)`("Self_" + _code, qual) -> Names()
      case qual ~ identifier ~ Some(params ~ init) =>
        val paramsʹ = if equation && init.isDefined
                      then params.map(_._1).init
                      else params.map(_._1)
        identifier match
          case s"Self_$n" if (try { n.toInt; true } catch _ => false) =>
            self += n.toInt
          case _ =>
        `(*)`(identifier, qual, paramsʹ*) -> params.map(_._2).reduce(_ ++ _)
      case qual ~ identifier ~ _ =>
        identifier match
          case s"Self_$n" if (try { n.toInt; true } catch _ => false) =>
            self += n.toInt
          case _ =>
        `(*)`(identifier, qual) -> Names()
    }

  /**
   * Agent identifiers start with upper case.
   * @return
   */
  def IDENT: Parser[String] =
      "" ~> // handle whitespace
      rep1(acceptIf(Character.isUpperCase)("agent identifier expected but '" + _ + "' found"),
          elem("agent identifier part", { (ch: Char) => Character.isJavaIdentifierPart(ch) || ch == '\'' || ch == '"' })) ^^ (_.mkString)

  /**
   * Qualified identifiers to agents in other packages.
   * @return
   */
  def qual: Parser[List[String]] =
    rep(qual_r) ^^ { _.map(_.stripPrefix("{").stripSuffix("}")) }


object Calculus:

  private val qual_r = "[{][^}]*[}]".r

  type Bind = (`(*)`, +)

  export Pre.*
  export AST.*

  enum Pre:

    case ν(names: String*) // forcibly

    case τ(code: Option[Code])

    case π(channel: λ, name: λ, polarity: Option[String], code: Option[Code])

    override def toString: String = this match
      case ν(names*) => names.mkString("ν(", ", ", ")")
      case π(channel, name, polarity, _) =>
        if polarity.isDefined
        then
          if polarity.get != "ν"
          then "" + channel + s"${polarity.get}(" + name + ")."
          else "" + channel + "<ν" + name + ">."
        else "" + channel + "<" + name + ">."
      case _ => "τ."

  enum AST:

    case +(scaling: Int, choices: AST.∥ *)

    case ∥(scaling: Int, components: AST.`.`*)

    case `.`(end: AST.+ | -, prefixes: Pre*)

    case ?:(cond: ((λ, λ), Boolean), t: AST.+, f: Option[AST.+])

    case !(parallelism: Int,
           pace: Option[(Long, String)],
           guard: Option[μ],
           sum: AST.+)

    case `⟦⟧`(definition: Definition,
              variables: Names,
              sum: AST.+,
              xid: String = null,
              assignment: Set[(Symbol, Symbol)] = Set.empty)

    case `{}`(identifier: String,
              pointers: List[Symbol],
              agent: Boolean = false,
              params: λ*)

    case `(*)`(identifier: String,
               qual: List[String],
               params: λ*)

    override def toString: String = this match
      case ∅() => "()"
      case +(-1, choices*) => choices.mkString(" + ")
      case +(sc, choices*) => sc + " * " + choices.mkString(" + ")

      case ∥(-1, components*) => components.mkString(" | ")
      case ∥(sc, components*) => sc + " * " + components.mkString(" | ")

      case `.`(∅()) => "()"
      case `.`(∅(), prefixes*) => prefixes.mkString(" ") + " ()"
      case `.`(end: +, prefixes*) =>
        prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + "(" + end + ")"
      case `.`(end, prefixes*) =>
        prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + end

      case ?:(cond, t, f) =>
        val test = "" + cond._1._1 + (if cond._2 then " ≠ " else " = ") + cond._1._2
        if f.isEmpty
        then
          "[ " + test + " ] " + t
        else
          "if " + test + " " + t + " else " + f.get

      case !(_, _, guard, sum) => "!" + guard.map("." + _).getOrElse("") + sum

      case `⟦⟧`(definition, variables, sum, _, assignment) =>
        val vars = if (variables.isEmpty)
                   then
                     ""
                   else
                     variables.map {
                       case it if assignment.exists(_._1 == it) =>
                         s"${it.name} = ${assignment.find(_._1 == it).get._2.name}"
                       case it => it.name
                     }.mkString("{", ", ", "}")
        s"""${Definition(definition.code, definition.term)}$vars = $sum"""

      case `{}`(identifier, pointers, agent, params*) =>
        val ps = if agent then params.mkString("(", ", ", ")") else ""
        s"""$identifier$ps{${pointers.map(_.name).mkString(", ")}}"""

      case `(*)`(identifier, qual, params*) =>
        import emitter.shared.Meta.\
        val args = params.map(_.toTerm).toList
        val term = qual match
          case h :: t => (t.map(\(_)) :+ \("π") :+ \(identifier)).foldLeft(h: Term)(Term.Select(_, _))
          case _ => \(identifier)
        Term.Apply(term, Term.ArgClause(args)).toString

  object ∅ :
    def unapply(self: AST): Boolean = self match
      case sum: + => sum.isVoid
      case _ => false

  case class λ(`val`: Any)(using val `type`: Option[(Type, Option[Type])] = None):
    val isSymbol: Boolean = `val`.isInstanceOf[Symbol]
    def asSymbol: Symbol = `val`.asInstanceOf[Symbol]

    type Kind = `val`.type

    val kind: String = `val` match
      case _: Symbol => "channel name"
      case _: BigDecimal => "decimal number"
      case _: Boolean => "True False"
      case _: String => "string literal"
      case _: Term => "Scalameta Term"
      case _ => "polyadic names"

    def toTerm: Term =
      import scala.meta._
      import dialects.Scala3
      `val` match
        case it: Symbol => Term.Name(it.name)
        case it: BigDecimal => Term.Apply(Term.Name("BigDecimal"), Term.ArgClause(Lit.String(it.toString)::Nil))
        case it: Boolean => Lit.Boolean(it)
        case it: String => Lit.String(it)
        case it: Term => it

    override def toString: String = `val` match
      case it: Symbol => it.name
      case it: BigDecimal => "" + it
      case it: Boolean => it.toString.capitalize
      case it: String => "\"" + it + "\""
      case it: Term => "/*" + it + "*/"
      case it: List[`λ`] => it.mkString(", ")


  // exceptions

  import Expression.ParsingException

  abstract class EquationParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class EquationQualifiedException(identifier: String, qual: List[String])
      extends EquationParsingException(s"""A qualified package ${qual.mkString(".")} is present in the left hand side of $identifier""")

  case class EquationParamsException(identifier: String, params: λ*)
      extends EquationParsingException(s"""The "formal" parameters (${params.mkString(", ")}) are not names in the left hand side of $identifier""")

  case class EquationFreeNamesException(identifier: String, free: Names)
      extends EquationParsingException(s"""The free names (${free.map(_.name).mkString(", ")}) in the right hand side are not formal parameters of the left hand side of $identifier""")

  case class PrefixChannelsParsingException(names: λ*)
      extends PrefixParsingException(s"""${names.mkString(", ")} are not channel names but ${names.map(_.kind).mkString(", ")}""")

  case class GuardParsingException(name: String)
      extends PrefixParsingException(s"$name is both the channel name and the binding parameter name in an input guard")

  case class ConsGuardParsingException(cons: String, name: String)
      extends PrefixParsingException(s"A name $name that knows how to CONS (`$cons') is used as replication guard")


  // functions

  extension (sum: +)
    private def isVoid: Boolean = sum match
      case +(_) => true
      case +(_, it*) => it.forall(_.components.forall { case `.`(sum: +) => sum.isVoid case _ => false })
      case _ => false

  extension [T <: AST](ast: T)

    def flatten: T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() =>
          `+`(-1)

        case +(sc, ∥(-1|1, `.`(sum: +)), it*) =>
          val lhs = sum.flatten
          val rhs = `+`(-1, it*).flatten
          `+`(sc, (lhs.choices ++ rhs.choices).filterNot(`+`(-1, _).isVoid)*)

        case +(sc, par, it*) =>
          val lhs: + = `+`(-1, par.flatten)
          val rhs = `+`(-1, it*).flatten
          `+`(sc, (lhs.choices ++ rhs.choices).filterNot(`+`(-1, _).isVoid)*)

        case ∥(sc, `.`(+(-1|1, par)), it*) =>
          val lhs = par.flatten
          val rhs = ∥(-1, it*).flatten
          ∥(sc, (lhs.components ++ rhs.components)*)

        case ∥(sc, seq, it*) =>
          val lhs: ∥ = ∥(-1, seq.flatten)
          val rhs = ∥(-1, it*).flatten
          ∥(sc, (lhs.components ++ rhs.components)*)

        case `.`(+(-1|1, ∥(-1|1, `.`(end, ps*))), it*) =>
          `.`(end, (it ++ ps)*).flatten

        case `.`(end, it*) =>
          `.`(end.flatten, it*)

        case ?:(cond, t, f) =>
          ?:(cond, t.flatten, f.map(_.flatten))

        case !(-1, None, None, sum) =>
          sum.flatten match
            case +(-1|1, ∥(-1|1, `.`(end: !))) => end
            case it => `!`(-1, None, None, it)

        case it @ !(_, _, _, sum) =>
          it.copy(sum = sum.flatten)

        case _ => ast
