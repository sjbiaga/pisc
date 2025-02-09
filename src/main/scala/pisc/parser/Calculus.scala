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

import scala.meta.Term

import Expression.Code
import PolyadicPi.*
import Calculus.*
import Encoding.*


abstract class Calculus extends PolyadicPi:

  def equation: Parser[Bind] =
    invocation(true)<~"=" >> {
      case (bind, bound) =>
        _code = -1
        given Bindings = Bindings() ++ bound.map(_ -> Occurrence(None, pos()))
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

  def choice(using Bindings): Parser[(+, Names)] =
    rep1sep(parallel, "+") ^^ { ps =>
      `+`(ps.map(_._1)*) -> ps.map(_._2).reduce(_ ++ _)
    }

  def parallel(using Bindings): Parser[(∥, Names)] =
    rep1sep(sequential, "|") ^^ { ss =>
      ∥(ss.map(_._1)*) -> ss.map(_._2).reduce(_ ++ _)
    }

  def sequential(using bindings: Bindings): Parser[(`.`, Names)] =
    given Bindings = Bindings(bindings)
    prefixes ~ opt( leaf | "("~>choice<~")" ) ^^ {
      case (ps, (bound, free)) ~ Some((end, free2)) =>
        bindings ++= binders
        `.`(end, ps*) -> (free ++ (free2 &~ bound))
      case (ps, (_, free)) ~ _ =>
        bindings ++= binders
        `.`(`+`(), ps*) -> free // inaction
    }

  def leaf(using Bindings): Parser[(-, Names)] =
    "["~test~"]"~choice ^^ { // (mis)match
      case _ ~ cond ~ _ ~ t =>
        ?:(cond._1, t._1, None) -> (cond._2 ++ t._2)
    } |
    "if"~test~"then"~choice~"else"~choice ^^ { // if then else
      case _ ~ cond ~ _ ~ t ~ _ ~ f =>
        ?:(cond._1, t._1, Some(f._1)) -> (cond._2 ++ (t._2 ++ f._2))
    } |
    test~"?"~choice~":"~choice ^^ { // Elvis operator
      case cond ~ _ ~ t ~ _ ~ f =>
        ?:(cond._1, t._1, Some(f._1)) -> (cond._2 ++ (t._2 ++ f._2))
    } |
    "!"~> opt( "."~>`μ.`<~"." ) >> { // [guarded] replication
      case Some(π @ (π(λ(ch: Symbol), true, _, _par*), _)) =>
        if _par.filter(_.isSymbol).exists(_.asSymbol == ch)
        then
          if _werr
          then
            throw GuardParsingException(ch.name)
          Console.err.println("Warning! " + GuardParsingException(ch.name).getMessage + ".")
        val bound = π._2._1
        BindingOccurrence(bound)
        choice ^^ {
          case (sum, free) =>
            `!`(Some(π._1), sum) -> ((free &~ bound) ++ π._2._2)
        }
      case Some(μ) =>
        choice ^^ {
          case (sum, free) =>
            `!`(Some(μ._1), sum) -> (free ++ μ._2._2)
        }
      case _ =>
        choice ^^ {
          case (sum, free) =>
            `!`(None, sum) -> free
        }
    } |
    capital |
    invocation() |
    instantiation

  def instantiation(using Bindings): Parser[(`⟦⟧`, Names)]

  def capital: Parser[(`{}`, Names)]

  def prefixes(using Bindings): Parser[(List[Pre], (Names, Names))] =
    rep(prefix) ^^ { ps =>
      val bound = ps.map(_._2._1)
      val free = ps.map(_._2._2)
        .zipWithIndex
        .foldLeft(Names()) { case (r, (ns, i)) =>
          bound.take(i) match
            case bs =>
              r ++= ns.filter { n => bs.indexWhere(_.contains(n)) < 0 }
        }
      ps.map(_._1) -> ((Names() :: bound).reduce(_ ++ _), free)
    }

  def prefix(using Bindings): Parser[(Pre, (Names, Names))] =
    "ν"~>"("~>rep1sep(name, ",")<~")" ^^ { // restriction
      case ns if !ns.forall(_._1.isSymbol) =>
        throw PrefixChannelsParsingException(ns.filterNot(_._1.isSymbol).map(_._1)*)
      case ns =>
        val bound = ns.map(_._2).reduce(_ ++ _)
        BindingOccurrence(bound)
        ν(ns.map(_._1.asSymbol.name)*) -> (bound, Names())
    } |
    `μ.`<~"." ^^ {
      case it @ (_, (bound, _)) =>
        BindingOccurrence(bound)
        it
    }

  def test: Parser[(((λ, λ), Boolean), Names)] = "("~>test<~")" |
    name~("="|"≠")~name ^^ {
      case (lhs, free_lhs) ~ mismatch ~ (rhs, free_rhs) =>
        (lhs -> rhs -> (mismatch != "=")) -> (free_lhs ++ free_rhs)
    }

  def invocation(equation: Boolean = false): Parser[(`(*)`, Names)] =
    qual ~ IDENT ~ opt( "("~>rep1sep(name, ",")<~")" ) ^^ {
      case qual ~ id ~ _ if equation && qual.nonEmpty =>
        throw EquationQualifiedException(id, qual)
      case _ ~ id ~ Some(params) if equation && !params.forall(_._1.isSymbol) =>
        throw EquationParamsException(id, params.filterNot(_._1.isSymbol).map(_._1)*)
      case qual ~ "Self" ~ Some(params) =>
        self += _code
        `(*)`("Self_" + _code, qual, params.map(_._1)*) -> params.map(_._2).reduce(_ ++ _)
      case qual ~ "Self" ~ _ =>
        self += _code
        `(*)`("Self_" + _code, qual) -> Names()
      case qual ~ id ~ Some(params) =>
        id match
          case s"Self_$n" if (try { n.toInt; true } catch _ => false) =>
            self += n.toInt
          case _ =>
        `(*)`(id, qual, params.map(_._1)*) -> params.map(_._2).reduce(_ ++ _)
      case qual ~ id ~ _ =>
        id match
          case s"Self_$n" if (try { n.toInt; true } catch _ => false) =>
            self += n.toInt
          case _ =>
        `(*)`(id, qual) -> Names()
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

  type Bind = (`(*)`, +)

  export Pre.*
  export AST.*

  enum Pre:

    case ν(names: String*) // forcibly

    case τ(code: Option[Code])

    case π(channel: λ, polarity: Boolean, code: Option[Code], names: λ*)

    override def toString: String = this match
      case ν(names*) => names.mkString("ν(", ", ", ")")
      case π(channel, polarity, _, names*) =>
        if polarity
        then "" + channel + names.mkString("(", ", ", ").")
        else "" + channel + names.mkString("<", ", ", ">.")
      case _ => "τ."

  enum AST:

    case +(choices: AST.∥ *)

    case ∥(components: AST.`.`*)

    case `.`(end: AST.+ | -, prefixes: Pre*)

    case ?:(cond: ((λ, λ), Boolean), t: AST.+, f: Option[AST.+])

    case !(guard: Option[μ], sum: AST.+)

    case `⟦⟧`(definition: Definition,
              variables: Names,
              sum: AST.+,
              assign: Set[(Symbol, Symbol)] = Set.empty)

    case `{}`(identifier: String,
              pointers: List[Symbol],
              agent: Boolean = false,
              params: λ*)

    case `(*)`(identifier: String,
               qual: List[String],
               params: λ*)

    override def toString: String = this match
      case ∅(_) => "()"
      case +(choices*) => choices.mkString(" + ")

      case ∥(components*) => components.mkString(" | ")

      case `.`(∅(_)) => "()"
      case `.`(∅(_), prefixes*) => prefixes.mkString(" ") + " ()"
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

      case !(guard, sum) => "!" + guard.map("." + _).getOrElse("") + sum

      case `⟦⟧`(definition, variables, sum, assign) =>
        val vars = if (variables.isEmpty)
                   then
                     ""
                   else
                     variables.map {
                       case it if assign.exists(_._1 == it) =>
                         s"${it.name} = ${assign.find(_._1 == it).get._2.name}"
                       case it => it.name
                     }.mkString("{", ", ", "}")
        s"""${Definition(definition.code, definition.term)}$vars = $sum"""

      case `{}`(identifier, pointers, agent, params*) =>
        val ps = if agent then params.mkString("(", ", ", ")") else ""
        s"""$identifier$ps{${pointers.map(_.name).mkString(", ")}}"""

      case `(*)`(identifier, _) => identifier
      case `(*)`(identifier, _, params*) => s"$identifier(${params.mkString(", ")})"

  object ∅ :
    def unapply[T <: AST](self: T): Option[Unit] = self match
      case sum: + if sum.isVoid => Some(())
      case _ => None

  private val qual_r = "[{][^}]*[}]".r

  case class λ(value: Any):
    val isSymbol: Boolean = value.isInstanceOf[Symbol]
    def asSymbol: Symbol = value.asInstanceOf[Symbol]

    type Kind = value.type

    val kind: String = value match
      case _: Symbol => "channel name"
      case _: BigDecimal => "decimal number"
      case _: Boolean => "True False"
      case _: String => "string literal"
      case _: Expr => "Scalameta Term"

    override def toString: String = value match
      case it: Symbol => it.name
      case it: BigDecimal => "" + it
      case it: Boolean => it.toString.capitalize
      case it: String => it
      case it: Expr => "" + it

  case class Expr(term: Term):
    override def toString: String = "/*" + term + "*/"


  // exceptions

  import Expression.ParsingException

  abstract class EquationParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class EquationQualifiedException(id: String, qual: List[String])
      extends EquationParsingException(s"""A qualified package ${qual.mkString(".")} is present in the left hand side of $id""")

  case class EquationParamsException(id: String, params: λ*)
      extends EquationParsingException(s"""The "formal" parameters (${params.mkString(", ")}) are not names in the left hand side of $id""")

  case class EquationFreeNamesException(id: String, free: Names)
      extends EquationParsingException(s"""The free names (${free.map(_.name).mkString(", ")}) in the right hand side are not formal parameters of the left hand side of $id""")

  case class GuardParsingException(name: String)
      extends PrefixParsingException(s"$name is both the channel name and a binding parameter name in an input guard")


  // functions

  extension (sum: +)
    @annotation.tailrec
    private def isVoid: Boolean = sum match
      case +() => true
      case +(∥(`.`(sum: +))) => sum.isVoid
      case _ => false

  extension [T <: AST](ast: T)

    def flatten: T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅(_) => ast

        case +(∥(`.`(sum: +)), it*) =>
          val lhs = sum.flatten
          val rhs = `+`(it*).flatten
          `+`((lhs.choices ++ rhs.choices).filterNot(`+`(_).isVoid)*)

        case +(par, it*) =>
          val lhs: + = `+`(par.flatten)
          val rhs = `+`(it*).flatten
          `+`((lhs.choices ++ rhs.choices).filterNot(`+`(_).isVoid)*)

        case ∥(`.`(+(par)), it*) =>
          val lhs = par.flatten
          val rhs = ∥(it*).flatten
          ∥((lhs.components ++ rhs.components)*)

        case ∥(seq, it*) =>
          val lhs: ∥ = ∥(seq.flatten)
          val rhs = ∥(it*).flatten
          ∥((lhs.components ++ rhs.components)*)

        case `.`(+(∥(`.`(end, ps*))), it*) =>
          `.`(end, (it ++ ps)*).flatten

        case `.`(end, it*) =>
          `.`(end.flatten, it*)

        case ?:(cond, t, f) =>
          ?:(cond, t.flatten, f.map(_.flatten))

        case !(None, sum) =>
          sum.flatten match
            case +(∥(`.`(end: !))) => end
            case it => `!`(None, it)

        case it @ !(_, sum) =>
          it.copy(sum = sum.flatten)

        case _ => ast
