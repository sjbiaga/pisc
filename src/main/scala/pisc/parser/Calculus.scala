/*
 * Copyright (c) 2023-2024 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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
import Pi._
import Calculus._
import Encoding._


abstract class Calculus extends Pi:

  def equation: Parser[Bind] =
    invocation(true)<~"=" >> {
      case (bind, binding) =>
        _code = -1
        given Names2 = Names2() ++ binding.map(_ -> Occurrence(None, pos()))
        choice ^^ {
          case (_sum, _free) =>
            val sum = _sum.flatten
            val free = _free ++ sum.capitals
            if (free &~ binding).nonEmpty
            then
              throw EquationFreeNamesException(bind.identifier, free &~ binding)
            bind -> sum
        }
    }

  def choice(using Names2): Parser[(+, Names)] =
    rep1sep(parallel, "+") ^^ { ps =>
      `+`(ps.map(_._1)*) -> ps.map(_._2).reduce(_ ++ _)
    }

  def parallel(using Names2): Parser[(∥, Names)] =
    rep1sep(sequential, "|") ^^ { ss =>
      ∥(ss.map(_._1)*) -> ss.map(_._2).reduce(_ ++ _)
    }

  def sequential(using binding2: Names2): Parser[(`.`, Names)] =
    given Names2 = Names2(binding2)
    prefixes ~ opt( leaf | "("~>choice<~")" ) ^^ { `pre ~ opt` =>
      binding2 ++= binders
      `pre ~ opt` match
        case pre ~ Some((end, free)) =>
          `.`(end, pre._1*) -> (pre._2._2 ++ (free &~ pre._2._1))
        case pre ~ _ =>
          `.`(∅, pre._1*) -> pre._2._2 // inaction
    }

  def leaf(using Names2): Parser[(`-`, Names)] =
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
      case Some(π @ (π(λ(ch: Symbol), λ(par: Symbol), true, _), _)) =>
        if ch == par
        then
          if _werr
          then
            throw GuardParsingException(ch.name)
          Console.err.println("Warning! " + GuardParsingException(ch.name).getMessage + ".")
        val binding = π._2._1
        Names2Occurrence(binding)
        choice ^^ {
          case (sum, free) =>
            `!`(Some(π._1), sum) -> ((free &~ binding) ++ π._2._2)
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

  def instantiation(using Names2): Parser[(`⟦⟧`, Names)]

  def capital: Parser[(`{}`, Names)]

  def prefixes(using Names2): Parser[(List[Pre], (Names, Names))] =
    rep(prefix) ^^ { ps =>
      val binding = ps.map(_._2._1)
      val free = ps.map(_._2._2)
        .zipWithIndex
        .foldLeft(Names()) { case (r, (ns, i)) =>
          ns.foldLeft(r) {
            case (r, n)
              if {
                val j = binding.indexWhere(_.contains(n))
                j < 0 || i <= j
              } => r + n
            case (r, _) => r
          }
        }
      ps.map(_._1) -> (if binding.nonEmpty then binding.reduce(_ ++ _) else Names(), free)
    }

  def prefix(using binding2: Names2): Parser[(Pre, (Names, Names))] =
    "ν"~>"("~>rep1sep(name_capacity, ",")<~")" ^^ { // restriction
      case ns if !ns.forall(_._1._1.isSymbol) =>
        throw PrefixChannelsParsingException(ns.filterNot(_._1._1.isSymbol).map(_._1._1)*)
      case ns =>
        val binding = ns.map(_._2).reduce(_ ++ _)
        Names2Occurrence(binding)
        ν(ns.map { it => it._1._2 -> it._1._1.asSymbol.name }*) -> (binding, Names())
    } |
    `μ.`<~"." ^^ {
      case it @ (_, (binding, _)) =>
        Names2Occurrence(binding)
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
  final private val qual_r = "[{][^}]*[}]".r


object Calculus:

  type Bind = (`(*)`, +)

  export Pre._

  enum Pre:

    case ν(cap_names: (Int, String)*) // forcibly

    case τ(code: Option[Code])

    case π(channel: λ, name: λ, polarity: Boolean, code: Option[Code])

    override def toString: String = this match
      case ν(cap_names*) => cap_names.map(_._2).mkString("ν(", ", ", ")")
      case π(channel, name, polarity, _) =>
        if polarity
        then "" + channel + "(" + name + ")."
        else "" + channel + "<" + name + ">."
      case _ => "τ."

  sealed trait AST extends Any

  case class +(choices: ∥ *) extends AST:
    override def toString: String = choices.mkString(" + ")

  object ∅ extends +():
    override def canEqual(that: Any): Boolean =
      that.isInstanceOf[+]

    @annotation.tailrec
    override def equals(any: Any): Boolean = any match
      case +() => true
      case +(∥(`.`(sum: +))) => equals(sum)
      case _ => false

    override def toString: String = "()"

  case class ∥(components: `.`*) extends AnyVal with AST:
    override def toString: String = components.mkString(" | ")

  case class `.`(end: &, prefixes: Pre*) extends AST:
    override def toString: String =
      prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + (if ∅ != end && end.isInstanceOf[+]
                                                                         then "(" + end + ")" else end)

  case class ?:(cond: ((λ, λ), Boolean), t: +, f: Option[+]) extends AST:
    override def toString: String =
      val test = "" + cond._1._1 + (if cond._2 then " ≠ " else " = ") + cond._1._2
      if f.isEmpty
      then
        "[ " + test + " ] " + t
      else
        "if " + test + " " + t + " else " + f.get

  case class !(guard: Option[μ], sum: +) extends AST:
    override def toString: String = "!" + guard.map("." + _).getOrElse("") + sum

  case class `⟦⟧`(definition: Definition,
                  variables: Names,
                  sum: +,
                  assign: Option[Set[(Symbol, Symbol)]] = None) extends AST:
    override def toString: String =
      val vars = ( if variables.nonEmpty
                   then
                     variables.map {
                       case it if assign.map(_.exists(_._1 == it)).getOrElse(false) =>
                         s"${it.name} = ${assign.get.find(_._1 == it).get._2.name}"
                       case it => it.name
                     }.mkString("{", ", ", "}")
                   else
                     ""
                 )
      s"""${Definition(definition.code, definition.term)}$vars = $sum"""

  case class `{}`(identifier: String,
                  pointers: List[Symbol],
                  agent: Boolean = false,
                  params: λ*) extends AST:
    override def toString: String =
      val ps = if agent then params.mkString("(", ", ", ")") else ""
      s"""$identifier$ps{${pointers.map(_.name).mkString(", ")}}"""

  case class `(*)`(identifier: String,
                   qual: List[String],
                   params: λ*) extends AST:
    override def toString: String =
      if params.isEmpty
      then identifier
      else s"$identifier(${params.mkString(", ")})"

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

  case class PrefixChannelsParsingException(names: λ*)
      extends PrefixParsingException(s"""${names.mkString(", ")} are not channel names but ${names.map(_.kind).mkString(", ")}""")

  case class GuardParsingException(name: String)
      extends PrefixParsingException(s"$name is both the channel name and the binding parameter name in an input guard")


  // functions

  extension[T <: AST](ast: T)

    def flatten: T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅ => ∅

        case +(∥(`.`(sum: +)), it*) =>
          val lhs = sum.flatten
          val rhs = `+`(it*).flatten
          `+`((lhs.choices ++ rhs.choices).filterNot(∅ == `+`(_))*)

        case +(par, it*) =>
          val lhs = `+`(par.flatten)
          val rhs = `+`(it*).flatten
          `+`((lhs.choices ++ rhs.choices).filterNot(∅ == `+`(_))*)

        case ∥(`.`(+(par)), it*) =>
          val lhs = par.flatten
          val rhs = ∥(it*).flatten
          ∥((lhs.components ++ rhs.components)*)

        case ∥(seq, it*) =>
          val lhs = ∥(seq.flatten)
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
