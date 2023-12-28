/*
 * Copyright (c) 2023-2023 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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

import scala.io.Source
import scala.util.parsing.combinator._
import scala.collection.Set
import scala.collection.mutable.LinkedHashSet

import Calculus._


class Calculus extends JavaTokenParsers:

  def equation: Parser[Bind] =
    agent~"="~choice ^^ {
      case (call, bound) ~ _ ~ (sum, free)
        if (free &~ bound).nonEmpty =>
        throw EquationFreeNamesException(call.identifier.asSymbol, free &~ bound)
      case (bind, _) ~ _ ~ (sum, _) =>
        bind -> sum
    }

  def choice: Parser[(Sum, Names)] = "("~>choice<~")" ^^ { identity } |
    rep1sep(parallel, "+") ^^ { ps =>
      Sum(ps.map(_._1): _*) -> ps.map(_._2).reduce(_ ++ _)
    }

  def parallel: Parser[(Par, Names)] = "("~>parallel<~")" ^^ { identity } |
    rep1sep(sequential, "|") ^^ { es =>
      Par(es.map(_._1): _*) -> es.map(_._2).reduce(_ ++ _)
    }

  def sequential: Parser[(End, Names)] =
    prefix ~ opt( "𝟎" | "("~>choice<~")" | agent ) ^^ {
      case pre ~ Some("𝟎") =>
        End(pre._1, `𝟎`) -> pre._2._2
      case pre ~ Some((sum: Sum, free: Names)) =>
        End(pre._1, sum) -> (pre._2._2 ++ (free &~ pre._2._1))
      case pre ~ Some((call: Call, free: Names)) =>
        End(pre._1, call) -> (pre._2._2 ++ (free &~ pre._2._1))
      case pre ~ _ =>
        End(pre._1, `𝟎`) -> pre._2._2
    }

  def prefix: Parser[(Seq, (Names, Names))] =
    rep(action) ^^ { ps =>
      val bound = ps.map(_._2._1)
      val free = ps.map(_._2._2)
        .zipWithIndex
        .foldLeft(Names()) { case (r, (ns, i)) =>
          ns.foldLeft(r) {
            case (r, n)
              if {
                val j = bound.indexWhere(_.contains(n))
                j < 0 || i <= j
              } => r + n
            case (r, _) => r
          }
        }
      Seq(ps.map(_._1): _*) -> (if bound.nonEmpty then bound.reduce(_ ++ _) else Names(), free)
    }

  def action: Parser[(Act, (Names, Names))] =
    "𝜏" <~ "." ^^ { _ => `𝜏` -> (Names(), Names()) } | // silent prefix
    "v"~>"("~>name<~")" ^^ { // restriction i.e. new name
      case ch if ch.isSymbol =>
        `v`(ch) -> (Names(ch), Names())
      case ch =>
        throw PrefixChannelParsingException(ch)
    } |
    name~"<"~name~">"<~"." ^^ { // negative prefix i.e. output
      case ch ~ _ ~ arg ~ _ if ch.isSymbol =>
        Pre(ch, arg, polarity = false) -> (Names(), Names(ch, arg))
      case ch ~ _ ~ _ ~ _ =>
        throw PrefixChannelParsingException(ch)
    } |
    name~"("~name~")"<~"." ^^ { // positive prefix i.e. input
      case ch ~ _ ~ par ~ _ if ch.isSymbol =>
        Pre(ch, par, polarity = true) -> (Names(par), Names(ch))
      case ch ~ _ ~ _ ~ _ =>
        throw PrefixChannelParsingException(ch)
    } |
    "["~name~"="~name~"]" ^^ { // match
      case _ ~ lhs ~ _ ~ rhs ~ _ =>
        Match(lhs, rhs) -> (Names(), Names(lhs, rhs))
    }

  def name: Parser[Opd] = ident ^^ { Opd.apply compose Symbol.apply } |
                          floatingPointNumber ^^ { Opd.apply compose BigDecimal.apply } |
                          stringLiteral ^^ { Opd.apply }

  def agent: Parser[(Call, Names)] =
    IDENT ~ opt( "("~>repsep(name, ",")<~")" ) ^^ {
      case id ~ Some(params) if !params.forall(_.isSymbol) =>
        throw EquationParamsException(id, params.filterNot(_.isSymbol).map(_.value):_ *)
      case id ~ Some(params) =>
        Call(Opd(Symbol(id)), params: _*) -> Names(params: _*)
      case id ~ _ =>
        Call(Opd(Symbol(id))) -> Names()
    }

  /**
   * Channel names start with lower case.
   * @return
   */
  override def ident: Parser[String] =
      "" ~> // handle whitespace
      rep1(acceptIf(Character.isLowerCase)("channel name expected but '" + _ + "' found"),
          elem("channel name part", { (ch: Char) => Character.isJavaIdentifierPart(ch) || ch == '\'' || ch == '"' })) ^^ (_.mkString)

  /**
   * Agent identifiers start with upper case.
   * @return
   */
  def IDENT: Parser[String] =
      "" ~> // handle whitespace
      rep1(acceptIf(Character.isUpperCase)("agent identifier expected but '" + _ + "' found"),
          elem("agent identifier part", { (ch: Char) => Character.isJavaIdentifierPart(ch) || ch == '\'' || ch == '"' })) ^^ (_.mkString)


object Calculus extends Calculus:

  type Names = Set[Symbol]

  type Bind = (Call, Sum)

  object Names:
    def apply(os: Opd*): Names = LinkedHashSet.from(os
      .filter(_.isSymbol)
      .map(_.asSymbol)
    )

  sealed trait AST extends Any

  case class Sum(choices: Par*) extends AnyVal with AST

  val `𝟎` = Sum()

  case class Par(components: End*) extends AnyVal with AST

  sealed trait Act extends Any with AST

  case class `v`(name: Opd) extends AnyVal with Act // forcibly

  case object `𝜏` extends Act

  case class Pre(channel: Opd, name: Opd, polarity: Boolean) extends Act

  case class Match(lhs: Opd, rhs: Opd) extends Act // forcibly

  case class Seq(actions: Act*) extends AnyVal with AST

  case class Opd(value: AnyRef) extends AST:
    val isSymbol: Boolean = value.isInstanceOf[Symbol]
    def asSymbol: Symbol = value.asInstanceOf[Symbol]

    val kind: String = value match {
      case _: Symbol => "channel name"
      case _: BigDecimal => "decimal number"
      case _: String => "string value"
    }

  case class End(prefix: Seq, process: AST) extends AST

  case class Call(identifier: Opd, params: Opd*) extends AST

  // exceptions

  sealed class ParsingException(msg: String, cause: Throwable = null)
    extends RuntimeException(msg, cause)

  sealed class EquationParsingException(msg: String, cause: Throwable = null)
    extends ParsingException(msg, cause)

  case class EquationParamsException(id: String, params: AnyRef*)
    extends EquationParsingException(s"The \"formal\" parameters (${params.mkString(", ")}) are not names in the left hand side of $id")

  case class EquationFreeNamesException(name: Symbol, free: Names)
    extends EquationParsingException(s"The free names (${free.map(_.name).mkString(", ")}) in the right hand side are not formal parameters of the left hand side of ${name.name}")

  sealed class ActionParsingException(msg: String, cause: Throwable = null)
    extends ParsingException(msg, cause)

  case class PrefixChannelParsingException(name: Opd)
    extends ActionParsingException(s"${name.value} is not a channel name but a ${name.kind}")

  def apply(source: Source): List[Bind] = source
    .getLines()
    .filterNot(_.matches("^ *#.*")) // commented lines
    .filterNot(_.isBlank) // empty lines
    .map {
      parseAll(equation, _) match {
        case Success(result, _) => result
        case failure: NoSuccess => scala.sys.error(failure.msg)
      }
    }
    .toList
