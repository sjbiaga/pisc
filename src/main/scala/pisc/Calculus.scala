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

import java.util.UUID

import scala.io.Source
import scala.util.parsing.combinator._
import scala.collection.Set
import scala.collection.mutable.LinkedHashSet

import Calculus._


class Calculus extends JavaTokenParsers:

  def equation: Parser[Bind] =
    agent(true)~"="~choice ^^ {
      case (bind, bound) ~ _ ~ (sum, free)
        if (free &~ bound).nonEmpty =>
        throw EquationFreeNamesException(bind.identifier.asSymbol.name, free &~ bound)
      case (bind, _) ~ _ ~ (sum_, _) =>
        var sum: AST = sum_
        var ast = flatten(sum)
        while ast != sum
        do
          sum = ast
          ast = flatten(sum)
        if !valid(false)(ast) then
          throw EquationChoiceException(bind.identifier.asSymbol.name)
        bind -> ast.asInstanceOf[Sum]
    }

  def choice: Parser[(Sum, Names)] = "("~>choice<~")" ^^ { identity } |
    rep1sep(parallel, "+") ^^ { ls =>
      val ps = ls.map(_._1)
      (Sum(ps.map(_.enabled).reduce(_ ++ _), ps: _*), ls.map(_._2).reduce(_ ++ _))
    }

  def parallel: Parser[(Par, Names)] = "("~>parallel<~")" ^^ { identity } |
    rep1sep(sequential, "|") ^^ { ls =>
      val ss = ls.map(_._1)
      Par(ss.map(_.enabled).reduce(_ ++ _), ss: _*) -> ls.map(_._2).reduce(_ ++ _)
    }

  def sequential: Parser[(Seq, Names)] =
    prefixes ~ opt( "𝟎" | "("~>choice<~")" | agent() ) ^^ {
      case pre ~ Some((sum: Sum, free: Names)) =>
        Seq(sum, Actions(sum, pre._1: _*), pre._1: _*) -> (pre._2._2 ++ (free &~ pre._2._1))
      case pre ~ Some((call: Call, free: Names)) =>
        Seq(call, Actions(pre._1: _*), pre._1: _*) -> (pre._2._2 ++ (free &~ pre._2._1))
      case pre ~ _ =>
        Seq(`𝟎`, Actions(pre._1: _*), pre._1: _*) -> pre._2._2
    }

  def prefixes: Parser[(List[Pre], (Names, Names))] =
    rep(prefix) ^^ { ps =>
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
      ps.map(_._1) -> (if bound.nonEmpty then bound.reduce(_ ++ _) else Names(), free)
    }

  def prefix: Parser[(Pre, (Names, Names))] =
    "𝜏"~>opt("@"~>rate)<~"." ^^ { r => `𝜏`(Act(), r) -> (Names(), Names()) } | // silent prefix
    "v"~>"("~>name<~")" ^^ { // restriction i.e. new name
      case ch if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case ch =>
        `v`(ch) -> (Names(ch), Names())
    } |
    name~opt("@"~>rate)~"<"~name~">"<~"." ^^ { // negative prefix i.e. output
      case ch ~ _ ~ _ ~ _ ~ _ if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case ch ~ r ~ _ ~ arg ~ _ =>
        IO(Act(), r, ch, arg, polarity = false) -> (Names(), Names(ch, arg))
    } |
    name~opt("@"~>rate)~"("~name~")"<~"." ^^ { // positive prefix i.e. input
      case ch ~ _ ~ _ ~ _ ~ _ if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case _ ~ _ ~ _ ~ par ~ _ if !par.isSymbol =>
        throw PrefixChannelParsingException(par)
      case ch ~ r ~ _ ~ par ~ _ =>
        IO(Act(), r, ch, par, polarity = true) -> (Names(par), Names(ch))
    } |
    "["~>test<~"]" ^^ { // (mis)match
      case ((lhs, rhs), mismatch) =>
        Match(lhs, rhs, mismatch) -> (Names(), Names(lhs, rhs))
    } |
    "if"~test~"then"~choice~"else"~choice ^^ { // if then else
      case _ ~ cond ~ _ ~ t ~ _ ~ f =>
        `?:`(cond, t._1, f._1) -> (Names(), Names(cond._1._1, cond._1._2) ++ (t._2 ++ f._2))
    } |
    test~"?"~choice~":"~choice ^^ { // Elvis operator
      case cond ~ _ ~ t ~ _ ~ f =>
        `?:`(cond, t._1, f._1) -> (Names(), Names(cond._1._1, cond._1._2) ++ (t._2 ++ f._2))
    } |
    "!"~>choice ^^ { // replication
      case (sum, free) => `!`(sum) -> (Names(), free)
    }

  def name: Parser[Opd] = ident ^^ { Opd.apply compose Symbol.apply } |
                          floatingPointNumber ^^ { Opd.apply } |
                          stringLiteral ^^ { Opd.apply } |
                          expression ^^ { Opd.apply compose Expr.apply }

  def test: Parser[((Opd, Opd), Boolean)] = "("~>test<~")" ^^ { identity } |
    name~("="|"≠")~name ^^ {
      case lhs ~ mismatch ~ rhs => lhs -> rhs -> (mismatch != "=")
    }

  def rate: Parser[Option[Any]] = "("~>rate<~")" ^^ { identity } |
                                  "∞" ^^ { _ => None } |
                                  floatingPointNumber ^^ { Option.apply compose BigDecimal.apply } |
                                  super.ident ^^ { Option.apply compose Symbol.apply } |
                                  expression ^^ { Option.apply compose Expr.apply }

  def agent(binding: Boolean = false): Parser[(Call, Names)] =
    qual ~ IDENT ~ opt( "("~>repsep(name, ",")<~")" ) ^^ {
      case path ~ id ~ _ if binding && path.nonEmpty =>
        throw EquationQualifiedException(id, path)
      case _ ~ id ~ Some(params) if binding && !params.forall(_.isSymbol) =>
        throw EquationParamsException(id, params.filterNot(_.isSymbol).map(_.value):_ *)
      case path ~ id ~ Some(params) =>
        Call(Opd(Symbol(id)), path, params: _*) -> Names(params: _*)
      case path ~ id ~ _ =>
        Call(Opd(Symbol(id)), path) -> Names()
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

  /**
   * Qualified identifiers to agents in other packages.
   * @return
   */
  def qual: Parser[List[String]] =
    rep("""[{][^}]*[}]""".r) ^^ { _.map(_.stripPrefix("{").stripSuffix("}")) }

  /** Scala comment enclosing any Scala expression.
   * @return
   */
  def expression: Parser[String] =
    """[/][*].*?[*][/]""".r ^^ { _.stripPrefix("/*").stripSuffix("*/") }


object Calculus extends Calculus:

  type Bind = (Call, Sum)

  type Names = Set[Symbol]

  object Names:
    def apply(os: Opd*): Names = LinkedHashSet.from(os
      .filter(_.isSymbol)
      .map(_.asSymbol)
    )

  type Actions = Set[String]

  object Actions:
    def apply(ls: Pre*): Actions = Set.from {
      val i = ls.indexWhere(_.isInstanceOf[Act])
      if i == -1
      then
        None
      else
        val j = ls.indexWhere(!_.isInstanceOf[`v`])
        if j < i
        then
          None
        else
          Some(ls(i).asInstanceOf[Act].uuid)
    }
    def apply(sum: Sum, ls: Pre*): Actions =
      val r = this(ls: _*)
      if r.isEmpty then sum.enabled else r

  sealed trait AST extends Any

  case class Sum(enabled: Actions, choices: Par*) extends AST

  val `𝟎` = Sum(Actions(), Par(Actions()))

  case class Par(enabled: Actions, components: Seq*) extends AST

  sealed trait Pre extends Any with AST

  case class `v`(name: Opd) extends AnyVal with Pre // forcibly

  case class `!`(ast: AST) extends AnyVal with Pre // forcibly

  sealed trait Act:
    val uuid: String
    val rate: Option[Option[Any]]

  object Act:
    def apply() = UUID.randomUUID.toString

  case class `𝜏`(override val uuid: String,
                 override val rate: Option[Option[Any]]
  ) extends Pre with Act

  case class IO(override val uuid: String,
                override val rate: Option[Option[Any]],
                channel: Opd,
                name: Opd,
                polarity: Boolean
  ) extends Pre with Act

  case class Match(lhs: Opd, rhs: Opd, mismatch: Boolean) extends Pre // forcibly

  case class `?:`(cond: ((Opd, Opd), Boolean), t: Sum, f: Sum) extends Pre // forcibly

  case class Opd(value: AnyRef) extends AST:
    val isSymbol: Boolean = value.isInstanceOf[Symbol]
    def asSymbol: Symbol = value.asInstanceOf[Symbol]

    val kind: String = value match {
      case _: Symbol => "channel name"
      case _: String => "scala value"
      case _: Expr => "scala expression"
    }

  case class Seq(process: AST, enabled: Actions, prefixes: Pre*) extends AST

  case class Call(identifier: Opd, path: List[String], params: Opd*) extends AST

  case class Expr(expression: String)


  // exceptions

  sealed class ParsingException(msg: String, cause: Throwable = null)
      extends RuntimeException(msg, cause)

  sealed class EquationParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class EquationQualifiedException(id: String, path: List[String])
      extends EquationParsingException(s"A qualified package ${path.mkString(".")} is present in the left hand side of $id")

  case class EquationParamsException(id: String, params: AnyRef*)
      extends EquationParsingException(s"The \"formal\" parameters (${params.mkString(", ")}) are not names in the left hand side of $id")

  case class EquationChoiceException(id: String)
      extends EquationParsingException(s"A probabilistic choice (inaction or agent) is not prefixed in the right hand side of $id")

  case class EquationFreeNamesException(id: String, free: Names)
      extends EquationParsingException(s"The free names (${free.map(_.name).mkString(", ")}) in the right hand side are not formal parameters of the left hand side of $id")

  sealed class PrefixParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class PrefixChannelParsingException(name: Opd)
      extends PrefixParsingException(s"${name.value} is not a channel name but a ${name.kind}")


  // functions

  val flatten: AST => AST = {

    case Sum(enabled, Par(_, Seq(sum: Sum, _, ps*), ss*), it*)
        if ps.isEmpty && ss.isEmpty =>
      val lhs = flatten(sum).asInstanceOf[Sum]
      if it.isEmpty
      then
        Sum(enabled, lhs.choices: _*)
      else
        val rhs = flatten(Sum(Set.empty, it: _*)).asInstanceOf[Sum]
        Sum(enabled, (lhs.choices ++ rhs.choices): _*)

    case Sum(enabled, lhs, it*)
        if it.nonEmpty =>
      val rhs = flatten(Sum(Set.empty, it: _*)).asInstanceOf[Sum]
      Sum(enabled, (lhs +: rhs.choices): _*)

    case Sum(enabled, par, _*) =>
      Sum(enabled, flatten(par).asInstanceOf[Par])

    case Par(enabled, Seq(Sum(_, Par(_, ss*), p*), _, ps*), it*)
        if p.isEmpty && ps.isEmpty =>
      if it.isEmpty
      then
        Par(enabled, ss: _*)
      else
        val rhs = flatten(Par(Set.empty, it: _*)).asInstanceOf[Par]
        Par(enabled, (ss ++ rhs.components): _*)

    case Par(enabled, lhs, it*)
        if it.nonEmpty =>
      val rhs = flatten(Par(Set.empty, it: _*)).asInstanceOf[Par]
      Par(enabled, (lhs +: rhs.components): _*)

    case Par(enabled, it @ Seq(ast, _, _*), _*) =>
      Par(enabled, Seq(flatten(ast), it.enabled, it.prefixes: _*))

    case `!`(sum) =>
      flatten(sum)

    case `?:`(cond, t, f) =>
      `?:`(cond, flatten(t).asInstanceOf[Sum], flatten(f).asInstanceOf[Sum])

    case it => it

  }


  def valid(choice: Boolean): AST => Boolean = {

    case Sum(_, ps*) =>
      ps.forall(valid(choice || ps.size > 1))

    case Par(_, ss*)
        if ss.nonEmpty =>
      ss.forall(valid(choice))

    case Seq(ast, _, ps*) =>
      valid(if Actions(ps: _*).nonEmpty then false else choice)(ast)

    case _: Par | _: Call => !choice

    case _ => ???

  }


  def apply(source: Source): List[Either[String, Bind]] = source
    .getLines()
    .foldLeft(List[String]("")) {
      case (r, l) if l.endsWith("\\") => r.init :+ (r.last + l.stripSuffix("\\"))
      case (r, l) => r :+ l
    }
    .filterNot(_.matches("^ *#.*")) // commented lines
    .filterNot(_.isBlank) // empty lines
    .map { it =>
      if it.matches("^ *@.*")
      then // Scala
        Left(it.replaceFirst("^([ ]*)@(.*)$", "$1$2"))
      else // Pi
        parseAll(equation, it) match
          case Success(result, _) => Right(result)
          case failure: NoSuccess => scala.sys.error(failure.msg)
    }
    .toList
