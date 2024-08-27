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
import scala.io.Source

import scala.util.parsing.combinator._

import generator.Meta.`()(null)`

import PolyadicPi._
import Calculus._


class PolyadicPi extends Expression:

  def `μ.`: Parser[(μ, (Names, Names))] =
    "τ" ~> opt( expression ) ^^ { // silent prefix
      case Some((it, free)) =>
        τ(Some(it)) -> (Names(), free)
      case _ =>
        τ(None) -> (Names(), Names())
    } |
    name ~ opt(arity) ~ ("<"~>opt(rep1sep(name, ","))<~">") ~ opt( expression ) ^^ { // negative prefix i.e. output
      case (ch, _) ~ _ ~ _ ~ _ if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case (ch, _) ~ None ~ None ~ _ =>
        throw PrefixArityParsingException(ch)
      case (ch, _) ~ Some(arity) ~ Some(args) ~ _ =>
        throw PrefixArityParsingException2(ch, arity, args.size)
      case (ch, name) ~ _ ~ Some(args) ~ Some((it, free2)) =>
        π(ch, polarity = false, Some(it), args.map(_._1)*) -> (Names(), name ++ args.map(_._2).reduce(_ ++ _) ++ free2)
      case (ch, name) ~ _ ~ Some(args) ~ _ =>
        π(ch, polarity = false, None, args.map(_._1)*) -> (Names(), name ++ args.map(_._2).reduce(_ ++ _))
      case (ch, name) ~ Some(arity) ~ _ ~ Some((it, free2)) =>
        π(ch, polarity = false, Some(it), Seq.fill(arity)(λ(Expr(`()(null)`)))*) -> (Names(), name ++ free2)
      case (ch, name) ~ Some(arity) ~ _ ~ _ =>
        π(ch, polarity = false, None, Seq.fill(arity)(λ(Expr(`()(null)`)))*) -> (Names(), name)
    } |
    name ~ ("("~>rep1sep(name, ",")<~")") ~ opt( expression ) ^^ { // positive prefix i.e. input
      case (ch, _) ~ _ ~ _  if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case _ ~ params ~ _ if !params.forall(_._1.isSymbol) =>
        throw PrefixChannelsParsingException(params.filterNot(_._1.isSymbol).map(_._1)*)
      case (ch, _) ~ params ~ _ if params.size > params.distinctBy { case (λ(Symbol(it)), _) => it }.size =>
        throw PrefixUniquenessParsingException(ch.asSymbol.name, params.map(_._1.asSymbol.name)*)
      case _ ~ _ ~ Some((Left(enums), _)) =>
        throw TermParsingException(enums)
      case (ch, name) ~ params ~ Some((it, free2)) =>
        π(ch, polarity = true, Some(it), params.map(_._1)*) -> (params.map(_._2).reduce(_ ++ _), name ++ free2)
      case (ch, name) ~ params ~ _ =>
        π(ch, polarity = true, None, params.map(_._1)*) -> (params.map(_._2).reduce(_ ++ _), name)
    }

  def name: Parser[(λ, Names)] = ident ^^ { it => λ(Symbol(it)) -> Set(Symbol(it)) } |
                                 floatingPointNumber ^^ { it => λ(BigDecimal(it)) -> Names() } |
                                 stringLiteral ^^ { λ(_) -> Names() } |
                                 ( "True" | "False" ) ^^ { it => λ(it == "True") -> Names() } |
                                 expression ^^ {
                                   case (Right(term), free) => λ(Expr(term)) -> free
                                   case (Left(enums), _) => throw TermParsingException(enums)
                                 }

  /**
   * Channel names start with lower case.
   * @return
   */
  override def ident: Parser[String] =
      "" ~> // handle whitespace
      rep1(acceptIf(Character.isLowerCase)("channel name expected but '" + _ + "' found"),
          elem("channel name part", { (ch: Char) => Character.isJavaIdentifierPart(ch) || ch == '\'' || ch == '"' })) ^^ (_.mkString)

  /** Arity. */
  def arity: Parser[Int] =
    "#"~>"""\d+""".r ^^ { _.toInt }


object PolyadicPi extends Calculus:

  type Names = Set[Symbol]

  object Names:
    def apply(os: λ*): Names = Set.from(os
      .filter(_.isSymbol)
      .map(_.asSymbol)
    )

  import scala.meta.Enumerator

  import Expression.ParsingException

  class PrefixParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class PrefixChannelParsingException(name: λ)
      extends PrefixParsingException(s"${name.value} is not a channel name but a ${name.kind}")

  case class PrefixChannelsParsingException(names: λ*)
      extends PrefixParsingException(s"${names.map(_.value).mkString(", ")} are not channel names but ${names.map(_.kind).mkString(", ")}")


  case class PrefixUniquenessParsingException(name: String, ps: String*)
      extends PrefixParsingException(s"""$name channel parameters names '${ps.mkString(", ")}' must be distinct""")

  case class PrefixArityParsingException(name: λ)
      extends PrefixParsingException(s"Without arguments, channel ${name.asSymbol.name} must specify arity")

  case class PrefixArityParsingException2(name: λ, arity: Int, size: Int)
      extends PrefixParsingException(s"${name.asSymbol.name} channel must not specify both arity ($arity) and arguments (#$size)")

  case class TermParsingException(enums: List[Enumerator])
      extends PrefixParsingException(s"The embedded Scalameta should be a Term, not Enumerator `$enums'")


  def apply(source: Source): List[Either[String, Bind]] = (source.getLines().toList :+ "")
    .foldLeft(List[String]() -> false) {
      case ((r, false), l) => (r :+ l) -> l.endsWith("\\")
      case ((r, true), l) => (r.init :+ r.last.stripSuffix("\\") + l) -> l.endsWith("\\")
    }._1
    .filterNot(_.matches("^[ ]*#.*")) // commented lines
    .filterNot(_.isBlank) // empty lines
    .map { it =>
      if it.matches("^[ ]*@.*")
      then // Scala
        Left(it.replaceFirst("^([ ]*)@(.*)$", "$1$2"))
      else // Pi
        parseAll(equation, it) match
          case Success(result, _) => Right(result)
          case failure: NoSuccess => scala.sys.error(failure.msg)
    }
    .toList
