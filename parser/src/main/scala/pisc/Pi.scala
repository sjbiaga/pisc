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

import scala.collection.Set
import scala.collection.mutable.LinkedHashSet
import scala.io.Source
import scala.util.parsing.combinator._

import Pi._
import Calculus._


class Pi extends JavaTokenParsers:

  def `π.`: Parser[(Pre, (Names, Names))] =
    "τ" ^^ { _ => // silent prefix
      `τ` -> (Names(), Names())
    } |
    name~"<"~name~">" ^^ { // negative prefix i.e. output
      case ch ~ _ ~ _ ~ _ if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case ch ~ _ ~ arg ~ _ =>
        π(ch, arg, polarity = false) -> (Names(), Names(ch, arg))
    } |
    name~"("~name~")" ^^ { // positive prefix i.e. input
      case ch ~ _ ~ _ ~ _ if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case _ ~ _ ~ par ~ _ if !par.isSymbol =>
        throw PrefixChannelParsingException(par)
      case ch ~ _ ~ par ~ _ =>
        π(ch, par, polarity = true) -> (Names(par), Names(ch))
    }

  def name: Parser[λ] = ident ^^ { λ.apply compose Symbol.apply } |
                        floatingPointNumber ^^ { λ.apply } |
                        stringLiteral ^^ { λ.apply } |
                        expression ^^ { λ.apply compose Expr.apply }

  /**
   * Channel names start with lower case.
   * @return
   */
  override def ident: Parser[String] =
      "" ~> // handle whitespace
      rep1(acceptIf(Character.isLowerCase)("channel name expected but '" + _ + "' found"),
          elem("channel name part", { (ch: Char) => Character.isJavaIdentifierPart(ch) || ch == '\'' || ch == '"' })) ^^ (_.mkString)

  /** Scala comment enclosing any Scala expression.
   * @return
   */
  def expression: Parser[String] =
    """[/][*].*?[*][/]""".r ^^ { _.stripPrefix("/*").stripSuffix("*/") }


object Pi extends Calculus:

  type Names = Set[Symbol]

  object Names:
    def apply(os: λ*): Names = LinkedHashSet.from(os
      .filter(_.isSymbol)
      .map(_.asSymbol)
    )

  class PrefixParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class PrefixChannelParsingException(name: λ)
      extends PrefixParsingException(s"${name.value} is not a channel name but a ${name.kind}")


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
