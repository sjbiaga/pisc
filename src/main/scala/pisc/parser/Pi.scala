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

import scala.collection.mutable.{ HashMap => Map, LinkedHashSet => Set }
import scala.io.Source

import scala.util.parsing.combinator._

import generator.Meta.`()(null)`

import Pi._
import Calculus._
import scala.util.parsing.combinator.pisc.parser.Extension


class Pi extends Expression:

  def `μ.`: Parser[(μ, (Names, Names))] =
    "τ" ~> opt( expression ) ^^ { // silent prefix
      case Some((it, free)) =>
        τ(Some(it)) -> (Names(), free)
      case _ =>
        τ(None) -> (Names(), Names())
    } |
    name ~ ("<"~>opt(name)<~">") ~ opt( expression ) ^^ { // negative prefix i.e. output
      case (ch, _) ~ _ ~ _ if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case (ch, name) ~ Some((arg, free)) ~ Some((it, free2)) =>
        π(ch, arg, polarity = false, Some(it)) -> (Names(), name ++ free ++ free2)
      case (ch, name) ~ Some((arg, free)) ~ _ =>
        π(ch, arg, polarity = false, None) -> (Names(), name ++ free)
      case (ch, name) ~ _ ~ Some((it, free2)) =>
        π(ch, λ(Expr(`()(null)`)), polarity = false, Some(it)) -> (Names(), name ++ free2)
      case (ch, name) ~ _ ~ _ =>
        π(ch, λ(Expr(`()(null)`)), polarity = false, None) -> (Names(), name)
    } |
    name ~ ("("~>name<~")") ~ opt( expression ) ^^ { // positive prefix i.e. input
      case (ch, _) ~ _ ~ _ if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case _ ~ (par, _) ~ _ if !par.isSymbol =>
        throw PrefixChannelParsingException(par)
      case _ ~ _ ~ Some((Left(enums), _)) =>
        throw TermParsingException(enums)
      case (ch, name) ~ (par, binding) ~ Some((it, free2)) =>
        π(ch, par, polarity = true, Some(it)) -> (binding, name ++ free2)
      case (ch, name) ~ (par, binding) ~ _ =>
        π(ch, par, polarity = true, None) -> (binding, name)
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

  private[parser] var eqtn = List[Bind]()
  private[parser] val defn = Map[Int, Define]()
  private[parser] val self = Set[Int]()


object Pi extends Extension:

  type Names = Set[Symbol]

  object Names:
    def apply(os: λ*): Names = Set.from(os
      .filter(_.isSymbol)
      .map(_.asSymbol)
    )


  // exceptions

  import scala.meta.Enumerator

  import Expression.ParsingException

  class PrefixParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class PrefixChannelParsingException(name: λ)
      extends PrefixParsingException(s"${name.value} is not a channel name but a ${name.kind}")

  case class TermParsingException(enums: List[Enumerator])
      extends PrefixParsingException(s"The embedded Scalameta should be a Term, not Enumerator `$enums'")


  // functions

  def ensure(implicit prog: List[Bind]): Unit =
    import Ensure._

    val i = main

    if i < 0 then throw MainParsingException

    given rec: Map[(String, Int), Int] = Map()
    given rep: Map[Int, Int] = Map()

    prog(i)._2.recursive(using "Main" -> 0 :: Nil)

    if rec.contains("Main" -> 0) then throw MainParsingException2

    for
      (i, n) <- rep
    do
      prog(i)._1 match
        case `(*)`(id, _, params*) =>
          Console.err.println("Warning! " + RecRepParsingException(id, params.size, n).getMessage + ".")

  def apply(prog: List[Bind]): Unit =

    ensure(prog)


  private var i: Int = 0

  def apply(source: Source): List[Either[String, Bind]] = (source.getLines().toList :+ "")
    .foldLeft(List[String]() -> false) {
      case ((r, false), l) => (r :+ l) -> l.endsWith("\\")
      case ((r, true), l) => (r.init :+ r.last.stripSuffix("\\") + l) -> l.endsWith("\\")
    }._1
    .filterNot(_.matches("^[ ]*#.*")) // commented lines
    .filterNot(_.isBlank) // empty lines
    .flatMap { it =>
      if it.matches("^[ ]*@.*")
      then // Scala
        Some(Left(it.replaceFirst("^([ ]*)@(.*)$", "$1$2")))
      else // Pi
        parseAll(line, it) match
          case Success(Left(equation), _) =>
            eqtn :+= equation
            val equations = eqtn.slice(i, eqtn.size)
            i = eqtn.size
            equations.map(Right(_))
          case Success(Right(definition @ (Encoding(code, _, _, _), _)), _) =>
            defn(code) = definition
            Nil
          case failure: NoSuccess =>
            scala.sys.error(failure.msg)
    }
    .filter {
      case Right((`(*)`(s"Self_$n", _, _*), _))
          if { try { n.toInt; true } catch _ => false } =>
        self.contains(n.toInt)
      case _ => true
    }
    .toList
