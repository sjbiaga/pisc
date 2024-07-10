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

import java.util.UUID

import scala.collection.mutable.{ HashMap => Map, LinkedHashSet => Set }
import scala.io.Source

import scala.meta.Term

import scala.util.parsing.combinator._

import generator.Meta.`()(null)`

import StochasticPi._
import Calculus._


trait StochasticPi extends Expression:

  def `μ.`: Parser[(μ, (Names, Names))] =
    "τ"~opt("@"~>rate) ~ opt( expression ) ^^ { // silent prefix
      case _ ~ r ~ Some((it, free)) =>
        τ(Some(it), r) -> (Names(), free)
      case _ ~ r ~ _ =>
        τ(None, r) -> (Names(), Names())
    } |
    name~opt("@"~>rate)~"<"~opt(name)~">" ~ opt( expression ) ^^ { // negative prefix i.e. output
      case (ch, _) ~ _ ~ _ ~ _ ~ _ ~ _ if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case (ch, name) ~ r ~ _ ~ Some((arg, free)) ~ _ ~ Some((it, free2)) =>
        π(ch, arg, polarity = false, r, Some(it)) -> (Names(), name ++ free ++ free2)
      case (ch, name) ~ r ~ _ ~ Some((arg, free)) ~ _ ~ _ =>
        π(ch, arg, polarity = false, r, None) -> (Names(), name ++ free)
      case (ch, name) ~ r ~ _ ~ _ ~ _ ~ Some((it, free2)) =>
        π(ch, λ(Expr(`()(null)`)), polarity = false, r, Some(it)) -> (Names(), name ++ free2)
      case (ch, name) ~ r ~ _ ~ _ ~ _ ~ _ =>
        π(ch, λ(Expr(`()(null)`)), polarity = false, r, None) -> (Names(), name)
    } |
    name~opt("@"~>rate)~"("~name~")" ~ opt( expression ) ^^ { // positive prefix i.e. input
      case (ch, _) ~ _ ~ _ ~ _ ~ _ ~ _ if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case _ ~ _ ~ _ ~ (par, _) ~ _ ~ _ if !par.isSymbol =>
        throw PrefixChannelParsingException(par)
      case _ ~ _ ~ _ ~ _ ~ _ ~ Some((Left(enums), _)) =>
        throw TermParsingException(enums)
      case (ch, name) ~ r ~ _ ~ (par, bound) ~ _ ~ Some((it, free2)) =>
        π(ch, par, polarity = true, r, Some(it)) -> (bound, name ++ free2)
      case (ch, name) ~ r ~ _ ~ (par, bound) ~ _ ~ _ =>
        π(ch, par, polarity = true, r, None) -> (bound, name)
    }

  def name: Parser[(λ, Names)] = ident ^^ { it => λ(Symbol(it)) -> Set(Symbol(it)) } |
                                 floatingPointNumber ^^ { it => λ(it) -> Names() } |
                                 stringLiteral ^^ { it => λ(it) -> Names() } |
                                 ( "True" | "False" ) ^^ { it => λ(it == "True") -> Names() } |
                                 expression ^^ {
                                   case (Right(term), free) => λ(Expr(term)) -> free
                                   case (Left(enums), _) => throw TermParsingException(enums)
                                 }

  def rate: Parser[Any] = "("~>rate<~")" |
                          "∞" ^^ { _ => -1L } |
                          wholeNumber<~"∞" ^^ { -_.toLong.abs } |
                          "⊤" ^^ { _ => 1L } |
                          wholeNumber<~"⊤" ^^ { _.toLong.abs } |
                          floatingPointNumber ^^ { BigDecimal.apply } |
                          super.ident ^^ { Symbol.apply } |
                          expression ^^ {
                            case (Right(term), free) => Some(Expr(term))
                            case (Left(enums), _) => throw TermParsingException(enums)
                          }

  /** A natural number. */
  override def wholeNumber: Parser[String] =
    """\d+""".r ^^ { s =>
      try
        if s.toLong == 0
        then
          throw WholeNumberFormatException("not be zero")
        else
          s
      catch
        case t: NumberFormatException =>
          throw WholeNumberFormatException("be a Long", t)
    }


object StochasticPi:

  type Actions = Set[String]

  object Actions:
    def apply(ps: Pre*): Actions = Set.from {
      ps
        .filter(_.isInstanceOf[Act with Key])
        .headOption
        .map(_.asInstanceOf[Act with Key].uuid)
    }
    def apply(end: `&`, ps: Pre*): Actions =
      val r = this(ps*)
      if r.nonEmpty
      then
        r
      else end match
        case it: `+` => it.enabled
        case `!`(Some(μ), _) => μ.enabled
        case `!`(_, it) => it.enabled
        case _ => r

  val nil = Set[String]()


  trait Act:
    val rate: Option[Any]


  trait Key:
    lazy val uuid: String = UUID.randomUUID.toString

  trait State extends Key:
    val enabled: Actions


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

  case class WholeNumberFormatException(msg: String, cause: Throwable = null)
      extends PrefixParsingException("The weight is a natural number that must " + msg, cause)

  case class PrefixChannelParsingException(name: λ)
      extends PrefixParsingException(s"${name.value} is not a channel name but a ${name.kind}")

  case class TermParsingException(enums: List[Enumerator])
      extends PrefixParsingException(s"The embedded Scalameta should be a Term, not Enumerator `$enums'")

  case object ProbabilisticChoiceException
      extends ParsingException("Probabilistic choice requires some prefix enabled on each branch")


  def apply(prog: List[Bind]): (List[Bind], (Map[String, Actions], Map[String, Actions])) =

    val discarded = Map[String, Actions]()

    lazy val split: AST => Actions = {

      case `∅` => nil
      case  _: `(*)` => nil

      case sum @ `+`(enabled, ps*) if ps.size == 1 =>
        split(ps.head)
        enabled

      case sum @ `+`(enabled, ps*) =>
        val ls = ps.map(split)

        ls.zipWithIndex.foreach { (it, i) =>
          if ls(i).isEmpty then throw ProbabilisticChoiceException
          val ks = (ls.take(i) ++ ls.drop(i+1)).reduce(_ ++ _)
          it.foreach { k =>
            if !discarded.contains(k)
            then
              discarded(k) = Actions()
            discarded(k) ++= ks
          }
        }

        enabled

      case `|`(ss*) =>
        ss.map(split).foldLeft(nil)(_ ++ _)

      case `.`(end, ps*) =>
        split(end)

        Actions(end, ps*)

      case `?:`(_, t, f) =>
        split(t)
        split(f)
        nil

      case `!`(Some(μ), sum) =>
        split(sum)
        μ.enabled

      case `!`(_, sum) =>
        split(sum)
        sum.enabled

      case _ => ???

    }

    lazy val trans: AST => Seq[(State, State)] = _ match

      case `∅` => Nil
      case  _: `(*)` => Nil

      case `+`(_, ps*) =>
        ps.map(trans).reduce(_ ++ _)

      case `|`(ss*) =>
        ss.map(trans).foldLeft(Seq.empty)(_ ++ _)

      case `.`(end, ps*) =>
        val k = ps.lastIndexWhere(_.isInstanceOf[State])
        ( for
            i <- 0 until ps.size
            if ps(i).isInstanceOf[State]
            j = ps.drop(i+1).indexWhere(_.isInstanceOf[State])
            if j >= 0
          yield
            ps(i).asInstanceOf[State] -> ps(i+1+j).asInstanceOf[State]
        ) ++
        ( if k < 0
          then end match
            case `!`(Some(μ), sum) =>
              Seq(μ -> μ) ++ Seq(μ -> sum)
            case _ =>
              Nil
          else end match
            case sum: `+` =>
              Seq(ps(k).asInstanceOf[State] -> sum)
            case `!`(Some(μ), sum) =>
              Seq(ps(k).asInstanceOf[State] -> μ) ++
              Seq(μ -> μ) ++ Seq(μ -> sum)
            case `!`(_, sum) =>
              Seq(ps(k).asInstanceOf[State] -> sum)
            case _ =>
              Nil
        ) ++ trans(end)

      case `?:`(_, t, f) => trans(t) ++ trans(f)

      case `!`(_, sum) => trans(sum)

      case _ => ???

    val enabled = Map[String, Actions]()

    prog
      .map {
        case it @ (bind, Some(sum)) =>
          try
            split(sum)
            it
          catch
            case t: ParsingException =>
              t.printStackTrace
              bind -> None
        case it => it
      }
      .map {
        case it @ (_, Some(sum)) =>
          trans(sum).foreach { (s, t) =>
            if !enabled.contains(s.uuid)
            then
              enabled(s.uuid) = Actions()
             enabled(s.uuid) ++= t.enabled
           }
          it
        case it => it
      } -> (discarded -> enabled)


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
      else // SPi
        parseAll(equation, it) match
          case Success(result, _) => Right(result)
          case failure: NoSuccess => scala.sys.error(failure.msg)
    }
    .toList
