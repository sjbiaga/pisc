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

import scala.io.Source

import scala.collection.mutable.{
  LinkedHashMap => Map,
  LinkedHashSet => Set
}

import generator.Meta.`()(null)`

import Pi.*
import Calculus.*
import Encoding.*
import scala.util.parsing.combinator.pisc.parser.Expansion
import Expansion.Duplications


abstract class Pi extends Expression:

  def μ: Parser[(μ, (Names, Names))] =
    "τ" ~> opt( expression ) ^^ { // silent prefix
      case Some((it, free)) =>
        τ(Some(it)) -> (Names(), free)
      case _ =>
        τ(None) -> (Names(), Names())
    } |
    name ~ ("<"~>opt(name)<~">") ~ opt( expression ) ^^ { // negative prefix i.e. output
      case (ch, _) ~ _ ~ _ if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case (ch, name) ~ Some((arg, free)) ~ Some((it, freeʹ)) =>
        π(ch, arg, polarity = None, Some(it)) -> (Names(), name ++ free ++ freeʹ)
      case (ch, name) ~ Some((arg, free)) ~ _ =>
        π(ch, arg, polarity = None, None) -> (Names(), name ++ free)
      case (ch, name) ~ _ ~ Some((it, freeʹ)) =>
        π(ch, λ(`()(null)`), polarity = None, Some(it)) -> (Names(), name ++ freeʹ)
      case (ch, name) ~ _ ~ _ =>
        π(ch, λ(`()(null)`), polarity = None, None) -> (Names(), name)
    } |
    name ~ ("("~>nameʹ<~")") ~ opt( expression ) ^^ { // positive prefix i.e. input
      case (ch, _) ~ _ ~ _ if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case _ ~ (par, _) ~ _ if !par.isSymbol =>
        throw PrefixChannelParsingException(par)
      case _ ~ _ ~ Some(((Left(enums), _), _)) =>
        throw TermParsingException(enums)
      case (ch, name) ~ (par, bound) ~ code =>
        val freeʹ = code.map(_._2).getOrElse(Names())
        π(ch, par, polarity = Some(""), code.map(_._1)) -> (bound, name ++ freeʹ)
    } |
    name ~ cons_r ~ ("("~>namesʹ<~")") ~ opt( expression ) ^^ { // polyadic unconsing
      case (ch, _) ~ _ ~ _ ~ _ if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case _ ~ _ ~ params ~ _ if !params.forall(_._1.isSymbol) =>
        throw PrefixChannelsParsingException(params.filterNot(_._1.isSymbol).map(_._1)*)
      case (ch, _) ~ _ ~ params ~ _
          if {
            val paramsʹ = params.filterNot { case (λ(Symbol("")), _) => true case _ => false }
            paramsʹ.size > paramsʹ.distinctBy { case (λ(Symbol(it)), _) => it }.size
          } =>
        throw PrefixChannelsParsingExceptionʹ(ch.asSymbol,
                                              params
                                                .filterNot { case (λ(Symbol("")), _) => true case _ => false }
                                                .map(_._1.asSymbol.name)*)
      case _ ~ _ ~ _ ~ Some(((Left(enums), _), _)) =>
        throw TermParsingException(enums)
      case (λ(ch: Symbol), _) ~ cons ~ params ~ _
          if params.exists { case (λ(`ch`), _) => true case _ => false } =>
        throw ConsItselfParsingException(ch, cons)
      case (ch, name) ~ cons ~ params ~ code =>
        val args = λ(params.map(_._1))
        val bound = params.map(_._2).reduce(_ ++ _)
        val free = code.map(_._2).getOrElse(Names())
        π(ch, args, polarity = Some(cons), code.map(_._1)) -> (bound, name ++ free &~ bound)
    }

  def name: Parser[(λ, Names)] = ident ^^ (Symbol(_)) ^^ { it => λ(it) -> Set(it) } |
                                 floatingPointNumber ^^ (BigDecimal(_)) ^^ { λ(_) -> Names() } |
                                 stringLiteral ^^ (_.stripPrefix("\"").stripSuffix("\"")) ^^ { λ(_) -> Names() } |
                                 ( "True" | "False" ) ^^ (_ == "True") ^^ { λ(_) -> Names() } |
                                 expression ^^ {
                                   case ((Right(term), _), free) => λ(term) -> free
                                   case ((Left(enums), _), _) => throw TermParsingException(enums)
                                 }

  def nameʹ: Parser[(λ, Names)] =
    name ~ opt(`type`) ^^ {
      case (λ, free) ~ tpe =>
        (λ.copy()(using tpe), free)
    }

  def names: Parser[List[(λ, Names)]] =
    rep1sep(name, ",")

  def namesʹ: Parser[List[(λ, Names)]] =
    rep1sep(opt(nameʹ) ^^ { _.getOrElse(λ(Symbol("")) -> Names()) }, ",")

  /**
   * Channel names start with lower case.
   * @return
   */
  override def ident: Parser[String] =
      "" ~> // handle whitespace
      rep1(acceptIf(Character.isLowerCase)("channel name expected but '" + _ + "' found"),
          elem("channel name part", { (ch: Char) => Character.isJavaIdentifierPart(ch) || ch == '\'' || ch == '"' })) ^^ (_.mkString)

  private[parser] var eqtn: List[Bind] = null
  private[parser] var defn: Map[Int, List[Define]] = null
  private[parser] var self: Set[Int] = null
  protected var _nest = -1
  private[parser] var _nth: Map[Int, Long] = null
  protected final def nest(b: Boolean) =
    _nth(_nest) += 1
    _nest += (if b then 1 else -1)
    if b
    then
      _nth(_nest) = 0L
      _cntr(_nest) = 0L
    else
      _nth -= _nest+1
      _cntr -= _nest+1
  private[parser] var _cntr: Map[Int, Long] = null

  private[parser] def pos(binds: Boolean = false) = { _cntr(_nest) += 1; Position(_cntr(_nest), binds) }
  private[parser] def pos_(binds: Boolean = false) = { _cntr(_nest) += 1; Position(-_cntr(_nest), binds) }

  protected final def path = (0 until _nest).map(_nth(_))

  protected var _dirs = List[Map[String, Any]]()

  protected var _dups: Boolean = false

  protected var _exclude: Boolean = false

  private[parser] var _id: helper.υidυ = null

  private[parser] var _χ_id: helper.υidυ = null

  protected final def id = _id()

  protected final def χ_id = _χ_id()

  protected final def copy: (Any, Any) =
    _id.copy -> _χ_id.copy

  protected final def paste(it: (Any, Any)) =
    _id.paste(it._1)
    _χ_id.paste(it._2)

  protected final def save[T](r: => ParseResult[T]): Option[(T, Input)] =
    val nest = _nest
    val cntr = Map.from(_cntr)
    _id.save {
      _χ_id.save {
        r match
          case Success(it, in) => Some(it -> in)
          case _ =>
            _cntr = cntr
            _nest = nest
            None
      }
    }

  protected object BindingOccurrence:
    def apply(names: Names)
             (using Bindings): Unit =
      names.foreach { it => this(it, if _code < 0 then None else Some(it), hardcoded = true) }
    def apply(name: Symbol, shadow: Option[Symbol], hardcoded: Boolean = false)
             (using bindings: Bindings): Unit =
      bindings.get(name) match
        case Some(Occurrence(_, it @ Position(k, false))) if k < 0 =>
          bindings += name -> Occurrence(shadow, it.copy(binds = true))
        case Some(Occurrence(Some(_), Position(k, true))) if _code >= 0 && (!hardcoded || k < 0) =>
          throw UniquenessBindingParsingException(_code, _nest, name, hardcoded)
        case Some(Occurrence(_, Position(_, false))) if _code >= 0 =>
          throw NonParameterBindingParsingException(_code, _nest, name, hardcoded)
        case Some(Occurrence(_, Position(_, false))) =>
        case _ =>
          bindings += name -> Occurrence(shadow, pos(true))


object Pi:

  private val cons_r = """[^/*{\[(<.,"'\p{Alnum}@\p{Space}'",.>)\]}*/]+""".r

  type Names = Set[Symbol]

  object Names:
    def apply(os: λ*): Names = Set.from(os
      .filter(_.isSymbol)
      .map(_.asSymbol)
    )
    def apply(names: Names): Names = Set.from(names)


  // exceptions

  import scala.meta.Enumerator

  import Expression.ParsingException

  abstract class PrefixParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg, cause)

  case class PrefixChannelParsingException(name: λ)
      extends PrefixParsingException(s"${name.`val`} is not a channel name but a ${name.kind}")

  case class PrefixChannelsParsingExceptionʹ(name: Symbol, ps: String*)
      extends PrefixParsingException(s"""For a polyadic name ${name.name}, the parameters names '${ps.mkString(", ")}' must be distinct""")

  case class TermParsingException(enums: List[Enumerator])
      extends PrefixParsingException(s"The embedded Scalameta should be a Term, not Enumerator `$enums'")

  case class ConsItselfParsingException(name: Symbol, cons: String)
      extends PrefixParsingException(s"A name ${name.name} that knows how to CONS (`$cons') is itself the result")


  // functions

  extension [T <: AST](ast: T)

    def shallow: T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case +(it*) =>
          `+`(it.map(_.shallow)*)

        case ∥(it*) =>
          ∥(it.map(_.shallow)*)

        case `.`(end, it*) =>
          `.`(end.shallow, it*)

        case ?:(cond, t, f) =>
          ?:(cond, t.shallow, f.map(_.shallow))

        case it @ !(_, sum) =>
          it.copy(sum = sum.shallow)

        case it @ `⟦⟧`(_, _, sum, _, _) =>
          it.copy(sum = sum.shallow)

        case `{}`(id, pointers, true, params*) =>
          `(*)`(id, Nil, (params ++ pointers.map(λ(_)))*)

        case _ => ast


  final class Main(override protected val in: String) extends Expansion:

    def line(using Duplications): Parser[Either[Bind, Option[Define]]] =
      equation ^^ { Left(_) } | definition ^^ { Right(_) }

    private def ensure(using prog: List[Bind]): Unit =
      import helper.Ensure.*

      val i = main

      if i < 0 then throw MainParsingException

      given rec: Map[(String, Int), Int] = Map()
      given rep: Map[Int, Int] = Map()

      prog(i)._2.recursive(using "Main" -> 0 :: Nil)

      if rec.contains("Main" -> 0) then throw MainParsingExceptionʹ

      for
        (i, n) <- rep
      do
        prog(i)._1 match
          case `(*)`(id, _, params*) =>
            warn(throw RecRepParsingException(id, params.size, n))

    def apply(prog: List[Bind]): List[Bind] =
      given List[Bind] = prog.map(_ -> _.shallow)
      ensure
      given_List_Bind

    private var i: Int = -1
    private var l: (Int, Int) = (-1, -1)
    override def ln: String = if l._1 == l._2 then s"line #${l._2}" else s"lines #${l._1}-#${l._2}"

    def apply(source: Source, errors: Boolean = false): List[Either[String, Bind]] =
      _werr = errors
      _dups = false
      _exclude = false
      _dirs = List(Map("errors" -> _werr,
                       "duplications" -> _dups,
                       "exclude" -> _exclude))
      eqtn = List()
      defn = Map()
      self = Set()
      _nest = 0
      _id = new helper.υidυ
      _χ_id = new helper.υidυ
      i = 0
      l = (0, 0)

      given Duplications()

      val r =
        (source.getLines() ++ Some(""))
          .zipWithIndex
          .foldLeft(List[(String, (Int, Int))]() -> false) {
            case ((r, false), (l, n)) => (r :+ (l, (n, n))) -> l.endsWith("\\")
            case ((r, true), (l, n)) => (r.init :+ (r.last._1.stripSuffix("\\") + l, (r.last._2._1, n))) -> l.endsWith("\\")
          }._1
          .flatMap { case (it, (m, n)) =>
            l = (m+1, n+1)
            if it.matches("^[ ]*#.*") // commented lines
            || it.isBlank // empty lines
            then
              None
            else if it.matches("^[ ]*@.*")
            then // Scala
              Some(Left(it.replaceFirst("^([ ]*)@(.*)$", "$1$2")))
            else // Pi
              _cntr = Map(0 -> 0L)
              _nth = Map(0 -> 0L)
              parseAll(line, it) match
                case Success(Left(equation), _) =>
                  if !_exclude
                  then
                    eqtn :+= equation
                    val equations = eqtn.slice(i, eqtn.size)
                    i = eqtn.size
                    equations.map(Right(_))
                  else
                    Nil
                case Success(Right(Some(definition)), _) =>
                  if !_exclude
                  then
                    if !defn.contains(_code) then defn(_code) = Nil
                    defn(_code) ::= definition
                  Nil
                case Success(Right(_), _) => // directive
                  Nil
                case failure: NoSuccess =>
                  scala.sys.error(failure.msg)
          }

      ( if i < eqtn.size && !_exclude
        then
          r ::: eqtn.slice(i, eqtn.size).map(Right(_))
        else
          r
      ).filter {
        case Right((`(*)`(s"Self_$n", _, _*), _))
            if { try { n.toInt; true } catch _ => false } =>
          self.contains(n.toInt)
        case _ => true
      }
