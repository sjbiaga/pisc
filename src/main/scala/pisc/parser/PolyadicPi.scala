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

import generator.Meta.`()(null)`

import PolyadicPi._
import Calculus._
import Encoding._
import scala.util.parsing.combinator.pisc.parser.Expansion


abstract class PolyadicPi extends Expression:

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
      case _ ~ _ ~ Some(((Left(enums), _), _)) =>
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
                                   case ((Right(term), _), free) => λ(Expr(term)) -> free
                                   case ((Left(enums), _), _) => throw TermParsingException(enums)
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

  private[parser] var _werr: Boolean = false
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
      _cntr -= _nest +1
  private[parser] var _cntr: Map[Int, Long] = null

  private[parser] def pos(binding: Boolean = false) = { _cntr(_nest) += 1; Position(_cntr(_nest), binding) }
  private[parser] def pos_(binding: Boolean = false) = { _cntr(_nest) += 1; Position(-_cntr(_nest), binding) }

  protected final def path = (0 until _nest).map(_nth(_))

  private[parser] var _id: helper.υidυ = null

  protected def id = _id()

  protected def copy: Any = _id.copy

  protected def paste(it: Any) = _id.paste(it)

  protected final def save[T](r: => (ParseResult[T], Any), fail: Boolean): Option[(T, Input)] =
    val nest = _nest
    val cntr = Map.from(_cntr)
    _id.save {
      r match
        case (Success(it, in), _) => Some(it -> in)
        case (failure: NoSuccess, _) if fail =>
          scala.sys.error(failure.msg)
        case _ =>
          _cntr = cntr
          _nest = nest
          None
    }

  protected object Names2Occurrence:
    def apply(names: Names)
             (using Names2): Unit =
      names.foreach { it => this(it, if _code < 0 then None else Some(it), hardcoded = true) }
    def apply(name: Symbol, shadow: Option[Symbol], hardcoded: Boolean = false)
             (using binding2: Names2): Unit =
      binding2.get(name) match
        case Some(Occurrence(_, it @ Position(k, false))) if k < 0 =>
          binding2 += name -> Occurrence(shadow, it.copy(binding = true))
        case Some(Occurrence(_, Position(k, true))) if _code >= 0 && (!hardcoded || k < 0) =>
          throw UniquenessBindingParsingException(_code, _nest, name, hardcoded)
        case Some(Occurrence(_, Position(_, false))) if _code >= 0 =>
          throw NonParameterBindingParsingException(_code, _nest, name, hardcoded)
        case Some(Occurrence(_, Position(_, false))) =>
        case _ =>
          binding2 += name -> Occurrence(shadow, pos(true))


object PolyadicPi:

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
      extends PrefixParsingException(s"${name.value} is not a channel name but a ${name.kind}")

  case class PrefixChannelsParsingException(names: λ*)
      extends PrefixParsingException(s"${names.mkString(", ")} are not channel names but ${names.map(_.kind).mkString(", ")}")


  case class PrefixUniquenessParsingException(name: String, ps: String*)
      extends PrefixParsingException(s"""$name channel parameters names '${ps.mkString(", ")}' must be distinct""")

  case class PrefixArityParsingException(name: λ)
      extends PrefixParsingException(s"Without arguments, channel ${name.asSymbol.name} must specify arity")

  case class PrefixArityParsingException2(name: λ, arity: Int, size: Int)
      extends PrefixParsingException(s"${name.asSymbol.name} channel must not specify both arity ($arity) and arguments (#$size)")

  case class TermParsingException(enums: List[Enumerator])
      extends PrefixParsingException(s"The embedded Scalameta should be a Term, not Enumerator `$enums'")


  // functions

  extension[T <: AST](ast: T)

    def shallow: T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅ => ∅

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

        case it @ `⟦⟧`(_, _, sum, _) =>
          it.copy(sum = sum.shallow)

        case `{}`(id, pointers, true, params*) =>
          `(*)`(id, Nil, (params ++ pointers.map(λ(_)))*)

        case it =>
          it


  final class Main extends Expansion:

    def line: Parser[Either[Bind, Define]] =
      equation ^^ { Left(_) } | definition ^^ { Right(_) }

    private def ensure(using prog: List[Bind]): Unit =
      import helper.Ensure._

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
            if _werr
            then
              throw RecRepParsingException(id, params.size, n)
            Console.err.println("Warning! " + RecRepParsingException(id, params.size, n).getMessage + ".")

    def apply(prog: List[Bind]): List[Bind] =
      given List[Bind] = prog.map(_ -> _.shallow)
      ensure
      given_List_Bind

    private var i: Int = -1
    private var l: (Int, Int) = (-1, -1)
    def ln = l

    def apply(source: Source, errors: Boolean = false): List[Either[String, Bind]] =
      _werr = errors
      eqtn = List()
      defn = Map()
      self = Set()
      _nest = 0
      _id = new helper.υidυ
      i = 0
      l = (0, 0)

      (source.getLines().toList :+ "")
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
        else // PolyadicPi
          _cntr = Map(0 -> 0L)
          _nth = Map(0 -> 0L)
          parseAll(line, it) match
            case Success(Left(equation), _) =>
              eqtn :+= equation
              val equations = eqtn.slice(i, eqtn.size)
              i = eqtn.size
              equations.map(Right(_))
            case Success(Right(definition), _) =>
              if !defn.contains(_code) then defn(_code) = Nil
              defn(_code) ::= definition
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
