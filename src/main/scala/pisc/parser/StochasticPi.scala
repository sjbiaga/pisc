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

import scala.collection.mutable.{
  HashMap => Map,
  ListBuffer => MutableList,
  LinkedHashSet => Set
}
import scala.io.Source

import scala.util.parsing.combinator._

import generator.Meta.`()(null)`

import StochasticPi._
import Calculus._
import Encoding.{ id => _, _ }
import scala.util.parsing.combinator.pisc.parser.Expansion


abstract class StochasticPi extends Expression:

  def `μ.`: Parser[(μ, (Names, Names))] =
    "τ"~opt("@"~>rate) ~ opt( expression ) ^^ { // silent prefix
      case _ ~ r ~ Some((it, free)) =>
        τ(Some(it), r.getOrElse(1L)) -> (Names(), free)
      case _ ~ r ~ _ =>
        τ(None, r.getOrElse(1L)) -> (Names(), Names())
    } |
    name ~ opt("@"~>rate) ~ ("<"~>opt(name)<~">") ~ opt( expression ) ^^ { // negative prefix i.e. output
      case (ch, _) ~ _ ~ _ ~ _  if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case (ch, name) ~ r ~  Some((arg, free)) ~ Some((it, free2)) =>
        π(ch, arg, polarity = false, r.getOrElse(1L), Some(it)) -> (Names(), name ++ free ++ free2)
      case (ch, name) ~ r ~ Some((arg, free)) ~ _ =>
        π(ch, arg, polarity = false, r.getOrElse(1L), None) -> (Names(), name ++ free)
      case (ch, name) ~ r ~ _ ~ Some((it, free2)) =>
        π(ch, λ(Expr(`()(null)`)), polarity = false, r.getOrElse(1L), Some(it)) -> (Names(), name ++ free2)
      case (ch, name) ~ r ~ _ ~ _ =>
        π(ch, λ(Expr(`()(null)`)), polarity = false, r.getOrElse(1L), None) -> (Names(), name)
    } |
    name ~ opt("@"~>rate) ~ ("("~>name<~")") ~ opt( expression ) ^^ { // positive prefix i.e. input
      case (ch, _) ~ _ ~ _ ~ _ if !ch.isSymbol =>
        throw PrefixChannelParsingException(ch)
      case _ ~ _ ~ (par, _) ~ _ if !par.isSymbol =>
        throw PrefixChannelParsingException(par)
      case _ ~ _ ~ _ ~ Some(((Left(enums), _), _)) =>
        throw TermParsingException(enums)
      case (ch, name) ~ r ~ (par, binding) ~ Some((it, free2)) =>
        π(ch, par, polarity = true, r.getOrElse(1L), Some(it)) -> (binding, name ++ free2)
      case (ch, name) ~ r ~ (par, binding) ~ _ =>
        π(ch, par, polarity = true, r.getOrElse(1L), None) -> (binding, name)
    }

  def name: Parser[(λ, Names)] = ident ^^ { it => λ(Symbol(it)) -> Set(Symbol(it)) } |
                                 floatingPointNumber ^^ { it => λ(BigDecimal(it)) -> Names() } |
                                 stringLiteral ^^ { λ(_) -> Names() } |
                                 ( "True" | "False" ) ^^ { it => λ(it == "True") -> Names() } |
                                 expression ^^ {
                                   case ((Right(term), _), free) => λ(Expr(term)) -> free
                                   case ((Left(enums), _), _) => throw TermParsingException(enums)
                                 }

  def rate: Parser[Any] = "("~>rate<~")" |
                          "∞" ^^ { _ => -1L } |
                          wholeNumber<~"∞" ^^ { -_.toLong.abs } |
                          "⊤" ^^ { _ => 1L } |
                          wholeNumber<~"⊤" ^^ { _.toLong.abs } |
                          floatingPointNumber ^^ { BigDecimal.apply } |
                          super.ident ^^ { Symbol.apply } |
                          expression ^^ {
                            case ((Right(term), _), free) => Some(Expr(term))
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

  private[parser] var _werr: Boolean = false
  private[parser] var eqtn: List[Bind] = null
  private[parser] var defn: Map[Int, List[Define]] = null
  private[parser] var self: Set[Int] = null
  private[parser] var _nest = -1
  protected final def nest(b: Boolean) = { _nest += (if b then 1 else -1); if b then _cntr(_nest) = 0L }
  private[parser] var _cntr: Map[Int, Long] = null

  protected final def save[T](r: => ParseResult[T], fail: Boolean): Option[(T, Input)] =
    val nest = _nest
    val cntr = Map.from(_cntr)
    _id.save {
      sπ_id.save {
        r match
          case Success(it, in) => Some(it -> in)
          case failure: NoSuccess if fail =>
            scala.sys.error(failure.msg)
          case _ =>
           _cntr = cntr
           _nest = nest
           None
      }
    }


object StochasticPi extends Expansion:

  type Actions = Set[String]

  object Actions:
    def apply(ps: Pre*): Actions = Set.from(
      ps
        .filter(_.isInstanceOf[Act])
        .headOption
        .map(_.asInstanceOf[Act].υidυ)
    )

  def nil = Actions()


  trait Act:
    val rate: Any
    final lazy val υidυ: String = id

  trait Sum:
    val enabled: Actions


  def line: Parser[Either[Bind, Define]] =
    equation ^^ { Left(_) } | definition ^^ { Right(_) }

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

  case class WholeNumberFormatException(msg: String, cause: Throwable = null)
      extends PrefixParsingException("The weight is a natural number that must " + msg, cause)

  case class PrefixChannelParsingException(name: λ)
      extends PrefixParsingException(s"${name.value} is not a channel name but a ${name.kind}")

  case class TermParsingException(enums: List[Enumerator])
      extends PrefixParsingException(s"The embedded Scalameta should be a Term, not Enumerator `$enums'")


  // functions

  private[parser] def pos(binding: Boolean = false) = { _cntr(_nest) += 1; Position(_cntr(_nest), binding) }
  private[parser] def pos_(binding: Boolean = false) = { _cntr(_nest) += 1; Position(-_cntr(_nest), binding) }

  extension[T <: AST](ast: T)

    def shallow: T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅ => ∅

        case +(_, it*) =>
          `+`(nil, it.map(_.shallow)*)

        case ||(it*) =>
          ||(it.map(_.shallow)*)

        case `.`(end, it*) =>
          `.`(end.shallow, it*)

        case ?:(cond, t, f) =>
          ?:(cond, t.shallow, f.map(_.shallow))

        case it @ !(_, sum) =>
          it.copy(sum = sum.shallow)

        case it @ `⟦⟧`(_, _, sum, _) =>
          it.copy(sum = sum.shallow)

        case `{}`(id, pointers, true, params*) =>
          `(*)`(id, (params ++ pointers.map(λ(_)))*)

        case it =>
          it

  def ensure(implicit prog: List[Bind]): Unit =
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
        case `(*)`(id, params*) =>
          if _werr
          then
            throw RecRepParsingException(id, params.size, n)
          Console.err.println("Warning! " + RecRepParsingException(id, params.size, n).getMessage + ".")

  def `(*) => +`(prog: List[Bind]): `(*)` => + = {
    case `(*)`(identifier, args*) =>
      prog
        .find {
          case (`(*)`(`identifier`, params*), _) if params.size == args.size => true
          case _ => false
        }
        .get
        ._2
  }

  extension[T <: AST](ast: T)

    def parse(using excluded: Map[String, Actions]): (T, Actions) =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      inline def τ = Calculus.τ(None, -Long.MaxValue)

      def insert[S](end: &, ps: Pre*): (S, Actions) =
        val ps2 = ps :+ τ
        `.`(end, ps2*).asInstanceOf[S] -> Actions(ps2*)

      def insert_+(sum: +): + =
        val (seq, enabled) = insert[`.`](sum)
        `+`(enabled, ||(seq))

      ast match

        case ∅ => (∅, nil)

        case it: + =>
          val sum = it.choices.foldLeft(`+`(nil)) {
            case (sum, par) =>
              val (par2, enabled2) = par.parse
              assert(enabled2.nonEmpty)
              assert((sum.enabled & enabled2).isEmpty)
              (`+`(sum.enabled ++ enabled2, (sum.choices :+ par2)*))
          }
          (sum, sum.enabled)

        case it: || =>
          val (par, enabled) = it.components.foldLeft((||(), nil)) {
            case ((par, enabled), seq) =>
              val (seq2, enabled2) = seq.parse
              assert(enabled2.nonEmpty)
              assert((enabled & enabled2).isEmpty)
              (||((par.components :+ seq2)*), enabled ++ enabled2)
          }
          (par, enabled)

        case `.`(end, ps*) =>
          val (it, enabled) = end.parse

          if Actions(ps*).nonEmpty
          then
            (`.`(it, ps*), Actions(ps*))
          else if enabled.nonEmpty
          then
            (`.`(it, ps*), enabled)
          else
            assert(∅ == it || it.isInstanceOf[`(*)`])
            insert(it, ps*)

        case ?:(c, _t, _f) =>
          def _t_f(_sum: +): + =
            val (sum, _) = _sum.parse

            if sum.enabled.isEmpty
            || sum.enabled.exists(excluded.contains(_))
            then
              insert_+(sum)
            else
              sum

          val (t, f) = _t_f(_t) -> _f.map(_t_f(_))

          f.foreach { sum => t.enabled.foreach(excluded(_) = sum.enabled) }
          f.foreach(_.enabled.foreach(excluded(_) = t.enabled))

          assert(t.enabled.nonEmpty)
          assert(f.map(_.enabled.nonEmpty).getOrElse(true))

          assert((t.enabled & f.map(_.enabled).getOrElse(nil)).isEmpty)
          (?:(c, t, f), t.enabled ++ f.map(_.enabled).getOrElse(nil))

        case !(Some(μ), sum) =>
          val (it, _) = sum.parse
          (`!`(Some(μ), it), Actions(μ))

        case !(_, sum) =>
          `!`(Some(τ), sum).parse

        case `⟦⟧`(definition, variables, _sum, assign) =>
          val n = assign.map(_.size).getOrElse(0)

          val sum = ( if variables.size == n
                      then
                        _sum
                      else
                        `+`(nil, ||(`.`(_sum, ν(variables.drop(n).map(_.name).toSeq*))))
                    )

           var (it, _) = sum.parse

          if it.enabled.isEmpty
          then
            it = insert_+(it)

          (`⟦⟧`(definition, variables, it, assign), it.enabled)

        case _: `{}` => ???

        case it: `(*)` => (it, nil)

    def split(using discarded_excluded: (Map[String, Actions], Map[String, Actions])): Actions =

      ast match

        case ∅ => nil

        case +(enabled, par) =>
          par.split
          enabled

        case +(enabled, ps*) =>
          val ls = ps.map(_.split)
          var ts = ls.take(0)
          var ds = ls.drop(1)

          val (discarded, _) = discarded_excluded

          ls.foreach { it =>
            assert(it.nonEmpty)
            val ks = (ts ++ ds).reduce(_ ++ _)
            assert((it & ks).isEmpty)
            it.foreach { k =>
              if !discarded.contains(k)
              then
                discarded(k) = nil
              discarded(k) ++= ks
            }
            ts :+= it
            ds = ds.drop(1)
          }

          enabled

        case ||(ss*) =>
          ss.map(_.split).reduce(_ ++ _)

        case `.`(_: `(*)`, ps*) =>
          assert(Actions(ps*).nonEmpty)
          Actions(ps*)

        case `.`(end, ps*) =>
          var enabled = end.split

          if Actions(ps*).nonEmpty then
            enabled = Actions(ps*)

          enabled

        case ?:(_, t, f) =>
          t.split
          f.foreach(_.split)

          val (_, excluded) = discarded_excluded

          t.enabled.foreach { k => assert(!excluded.contains(k)) }
          f.foreach(_.enabled.foreach { k => assert(!excluded.contains(k)) })

          f.foreach { sum => t.enabled.headOption.foreach(excluded(_) = sum.enabled) }
          f.foreach(_.enabled.headOption.foreach(excluded(_) = t.enabled))

          assert((t.enabled & f.map(_.enabled).getOrElse(nil)).isEmpty)
          t.enabled ++ f.map(_.enabled).getOrElse(nil)

        case !(Some(μ), sum) =>
          sum.split
          Actions(μ)

        case _: ! => ??? // impossible by 'parse'

        case `⟦⟧`(_, _, sum, _) =>
          sum.split
          sum.enabled

        case _: `{}` => ???

        case _: `(*)` => ??? // always "guarded"

    def graph(using prog: List[Bind]): Seq[(Act, Act | Sum)] =

      ast match

        case ∅ => Nil

        case +(_, ps*) =>
          ps.map(_.graph).reduce(_ ++ _)

        case ||(ss*) =>
          ss.map(_.graph).reduce(_ ++ _)

        case `.`(end, ps*) =>

          inline given Conversion[(Pre, Act | Sum), (Act, Act | Sum)] = _.asInstanceOf[Act] -> _

          end.graph ++
          ( for
              i <- 0 until ps.size
              if ps(i).isInstanceOf[Act]
              j = ps.drop(i+1).indexWhere(_.isInstanceOf[Act])
              if j >= 0
            yield
              ps(i).asInstanceOf[Act] -> ps(i+1+j).asInstanceOf[Act]
          ) ++
          {
            val k = ps.lastIndexWhere(_.isInstanceOf[Act])
            if k >= 0
            then end match
              case sum: + =>
                Seq(ps(k) -> sum)
              case ?:(_, t, Some(f)) =>
                Seq(ps(k) -> t, ps(k) -> f)
              case ?:(_, t, _) =>
                Seq(ps(k) -> t)
              case !(Some(μ), _) =>
                Seq(ps(k) -> μ)
              case _: ! => ??? // impossible by 'parse'
              case `⟦⟧`(_, _, sum, _) =>
                Seq(ps(k) -> sum)
              case _: `{}` => ???
              case it: `(*)` =>
                val sum = `(*) => +`(prog)(it)
                Seq(ps(k) -> sum)
            else Nil
          }

        case ?:(_, t, f) =>
          t.graph ++ f.map(_.graph).getOrElse(Nil)

        case !(Some(μ), sum) =>
          sum.graph ++ Seq(μ -> μ, μ -> sum)

        case _: ! => ??? // impossible by 'parse'

        case `⟦⟧`(_, _, sum, _) =>
          sum.graph

        case _: `{}` => ???

        case _: `(*)` => Nil

  def apply(prog: List[Bind]): (List[Bind], (Map[String, Actions], Map[String, Actions], Map[String, Actions])) =

    given excluded: Map[String, Actions] = Map()

    given List[Bind] = prog.map(_ -> _.shallow.parse._1)

    ensure

    val discarded = Map[String, Actions]()

    excluded.clear

    val enabled = Map[String, Actions]()

    given_List_Bind
      .tapEach {
        case (_, sum) =>
          sum.split(using discarded -> excluded)

          sum.graph.foreach { (s, t) =>
            if !enabled.contains(s.υidυ)
            then
              enabled(s.υidυ) = nil
            t match
              case it: Act =>
                enabled(s.υidυ) += it.υidυ
              case it: Sum =>
                enabled(s.υidυ) ++= it.enabled
          }
      } -> (discarded, excluded, enabled)


  private var i: Int = -1
  private var l: (Int, Int) = (-1, -1)
  def ln = l

  def apply(source: Source, errors: Boolean = false): List[Either[String, Bind]] =
    _werr = errors
    eqtn = List()
    defn = Map()
    self = Set()
    _nest = 0
    _cntr = Map(0 -> 0L)
    _id = new helper.υidυ
    sπ_id = new helper.υidυ
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
      else // SPi
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
      case Right((`(*)`(s"Self_$n", _*), _))
          if { try { n.toInt; true } catch _ => false } =>
        self.contains(n.toInt)
      case _ => true
    }
    .toList


  private[parser] var sπ_id: helper.υidυ = null

  def id = sπ_id()
