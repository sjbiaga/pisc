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


class StochasticPi extends Expression:

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
      case _ ~ _ ~ _ ~ Some((Left(enums), _)) =>
        throw TermParsingException(enums)
      case (ch, name) ~ r ~ (par, bound) ~ Some((it, free2)) =>
        π(ch, par, polarity = true, r.getOrElse(1L), Some(it)) -> (bound, name ++ free2)
      case (ch, name) ~ r ~ (par, bound) ~ _ =>
        π(ch, par, polarity = true, r.getOrElse(1L), None) -> (bound, name)
    }

  def name: Parser[(λ, Names)] = ident ^^ { it => λ(Symbol(it)) -> Set(Symbol(it)) } |
                                 floatingPointNumber ^^ { it => λ(BigDecimal(it)) -> Names() } |
                                 stringLiteral ^^ { λ(_) -> Names() } |
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


object StochasticPi extends Calculus:

  type Actions = Set[String]

  object Actions:
    def apply(ps: Pre*): Actions = Set.from(
      ps
        .filter(_.isInstanceOf[Act])
        .headOption
        .map(_.asInstanceOf[Act].uuid)
    )

  def nil = Actions()


  trait Act:
    val rate: Any
    final lazy val uuid: String = UUID.randomUUID.toString

  trait Sum:
    val enabled: Actions


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

  case class WholeNumberFormatException(msg: String, cause: Throwable = null)
      extends PrefixParsingException("The weight is a natural number that must " + msg, cause)

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

    recursive(prog(i)._2)(using "Main" -> 0 :: Nil)

    if rec.contains("Main" -> 0) then throw MainParsingException2

    for
      (i, n) <- rep
    do
      prog(i)._1 match
        case `(*)`(id, params*) =>
          Console.err.println("Warning! " + RecRepParsingException(id, params.size, n).getMessage + ".")

  def `(*) => +`(prog: List[Bind]): `(*)` => `+` = {
    case `(*)`(identifier, args*) =>
      prog
        .find {
          case (`(*)`(`identifier`, params*), _) if params.size == args.size => true
          case _ => false
        }
        .get
        ._2
  }

  def apply(_prog: List[Bind]): (List[Bind], (Map[String, Actions], Map[String, Actions], Map[String, Actions])) =

    val excluded = Map[String, Actions]()

    def parse[T <: AST](ast: T): (T, Actions) =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      inline def τ = Calculus.τ(None, -Long.MaxValue)

      def insert[S](end: `&`, ps: Pre*): (S, Actions) =
        val ps2 = ps :+ τ
        `.`(end, ps2*).asInstanceOf[S] -> Actions(ps2*)

      def insert_+(sum: `+`): `+` =
        val (seq, enabled) = insert[`.`](sum)
        `+`(enabled, `|`(seq))

      ast match

        case `∅` => (∅, nil)

        case it: `+` =>
          val sum = it.choices.foldLeft(`+`(nil)) {
            case (sum, par) =>
              val (par2, enabled2) = parse(par)
              assert(enabled2.nonEmpty)
              assert((sum.enabled & enabled2).isEmpty)
              (`+`(sum.enabled ++ enabled2, (sum.choices :+ par2)*))
          }
          (sum, sum.enabled)

        case it: `|` =>
          val (par, enabled) = it.components.foldLeft((`|`(), nil)) {
            case ((par, enabled), seq) =>
              val (seq2, enabled2) = parse(seq)
              assert(enabled2.nonEmpty)
              assert((enabled & enabled2).isEmpty)
              (`|`((par.components :+ seq2)*), enabled ++ enabled2)
          }
          (par, enabled)

        case `.`(end, ps*) =>
          val (it, enabled) = parse(end)

          if Actions(ps*).nonEmpty
          then
            (`.`(it, ps*), Actions(ps*))
          else if enabled.nonEmpty
          then
            (`.`(it, ps*), enabled)
          else
            assert(∅ == it || it.isInstanceOf[`(*)`])
            insert(it, ps*)

        case `?:`(c, _t, _f) =>
          def _t_f(_sum: `+`): `+` =
            val (sum, _) = parse(_sum)

            if sum.enabled.isEmpty
            || sum.enabled.exists(excluded.contains(_))
            then
              insert_+(sum)
            else
              sum

          val (t, f) = _t_f(_t) -> _t_f(_f)

          t.enabled.foreach(excluded(_) = f.enabled)
          f.enabled.foreach(excluded(_) = t.enabled)

          assert(t.enabled.nonEmpty)
          assert(f.enabled.nonEmpty)

          assert((t.enabled & f.enabled).isEmpty)
          (`?:`(c, t, f), t.enabled ++ f.enabled)

        case `!`(Some(μ), sum) =>
          val (it, _) = parse(sum)
          (`!`(Some(μ), it), Actions(μ))

        case `!`(_, sum) =>
          parse(`!`(Some(τ), sum))

        case it: `(*)` => (it, nil)

    val prog = _prog.map(_ -> parse(_)._1)

    ensure(prog)

    val discarded = Map[String, Actions]()

    lazy val split: AST => Actions = {

      case `∅` => nil

      case `+`(enabled, par) =>
        split(par)
        enabled

      case `+`(enabled, ps*) =>
        val ls = ps.map(split)

        ls.zipWithIndex.foreach { (it, i) =>
          assert(it.nonEmpty)
          val ks = (ls.take(i) ++ ls.drop(i+1)).reduce(_ ++ _)
          assert((it & ks).isEmpty)
          it.foreach { k =>
            if !discarded.contains(k)
            then
              discarded(k) = nil
            discarded(k) ++= ks
          }
        }

        enabled

      case `|`(ss*) =>
        ss.map(split).reduce(_ ++ _)

      case `.`(_: `(*)`, ps*) =>
        assert(Actions(ps*).nonEmpty)
        Actions(ps*)

      case `.`(end, ps*) =>
        var enabled = split(end)

        if Actions(ps*).nonEmpty then
          enabled = Actions(ps*)

        enabled

      case `?:`(_, t, f) =>
        split(t)
        split(f)

        t.enabled.foreach { k => assert(!excluded.contains(k)) }
        f.enabled.foreach { k => assert(!excluded.contains(k)) }

        t.enabled.headOption.foreach(excluded(_) = f.enabled)
        f.enabled.headOption.foreach(excluded(_) = t.enabled)

        assert((t.enabled & f.enabled).isEmpty)
        t.enabled ++ f.enabled

      case `!`(Some(μ), sum) =>
        split(sum)
        Actions(μ)

      case _: `!` => ??? // caught by 'parse'

      case _: `(*)` => ??? // always "guarded"

    }

    lazy val graph: AST => Seq[(Act, `Act | Sum`)] = {

      case `∅` => Nil

      case `+`(_, ps*) =>
        ps.map(graph).reduce(_ ++ _)

      case `|`(ss*) =>
        ss.map(graph).reduce(_ ++ _)

      case `.`(end, ps*) =>

        inline given Conversion[(Pre, `Act | Sum`), (Act, `Act | Sum`)] = _.asInstanceOf[Act] -> _

        graph(end) ++
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
            case sum: `+` =>
              Seq(ps(k) -> sum)
            case `?:`(_, t, f) =>
              Seq(ps(k) -> t, ps(k) -> f)
            case `!`(Some(μ), sum) =>
              Seq(ps(k) -> μ, μ -> μ, μ -> sum)
            case _: `!` => ??? // caught by 'parse'
            case it: `(*)` =>
              val sum = `(*) => +`(prog)(it)
              Seq(ps(k) -> sum)
          else Nil
        }

      case `?:`(_, t, f) =>
        graph(t) ++ graph(f)

      case `!`(Some(μ), sum) =>
        graph(sum) ++ Seq(μ -> μ, μ -> sum)

      case  _: `(*)` => Nil

    }

    excluded.clear

    val enabled = Map[String, Actions]()

    prog
      .tapEach {
        case (_, sum) =>
          split(sum)

          graph(sum).foreach { (s, t) =>
            if !enabled.contains(s.uuid)
            then
              enabled(s.uuid) = nil
            t match
              case it: Act =>
                enabled(s.uuid) += it.uuid
              case it: Sum =>
                enabled(s.uuid) ++= it.enabled
          }
      } -> (discarded, excluded, enabled)


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
