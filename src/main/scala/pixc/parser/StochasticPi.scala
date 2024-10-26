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

package pixc
package parser

import java.util.UUID

import scala.collection.mutable.{
  LinkedHashMap => Map,
  ListBuffer => MutableList,
  LinkedHashSet => Set
}
import scala.io.Source

import scala.util.parsing.combinator._

import generator.Meta.`()(null)`

import StochasticPi._
import Calculus._
import scala.util.parsing.combinator.pixc.parser.Expansion


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

  def name: Parser[(λ, Names)] = ident("channel") ^^ { it => λ(Symbol(it)) -> Set(Symbol(it)) } |
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
   * Channel or transaction names start with lower case.
   * @return
   */
  def ident(what: String): Parser[String] =
      "" ~> // handle whitespace
      rep1(acceptIf(Character.isLowerCase)(s"$what name expected but '" + _ + "' found"),
          elem(s"$what name part", { (ch: Char) => Character.isJavaIdentifierPart(ch) || ch == '\'' || ch == '"' })) ^^ (_.mkString)

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
  private[parser] var xctn: Map[(Symbol, Int), List[(`⟦⟧` Either χ, Names2)]] = null
  private[parser] var _nest = -1
  protected final def nest(b: Boolean) = { _nest += (if b then 1 else -1); if b then _cntr(_nest) = 0L }
  private[parser] var _cntr: Map[Int, Long] = null
  protected final def pos(binding: Boolean = false) = { _cntr(_nest) += 1; Position(_cntr(_nest), binding) }
  protected final def pos_(binding: Boolean = false) = { _cntr(_nest) += 1; Position(-_cntr(_nest), binding) }

  protected final def save[T](r: => ParseResult[T], fail: Boolean): Option[(T, Input)] =
    val nest = _nest
    val cntr = Map.from(_cntr)
    val id = scala.collection.mutable.Seq.from(_id)
    val ix = _ix
    r match
      case Success(it, in) => Some(it -> in)
      case failure: NoSuccess if fail =>
        scala.sys.error(failure.msg)
      case _ =>
       _ix = ix
       _id = id
       _cntr = cntr
       _nest = nest
       None


object StochasticPi extends Expansion:

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
    def apply(names: Names): Names = Set.from(names)

  final case class Position(counter: Long, binding: Boolean)

  final case class Occurrence(shadow: Symbol | Option[Symbol], position: Position):
    val isBinding = if !position.binding then 0 else math.signum(position.counter)

  object Rebind:
    def unapply(self: Occurrence): Option[Symbol] =
      self.shadow match
        case it: Symbol => Some(it)
        case _ => None

  object Shadow:
    def unapply(self: Occurrence): Option[Symbol] =
      self.shadow match
        case it @ Some(_) => it
        case _ => None

  type Names2 = Map[Symbol, Occurrence]

  object Names2:
    def apply(): Names2 = Map()
    def apply(binding2: Names2): Names2 = Map.from(binding2)
    def apply(names: Names)
             (using Names2): Unit =
      names.foreach { it => this(it, if _code < 0 then None else Some(it), hardcoded = true) }
    def apply(name: Symbol, shadow: Option[Symbol], hardcoded: Boolean = false)
             (using binding2: Names2): Unit =
      binding2.get(name) match
        case Some(Occurrence(_, it @ Position(k, false))) if k < 0 =>
          binding2 += name -> Occurrence(shadow, it.copy(binding = true))
        case Some(Occurrence(_, Position(k, true))) if _code >= 0 && (!hardcoded || k < 0) =>
           throw UniquenessBindingParsingException(name, hardcoded)
        case Some(Occurrence(_, Position(_, false))) if _code >= 0 =>
           throw NonParameterBindingParsingException(name, hardcoded)
        case Some(Occurrence(_, Position(_, false))) =>
        case _ =>
          binding2 += name -> Occurrence(shadow, pos(true))


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

  abstract class BindingParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg
                                 + s" at nesting level #$_nest"
                                 + (if _code >= 0 then s" in the right hand side of encoding $_code" else ""), cause)

  case class UniquenessBindingParsingException(name: Symbol, hardcoded: Boolean)
      extends BindingParsingException(s"""A binding name (${name.name}) does not correspond to a unique ${if hardcoded then "hardcoded" else "encoded"} binding occurrence, but is duplicated""")

  case class NonParameterBindingParsingException(name: Symbol, hardcoded: Boolean)
      extends BindingParsingException(s"""A binding name (${name.name}) in ${if hardcoded then "a hardcoded" else "an encoded"} binding occurrence does not correspond to a parameter""")


  // functions

  def index(prog: List[Bind]): `(*)` => Int = {
    case `(*)`(identifier, _, args*) =>
      prog
        .indexWhere {
          case (`(*)`(`identifier`, _, params*), _) if params.size == args.size => true
          case _ => false
        }
  }

  def ensure(using rec: Map[(String, Int), Int])
            (implicit prog: List[Bind]): Unit =
    import Ensure._

    var i = main

    if i < 0 then throw MainParsingException

    given rep: Map[Int, Int] = Map()

    prog(i)._2.recursive(using "Main" -> 0 :: Nil)

    if rec.contains("Main" -> 0) then throw MainParsingException2

    prog.foreach {
      case (it @ `(*)`("Main", _), _) =>
        val i = index(prog)(it)
        rec("Main" -> 0) = -(i+1)
      case (it @ `(*)`(id, _, params*), _) if !rec.contains(id -> params.size) =>
        val i = index(prog)(it)
        val sum = prog(i)._2
        sum.recursive(using id -> params.size :: Nil)
        if !rec.contains(id -> params.size)
        then
          rec(id -> params.size) = -(i+1)
      case _ =>
    }

    for
      (i, n) <- rep
    do
      prog(i)._1 match
        case `(*)`(id, _, params*) =>
          if _werr
          then
            throw RecRepParsingException(id, params.size, n)
          Console.err.println("Warning! " + RecRepParsingException(id, params.size, n).getMessage + ".")

    i = rec("Main" -> 0)
    if !prog(-i-1)._2.replication(using "Main" -> 0 :: Nil)
    then throw StartParsingException("Main", 0, "replication")

    prog.foreach {
      case (`(*)`(id, _, params*), _) if rec(id -> params.size) > 0 =>
        val i = rec(id -> params.size)
        val sum = prog(i-1)._2
        if !sum.recursion(using id -> params.size :: Nil)
        then throw StartParsingException(id, params.size, "recursion")
      case _ =>
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

        case `.`(end, _ps*) =>
          val ps = _ps.map {
            case xa @ χ(Right(exp), _) =>
              xa.copy(dir = Right(exp.parse._1))
            case it => it
          }

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

        case `⟦⟧`(encoding @ Encoding(_, _, _, _, bound), _sum, uuid, name, assign) =>
          val n = assign.map(_.size).getOrElse(0)

          val sum = ( if bound.size == n
                      then
                        _sum
                      else
                        `+`(nil, ||(`.`(_sum, ν(bound.drop(n).map(_.name).toSeq*))))
                    )

           var (it, _) = sum.parse

          if it.enabled.isEmpty
          then
            it = insert_+(it)

          (`⟦⟧`(encoding, it, uuid, name, assign), it.enabled)

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

          ps.foreach {
            case χ(Right(exp), _) =>
              exp.split
            case _ =>
          }

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

        case `⟦⟧`(_, sum, _, _, _) =>
          sum.split
          sum.enabled

        case _: `{}` => ???

        case _: `(*)` => ??? // always "guarded"

    def graph(using rec: Map[(String, Int), Int])
             (implicit prog: List[Bind]): Seq[(Act, Act | Sum)] =

      ast match

        case ∅ => Nil

        case +(_, ps*) =>
          ps.map(_.graph).reduce(_ ++ _)

        case ||(ss*) =>
          ss.map(_.graph).reduce(_ ++ _)

        case `.`(end, ps*) =>

          inline given Conversion[(Pre, Act | Sum), (Act, Act | Sum)] = _.asInstanceOf[Act] -> _

          end.graph ++
          ps.flatMap {
            case χ(Right(exp), _) => exp.graph
            case _ => Nil
          } ++
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
              case `⟦⟧`(_, sum, _, _, _) =>
                Seq(ps(k) -> sum)
              case _: `{}` => ???
              case `(*)`(id, _, params*) =>
                val i = rec(id -> params.size)
                val sum = prog(i.abs-1)._2
                Seq(ps(k) -> sum)
            else Nil
          }

        case ?:(_, t, f) =>
          t.graph ++ f.map(_.graph).getOrElse(Nil)

        case !(Some(μ), sum) =>
          sum.graph ++ Seq(μ -> μ, μ -> sum)

        case _: ! => ??? // impossible by 'parse'

        case `⟦⟧`(_, sum, _, _, _) =>
          sum.graph

        case _: `{}` => ???

        case _: `(*)` => Nil

  def apply(_prog: List[Bind]): (List[Bind], (Map[String, Actions], Map[String, Actions], Map[String, Actions])) =

    given excluded: Map[String, Actions] = Map()

    implicit val prog: List[Bind] = _prog.map(_ -> _.parse._1)

    given rec: Map[(String, Int), Int] = Map()

    ensure

    val discarded = Map[String, Actions]()

    excluded.clear

    val enabled = Map[String, Actions]()

    prog
      .tapEach {
        case (_, sum) =>
          sum.split(using discarded -> excluded)

          sum.graph.foreach { (s, t) =>
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


  object Χ:

    private def apply(it: Symbol, _1_2: 1 | 2)(using (Names2, Names2)): Either[1 | 2, Position] =
      val binding2 = if _1_2 == 1 then summon[(Names2, Names2)]._1 else summon[(Names2, Names2)]._2
      binding2.find { case (_, Shadow(`it`)) => true case _ => false } match
        case Some((_, it)) => Right(it.position)
        case _ => binding2.get(it) match
          case Some(it) => Right(it.position)
          case _ => Left(_1_2)

    private def equal(using binding: (MutableList[Symbol], MutableList[Symbol]))
                     (using (Names2, Names2))
                     (lhs: Symbol, rhs: Symbol): Boolean =
     val (i, j) = binding._1.indexOf(lhs) -> binding._2.indexOf(rhs)
     i >= 0 && j >= 0 && i == j ||
     i < 0 && j < 0 &&
     this(lhs, 1) == this(rhs, 2)

    private def equal2(using binding: (MutableList[Symbol], MutableList[Symbol]))
                      (using (Names2, Names2))
                      (lhs: Symbol, rhs: Symbol): Boolean =
      val _1 = this(lhs, 1)
      val _2 = this(rhs, 2)
      _1 == _2 || _1.isLeft && _2.isLeft && {
        binding._1.prepend(lhs); binding._2.prepend(rhs)
        true
     }

    private inline def mark(using binding: (MutableList[Symbol], MutableList[Symbol])): (Int, Int) =
      binding._1.size -> binding._2.size

    private inline def backtrack(using binding: (MutableList[Symbol], MutableList[Symbol]))
                                (ln: Int, rn: Int): Boolean =
      binding._1.dropInPlace(binding._1.size - ln); binding._2.dropInPlace(binding._2.size - rn)
      true

    def congruent(using binding: (MutableList[Symbol], MutableList[Symbol]))
                 (using (Names2, Names2)): ((AST, AST)) => Boolean = {

      case (lhs: +, rhs: +) =>
        (lhs.choices zip rhs.choices).foldLeft(lhs.choices.size == rhs.choices.size) {
          case (false, _) => false
          case (_, it) => congruent(it)
        }

      case (lhs: ||, rhs: ||) =>
        (lhs.components zip rhs.components).foldLeft(lhs.components.size == rhs.components.size) {
          case (false, _) => false
          case (_, it) => congruent(it)
        }

      case (`.`(lhs, _lit*), `.`(rhs, _rit*)) =>
        val lit = _lit.filterNot { case _: τ => true case _ => false }
        val rit = _rit.filterNot { case _: τ => true case _ => false }
        val (ln, rn) = mark
        if lit.size == rit.size
        && (lit zip rit).foldLeft(true) {
          case (false, _) => false
          case (_, (ν(lnames*)
                   ,ν(rnames*))) =>
            lnames
              .map(Symbol(_))
              .foreach(binding._1.prepend(_))
            rnames
              .map(Symbol(_))
              .foreach(binding._2.prepend(_))
            true
          case (_, (π(λ(lch: Symbol), λ(lpar: Symbol), true, _, _)
                   ,π(λ(rch: Symbol), λ(rpar: Symbol), true, _, _))) =>
            equal(lch, rch) && equal2(lpar, rpar)
          case (_, (π(λ(lch: Symbol), λ(larg: Symbol), false, _, _)
                   ,π(λ(rch: Symbol), λ(rarg: Symbol), false, _, _))) =>
            equal(lch, rch) && equal(larg, rarg)
          case (_, (π(_, λ(_: Expr), false, _, _), _))
             | (_, (_, π(_, λ(_: Expr), false, _, _))) => false
          case (_, (π(λ(lch: Symbol), λ(larg), false, _, _)
                   ,π(λ(rch: Symbol), λ(rarg), false, _, _))) =>
            equal(lch, rch) && larg == rarg
          case (_, (χ(Right(lexp), _)
                   ,χ(Right(rexp), _))) =>
            equal2(lexp.trans, rexp.trans) && congruent(lexp -> rexp)
          case (_, (χ(Left(ltrans), _)
                   ,χ(Left(rtrans), _))) =>
            ltrans == rtrans
          case _ => false
        }
        then
          congruent(lhs -> rhs) && backtrack(ln, rn)
        else
          false

      case (?:(((λ(_: Expr), _), _), _, _), _) | (?:(((_, λ(_: Expr)), _), _, _), _) |
           (_, ?:(((λ(_: Expr), _), _), _, _)) | (_, ?:(((_, λ(_: Expr)), _), _, _)) => false

      case (?:(((λ(llhs: Symbol), λ(lrhs: Symbol)), lm), lt, lf)
           ,?:(((λ(rlhs: Symbol), λ(rrhs: Symbol)), rm), rt, rf))
          if equal(llhs, rlhs) && equal(lrhs, rrhs)
          || equal(llhs, rrhs) && equal(lrhs, rlhs) =>
        if lm == rm
        then
          congruent(lt -> rt) && (lf zip rf).map(congruent(_)).getOrElse(lf.isEmpty == rf.isEmpty)
        else
          congruent(lt -> rf.getOrElse(∅)) && congruent(lf.getOrElse(∅) -> rt)

      case (?:(((λ(llhs), λ(lrhs)), lm), lt, lf)
           ,?:(((λ(rlhs), λ(rrhs)), rm), rt, rf))
          if llhs == rlhs && lrhs == rrhs
          || llhs == rrhs && lrhs == rlhs =>
        if lm == rm
        then
          congruent(lt -> rt) && (lf zip rf).map(congruent(_)).getOrElse(lf.isEmpty == rf.isEmpty)
        else
          congruent(lt -> rf.getOrElse(∅)) && congruent(lf.getOrElse(∅) -> rt)

      case (!(Some(π(λ(lch: Symbol), λ(lpar: Symbol), true, _, _)), lsum)
           ,!(Some(π(λ(rch: Symbol), λ(rpar: Symbol), true, _, _)), rsum)) =>
        val (ln, rn) = mark
        equal(lch, rch) && equal2(lpar, rpar) && congruent(lsum -> rsum) && backtrack(ln, rn)

      case (!(Some(π(_, λ(_: Expr), false, _, _)), _), _)
         | (_, !(Some(π(_, λ(_: Expr), false, _, _)), _)) => false

      case (!(Some(π(λ(lch: Symbol), λ(larg), false, _, _)), lsum)
           ,!(Some(π(λ(rch: Symbol), λ(rarg), false, _, _)), rsum)) =>
        equal(lch, rch) && larg == rarg && congruent(lsum -> rsum)

      case (!(_, lsum), !(_, rsum)) => congruent(lsum -> rsum)

      case (`⟦⟧`(Encoding(lcode, _, _, lconst, lbound), lsum, _, lname, lassign)
           ,`⟦⟧`(Encoding(rcode, _, _, rconst, rbound), rsum, _, rname, rassign))
          if lcode == rcode
          && lname == rname
          && lconst == rconst
          && lassign.map(_.size).getOrElse(0) == lbound.size
          && rassign.map(_.size).getOrElse(0) == rbound.size
          && lbound.size == rbound.size =>
        val (ln, rn) = mark
        (lconst ++ lbound).foreach(binding._1.prepend(_))
        (rconst ++ rbound).foreach(binding._2.prepend(_))
        (lassign zip rassign)
          .map(_ zip _)
          .map(
            _.foldLeft(true) {
              case (false, _) => false
              case (_, ((lvariable, lpointer), (rvariable, rpointer))) =>
                binding._1.prepend(lvariable); binding._2.prepend(rvariable)
                equal(lpointer, rpointer)
            }
          ).getOrElse(true)
        && congruent(lsum -> rsum) && backtrack(ln, rn)

      case (_: `{}`, _) | (_, _: `{}`) => ???

      case (`(*)`(lid, lqual, largs*)
           ,`(*)`(rid, rqual, rargs*))
          if lid == rid
          && lqual == rqual
          && largs.size == rargs.size =>
        (largs zip rargs).foldLeft(true) {
          case (false, _) => false
          case (_, (λ(larg: Symbol), λ(rarg: Symbol))) => equal(larg, rarg)
          case (_, (λ(_: Expr), _)) | (_, (_, λ(_: Expr))) => false
          case (_, (λ(larg), λ(rarg))) => larg == rarg
        }

      case _ => false

    }

    def apply(): scala.collection.Map[String, scala.collection.Set[String]] =

      val expansions = xctn.mapValues(_.filter(_._1.isLeft).map(_.left.get -> _)).filter(_._2.nonEmpty)
      val transactions = xctn.mapValues(_.filter(_._1.isRight).map(_.right.get -> _)).filter(_._2.nonEmpty)

      transactions.keySet.flatMap { it =>
        transactions
          .filterKeys(_ == it)
          .values
          .flatten
          .map(_._1.exp.uuid -> Set.empty)
      }
        .toMap
      ++
      (expansions.keySet & transactions.keySet).flatMap { it =>
        for
          (exp, binding2_exp) <- expansions.filterKeys(_ == it).values.flatten
          (xct, binding2_xct) <- transactions.filterKeys(_ == it).values.flatten
          given (MutableList[Symbol]
                ,MutableList[Symbol]) = MutableList[Symbol]()
                                     -> MutableList[Symbol]()
          given (Names2, Names2) = binding2_exp -> binding2_xct
          if congruent(exp -> xct.exp)
        yield
          xct.exp.uuid -> exp.uuid
      }
        .groupBy(_._1)
        .mapValues(_.map(_._2))
        .toMap


  private var i: Int = -1
  private var l: (Int, Int) = (-1, -1)
  def ln = l

  def apply(source: Source, errors: Boolean = false): List[Either[String, Bind]] =
    _werr = errors
    eqtn = List()
    defn = Map()
    self = Set()
    xctn = Map()
    _nest = 0
    _cntr = Map(0 -> 0L)
    _id = scala.collection.mutable.Seq('0')
    _ix = 0
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
      case Right((`(*)`(s"Self_$n", _, _*), _))
          if { try { n.toInt; true } catch _ => false } =>
        self.contains(n.toInt)
      case _ => true
    }
    .toList
