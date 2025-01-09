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

package pixc
package parser

import scala.collection.mutable.{
  LinkedHashMap => Map,
  ListBuffer => MutableList,
  LinkedHashSet => Set
}
import scala.io.Source

import generator.Meta.`()(null)`

import Pi._
import Calculus._
import Encoding._
import scala.util.parsing.combinator.pixc.parser.Expansion


abstract class Pi extends Expression:

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
      case _ ~ _ ~ Some(((Left(enums), _), _)) =>
        throw TermParsingException(enums)
      case (ch, name) ~ (par, binding) ~ Some((it, free2)) =>
        π(ch, par, polarity = true, Some(it)) -> (binding, name ++ free2)
      case (ch, name) ~ (par, binding) ~ _ =>
        π(ch, par, polarity = true, None) -> (binding, name)
    }

  def name: Parser[(λ, Names)] = ident("channel") ^^ { it => λ(Symbol(it)) -> Set(Symbol(it)) } |
                                 floatingPointNumber ^^ { it => λ(BigDecimal(it)) -> Names() } |
                                 stringLiteral ^^ { λ(_) -> Names() } |
                                 ( "True" | "False" ) ^^ { it => λ(it == "True") -> Names() } |
                                 expression ^^ {
                                   case ((Right(term), _), free) => λ(Expr(term)) -> free
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

  private[parser] var _werr: Boolean = false
  private[parser] var eqtn: List[Bind] = null
  private[parser] var defn: Map[Int, List[Define]] = null
  private[parser] var self: Set[Int] = null
  private[parser] var xctn: Map[(Symbol, Int), List[(`⟦⟧` Either χ, Names2)]] = null
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

  private[parser] def pos(binding: Boolean = false) = { _cntr(_nest) += 1; Position(_cntr(_nest), binding) }
  private[parser] def pos_(binding: Boolean = false) = { _cntr(_nest) += 1; Position(-_cntr(_nest), binding) }

  protected final def path = (0 until _nest).map(_nth(_))

  private[parser] var _id: helper.υidυ = null

  protected var χ_id: helper.υidυ = null

  protected final def id = _id()

  protected final def copy: (Any, Any) =
    _id.copy -> χ_id.copy

  protected final def paste(it: (Any, Any)) =
    _id.paste(it._1)
    χ_id.paste(it._2)

  protected final def save[T](r: => (ParseResult[T], Any), fail: Boolean): Option[(T, Input)] =
    val nest = _nest
    val cntr = Map.from(_cntr)
    _id.save {
      χ_id.save {
        r match
          case (Success(it, in), _) => Some(it -> in)
          case (failure: NoSuccess, _) if fail =>
            scala.sys.error(failure.msg)
          case _ =>
            _cntr = cntr
            _nest = nest
            None
      }
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


object Pi:

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

        case it @ `⟦⟧`(_, _, sum, _, _, _) =>
          it.copy(sum = sum.shallow)

        case `{}`(id, pointers, true, params*) =>
          `(*)`(id, Nil, (params ++ pointers.map(λ(_)))*)

        case it =>
          it

  final class Main extends Expansion:

    def line: Parser[Either[Bind, Define]] =
      equation ^^ { Left(_) } | definition ^^ { Right(_) }

    private def index(prog: List[Bind]): `(*)` => Int = {
      case `(*)`(identifier, _, args*) =>
        prog
          .indexWhere {
            case (`(*)`(`identifier`, _, params*), _) if params.size == args.size => true
            case _ => false
          }
    }

    private def ensure(using prog: List[Bind]): Unit =
      import helper.Ensure._

      var i = main

      if i < 0 then throw MainParsingException

      given rec: Map[(String, Int), Int] = Map()
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

    def apply(prog: List[Bind]): List[Bind] =
      given List[Bind] = prog.map(_ -> _.shallow)
      ensure
      given_List_Bind

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

        case (lhs: ∥, rhs: ∥) =>
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
            case (_, (π(λ(lch: Symbol), λ(lpar: Symbol), true, _)
                     ,π(λ(rch: Symbol), λ(rpar: Symbol), true, _))) =>
              equal(lch, rch) && equal2(lpar, rpar)
            case (_, (π(λ(lch: Symbol), λ(larg: Symbol), false, _)
                     ,π(λ(rch: Symbol), λ(rarg: Symbol), false, _))) =>
              equal(lch, rch) && equal(larg, rarg)
            case (_, (π(_, λ(_: Expr), false, _), _))
               | (_, (_, π(_, λ(_: Expr), false, _))) => false
            case (_, (π(λ(lch: Symbol), λ(larg), false, _)
                     ,π(λ(rch: Symbol), λ(rarg), false, _))) =>
              equal(lch, rch) && larg == rarg
            case (_, (χ(Right(lexp))
                     ,χ(Right(rexp)))) =>
              equal2(lexp.trans, rexp.trans) && congruent(lexp -> rexp)
            case (_, (χ(Left(ltrans))
                     ,χ(Left(rtrans)))) =>
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

        case (!(Some(π(λ(lch: Symbol), λ(lpar: Symbol), true, _)), lsum)
             ,!(Some(π(λ(rch: Symbol), λ(rpar: Symbol), true, _)), rsum)) =>
          val (ln, rn) = mark
          equal(lch, rch) && equal2(lpar, rpar) && congruent(lsum -> rsum) && backtrack(ln, rn)

        case (!(Some(π(_, λ(_: Expr), false, _)), _), _)
           | (_, !(Some(π(_, λ(_: Expr), false, _)), _)) => false

        case (!(Some(π(λ(lch: Symbol), λ(larg), false, _)), lsum)
             ,!(Some(π(λ(rch: Symbol), λ(rarg), false, _)), rsum)) =>
          equal(lch, rch) && larg == rarg && congruent(lsum -> rsum)

        case (!(_, lsum), !(_, rsum)) => congruent(lsum -> rsum)

        case (`⟦⟧`(Definition(lcode, _, lconstants, lvariables, _), _, lsum, _, lname, lassign)
             ,`⟦⟧`(Definition(rcode, _, rconstants, rvariables, _), _, rsum, _, rname, rassign))
            if lcode == rcode
            && lname == rname
            && lconstants == rconstants
            && lassign.map(_.size).getOrElse(0) == lvariables.size
            && rassign.map(_.size).getOrElse(0) == rvariables.size
            && lvariables.size == rvariables.size =>
          val (ln, rn) = mark
          (lconstants ++ lvariables).foreach(binding._1.prepend(_))
          (rconstants ++ rvariables).foreach(binding._2.prepend(_))
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
            .map(_._1.exp.υidυ -> Set.empty)
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
            xct.exp.υidυ -> exp.υidυ
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
      _id = new helper.υidυ
      χ_id = new helper.υidυ
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
        else // Pi
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
