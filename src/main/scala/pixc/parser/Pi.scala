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

import scala.io.Source

import scala.collection.mutable.{
  LinkedHashMap => Map,
  ListBuffer => MutableList,
  LinkedHashSet => Set
}

import scala.meta.Term

import generator.Meta.`()(null)`

import Pi.*
import Calculus.*
import Encoding.*
import scala.util.parsing.combinator.pixc.parser.Expansion
import Expansion.Duplications


abstract class Pi extends Expression:

  def μ(using bindings: Bindings): Parser[(μ, (Names, Names))] =
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
      case _ ~ (par, _, _) ~ _ if !par.isSymbol =>
        throw PrefixChannelParsingException(par)
      case _ ~ _ ~ Some(((Left(enums), _), _)) =>
        throw TermParsingException(enums)
      case (λ(ch: Symbol), _) ~ (λ(arg: Symbol), _, _) ~ _
          if ch == arg && bindings.get(ch).map(_.position.counter == Long.MinValue).getOrElse(false) =>
        throw ConsItselfParsingException(ch, bindings.get(ch).get.asInstanceOf[ConsOccurrence].cons)
      case (ch, name) ~ (par, cons, bound) ~ code =>
        if cons.nonEmpty
        then
          val arg = par.asSymbol
          bindings.get(arg) match
            case Some(Cons(`cons`)) =>
            case Some(Occurrence(_, Position(counter, binds))) if counter < 0 =>
              val extra = if counter == Long.MinValue then ""
                          else (if binds then " binding" else "") + " parameter"
              throw ConsBindingParsingException(_code, _nest, cons, arg, extra)
            case _ =>
          bindings(arg) = ConsOccurrence(cons)
        val freeʹ = code.map(_._2).getOrElse(Names())
        π(ch, par, polarity = Some(cons), code.map(_._1)) -> (bound, name ++ freeʹ)
    }

  def name: Parser[(λ, Names)] = ident("channel") ^^ (Symbol(_)) ^^ { it => λ(it) -> Set(it) } |
                                 floatingPointNumber ^^ (BigDecimal(_)) ^^ { λ(_) -> Names() } |
                                 stringLiteral ^^ (_.stripPrefix("\"").stripSuffix("\"")) ^^ { λ(_) -> Names() } |
                                 ( "True" | "False" ) ^^ (_ == "True") ^^ { λ(_) -> Names() } |
                                 expression ^^ {
                                   case ((Right(term), _), free) => λ(term) -> free
                                   case ((Left(enums), _), _) => throw TermParsingException(enums)
                                 }

  def nameʹ: Parser[(λ, String, Names)] =
    name ~ opt(`type`) ^^ {
      case (λ, free) ~ tpe =>
        (λ.copy()(using tpe), "", free)
    } |
    cons_r ~ name ~ opt(`type`) ~ cons_r ^^ {
      case l ~ _ ~ _ ~ r if l != r =>
        throw ConsParsingException(l, r)
      case cons ~ (λ, free) ~ tpe ~ _ =>
        (λ.copy()(using tpe), cons, free)
    }

  /**
   * Channel or transaction names start with lower case.
   * @return
   */
  def ident(what: String): Parser[String] =
      "" ~> // handle whitespace
      rep1(acceptIf(Character.isLowerCase)(s"$what name expected but '" + _ + "' found"),
          elem(s"$what name part", { (ch: Char) => Character.isJavaIdentifierPart(ch) || ch == '\'' || ch == '"' })) ^^ (_.mkString)

  private[parser] var eqtn: List[Bind] = null
  private[parser] var defn: Map[Int, List[Define]] = null
  private[parser] var self: Set[Int] = null
  private[parser] var xctn: Map[(Symbol, Int), List[(`⟦⟧` Either χ, Bindings)]] = null
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

  protected var _dirs = List[Map[String, Any]]()

  protected var _dups: Boolean = false

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
        case Some(Occurrence(_, Position(Long.MinValue, true))) =>
        case Some(Occurrence(_, Position(_, false))) if _code >= 0 =>
          throw NonParameterBindingParsingException(_code, _nest, name, hardcoded)
        case Some(Occurrence(_, Position(_, false))) =>
        case _ =>
          bindings += name -> Occurrence(shadow, pos(true))


object Pi:

  private val cons_r = """[^/*{\[(<.,"'\p{Alnum}\p{Space}'",.>)\]}*/]+""".r

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

  case class TermParsingException(enums: List[Enumerator])
      extends PrefixParsingException(s"The embedded Scalameta should be a Term, not Enumerator `$enums'")

  case class ConsParsingException(left: String, right: String)
      extends PrefixParsingException(s"The CONS operator varies between `$left' and `$right'")

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

        case it @ `⟦⟧`(_, _, sum, _, _, _) =>
          it.copy(sum = sum.shallow)

        case `{}`(identifier, pointers, true, params*) =>
          `(*)`(identifier, Nil, (params ++ pointers.map(λ(_)))*)

        case _ => ast

  final class Main(override protected val in: String) extends Expansion:

    def line(using Duplications): Parser[Either[Bind, Option[Define]]] =
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
      import helper.Ensure.*

      var i = main

      if i < 0 then throw MainParsingException

      given rec: Map[(String, Int), Int] = Map()
      given rep: Map[Int, Int] = Map()

      prog(i)._2.recursive(using "Main" -> 0 :: Nil)

      if rec.contains("Main" -> 0) then throw MainParsingExceptionʹ

      prog.foreach {
        case (it @ `(*)`("Main", _), _) =>
          val i = index(prog)(it)
          rec("Main" -> 0) = -(i+1)
        case (it @ `(*)`(identifier, _, params*), _) if !rec.contains(identifier -> params.size) =>
          val i = index(prog)(it)
          val sum = prog(i)._2
          sum.recursive(using identifier -> params.size :: Nil)
          if !rec.contains(identifier -> params.size)
          then
            rec(identifier -> params.size) = -(i+1)
        case _ =>
      }

      for
        (i, n) <- rep
      do
        prog(i)._1 match
          case `(*)`(identifier, _, params*) =>
            warn(throw RecRepParsingException(identifier, params.size, n))

      i = rec("Main" -> 0)
      if !prog(-i-1)._2.replication(using "Main" -> 0 :: Nil)
      then throw StartParsingException("Main", 0, "replication")

      prog.foreach {
        case (`(*)`(identifier, _, params*), _) if rec(identifier -> params.size) > 0 =>
          val i = rec(identifier -> params.size)
          val sum = prog(i-1)._2
          if !sum.recursion(using identifier -> params.size :: Nil)
          then throw StartParsingException(identifier, params.size, "recursion")
        case _ =>
      }

    def apply(prog: List[Bind]): List[Bind] =
      given List[Bind] = prog.map(_ -> _.shallow)
      ensure
      given_List_Bind

    object Χ:

      private def apply(it: Symbol, _1_2: 1 | 2)(using (Bindings, Bindings)): Either[1 | 2, Position] =
        val bindings = if _1_2 == 1 then summon[(Bindings, Bindings)]._1 else summon[(Bindings, Bindings)]._2
        bindings.find { case (_, Shadow(`it`)) => true case _ => false } match
          case Some((_, it)) => Right(it.position)
          case _ => bindings.get(it) match
            case Some(it) => Right(it.position)
            case _ => Left(_1_2)

      private def equal(using bound: (MutableList[Symbol], MutableList[Symbol]))
                       (using (Bindings, Bindings))
                       (lhs: Symbol, rhs: Symbol): Boolean =
       val (i, j) = bound._1.indexOf(lhs) -> bound._2.indexOf(rhs)
       i >= 0 && j >= 0 && i == j ||
       i < 0 && j < 0 &&
       this(lhs, 1) == this(rhs, 2)

      private def equalʹ(using bound: (MutableList[Symbol], MutableList[Symbol]))
                        (using (Bindings, Bindings))
                        (lhs: Symbol, rhs: Symbol): Boolean =
        val _1 = this(lhs, 1)
        val _2 = this(rhs, 2)
        _1 == _2 || _1.isLeft && _2.isLeft && {
          bound._1.prepend(lhs); bound._2.prepend(rhs)
          true
       }

      private inline def mark(using bound: (MutableList[Symbol], MutableList[Symbol])): (Int, Int) =
        bound._1.size -> bound._2.size

      private inline def backtrack(using bound: (MutableList[Symbol], MutableList[Symbol]))
                                  (ln: Int, rn: Int): Boolean =
        bound._1.dropInPlace(bound._1.size - ln); bound._2.dropInPlace(bound._2.size - rn)
        true

      def congruent(using bound: (MutableList[Symbol], MutableList[Symbol]))
                   (using (Bindings, Bindings)): ((AST, AST)) => Boolean = {

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
                .foreach(bound._1.prepend(_))
              rnames
                .map(Symbol(_))
                .foreach(bound._2.prepend(_))
              true
            case (_, (π(λ(lch: Symbol), λ(lpar: Symbol), Some(lcons), _)
                     ,π(λ(rch: Symbol), λ(rpar: Symbol), Some(rcons), _))) =>
              equal(lch, rch) && equalʹ(lpar, rpar) && lcons == rcons
            case (_, (π(λ(lch: Symbol), λ(larg: Symbol), None, _)
                     ,π(λ(rch: Symbol), λ(rarg: Symbol), None, _))) =>
              equal(lch, rch) && equal(larg, rarg)
            case (_, (π(_, λ(_: Term), None, _), _))
               | (_, (_, π(_, λ(_: Term), None, _))) => false
            case (_, (π(λ(lch: Symbol), λ(larg), None, _)
                     ,π(λ(rch: Symbol), λ(rarg), None, _))) =>
              equal(lch, rch) && larg == rarg
            case (_, (χ(Right(lexp))
                     ,χ(Right(rexp)))) =>
              equalʹ(lexp.trans, rexp.trans) && congruent(lexp -> rexp)
            case (_, (χ(Left(ltrans))
                     ,χ(Left(rtrans)))) =>
              ltrans == rtrans
            case _ => false
          }
          then
            congruent(lhs -> rhs) && backtrack(ln, rn)
          else
            false

        case (?:(((λ(_: Term), _), _), _, _), _) | (?:(((_, λ(_: Term)), _), _, _), _) |
             (_, ?:(((λ(_: Term), _), _), _, _)) | (_, ?:(((_, λ(_: Term)), _), _, _)) => false

        case (?:(((λ(llhs: Symbol), λ(lrhs: Symbol)), lm), lt, lf)
             ,?:(((λ(rlhs: Symbol), λ(rrhs: Symbol)), rm), rt, rf))
            if equal(llhs, rlhs) && equal(lrhs, rrhs)
            || equal(llhs, rrhs) && equal(lrhs, rlhs) =>
          if lm == rm
          then
            congruent(lt -> rt) && (lf zip rf).map(congruent(_)).getOrElse(lf.isEmpty == rf.isEmpty)
          else
            congruent(lt -> rf.getOrElse(`+`())) && congruent(lf.getOrElse(`+`()) -> rt)

        case (?:(((λ(llhs), λ(lrhs)), lm), lt, lf)
             ,?:(((λ(rlhs), λ(rrhs)), rm), rt, rf))
            if llhs == rlhs && lrhs == rrhs
            || llhs == rrhs && lrhs == rlhs =>
          if lm == rm
          then
            congruent(lt -> rt) && (lf zip rf).map(congruent(_)).getOrElse(lf.isEmpty == rf.isEmpty)
          else
            congruent(lt -> rf.getOrElse(`+`())) && congruent(lf.getOrElse(`+`()) -> rt)

        case (!(Some(π(λ(lch: Symbol), λ(lpar: Symbol), Some(lcons), _)), lsum)
             ,!(Some(π(λ(rch: Symbol), λ(rpar: Symbol), Some(rcons), _)), rsum)) =>
          val (ln, rn) = mark
          equal(lch, rch) && equalʹ(lpar, rpar) && lcons == rcons && congruent(lsum -> rsum) && backtrack(ln, rn)

        case (!(Some(π(_, λ(_: Term), None, _)), _), _)
           | (_, !(Some(π(_, λ(_: Term), None, _)), _)) => false

        case (!(Some(π(λ(lch: Symbol), λ(larg), None, _)), lsum)
             ,!(Some(π(λ(rch: Symbol), λ(rarg), None, _)), rsum)) =>
          equal(lch, rch) && larg == rarg && congruent(lsum -> rsum)

        case (!(_, lsum), !(_, rsum)) => congruent(lsum -> rsum)

        case (`⟦⟧`(Definition(lcode, _, lconstants, lvariables, _), _, lsum, _, lname, lassign)
             ,`⟦⟧`(Definition(rcode, _, rconstants, rvariables, _), _, rsum, _, rname, rassign))
            if lcode == rcode
            && lname == rname
            && lconstants == rconstants
            && lassign.size == lvariables.size
            && rassign.size == rvariables.size
            && lvariables.size == rvariables.size =>
          val (ln, rn) = mark
          (lconstants ++ lvariables).foreach(bound._1.prepend(_))
          (rconstants ++ rvariables).foreach(bound._2.prepend(_))
          (lassign zip rassign)
            .foldLeft(true) {
              case (false, _) => false
              case (_, ((lvariable, lpointer), (rvariable, rpointer))) =>
                bound._1.prepend(lvariable); bound._2.prepend(rvariable)
                equal(lpointer, rpointer)
            }
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
            case (_, (λ(_: Term), _)) | (_, (_, λ(_: Term))) => false
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
            .map(_._1.exp.xid -> Set.empty)
        }
          .toMap
        ++
        (expansions.keySet & transactions.keySet).flatMap { it =>
          for
            (exp, bindings_exp) <- expansions.filterKeys(_ == it).values.flatten
            (xct, bindings_xct) <- transactions.filterKeys(_ == it).values.flatten
            given (MutableList[Symbol]
                  ,MutableList[Symbol]) = MutableList[Symbol]()
                                       -> MutableList[Symbol]()
            given (Bindings, Bindings) = bindings_exp -> bindings_xct
            if congruent(exp -> xct.exp)
          yield
            xct.exp.xid -> exp.xid
        }
          .groupBy(_._1)
          .mapValues(_.map(_._2))
          .toMap


    private var i: Int = -1
    private var l: (Int, Int) = (-1, -1)
    override def ln: String = if l._1 == l._2 then s"line #${l._2}" else s"lines #${l._1}-#${l._2}"

    def apply(source: Source, errors: Boolean = false): List[Either[String, Bind]] =
      _werr = errors
      _dups = false
      _dirs = List(Map("errors" -> _werr, "duplications" -> _dups))
      eqtn = List()
      defn = Map()
      self = Set()
      xctn = Map()
      _nest = 0
      _id = new helper.υidυ
      _χ_id = new helper.υidυ
      i = 0
      l = (0, 0)

      given Duplications()

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
                eqtn :+= equation
                val equations = eqtn.slice(i, eqtn.size)
                i = eqtn.size
                equations.map(Right(_))
              case Success(Right(Some(definition)), _) =>
                if !defn.contains(_code) then defn(_code) = Nil
                defn(_code) ::= definition
                Nil
              case Success(Right(_), _) => // directive
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
