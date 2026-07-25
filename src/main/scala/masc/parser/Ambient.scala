/*
 * Copyright (c) 2023-2026 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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

package masc
package parser

import scala.io.Source

import scala.collection.mutable.{
  LinkedHashMap => Map,
  LinkedHashSet => Set
}

import Ambient.*
import Calculus.{ AST => _, * }
import Encoding.*
import scala.util.parsing.combinator.masc.parser.Expansion
import Expansion.Duplications


abstract class Ambient extends Expression:

  def caps: Parser[(List[AST], Names)] =
    rep1sep(cap, ".") ^^ { _.unzip match
      case (cs, ns) =>
        cs.filterNot(_.isInstanceOf[ε.type]) -> ns.reduceOption(_ ++ _).getOrElse(Names())
    }

  def cap: Parser[(AST, Names)] =
    "ε" ^^ { _ => ε -> Names() } | // null
    ("in"|"out"|"open") ~ name ^^ { // enter | exit | open
      case op ~ (amb, free) =>
        ζ(Op.valueOf(op), amb) -> free
    } |
    not("ν" | "τ") ~> name <~ not("[") ^^ { Λ(_) -> _ }

  def name: Parser[(String, Names)] = ident ^^ { it => it -> Set(it) }

  def names: Parser[List[(String, Names)]] =
    rep1sep(name, ",")

  def scale: Parser[Int] =
    opt( wholeNumber <~ "*" ) ^^ { _.fold(-1)(_.toInt.abs) }

  def pace: Parser[(Long, String)] =
    wholeNumber ~ opt( ","~> ident ) ^^ {
      case amount ~ unit =>
        amount.toLong.abs -> unit.getOrElse(_paceunit)
    }

  /**
   * Ambient names start with lower case.
   * @return
   */
  override def ident: Parser[String] =
      "" ~> // handle whitespace
      rep1(acceptIf(Character.isLowerCase)("ambient name expected but '" + _ + "' found"),
          elem("ambient name part", { (ch: Char) => Character.isJavaIdentifierPart(ch) || ch == '\'' || ch == '"' })) ^^ (_.mkString)

  protected val emitter: Emitter

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

  private[parser] def pos(bound: Boolean = false) = { _cntr(_nest) += 1; Position(_cntr(_nest), bound) }
  private[parser] def pos_(bound: Boolean = false) = { _cntr(_nest) += 1; Position(-_cntr(_nest), bound) }

  protected final def path = (0 until _nest).map(_nth(_))

  protected var _dirs = List[Map[String, Any]]()

  protected var _dups: Boolean = false

  protected var _exclude: Boolean = false

  protected var _paceunit: String = null

  protected var _scaling: Boolean = false

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
        var ok = false
        try
          r match
            case Success(it, in) =>
              ok = true
              Some(it -> in)
            case _ =>
              None
        finally
          if !ok
          then
            _cntr = cntr
            _nest = nest
      }
    }

  protected object BindingOccurrence:
    def apply(names: Names)
             (using Bindings, Int): Unit =
      if _code < 0
      then
        names.foreach(this(_, None, true))
      else
        (names zip names.map(Some(_))).foreach(this(_, _, true))
    def apply(name: String, shadow: Option[String], hardcoded: Boolean = false)
             (using bindings: Bindings, scaling: Int): Unit =
      bindings.get(name) match
        case Some(Occurrence(_, _, true)) =>
          throw UniquenessBindingParsingException(_code, _nest, name, hardcoded, "clobbered")
        case Some(Occurrence(Some(_), Position(counter, true, _), _)) if counter < 0 =>
          throw UniquenessBindingParsingException(_code, _nest, name, hardcoded, "duplicated")
        case Some(Occurrence(_, it @ Position(counter, false, _), _)) if counter < 0 =>
          if scaling != 1 then throw UniquenessBindingParsingException(_code, _nest, name, hardcoded, "scaled")
          bindings += name -> Occurrence(shadow, it.copy(binds = true, path = path))
        case _ =>
          bindings += name -> Occurrence(shadow, pos(true))

  protected object PendingOccurrence:
    def apply(names: Names)
             (using Bindings): Unit =
      names.foreach(this(_))
    def apply(name: String)
             (using bindings: Bindings): Unit =
      bindings.get(name) match
        case Some(Occurrence(Some(_), Position(counter, true, it), false)) if counter < 0 && !path.startsWith(it) =>
          throw ScopeBindingParsingException(_code, _nest, name)
        case Some(it @ Occurrence(None, Position(counter, false, _), false)) if counter < 0 =>
          bindings += name -> it.copy(pending = true)
        case _ =>


object Ambient:

  enum Op { case in, out, open }

  export AST.*

  enum AST:

    case Λ(name: String)

    case ε

    case ζ(op: Op, amb: String)

    override def toString: String = this match
      case Λ(name) => name
      case ζ(op, amb) => s"$op $amb"
      case _ => "ε"

  enum Emitter(val canScale: Boolean = false):
    case ce extends Emitter()
    private[parser] case test extends Emitter()

  type Names = Set[String]

  object Names:
    def apply(): Names = Set()
    def apply(names: Names): Names = Set.from(names)


  // functions

  extension [T <: Calculus.AST](ast: T)

    def shallow: T =

      ast.map(_.shallow) {

        case `{}`(identifier, pointers, true, params*) =>
          `(*)`(identifier, Nil, (params ++ pointers)*)

        case it => it

      }


  class Main(override protected val emitter: Emitter,
             override protected val in: String) extends Expansion:

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
          case `(*)`(identifier, _, params*) =>
            warn(throw RecRepParsingException(identifier, params.size, n))

    def apply(prog: List[Bind]): List[Bind] =
      given List[Bind] = prog.map(_ -> _.shallow)
      ensure
      given_List_Bind

    private var i: Int = -1
    private var l: (Int, Int) = (-1, -1)
    override def ln: String = if l._1 == l._2 then s"line #${l._2}" else s"lines #${l._1}-#${l._2}"

    protected def _init: Unit =
      _dups = false
      _exclude = false
      _paceunit = "second"
      _scaling = false
      _dirs = List(Map("errors"       -> _werr,
                       "duplications" -> _dups,
                       "exclude"      -> _exclude,
                       "paceunit"     -> _paceunit,
                       "scaling"      -> _scaling))
      eqtn = List()
      defn = Map()
      self = Set()
      _nest = 0
      _id = new helper.υidυ
      _χ_id = new helper.υidυ
      i = 0
      l = (0, 0)

    def apply(source: Source, errors: Boolean = false): List[Either[String, Bind]] =
      _werr = errors
      _init

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
            else // Ambient
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
                  if !defn.contains(_code) then defn(_code) = Nil
                  defn(_code) ::= definition
                  Nil
                case Success(Right(_), _) => // directive | excluded
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
