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

package masc
package parser

import scala.collection.mutable.{
  LinkedHashMap => Map,
  ListBuffer => MutableList,
  LinkedHashSet => Set
}
import scala.io.Source

import scala.util.parsing.combinator._

import Ambient._
import Calculus.{ AST => _, _ }
import scala.util.parsing.combinator.masc.parser.Expansion


abstract class Ambient extends Expression:

  def caps: Parser[(List[AST], Names)] =
    repsep(cap, ",") ^^ { cs =>
      cs.map(_._1).filterNot(_.isInstanceOf[ε.type]) -> cs.map(_._2).foldLeft(Names())(_ ++ _)
    }

  def cap: Parser[(AST, Names)] =
    "ε" ^^ { _ => ε -> Names() } | // null
    ("in"|"out"|"open") ~ name ^^ { // enter | exit | open
      case op ~ (amb, free) =>
        ζ(Op.valueOf(op), amb) -> free
    } |
    name ^^ { Λ(_) -> _ }

  def name: Parser[(String, Names)] = ident ^^ { it => it -> Set(it) }

  /**
   * Ambient names start with lower case.
   * @return
   */
  override def ident: Parser[String] =
      "" ~> // handle whitespace
      rep1(acceptIf(Character.isLowerCase)("ambient name expected but '" + _ + "' found"),
          elem("ambient name part", { (ch: Char) => Character.isJavaIdentifierPart(ch) || ch == '\'' || ch == '"' })) ^^ (_.mkString)

  private[parser] var _werr: Boolean = false
  private[parser] var eqtn: List[Bind] = null
  private[parser] var defn: Map[Int, List[Define]] = null
  private[parser] var self: Set[Int] = null
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


object Ambient extends Expansion:

  enum Op { case in, out, open }

  sealed trait AST extends Any

  case class Λ(name: String) extends AnyVal with AST:
    override def toString: String = name

  case object ε extends AST:
    override def toString: String = "ε"

  case class ζ(op: Op, amb: String) extends AST:
    override def toString: String = s"$op $amb"

  type Names = Set[String]

  object Names:
    def apply(): Names = Set()
    def apply(names: Names): Names = Set.from(names)

  final case class Position(counter: Long, binding: Boolean)

  final case class Occurrence(shadow: String | Option[String], position: Position):
    val isBinding = if !position.binding then 0 else math.signum(position.counter)

  object Rebind:
    def unapply(self: Occurrence): Option[String] =
      self.shadow match
        case it: String => Some(it)
        case _ => None

  object Shadow:
    def unapply(self: Occurrence): Option[String] =
      self.shadow match
        case it @ Some(_) => it
        case _ => None

  type Names2 = Map[String, Occurrence]

  object Names2:
    def apply(): Names2 = Map()
    def apply(binding2: Names2): Names2 = Map.from(binding2)
    def apply(names: Names)
             (using Names2): Unit =
      names.foreach { it => this(it, if _code < 0 then None else Some(it), hardcoded = true) }
    def apply(name: String, shadow: Option[String], hardcoded: Boolean = false)
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

  import Expression.ParsingException

  abstract class BindingParsingException(msg: String, cause: Throwable = null)
      extends ParsingException(msg
                                 + s" at nesting level #$_nest"
                                 + (if _code >= 0 then s" in the right hand side of encoding $_code" else ""), cause)

  case class UniquenessBindingParsingException(name: String, hardcoded: Boolean)
      extends BindingParsingException(s"""A binding name ($name) does not correspond to a unique ${if hardcoded then "hardcoded" else "encoded"} binding occurrence, but is duplicated""")

  case class NonParameterBindingParsingException(name: String, hardcoded: Boolean)
      extends BindingParsingException(s"""A binding name ($name) in ${if hardcoded then "a hardcoded" else "an encoded"} binding occurrence does not correspond to a parameter""")


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
          if _werr
          then
            throw RecRepParsingException(id, params.size, n)
          Console.err.println("Warning! " + RecRepParsingException(id, params.size, n).getMessage + ".")

  def apply(prog: List[Bind]): Unit =

    ensure(prog)


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
      else // Ambient
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
