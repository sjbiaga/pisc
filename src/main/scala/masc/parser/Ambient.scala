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

import scala.collection.Set
import scala.collection.mutable.LinkedHashSet
import scala.io.Source

import scala.util.parsing.combinator._

import Ambient._
import Calculus.Bind

import scala.meta.{ Enumerator, Term }


class Ambient extends Expression:

  def caps: Parser[(List[AST], Names)] =
    repsep(cap, ".") ^^ { cs =>
      cs.map(_._1).filterNot(_.isInstanceOf[ε.type]) -> cs.map(_._2).foldLeft(Names())(_ ++ _)
    }

  def cap: Parser[(AST, Names)] =
    "ε" ^^ { _ => ε -> Names() } | // null
    ("in"|"out"|"open") ~ name ^^ { // enter | exit | open
      case op ~ (amb, free) =>
        ζ(Op.valueOf(op), amb) -> free
    } |
    name ^^ { Λ(_) -> _ }

  def name: Parser[(String, Names)] = ident ^^ { it => it -> LinkedHashSet(it) }

  /**
   * Ambient names start with lower case.
   * @return
   */
  override def ident: Parser[String] =
      "" ~> // handle whitespace
      rep1(acceptIf(Character.isLowerCase)("ambient name expected but '" + _ + "' found"),
          elem("ambient name part", { (ch: Char) => Character.isJavaIdentifierPart(ch) || ch == '\'' || ch == '"' })) ^^ (_.mkString)


object Ambient extends Calculus:

  enum Op { case in, out, open }

  sealed trait AST extends Any

  case class Λ(name: String) extends AnyVal with AST

  case object ε extends AST

  case class ζ(op: Op, amb: String) extends AST

  type Names = Set[String]

  object Names:
    def apply(): Names = LinkedHashSet()


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
      else // Ambient
        parseAll(equation, it) match
          case Success(result, _) => Right(result)
          case failure: NoSuccess => scala.sys.error(failure.msg)
    }
    .toList
