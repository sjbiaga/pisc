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

import scala.collection.mutable.{
  LinkedHashMap => Map,
  LinkedHashSet => Set
}

import munit.FunSuite

import Pi.*
import Calculus.*
import Encoding.*
import scala.util.parsing.combinator.pisc.parser.Expansion
import Expansion.*
import ExpansionParserSuite.*


class ExpansionParserSuite extends FunSuite:

  test("instantiation - no definition") {

    val `13` = new ExpansionParserTest:
      override def test =
        parseAll(instantiation(using Bindings(), Duplications()), "⟦⟧")

    interceptMessage[NoDefinitionException]("No definition for encoding 0") {
      `13`.test
    }

  }

  test("instantiation - rename - pointers - no binding") {

    val `13` = new ExpansionParserTest:
      override def test =
        _code = -1
        parseAll(instantiation(using Bindings(), Duplications()), "⟦⟧{x}")
      parseAll(definition, "⟦⟧ = ") match
        case Success(Some(it), _) =>
          defn(0) = it :: Nil

    interceptMessage[NoBindingParsingException]("No binding for x at nesting level #1") {
      `13`.test
    }

  }

  test("instantiation - rename - no binding") {

    val `13` = new ExpansionParserTest:
      override def test =
        _code = -1
        parseAll(instantiation(using Bindings(), Duplications()), "⟦ x<y>. 1 ⟧") match
          case Failure("No binding for x at nesting level #1", _) =>
          case _ =>
            assert(false)
      parseAll(definition, "⟦ 'P `1` _ ⟧ = P{}") match
        case Success(Some(it), _) =>
          defn(0) = it :: Nil

    `13`.test

  }

  test("instantiation - choice - empty and unique") {

    val `13` = new ExpansionParserTest:
      override def test =
        parseAll(instantiation(using Bindings(), Duplications()), "⟦⟧") match
          case Success((`⟦⟧`(Definition(0, None, _, _, ∅()), _, ∅(), _, _), _), _) =>
          case _ =>
            assert(false)
      override def instance(defs: List[Define], end: String)
                           (using Bindings, Duplications): Parser[(`⟦⟧`, Names)] =
        new Parser[(`⟦⟧`, Names)]:
          override def apply(_in: Input): ParseResult[(`⟦⟧`, Names)] =
            Failure(null, _in)
      parseAll(definition, "⟦⟧ = ") match
        case Success(Some(it), _) =>
          defn(0) = it :: Nil

    `13`.test

  }

  test("instantiation - instance - empty and not unique") {

    val `13` = new ExpansionParserTest:
      override def test =
        parseAll(instantiation(using Bindings(), Duplications()), "⟦⟧") match
          case Failure("An instantiation of a template is undefined", _) =>
          case _ =>
            assert(false)
      parseAll(definition, "⟦⟧ = ") match
        case Success(Some(it), _) =>
          defn(0) = it :: Nil
      parseAll(definition, "⟦1 1⟧ = ") match
        case Success(Some(it), _) =>
          defn(1) = it :: Nil

    `13`.test

  }

  test("instantiation - instance - undefined") {

    val `13` = new ExpansionParserTest:
      override def test =
        parseAll(instantiation(using Bindings(), Duplications()), "⟦ Cons ⟧") match
          case Failure("An instantiation of a template is undefined", _) =>
          case _ =>
            assert(false)
      parseAll(definition, "⟦ Nil ⟧ = ") match
        case Success(Some(it), _) =>
          defn(0) = it :: Nil

    `13`.test

  }

  test("instantiation - instance - ambiguous") {

    val `13` = new ExpansionParserTest:
      override def test =
        parseAll(instantiation(using Bindings(), Duplications()), "⟦ Nil ⟧") match
          case Failure("An instantiation of a template is ambiguous", _) =>
          case _ =>
            assert(false)
      parseAll(definition, """⟦ t"Nil" ⟧ = """) match
        case Success(Some(it), _) =>
          defn(it._2.code) = it :: Nil
      parseAll(definition, """⟦1 t"Nil" 1⟧ = """) match
        case Success(Some(it), _) =>
          defn(it._2.code) = it :: Nil

    `13`.test

  }


object ExpansionParserSuite:

  import scala.util.matching.Regex

  abstract class ExpansionParserTest extends Expansion:
    override protected def in: String = getClass.getSimpleName
    override def ln: String = "line #0"

    eqtn = List()
    defn = Map()
    self = Set()
    _nest = 0
    _id = new helper.υidυ
    _χ_id = new helper.υidυ
    _cntr = Map(0 -> 0L)
    _nth = Map(0 -> 0L)

    given Duplications()

    def test: Unit
