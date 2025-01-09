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

import scala.collection.mutable.{ HashMap => Map, LinkedHashMap => Map2, LinkedHashSet => Set }

import munit.FunSuite

import Pi._
import Calculus._
import Encoding._
import EncodingParserSuite._


class EncodingParserSuite extends FunSuite:

  test("pointers - with empty braces") {

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(pointers, "{}") match
          case Failure(_, _) =>
          case _ =>
            assert(false)

    `13`.test

  }

  test("pointers - with non-names") {

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(pointers, """{True, "string", m, n}""")

    interceptMessage[PointersParsingException]("""True, "string" are not channel names but True False, string literal""") {
      `13`.test
    }

  }

  test("pointers - with only names") {

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(pointers, "{m, n}") match
          case Success((List(Symbol("m"), Symbol("n")), free), _) =>
            assertEquals(free, Names() + Symbol("m") + Symbol("n"))
          case _ =>
            assert(false)

    `13`.test

  }

  test("leaf - capital - empty braces") {

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(capital, "P{}") match
          case Success((`{}`("P", Nil, false), free), _) =>
            assert(free.isEmpty)
          case _ =>
            assert(false)

    `13`.test

  }

  test("leaf - capital - agent with parameters and with pointers") {

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(capital, "P(True){n}") match
          case Success((`{}`("P", List(Symbol("n")), true, λ(true)), free), _) =>
            assertEquals(free, Names() + Symbol("n"))
          case _ =>
            assert(false)

    `13`.test

  }

  test("leaf - capital - agent without parameters and with pointers") {

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(capital, "P(){n}") match
          case Success((`{}`("P", List(Symbol("n")), true), free), _) =>
            assertEquals(free, Names() + Symbol("n"))
          case _ =>
            assert(false)

    `13`.test

  }

  test("leaf - capital - agent with parameters and with empty braces") {

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(capital, "P(True){}") match
          case Success((`{}`("P", Nil, true, λ(true)), free), _) =>
            assert(free.isEmpty)
          case _ =>
            assert(false)

    `13`.test

  }

  test("leaf - capital - agent without parameters and with empty braces") {

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(capital, "P(){}") match
          case Success((`{}`("P", Nil, true), free), _) =>
            assert(free.isEmpty)
          case _ =>
            assert(false)

    `13`.test

  }

  test("definition - without parameters without constants without pointers") {

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(definition, "⟦⟧ =") match
          case Success((Macro(Nil, 0, cs1, vs1, b2, ∅), Definition(0, _, cs2, vs2, ∅)), _) =>
            assert(cs1.isEmpty)
            assert(vs1.isEmpty)
            assert(b2.isEmpty)
            assertEquals(cs1, cs2)
            assertEquals(vs1, vs2)
          case _ =>
            assert(false)

    `13`.test

  }

  test("definition - with parameters without constants without pointers") {

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(definition, "⟦ 'x `1` 'y ⟧ =") match
          case Success((Macro(List(Symbol("x"), Symbol("y")), 2, cs1, vs1, b2, ∅), Definition(0, _, cs2, vs2, ∅)), _) =>
            assert(cs1.isEmpty)
            assert(vs1.isEmpty)
            assertEquals(b2, Map2(
                           Symbol("x") -> Occurrence(None, Position(-1, false)),
                           Symbol("y") -> Occurrence(None, Position(-2, false))))
            assertEquals(cs1, cs2)
            assertEquals(vs1, vs2)
          case _ =>
            assert(false)

    `13`.test

  }

  test("definition - without parameters with constants without pointers") {

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(definition, "⟦ Nil ⟧(nil, cons) =") match
          case Success((Macro(Nil, 0, cs1, vs1, b2, ∅), Definition(0, _, cs2, vs2, ∅)), _) =>
            assertEquals(cs1, Names() + Symbol("nil") + Symbol("cons"))
            assert(vs1.isEmpty)
            assertEquals(b2, Map2(
                           Symbol("nil") -> Occurrence(None, Position(1, false)),
                           Symbol("cons") -> Occurrence(None, Position(2, false))))
            assertEquals(cs1, cs2)
            assertEquals(vs1, vs2)
          case _ =>
            assert(false)

    `13`.test

  }

  test("definition - without parameters without constants with pointers") {

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(definition, "⟦ Nil ⟧{x} =") match
          case Success((Macro(Nil, 0, cs1, vs1, b2, ∅), Definition(0, _, cs2, vs2, ∅)), _) =>
            assert(cs1.isEmpty)
            assertEquals(vs1, Names() + Symbol("x"))
            assertEquals(b2, Map2(Symbol("x") -> Occurrence(None, Position(1, false))))
            assertEquals(cs1, cs2)
            assertEquals(vs1, vs2)
          case _ =>
            assert(false)

    `13`.test

  }

  test("definition - with parameters with constants without pointers - non-empty intersection") {

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(definition, "⟦ 'x `1` 'y ⟧(x) =")

    interceptMessage[DefinitionParametersException]("The parameters, constants, and variables must all be different in the left hand side of encoding 0") {
      `13`.test
    }

  }

  test("definition - with parameters without constants with pointers - non-empty intersection") {

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(definition, "⟦ 'x `1` 'y ⟧{x} =")

    interceptMessage[DefinitionParametersException]("The parameters, constants, and variables must all be different in the left hand side of encoding 0") {
      `13`.test
    }

  }

  test("definition - without parameters with constants with pointers - non-empty intersection") {

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(definition, "⟦ Nil ⟧(x){x} =")

    interceptMessage[DefinitionParametersException]("The parameters, constants, and variables must all be different in the left hand side of encoding 0") {
      `13`.test
    }

  }

  test("definition - with free capitals") {

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(definition, "⟦ Nil ⟧{x} = P{}")

    interceptMessage[DefinitionFreeNamesException]("The free names (P) in the right hand side are not formal parameters of the left hand side of encoding 0") {
      `13`.test
    }

  }

  test("definition - with free names") {

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(definition, "⟦ Nil ⟧{x} = ch<x>.")

    interceptMessage[DefinitionFreeNamesException]("The free names (ch) in the right hand side are not formal parameters of the left hand side of encoding 0") {
      `13`.test
    }

  }

  test("definition - with parameters - Self check positive") {

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(definition, "⟦ 'x `1` 'y ⟧ =") match
          case Success(_, _) =>
            eqtn.headOption match
              case Some((`(*)`("Self_0", Nil, λ(Symbol("x")), λ(Symbol("y"))), ∅)) =>
              case _ =>
                assert(false)
          case _ =>
            assert(false)

    `13`.test

  }

  test("definition - with parameters - Self check negative") {

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(definition, "⟦ 'x `1` 'P ⟧ =") match
          case Success(_, _) =>
            assert(eqtn.isEmpty)
          case _ =>
            assert(false)

    `13`.test

  }


object EncodingParserSuite:

  import scala.util.matching.Regex

  abstract class EncodingParserTest extends Encoding:
    /**
      * A parser that matches a regex string and returns the Match
      * [[https://stackoverflow.com/questions/1815716/accessing-scala-parser-regular-expression-match-data]]
      */
    def regexMatch(r: Regex): Parser[Regex.Match] = new Parser[Regex.Match] {
      def apply(in: Input) = {
        val source = in.source
        val offset = in.offset
        val start = handleWhiteSpace(source, offset)
        (r findPrefixMatchOf source.subSequence(start, source.length)) match {
          case Some(matched) =>
            Success(matched, in.drop(start + matched.end - offset))
          case None =>
            Failure("string matching regex `"+r+"' expected but `"+in.first+"' found", in.drop(start - offset))
        }
      }
    }
    override def instance(defs: List[Define], end: String)
                         (using Names2): Parser[(`⟦⟧`, Names)] =
      new Parser[(`⟦⟧`, Names)]:
        override def apply(_in: Input): ParseResult[(`⟦⟧`, Names)] =
          Failure(null, _in)

    eqtn = List()
    defn = Map()
    self = Set()
    _nest = 0
    _cntr = Map(0 -> 0L)
    _nth = Map(0 -> 0L)

    def test: Unit
