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

package pisc
package parser

import scala.collection.mutable.{
  LinkedHashMap => Map,
  ListBuffer => MutableList
}

import munit.FunSuite

import Expression.Code
import Pi.*
import Calculus.*
import Encoding.*
import scala.util.parsing.combinator.pisc.parser.Expansion.{ replace, Duplications }
import EncodingFunctionsSuite.given


class EncodingFunctionsSuite extends FunSuite:

  test("capitals - P{} + ( Q{} | if True = False then !R{} else ⟦ S{} ^ T(){} ^ U ⟧ ) -> Set(P,Q,R,S)") {

    val `13` = `+`(-1, ∥(-1, `.`('P')),
                       ∥(-1, `.`('Q'),
                             `.`(?:(((λ(true), λ(false)), false),
                                    `+`(-1, ∥(-1, `.`(`!`(-1, None, None, `+`(-1, ∥(-1, `.`('R'))))))),
                                    Some(`+`(-1, ∥(-1, `.`(`⟦⟧`(null, `+`(-1, ∥(-1, `.`('S'),
                                                                                    `.`(`{}`("T", Nil, true)),
                                                                                    `.`(`(*)`("U", Nil)))),
                                                                null)))))))))

    assertEquals(`13`.capitals, Names() + Symbol("P") + Symbol("Q") + Symbol("R") + Symbol("S"))

  }

  test("renamed - NoBPEx") {

    interceptMessage[Throwable]("x") {
      renamed(Symbol("x"))(using MutableList())(using Bindings())
    }

  }

  test("renamed - from refresh") {

    given MutableList[(Symbol, Symbol)] = MutableList(Symbol("x") -> Symbol("x_υ1υ"))
    given Bindings()
    assertEquals(renamed(Symbol("x")), Symbol("x_υ1υ"))

  }

  test("renamed - from bindings - with binder") {

    given MutableList[(Symbol, Symbol)]()
    given Bindings = Bindings() + (Symbol("x") -> Occurrence(Symbol("x_υ1υ"), Position(-1, true)))
    assertEquals(renamed(Symbol("x")), Symbol("x_υ1υ"))

  }

  test("renamed - from bindings - with shadow") {

    given MutableList[(Symbol, Symbol)]()
    given Bindings = Bindings() + (Symbol("x") -> Occurrence(Some(Symbol("x_υ1υ")), Position(-1, true)))
    assertEquals(renamed(Symbol("x")), Symbol("x_υ1υ"))

  }

  test("renamed - from bindings - by itself") {

    given MutableList[(Symbol, Symbol)]()
    given Bindings = Bindings() + (Symbol("x") -> Occurrence(None, Position(1, true)))
    assertEquals(renamed(Symbol("x")), Symbol("x"))

  }

  test("renamed - from bindings - itself a shadow") {

    given MutableList[(Symbol, Symbol)]()
    given Bindings = Bindings() + (Symbol("x_υ1υ") -> Occurrence(Some(Symbol("x_υ2υ")), Position(-1, true)))
    assertEquals(renamed(Symbol("x_υ2υ")), Symbol("x_υ2υ"))

  }

  test("recoded - no code") {

    given Option[Code] = None
    given MutableList[(Symbol, Symbol)]()
    given Bindings()
    given Names()
    assertEquals(recoded, None)

  }

  test("recoded - via refresh") {

    import scala.meta.*
    import dialects.Scala3

    val term = "println('x)".parse[Term].get

    given Option[Code] = Some(Right(null) -> term)
    given MutableList[(Symbol, Symbol)] = MutableList(Symbol("x") -> Symbol("x_υ1υ"))
    given Bindings()

    recoded match
      case Some(Right(Term.Apply(Term.Name("println"), Term.Name("x_υ1υ") :: Nil))
                ->
                Term.Apply(Term.Name("println"), Term.QuotedMacroExpr(Term.Name("x_υ1υ")) :: Nil)) =>
      case _ =>
        assert(false)

  }

  test("recoded - via refresh or bindings") {

    import scala.meta.{ Position => _, * }
    import dialects.Scala3

    val term = "println('x + 'y)".parse[Term].get

    given Option[Code] = Some(Right(null) -> term)
    given MutableList[(Symbol, Symbol)] = MutableList(Symbol("x") -> Symbol("x_υ1υ"))
    given Bindings = Bindings() + (Symbol("y") -> Occurrence(Some(Symbol("y_υ2υ")), Position(-1, true)))

    recoded match
      case Some(Right(Term.Apply(Term.Name("println"), Term.ApplyInfix(Term.Name("x_υ1υ"), Term.Name("+"), _, Term.Name("y_υ2υ") :: Nil) :: Nil))
                ->
                Term.Apply(Term.Name("println"), Term.ApplyInfix(Term.QuotedMacroExpr(Term.Name("x_υ1υ")), Term.Name("+"), _, Term.QuotedMacroExpr(Term.Name("y_υ2υ")) :: Nil) :: Nil)) =>
      case _ =>
        assert(false)

  }

  test("purged - yes - shadow with a shadow") {

    given bindings: Bindings = Bindings()
                             + (Symbol("x_υ1υ") -> Occurrence(Some(Symbol("x_υ2υ")), Position(-1, true)))
                             + (Symbol("x_υ2υ") -> Occurrence(Symbol("x_υ3υ"), Position(-1, true)))

    purged

    assert(bindings.size == 1)

    bindings.head match
      case (Symbol("x_υ1υ"), Binder(Symbol("x_υ3υ"))) =>
      case _ =>
        assert(false)

  }

  test("purged - yes - shadow with a binder") {

    given bindings: Bindings = Bindings()
                             + (Symbol("x_υ1υ") -> Occurrence(Some(Symbol("x_υ2υ")), Position(-1, true)))
                             + (Symbol("x_υ2υ") -> Occurrence(Symbol("x_υ3υ"), Position(-1, true)))

    purged

    assert(bindings.size == 1)

    bindings.head match
      case (Symbol("x_υ1υ"), Binder(Symbol("x_υ3υ"))) =>
      case _ =>
        assert(false)

  }

  test("purged - not - shadow with neither shadow nor binder") {

    given bindings: Bindings = Bindings()
                             + (Symbol("x_υ1υ") -> Occurrence(Some(Symbol("x_υ2υ")), Position(-1, true)))

    purged

    assert(bindings.size == 1)

    bindings.head match
      case (Symbol("x_υ1υ"), Shadow(Symbol("x_υ2υ"))) =>
      case _ =>
        assert(false)

  }

  test("cleaned") {

    given Bindings = Bindings()
                   + (Symbol("x_υ1υ") -> Occurrence(Some(Symbol("x_υ2υ")), Position(-1, true)))
                   + (Symbol("y_υ3υ") -> Occurrence(Symbol("y_υ4υ"), Position(-2, true)))
                   + (Symbol("z") -> Occurrence(None, Position(3, true)))

    val bindings = cleaned

    assert(bindings.size == 2)

    bindings.head match
      case (Symbol("x_υ1υ"), Shadow(Symbol("x_υ2υ"))) =>
      case _ =>
        assert(false)
    bindings.tail.head match
      case (Symbol("y_υ3υ"), Binder(Symbol("y_υ4υ"))) =>
      case _ =>
        assert(false)

  }

  test("rename - rebind - via refresh") {

    val `13` = `.`(∅(), π(λ(Symbol("ch")), λ(Symbol("x")), Some(""), None))

    val id = new helper.υidυ
    given MutableList[(Symbol, Symbol)] = MutableList(Symbol("ch") -> Symbol("ch"))
    given Bindings()
    given Duplications()

    `13`.rename()(id()) match
      case `.`(∅(), π(λ(Symbol("ch")), λ(Symbol("x_υ1υ")), Some(_), None)) =>
      case _ =>
        assert(false)

  }

  test("rename - rebind - via Bindings - shadow is itself") {

    val `13` = `.`(∅(), π(λ(Symbol("ch")), λ(Symbol("x")), Some(""), None))

    val id = new helper.υidυ
    given MutableList[(Symbol, Symbol)] = MutableList(Symbol("ch") -> Symbol("ch"))
    given bindings: Bindings = Bindings()
                             + (Symbol("x") -> Occurrence(Some(Symbol("x")), Position(1, true)))
    given Duplications()

    `13`.rename()(id()) match
      case `.`(∅(), π(λ(Symbol("ch")), λ(Symbol("x_υ1υ")), Some(""), None)) =>
        assert(bindings.size == 1)
        bindings.head match
          case Symbol("x") -> Shadow(Symbol("x_υ1υ")) =>
          case _ =>
            assert(false)
      case _ =>
        assert(false)

  }

  test("rename - rebind - via Bindings - shadow is itself - definition is true") {

    val `13` = `.`(∅(), π(λ(Symbol("ch")), λ(Symbol("x")), Some(""), None))

    val id = new helper.υidυ
    given MutableList[(Symbol, Symbol)] = MutableList(Symbol("ch") -> Symbol("ch"))
    given bindings: Bindings = Bindings()
                             + (Symbol("x") -> Occurrence(Some(Symbol("x")), Position(1, true)))
    given Duplications()

    `13`.rename(collect = true)(id()) match
      case `.`(∅(), π(λ(Symbol("ch")), λ(Symbol("x_υ1υ")), Some(_), None)) =>
        assert(bindings.size == 1)
        bindings.head match
          case Symbol("x") -> Shadow(Symbol("x_υ1υ")) =>
          case _ =>
            assert(false)
      case _ =>
        assert(false)

  }

  test("rename - rebind - via Bindings - with shadow") {

    val `13` = `.`(∅(), π(λ(Symbol("ch")), λ(Symbol("x_υ0υ")), Some(""), None))

    val id = new helper.υidυ
    given MutableList[(Symbol, Symbol)] = MutableList(Symbol("ch") -> Symbol("ch"))
    given bindings: Bindings = Bindings()
                             + (Symbol("x") -> Occurrence(Some(Symbol("x_υ0υ")), Position(-1, true)))
    given Duplications()

    `13`.rename()(id()) match
      case `.`(∅(), π(λ(Symbol("ch")), λ(Symbol("x_υ1υ")), Some(_), None)) =>
        assert(bindings.size == 2)
        bindings.tail.head match
          case Symbol("x_υ0υ") -> Shadow(Symbol("x_υ1υ")) =>
          case _ =>
            assert(false)
      case _ =>
        assert(false)

  }

  test("rename - rebind - via Bindings - with shadow - definition is true") {

    val `13` = `.`(∅(), π(λ(Symbol("ch")), λ(Symbol("x_υ0υ")), Some(""), None))

    val id = new helper.υidυ
    given MutableList[(Symbol, Symbol)] = MutableList(Symbol("ch") -> Symbol("ch"))
    given bindings: Bindings = Bindings()
                             + (Symbol("x") -> Occurrence(Some(Symbol("x_υ0υ")), Position(-1, true)))
    given Duplications()

    `13`.rename(collect = true)(id()) match
      case `.`(∅(), π(λ(Symbol("ch")), λ(Symbol("x_υ1υ")), Some(_), None)) =>
        assert(bindings.size == 2)
        bindings.tail.head match
          case Symbol("x_υ0υ") -> Binder(Symbol("x_υ1υ")) =>
          case _ =>
            assert(false)
      case _ =>
        assert(false)

  }

  test("rename - Macro.apply - with shadow") {

    val `13` = Macro(
      List(Symbol("x"), Symbol("ch")),
      2,
      Names(),
      Names(),
      Bindings() + (Symbol("x") -> Occurrence(Some(Symbol("x_υ0υ")), Position(-1, true)))
                 + (Symbol("ch") -> Occurrence(None, Position(-2, false))),
      `+`(-1, ∥(-1, `.`(∅(), π(λ(Symbol("ch")), λ(Symbol("x_υ0υ")), Some(""), None))))
    )

    val id = new helper.υidυ
    val χ_id= new helper.υidυ
    given Duplications()

    `13`(0, null, false)(id(), χ_id()) match
      case (Definition(0, Some(_), _, _, +(-1, ∥(-1, `.`(∅(), π(λ(Symbol("ch")), λ(Symbol("x_υ1υ")), Some(_), None)))))
           ,(1, List(Some(Symbol("x_υ1υ")), None))) =>
      case _ =>
        assert(false)

  }

  test("rename - Macro.apply - without shadow") {

    val `13` = Macro(
      List(Symbol("x"), Symbol("ch")),
      2,
      Names(),
      Names(),
      Bindings() + (Symbol("x") -> Occurrence(None, Position(-1, false)))
                 + (Symbol("ch") -> Occurrence(None, Position(-2, false))),
      `+`(-1, ∥(-1, `.`(∅(), π(λ(Symbol("ch")), λ(Symbol("x")), Some(""), None))))
    )

    val id = new helper.υidυ
    val χ_id= new helper.υidυ
    given Duplications()

    `13`(0, null, false)(id(), χ_id()) match
      case (Definition(0, Some(_), _, _, +(_, ∥(_, `.`(∅(), π(λ(Symbol("ch")), λ(Symbol("x_υ1υ")), Some(_), None)))))
           ,(2, List(None, None))) =>
      case _ =>
        assert(false)

  }

  test("rename - restriction - via refresh") {

    val `13` = `+`(-1, ∥(-1,  `.`(?:(((λ(true), λ(false)), false),
                                     `+`(-1, ∥(-1, `.`(`!`(-1, None, None, ∅()), ν("x")))),
                                     Some(`+`(-1, ∥(-1, `.`(`(*)`("P", Nil)))))))))

    val id = new helper.υidυ
    given MutableList[(Symbol, Symbol)]()
    given Bindings()
    given Duplications()

    `13`.rename()(id()) match
      case +(-1, ∥(-1, `.`(?:(((λ(true), λ(false)), false),
                              +(-1, ∥(-1, `.`(!(_, _, None, ∅()), ν("x_υ1υ")))),
                              Some(+(-1, ∥(-1, `.`(`(*)`("P", Nil))))))))) =>
      case _ =>
        assert(false)

  }

  test("rename - by restriction - via refresh") {

    val `13` = `+`(-1, ∥(-1, `.`(?:(((λ(Symbol("x")), λ(Symbol("y"))), false),
                                    `+`(-1, ∥(-1, `.`(`!`(-1, None, Some(π(λ(Symbol("x")), λ(Symbol("y")), None, None)), ∅())))),
                                    Some(`+`(-1, ∥(-1, `.`(`(*)`("P", Nil, λ(Symbol("x")), λ(Symbol("y")))))))),
                                 ν("x", "y"))))

    val id = new helper.υidυ
    given MutableList[(Symbol, Symbol)]()
    given Bindings()
    given Duplications()

    `13`.rename()(id()) match
      case +(_, ∥(_, `.`(?:(((λ(Symbol("x_υ1υ")), λ(Symbol("y_υ2υ"))), false),
                            +(_, ∥(_, `.`(!(_, _, Some(π(λ(Symbol("x_υ1υ")), λ(Symbol("y_υ2υ")), None, None)), ∅())))),
                            Some(+(_, ∥(_, `.`(`(*)`("P", Nil, λ(Symbol("x_υ1υ")), λ(Symbol("y_υ2υ")))))))),
                         ν("x_υ1υ", "y_υ2υ")))) =>
      case _ =>
        assert(false)

  }

  test("rename - by Definition.apply - via refresh - using parser") {

    import EncodingParserSuite.*

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(definition, "⟦ 'x `1` 'y ⟧{z} =") match
          case Success(Some((_, definition @ Definition(_, _, _, _, ∅()))), _) =>
            given MutableList[(Symbol, Symbol)]()
            given Bindings()
            given Int = 1

            given Map[String, λ | (+ | `⟦⟧`)]()
            val `13` = definition(_code, _nest, _settings.dups, duplicated, _.replace{ (_, _) => }.flatten)(id)

            `13`.rename()(id) match
              case `⟦⟧`(Definition(_, _, _, variables, _), ∅(), _, Nil) =>
                variables.headOption match
                  case Some(Symbol("z_υ1υ")) =>
                  case _ =>
                    assert(false)
              case _ =>
                assert(false)
          case _ =>
            assert(false)

    `13`.test

  }

  test("rename - by Definition.apply - via refresh and bindings - using parser") {

    import EncodingParserSuite.*

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(definition, "⟦ 'x `1` 'y ⟧{z} = ν(x) z(y). y<x>.") match
          case Success((Some((it, definition @ Definition(_, _, _, _, +(_, ∥(_, `.`(∅(),
                                                                                    ν("x"),
                                                                                    π(λ(Symbol("z")), λ(Symbol("y")), Some(""), None),
                                                                                    π(λ(Symbol("y")), λ(Symbol("x")), None, None)))))))), _) =>
            given MutableList[(Symbol, Symbol)]()
            given Bindings = Bindings(it.bindings)
            given Int = 1

            given Map[String, λ | (+ | `⟦⟧`)]()
            val `13` = definition(_code, _nest, _settings.dups, duplicated, _.replace{ (_, _) => }.flatten)(id)

            `13`.rename()(id) match
              case `⟦⟧`(Definition(_, _, _, variables, _),
                        +(_, ∥(_, `.`(∅(),
                                      ν("x_υ2υ"),
                                      π(λ(Symbol("z_υ1υ")), λ(Symbol("y_υ3υ")), Some(""), None),
                                      π(λ(Symbol("y_υ3υ")), λ(Symbol("x_υ2υ")), None, None)))), _, _) =>
                variables.headOption match
                  case Some(Symbol("z_υ1υ")) =>
                  case _ =>
                    assert(false)
              case _ =>
                assert(false)
          case _ =>
            assert(false)

    `13`.test

  }

  test("rename - Macro.apply - with shadow - by restriction - using parser") {

    import EncodingParserSuite.*

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(definition, "⟦ 'x `1` 'y ⟧ = ν(y) x<y>.") match
          case Success(Some((it @ Macro(_, 2, _, _, bindings, _), _)), _) =>
            bindings.get(Symbol("y")) match
              case Some(Shadow(Symbol("y"))) =>
                it(0, null, false)(id, χ_id) match
                  case (Definition(_, _, _, _,
                                   +(_, ∥(_, `.`(∅(), ν("y_υ1υ"), π(λ(Symbol("x")), λ(Symbol("y_υ1υ")), None, None))))),
                        1 -> List(None, Some(Symbol("y_υ1υ")))) =>
                  case _ =>
                    assert(false)
              case _ =>
                assert(false)
          case _ =>
            assert(false)

    `13`.test

  }

  test("rename - Macro.apply - with shadow - by input prefix - using parser") {

    import EncodingParserSuite.*

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(definition, "⟦ 'x `1` 'y ⟧ = x(y).") match
          case Success(Some((it @ Macro(_, 2, _, _, bindings, _), _)), _) =>
            bindings.get(Symbol("y")) match
              case Some(Shadow(Symbol("y"))) =>
                it(0, null, false)(id, χ_id) match
                  case (Definition(_, _, _, _,
                                   +(_, ∥(_, `.`(∅(), π(λ(Symbol("x")), λ(Symbol("y_υ1υ")), Some(_), None))))),
                        1 -> List(None, Some(Symbol("y_υ1υ")))) =>
                  case _ =>
                    assert(false)
              case _ =>
                assert(false)
          case _ =>
            assert(false)

    `13`.test

  }

  test("rename - Macro.apply - with shadows - by restriction and input prefix - using parser") {

    import EncodingParserSuite.*

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(definition, "⟦ 'x `1` 'y ⟧ = ν(x) x(y).") match
          case Success(Some((it @ Macro(_, 2, _, _, bindings, _), _)), _) =>
            bindings.get(Symbol("x")) -> bindings.get(Symbol("y")) match
              case Some(Shadow(Symbol("x"))) -> Some(Shadow(Symbol("y"))) =>
                it(0, null, false)(id, χ_id) match
                  case (Definition(_, _, _, _,
                                   +(_, ∥(_, `.`(∅(), ν("x_υ1υ"), π(λ(Symbol("x_υ1υ")), λ(Symbol("y_υ2υ")), Some(_), None))))),
                        0 -> List(Some(Symbol("x_υ1υ")), Some(Symbol("y_υ2υ")))) =>
                  case _ =>
                    assert(false)
              case _ =>
                assert(false)
          case _ =>
            assert(false)

    `13`.test

  }

  test("rename - Macro.apply - with pointers - via refresh - using parser") {

    import EncodingParserSuite.*

    val `13` = new EncodingParserTest:
      override def test =
        parseAll(definition, """⟦ t"1" ⟧{z} =""") match
          case Success(Some((it @ Macro(_, 0, _, _, bindings, _), _)), _) =>
            bindings.get(Symbol("z")) match
              case Some(Occurrence(None, _, _)) =>
                it(0, null, false)(id, χ_id) match
                  case (Definition(_, _, _, variables, ∅()), 0 -> Nil) =>
                    variables.headOption match
                      case Some(Symbol("z_υ1υ")) =>
                      case _ =>
                        assert(false)
                  case _ =>
                    assert(false)
              case _ =>
                assert(false)
          case _ =>
            assert(false)

    `13`.test

  }

  test("rename - with pointers - via parser") {

    import ExpansionParserSuite.*

    val `13` = new ExpansionParserTest:
      override def test =
        parseAll(definition, """⟦ t"1" ⟧{z} =""") match
          case Success(Some(definition), _) =>
            if !defn.contains(_code) then defn(_code) = Nil
            defn(_code) ::= definition
            parseAll(equation, "P(x) = ⟦ 1 ⟧{x}") match
              case Success((_, +(_, ∥(_, `.`(`⟦⟧`(Definition(_, _, _, variables, _), ∅(), _, pointers))))), _) =>
                val assignment = variables zip pointers
                assignment.headOption match
                  case Some((Symbol("z_υ2υ"), Symbol("x"))) =>
                  case _ =>
                    assert(false)
              case _ =>
                assert(false)
          case _ =>
            assert(false)

    `13`.test

  }

  test("rename - with pointers - twice - via parser") {

    import ExpansionParserSuite.*

    val `13` = new ExpansionParserTest:
      override def test =
        parseAll(definition, """⟦1 t"1" 1⟧{z} =""") match
          case Success(Some(definition1 @ (_, Definition(_, _, _, _, ∅()))), _) =>
            if !defn.contains(_code) then defn(_code) = Nil
            defn(_code) ::= definition1
            parseAll(definition, """⟦2 t"2" 2⟧{y} = ⟦1 1 1⟧{y}""") match
              case Success(Some(definition2 @ (_, Definition(_, _, _, _, +(_, ∥(_, `.`(`⟦⟧`(Definition(_, _, _, variables, _), ∅(), _, pointers))))))), _) =>
                if !defn.contains(_code) then defn(_code) = Nil
                defn(_code) ::= definition2
                val assignment = variables zip pointers
                assignment.headOption match
                  case Some((Symbol("z_υ2υ"), Symbol("y"))) =>
                    parseAll(equation, "P(x) = ⟦2 2 2⟧{x}") match
                      case Success((_, +(_, ∥(_, `.`(`⟦⟧`(Definition(_, _, _, variables2, _), +(_, ∥(_, `.`(`⟦⟧`(Definition(_, _, _, variables1, _), ∅(), _, pointers1)))), _, pointers2))))), _) =>
                        val assignment1 = variables1 zip pointers1
                        val assignment2 = variables2 zip pointers2
                        assignment1.headOption -> assignment2.headOption match
                          case Some((Symbol("z_υ6υ"), Symbol("y_υ5υ"))) ->
                               Some((Symbol("y_υ5υ"), Symbol("x"))) =>
                          case _ =>
                            assert(false)
                      case _ =>
                        assert(false)
                  case _ =>
                    assert(false)
              case _ =>
                assert(false)
          case _ =>
            assert(false)

    `13`.test

  }


object EncodingFunctionsSuite:

  given Conversion[Char, `{}`] = { it => `{}`(it.toString, Nil, false)  }
