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
  LinkedHashSet => Set
}

import munit.FunSuite

import _root_.pisc.parser.Pi.*
import _root_.pisc.parser.Calculus.*
import _root_.pisc.parser.Directive.Settings
import _root_.pisc.parser.Encoding.*
import scala.util.parsing.combinator.pisc.parser.Expansion
import Expansion.*
import ExpansionFunctionsSuite.*


class ExpansionFunctionsSuite extends FunSuite:

  test("balanced - ⟦ ⟦ ⟧ $ ⟧ $") {

    balanced(Right("$"))("⟦ ⟦ ⟧ $ ⟧ $", 0) match
      case None =>
        assert(false)
      case _ =>

  }

  test("balanced - ⟦ ⟦ ⟧ $") {

    balanced(Right("$"))("⟦ ⟦ ⟧ $", 0) match
      case Some(_) =>
        assert(false)
      case _ =>

  }

  test("balanced - ⟧ $") {

    balanced(Right("$"))("⟧ $", 0) match
      case Some(_) =>
        assert(false)
      case _ =>

  }

  test("balanced - ⟦ ⟦ ⟧ ⟧ $") {

    balanced(Right("$"))("⟦ ⟦ ⟧ ⟧ $", 0) match
      case None =>
        assert(false)
      case _ =>

  }

  test("balanced - ⟦ ⟦ ⟧ ⟧ ⟦ ⟧ $") {

    balanced(Right("$"))("⟦ ⟦ ⟧ ⟧ ⟦ ⟧ $", 0) match
      case None =>
        assert(false)
      case _ =>

  }

  test("balanced - ⟦ ⟧ ⟦ ⟧ ⟦ ⟧ $") {

    balanced(Right("$"))("⟦ ⟧ ⟦ ⟧ ⟦ ⟧ $", 0) match
      case None =>
        assert(false)
      case _ =>

  }

  test("balanced - ⟦ ⟧ ⟦ ⟧ ⟦ ⟧ $") {

    balanced(Right("$"))("⟦ ⟧ ⟦ ⟧ ⟦ ⟧ $", 0) match
      case None =>
        assert(false)
      case _ =>

  }

  test("balanced - ⟦ ⟦ ⟦ ⟦ ⟧ ⟧ ⟧ ⟦ ⟧ ⟧ $") {

    balanced(Right("$"))("⟦ ⟦ ⟦ ⟦ ⟧ ⟧ ⟧ ⟦ ⟧ ⟧ $", 0) match
      case None =>
        assert(false)
      case _ =>

  }

  test("balanced - ⟦ ⟦ ⟧ ⟦ ⟧ ⟧ $") {

    balanced(Right("$"))("⟦ ⟦ ⟧ ⟦ ⟧ ⟧ $", 0) match
      case None =>
        assert(false)
      case _ =>

  }

  test("balanced - ⟦ ⟧ ⟧ ⟦ ⟦ ⟧ $") {

    balanced(Right("$"))("⟦ ⟧ ⟧ ⟦ ⟦ ⟧ $", 0) match
      case Some(_) =>
        assert(false)
      case _ =>

  }

  test("balanced - ⟦ ⟧ ⟦ ⟧ ⟧ ⟦ $") {

    balanced(Right("$"))("⟦ ⟧ ⟦ ⟧ ⟧ ⟦ $", 0) match
      case Some(_) =>
        assert(false)
      case _ =>

  }

  test("balanced - ⟦ ⟦ ⟦ ⟧ ⟧ ⟧ ⟧ ⟦ $") {

    balanced(Right("$"))("⟦ ⟦ ⟦ ⟧ ⟧ ⟧ ⟧ ⟦ $", 0) match
      case Some(_) =>
        assert(false)
      case _ =>

  }

  test("update - output prefix") {
    val `13` = new ExpansionFunctionTest:
      override def test =
        assertMatches(parseAll(choice, "x<x>.")) {
          case Success((sum, _), _) =>
            val shadow = Symbol("x_shadow")
            given Bindings = Map(Symbol("x") -> Occurrence(Some(shadow), Position(1, true)))

            val bindings = Bindings(given_Bindings)
            sum.update(using bindings) match
              case +(_, ∥(_, `.`(_, π(λ(`shadow`), λ(`shadow`), _, _)))) =>
                bindings == given_Bindings
              case _ => false
        }

    `13`.test
  }

  test("update - input prefix . output prefix") {
    val `13` = new ExpansionFunctionTest:
      override def test =
        assertMatches(parseAll(choice, "x(x). x<x>.")) {
          case Success((sum, _), _) =>
            val name = Symbol("x")
            val shadow = Symbol("x_shadow")
            given Bindings = Map(name -> Occurrence(Some(shadow), Position(1, true)))

            val bindings = Bindings(given_Bindings)
            sum.update(using bindings) match
              case +(_, ∥(_, `.`(_, π(λ(`shadow`), λ(`name`), _, _),
                                    π(λ(`name`), λ(`name`), _, _)))) =>
                bindings == given_Bindings
              case _ => false
        }

    `13`.test
  }

  test("update - replication - output prefix guard . output prefix") {
    val `13` = new ExpansionFunctionTest:
      override def test =
        assertMatches(parseAll(leaf, "! .x<x>. x<x>.")) {
          case Success((rep, _), _) =>
            val shadow = Symbol("x_shadow")
            given Bindings = Map(Symbol("x") -> Occurrence(Some(shadow), Position(1, true)))

            val bindings = Bindings(given_Bindings)
            rep.update(using bindings) match
              case !(_, _, Some(π(λ(`shadow`), λ(`shadow`), _, _)),
                     +(_, ∥(_, `.`(_, π(λ(`shadow`), λ(`shadow`), _, _))))) =>
                bindings == given_Bindings
              case _ => false
        }

    `13`.test
  }

  test("update - replication - input prefix guard . output prefix") {
    val `13` = new ExpansionFunctionTest:
      override def test =
        assertMatches(parseAll(leaf, "! .x(x). x<x>.")) {
          case Success((rep, _), _) =>
            val name = Symbol("x")
            val shadow = Symbol("x_shadow")
            given Bindings = Map(name -> Occurrence(Some(shadow), Position(1, true)))

            val bindings = Bindings(given_Bindings)
            rep.update(using bindings) match
              case !(_, _, Some(π(λ(`shadow`), λ(`name`), _, _)),
                     +(_, ∥(_, `.`(_, π(λ(`name`), λ(`name`), _, _))))) =>
                bindings == given_Bindings
              case _ => false
        }

    `13`.test
  }

  test("concatenate - capitals - concatenation") {
    val `13`: `{}` = `{}`("P", List(Symbol("p")))

    assertMatches(`13`.concatenate({ (_, _) => })(using List(Symbol("q")))) {
      case `{}`("P", List(Symbol("p"), Symbol("q")), _, _*) => true
    }
  }

  test("concatenate - instantiation - no recursion") {
    val P: + = `+`(-1, ∥(-1, `.`(`{}`("P", Nil))))
    val `13` = `⟦⟧`(Definition(0, None, Names(), Names() + Symbol("x"), P), P)

    assertMatches(`13`.concatenate({ (_, _) => })(using List(Symbol("p")))) {
      case `⟦⟧`(Definition(0, None, _, variables, _), +(_, ∥(_, `.`(`{}`("P", Nil, _, _*)))), _, pointers) =>
        assertEquals(variables.head, Symbol("x"))
        assertEquals(pointers.head, Symbol("p"))
        true
    }
  }

  test("concatenate - instantiation - too many pointers") {
    case class TooMP(code: Int, amount: Int) extends Throwable(code + " " + amount)

    val `13` = `⟦⟧`(Definition(0, None, Names(), Names() + Symbol("x"), ∅()), ∅())

    interceptMessage[TooMP]("0 1") {
      `13`.concatenate({ (code, amount) => throw TooMP(code, amount) })(using List(Symbol("p"), Symbol("q")))
    }
  }


object ExpansionFunctionsSuite:

  abstract class ExpansionFunctionTest extends Expansion:
    override protected val emitter: Emitter = Emitter.test
    override protected val in: String = getClass.getSimpleName
    override val ln: String = "line #0"

    eqtn = List()
    defn = Map()
    self = Set()
    _nest = 0
    _id = new helper.υidυ
    _χ_id = new helper.υidυ
    _cntr = Map(0 -> 0L)
    _nth = Map(0 -> 0L)
    _settings = Settings()

    given Bindings()
    given Duplications()
    given Int = 1

    def test: Unit
