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

package pisc
package parser

import scala.collection.mutable.{ LinkedHashMap => Map2, ListBuffer => MutableList }

import munit.FunSuite

import Pi._
import Calculus._
import Encoding._
import EncodingFunctionsSuite.given


class EncodingFunctionsSuite extends FunSuite:

  test("capitals - P{} + ( Q{} | if True = False then !R{} else ⟦ S{} ^ T(){} ^ U ⟧ ) -> Set(P,Q,R,S)") {

    val `13` = `+`(∥(`.`('P')),
                   ∥(`.`('Q'),
                     `.`(?:(((λ(true), λ(false)), false),
                            `+`(∥(`.`(`!`(None, `+`(∥(`.`('R'))))))),
                            Some(`+`(∥(`.`(`⟦⟧`(null, null, `+`(∥(`.`('S'),
                                                                  `.`(`{}`("T", Nil, true)),
                                                                  `.`(`(*)`("U", Nil)))))))))))))

    assertEquals(`13`.capitals, Names() + Symbol("P") + Symbol("Q") + Symbol("R") + Symbol("S"))

  }

  test("renamed - NoBPEx") {

    interceptMessage[Throwable]("x") {
      renamed(Symbol("x"))(using MutableList())(using Names2())
    }

  }

  test("renamed - from refresh") {

    given MutableList[(Symbol, λ)] = MutableList(Symbol("x") -> λ(Symbol("x_υ1υ")))
    given Names2()
    assertEquals(renamed(Symbol("x")), λ(Symbol("x_υ1υ")))

  }

  test("renamed - from binding2 - with binder") {

    given MutableList[(Symbol, λ)]()
    given Names2 = Names2() + (Symbol("x") -> Occurrence(Symbol("x_υ1υ"), Position(-1, true)))
    assertEquals(renamed(Symbol("x")), λ(Symbol("x_υ1υ")))

  }

  test("renamed - from binding2 - with shadow") {

    given MutableList[(Symbol, λ)]()
    given Names2 = Names2() + (Symbol("x") -> Occurrence(Some(Symbol("x_υ1υ")), Position(-1, true)))
    assertEquals(renamed(Symbol("x")), λ(Symbol("x_υ1υ")))

  }

  test("renamed - from binding2 - by itself") {

    given MutableList[(Symbol, λ)]()
    given Names2 = Names2() + (Symbol("x") -> Occurrence(None, Position(1, true)))
    assertEquals(renamed(Symbol("x")), λ(Symbol("x")))

  }

  test("renamed - from binding2 - itself a shadow") {

    given MutableList[(Symbol, λ)]()
    given Names2 = Names2() + (Symbol("x_υ1υ") -> Occurrence(Some(Symbol("x_υ2υ")), Position(-1, true)))
    assertEquals(renamed(Symbol("x_υ2υ")), λ(Symbol("x_υ2υ")))

  }

  test("purged - yes - shadow with a shadow") {

    given binding2: Names2 = Names2()
                           + (Symbol("x_υ1υ") -> Occurrence(Some(Symbol("x_υ2υ")), Position(-1, true)))
                           + (Symbol("x_υ2υ") -> Occurrence(Some(Symbol("x_υ3υ")), Position(-1, true)))

    purged

    assert(binding2.size == 1)

    binding2.head match
      case (Symbol("x_υ1υ"), Shadow(Symbol("x_υ3υ"))) =>
        assert(true)
      case _ =>
        assert(false)

  }

  test("purged - yes - shadow with a binder") {

    given binding2: Names2 = Names2()
                           + (Symbol("x_υ1υ") -> Occurrence(Some(Symbol("x_υ2υ")), Position(-1, true)))
                           + (Symbol("x_υ2υ") -> Occurrence(Symbol("x_υ3υ"), Position(-1, true)))

    purged

    assert(binding2.size == 1)

    binding2.head match
      case (Symbol("x_υ1υ"), Binder(Symbol("x_υ3υ"))) =>
        assert(true)
      case _ =>
        assert(false)

  }

  test("purged - not - shadow with neither shadow nor binder") {

    given binding2: Names2 = Names2()
                           + (Symbol("x_υ1υ") -> Occurrence(Some(Symbol("x_υ2υ")), Position(-1, true)))

    purged

    assert(binding2.size == 1)

    binding2.head match
      case (Symbol("x_υ1υ"), Shadow(Symbol("x_υ2υ"))) =>
        assert(true)
      case _ =>
        assert(false)

  }

  test("binders") {

    given Names2 = Names2()
                 + (Symbol("x_υ1υ") -> Occurrence(Some(Symbol("x_υ2υ")), Position(-1, true)))
                 + (Symbol("y_υ3υ") -> Occurrence(Symbol("y_υ4υ"), Position(-2, true)))
                 + (Symbol("z") -> Occurrence(None, Position(3, true)))

    val binding2 = binders

    assert(binding2.size == 2)

    binding2.head match
      case (Symbol("x_υ1υ"), Shadow(Symbol("x_υ2υ"))) =>
        binding2.tail.head match
          case (Symbol("y_υ3υ"), Binder(Symbol("y_υ4υ"))) =>
            assert(true)
          case _ =>
            assert(false)
      case _ =>
        assert(false)

  }

  test("rename - rebind - via refresh") {

    val `13` = `.`(∅, π(λ(Symbol("ch")), λ(Symbol("x")), true, None))

    val id = new helper.υidυ
    given MutableList[(Symbol, λ)] = MutableList(Symbol("ch") -> λ(Symbol("ch")))
    given binding2: Names2 = Names2()
    given Names()

    `13`.rename(id(), Names()) match
      case `.`(∅, π(λ(Symbol("ch")), λ(Symbol("x_υ1υ")), true, None)) =>
        assert(binding2.isEmpty)
      case _ =>
        assert(false)

  }

  test("rename - rebind - via Names2 - shadow is itself") {

    val `13` = `.`(∅, π(λ(Symbol("ch")), λ(Symbol("x")), true, None))

    val id = new helper.υidυ
    given MutableList[(Symbol, λ)] = MutableList(Symbol("ch") -> λ(Symbol("ch")))
    given binding2: Names2 = Names2()
                           + (Symbol("x") -> Occurrence(Some(Symbol("x")), Position(1, true)))
    given Names()

    `13`.rename(id(), Names()) match
      case `.`(∅, π(λ(Symbol("ch")), λ(Symbol("x_υ1υ")), true, None)) =>
        assert(binding2.size == 1)
        binding2.head match
          case Symbol("x") -> Shadow(Symbol("x_υ1υ")) =>
            assert(true)
          case _ =>
            assert(false)
      case _ =>
        assert(false)

  }

  test("rename - rebind - via Names2 - shadow is itself - definition is true") {

    val `13` = `.`(∅, π(λ(Symbol("ch")), λ(Symbol("x")), true, None))

    val id = new helper.υidυ
    given MutableList[(Symbol, λ)] = MutableList(Symbol("ch") -> λ(Symbol("ch")))
    given binding2: Names2 = Names2()
                           + (Symbol("x") -> Occurrence(Some(Symbol("x")), Position(1, true)))
    given Names()

    `13`.rename(id(), Names(), definition = true) match
      case `.`(∅, π(λ(Symbol("ch")), λ(Symbol("x_υ1υ")), true, None)) =>
        assert(binding2.size == 1)
        binding2.head match
          case Symbol("x") -> Shadow(Symbol("x_υ1υ")) =>
            assert(true)
          case _ =>
            assert(false)
      case _ =>
        assert(false)

  }

  test("rename - rebind - via Names2 - with shadow") {

    val `13` = `.`(∅, π(λ(Symbol("ch")), λ(Symbol("x_υ0υ")), true, None))

    val id = new helper.υidυ
    given MutableList[(Symbol, λ)] = MutableList(Symbol("ch") -> λ(Symbol("ch")))
    given binding2: Names2 = Names2()
                           + (Symbol("x") -> Occurrence(Some(Symbol("x_υ0υ")), Position(-1, true)))
    given Names()

    `13`.rename(id(), Names()) match
      case `.`(∅, π(λ(Symbol("ch")), λ(Symbol("x_υ1υ")), true, None)) =>
        assert(binding2.size == 2)
        binding2.tail.head match
          case Symbol("x_υ0υ") -> Shadow(Symbol("x_υ1υ")) =>
            assert(true)
          case _ =>
            assert(false)
      case _ =>
        assert(false)

  }

  test("rename - rebind - via Names2 - with shadow - definition is true") {

    val `13` = `.`(∅, π(λ(Symbol("ch")), λ(Symbol("x_υ0υ")), true, None))

    val id = new helper.υidυ
    given MutableList[(Symbol, λ)] = MutableList(Symbol("ch") -> λ(Symbol("ch")))
    given binding2: Names2 = Names2()
                           + (Symbol("x") -> Occurrence(Some(Symbol("x_υ0υ")), Position(-1, true)))
    given Names()

    `13`.rename(id(), Names(), definition = true) match
      case `.`(∅, π(λ(Symbol("ch")), λ(Symbol("x_υ1υ")), true, None)) =>
        assert(binding2.size == 2)
        binding2.tail.head match
          case Symbol("x_υ0υ") -> Binder(Symbol("x_υ1υ")) =>
            assert(true)
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
      Names2() + (Symbol("x") -> Occurrence(Some(Symbol("x_υ0υ")), Position(-1, true)))
               + (Symbol("ch") -> Occurrence(None, Position(-2, false))),
      `+`(∥(`.`(∅, π(λ(Symbol("ch")), λ(Symbol("x_υ0υ")), true, None))))
    )

    val id = new helper.υidυ

    `13`(0, id(), 1, 0, null) match
      case (Definition(0, Some(_), _, _, +(∥(`.`(∅, π(λ(Symbol("ch")), λ(Symbol("x_υ1υ")), true, None)))))
           ,(1, List(Some(Symbol("x_υ1υ")), None))) =>
        assert(true)
      case _ =>
        assert(false)

  }

  test("rename - Macro.apply - without shadow") {

    val `13` = Macro(
      List(Symbol("x"), Symbol("ch")),
      2,
      Names(),
      Names(),
      Names2() + (Symbol("x") -> Occurrence(None, Position(-1, false)))
               + (Symbol("ch") -> Occurrence(None, Position(-2, false))),
      `+`(∥(`.`(∅, π(λ(Symbol("ch")), λ(Symbol("x")), true, None))))
    )

    val id = new helper.υidυ

    `13`(0, id(), 1, 0, null) match
      case (Definition(0, Some(_), _, _, +(∥(`.`(∅, π(λ(Symbol("ch")), λ(Symbol("x_υ1υ")), true, None)))))
           ,(2, List(None, None))) =>
        assert(true)
      case _ =>
        assert(false)

  }

  test("rename - restriction - via refresh") {

    val `13` = `+`(∥(`.`(?:(((λ(true), λ(false)), false),
                            `+`(∥(`.`(`!`(None, ∅), ν("x")))),
                            Some(`+`(∥(`.`(`(*)`("P", Nil)))))))))

    val id = new helper.υidυ
    given MutableList[(Symbol, λ)]()
    given binding2: Names2()
    given Names()

    `13`.rename(id(), Names()) match
      case +(∥(`.`(?:(((λ(true), λ(false)), false),
                      +(∥(`.`(!(None, ∅), ν("x_υ1υ")))),
                      Some(+(∥(`.`(`(*)`("P", Nil))))))))) =>
        assert(binding2.isEmpty)
      case _ =>
        assert(false)

  }


object EncodingFunctionsSuite:

  given Conversion[Char, `{}`] = { it => `{}`(it.toString, Nil, false)  }
