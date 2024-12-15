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

import scala.collection.mutable.{ HashMap => Map, LinkedHashSet => Set }

import munit.FunSuite

import Calculus._
import CalculusParserSuite._
import CalculusFunctionsSuite._
import CalculusFunctionsSuite.given


class CalculusFunctionsSuite extends FunSuite:

  test("flatten - directly - ( P ) + Q -> P + Q") {

    val `13` = `+`(∥(`.`(`+`(∥(`.`('P'))))), ∥(`.`('Q')))

    assertEquals(`13`.flatten, `+`(∥(`.`('P')), ∥(`.`('Q'))))

  }

  test("flatten - directly - ( P ) | Q -> P | Q") {

    val `13` = ∥(`.`(`+`(∥(`.`('P')))), `.`('Q'))

    assertEquals(`13`.flatten, ∥(`.`('P'), `.`('Q')))

  }

  test("flatten - directly - τ. ( τ.P ) -> τ.τ.P") {

    val `13` = `.`(`+`(∥(`.`('P', τ_))), τ_)

    assertEquals(`13`.flatten, `.`('P', τ_, τ_))

  }

  test("flatten - directly - !!P -> !P") {

    val `13` = `!`(None, `+`(∥(`.`(`!`(None, `+`(∥(`.`('P'))))))))

    assertEquals(`13`.flatten, `!`(None, `+`(∥(`.`('P')))))

  }

  test("flatten - via parser - ( P + Q ) + R -> P + Q + R") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(equation, "Main = ( P + Q ) + R") match
          case Success((_, +(∥(`.`(`(*)`("P", _))), ∥(`.`(`(*)`("Q", _))), ∥(`.`(`(*)`("R", _))))), _) =>
            assert(true)
          case _ =>
            assert(false)

    `13`.test

  }

  test("flatten - via parser - ( P | Q ) | R -> P | Q | R") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(equation, "Main = ( P | Q ) | R") match
          case Success((_, +(∥(`.`(`(*)`("P", _)), `.`(`(*)`("Q", _)), `.`(`(*)`("R", _))))), _) =>
            assert(true)
          case _ =>
            assert(false)

    `13`.test

  }

  test("flatten - via parser - τ. ( τ.P ) -> τ.τ.P") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(equation, "Main = τ. ( τ.P )") match
          case Success((_, +(∥(`.`(`(*)`("P", _), τ(_), τ(_))))), _) =>
            assert(true)
          case _ =>
            assert(false)

    `13`.test

  }

  test("flatten - via parser - !!P -> !P") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(equation, "Main = !!P") match
          case Success((_, +(∥(`.`(!(_, +(∥(`.`(`(*)`("P", _))))))))), _) =>
            assert(true)
          case _ =>
            assert(false)

    `13`.test

  }


object CalculusFunctionsSuite:

  val τ_ = τ(None)

  given Conversion[Char, `(*)`] = { it => `(*)`(it.toString, Nil)  }
