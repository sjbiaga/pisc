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

package basc
package parser
package e4s

import io.circe.{ Codec, Json }
import io.circe.syntax.*

import Pre.*, AST.*


enum AST extends Positional with Free derives Codec.AsObject:

  case +(override val line: Int,
         override val column: Int,
         override val free: Names,
         operands: AST*)

  case ∥(override val line: Int,
         override val column: Int,
         override val free: Names,
         operands: AST*)

  case `.`(override val line: Int,
           override val column: Int,
           override val free: Names,
           operand: AST,
           prefixes: Pre*)

  case ?:(override val line: Int,
          override val column: Int,
          override val free: Names,
          lhs: Json,
          rhs: Json,
          mismatch: Boolean,
          t: AST,
          f: Option[AST])

  case `¡`(override val line: Int,
           override val column: Int,
           override val free: Names,
           parallelism: Int,
           pace: Option[(Long, String)],
           guard: Pre,
           operand: AST)

  case !(override val line: Int,
         override val column: Int,
         override val free: Names,
         parallelism: Int,
         pace: Option[(Long, String)],
         guard: Pre,
         operand: AST)

  case `[]`(override val line: Int,
            override val column: Int,
            override val free: Names,
            label: Option[String],
            operand: AST)

  case `(*)`(override val line: Int,
             override val column: Int,
             override val free: Names,
             identifier: String,
             parameters: Json*)


object AST:

  import Names.given

  extension (self: ast.AST)

    def toAST: AST =

      val (line, column) = self.location

      self match

        case it @ ast.AST.∅() =>
          `+`(line, column, it.free)

        case it @ ast.AST.+(choices*) =>
          `+`(line, column, it.free, choices.map(_.toAST)*)

        case it @ ast.AST.∥(components*) =>
          ∥(line, column, it.free, components.map(_.toAST)*)

        case it @ ast.AST.`.`(end, prefixes*) =>
          `.`(line, column, it.free, end.toAST, prefixes.map(_.toPre)*)

        case it @ ast.AST.?:(((lhs, rhs), mismatch), t, f) =>
          ?:(line, column, it.free, lhs.toJson, rhs.toJson, mismatch, t.toAST, f.map(_.toAST))

        case it @ ast.AST.!(parallelism, pace: Option[(Long, String)], guard, sum) if parallelism < -1 =>
          `¡`(line, column, it.free, -(parallelism%Int.MaxValue), pace, guard.get.toPre, sum.toAST)

        case it @ ast.AST.!(parallelism, pace: Option[(Long, String)], guard, sum) =>
          `!`(line, column, it.free, parallelism, pace, guard.get.toPre, sum.toAST)

        case it @ ast.AST.`[]`(label, sum) =>
          `[]`(line, column, it.free, label, sum.toAST)

        case ast.AST.`⟦⟧`(_, sum, _, _) => sum.toAST

        case it @ ast.AST.`(*)`(identifier, params*) =>
          `(*)`(line, column, it.free, identifier, params.map(_.toJson)*)
