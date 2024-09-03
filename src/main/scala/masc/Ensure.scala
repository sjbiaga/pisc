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

import scala.collection.mutable.{ LinkedHashSet => Set }

import parser.Calculus._


object Ensure:

  case object MainParsingException
      extends EquationParsingException("Exactly one Main parameterless agent not found")

  case object MainParsingException2
      extends EquationParsingException("The parameterless Main agent is recursive")

  private def index2(prog: List[Bind]): ((String, Int)) => Int = {
    case (identifier, size) =>
      prog
        .indexWhere {
          case (`(*)`(`identifier`, _, params*), _) if params.size == size => true
          case _ => false
        }
  }

  def main(using prog: List[Bind]): Int =
    if 1 == prog
      .count {
        case (`(*)`("Main", _), _) => true
        case _ => false
      }
    then prog
      .indexWhere {
        case (`(*)`("Main", _), _) => true
        case _ => false
      }
    else -1

  /**
    * When an existing invocation is found on the reachability graph,
    * all definitions up to its definition are marked as recursive.
    */
  def recursive(ast: AST)
               (using stack: List[(String, Int)])
               (using rec: Set[(String, Int)])
               (using prog: List[Bind]): Unit =

    ast match

      case `∅` =>

      case `|`(it*) =>
       it.foldLeft(())((_, seq) => recursive(seq))

      case `.`(end, _*) =>
        recursive(end)

      case `!`(_, par) =>
        recursive(par)

      case it @ `(*)`(id, _, params*) if stack.contains(id -> params.size) =>
        val k = stack.indexOf(id -> params.size)
        for
          j <- 0 to k
          i = index2(prog)(stack(j))
          sign = prog(i)._1.identifier -> prog(i)._1.params.size
          if !rec.contains(sign)
        do
          rec += sign

      case `(*)`(id, _, params*) =>
        val i = index2(prog)(id -> params.size)
        recursive(prog(i)._2)(using id -> params.size :: stack)

      case `[]`(_, par) =>
        recursive(par)

      case `go.`(_, par) =>
        recursive(par)

      case _ =>
