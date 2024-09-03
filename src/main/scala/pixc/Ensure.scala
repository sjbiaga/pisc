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

package pixc

import scala.collection.mutable.{ HashMap => Map }

import parser.Calculus._


object Ensure:

  case object MainParsingException
      extends EquationParsingException("Exactly one Main parameterless agent not found")

  case object MainParsingException2
      extends EquationParsingException("The parameterless Main agent is recursive")

  case class StartParsingException(id: String, arity: Int, by: String)
      extends EquationParsingException(s"$id#$arity leads to a start transaction prefix by $by")

  private def index2(prog: List[Bind]): ((String, Int)) => Int = {
    case (identifier, size) =>
      prog
        .indexWhere {
          case (`(*)`(λ(Symbol(`identifier`)), params*), _) if params.size == size => true
          case _ => false
        }
  }

  def main(using prog: List[Bind]): Int =
    if 1 == prog
      .count {
        case (`(*)`(λ(Symbol("Main"))), _) => true
        case _ => false
      }
    then prog
      .indexWhere {
        case (`(*)`(λ(Symbol("Main"))), _) => true
        case _ => false
      }
    else -1

  /**
    * When an existing invocation is found on the reachability graph,
    * all definitions up to its definition are marked as recursive.
    */
  def recursive(ast: AST)
               (using stack: List[(String, Int)])
               (using rec: Map[(String, Int), Int])
               (using prog: List[Bind]): Unit =

    ast match

      case `∅` =>

      case `+`(_, it*) =>
       it.foldLeft(())((_, par) => recursive(par))

      case `|`(it*) =>
       it.foldLeft(())((_, seq) => recursive(seq))

      case `.`(end, it*) =>
        it.foldLeft(recursive(end)) {
          case (_, χ(_, Some(sum), _)) => recursive(sum)
          case _ =>
        }

      case `?:`(_, t, f) =>
        recursive(t)
        recursive(f)

      case `!`(_, sum) =>
        recursive(sum)

      case `[]`(_, sum) =>
        recursive(sum)

      case it @ `(*)`(λ(Symbol(id)), params*) if stack.contains(id -> params.size) =>
        val k = stack.indexOf(id -> params.size)
        for
          j <- 0 to k
          if !rec.contains(id -> params.size)
        do
          val i = index2(prog)(stack(j))
          rec(id -> params.size) = i+1

      case it @ `(*)`(λ(Symbol(id)), params*) =>
        val i = index2(prog)(id -> params.size)
        recursive(prog(i)._2)(using id -> params.size :: stack)

      case _ : `(*)` =>

  /**
    * Called for "Main".
    */
  def replication(ast: AST)
                 (using stack: List[(String, Int)])
                 (using rec: Map[(String, Int), Int])
                 (using prog: List[Bind])
                 (implicit repl: Boolean = false): Boolean =

    ast match

      case `∅` => true

      case `+`(_, it*) =>
       it.foldLeft(true) {
         case (false, _) => false
         case (_, par) => replication(par)
       }

      case `|`(it*) =>
       it.foldLeft(true) {
         case (false, _) => false
         case (_, seq) => replication(seq)
       }

      case `.`(end, it*) =>
        if it
          .exists {
            case χ(_, Some(_), _) => true
            case _ => false
          } && repl
        then
          false
        else
          it.foldLeft(replication(end)) {
            case (false, _) => false
            case (_, χ(_, Some(sum), _)) => replication(sum)
            case _ => true
          }

      case `?:`(_, t, f) =>
        replication(t) && replication(f)

      case `!`(_, sum) =>
        replication(sum)(true)

      case `[]`(_, sum) =>
        replication(sum)

      case `(*)`(λ(Symbol(id)), params*)
          if stack.contains(id -> params.size) => true

      case `(*)`(λ(Symbol(id)), params*) =>
        val i = rec(id -> params.size)
        val sum = prog(i.abs-1)._2
        replication(sum)(using id -> params.size :: stack)

      case _: `(*)` => ???

  /**
    * Called only for recursive agents.
    */
  def recursion(ast: AST)
                (using stack: List[(String, Int)])
                (using rec: Map[(String, Int), Int])
                (using prog: List[Bind]): Boolean =

    ast match

      case `∅` => true

      case `+`(_, it*) =>
       it.foldLeft(true) {
         case (false, _) => false
         case (_, par) => recursion(par)
       }

      case `|`(it*) =>
       it.foldLeft(true) {
         case (false, _) => false
         case (_, seq) => recursion(seq)
       }

      case `.`(end, it*) =>
        if it
          .exists {
            case χ(_, Some(_), _) => true
            case _ => false
          }
        then
          false
        else
          it.foldLeft(recursion(end)) {
            case (false, _) => false
            case (_, χ(_, Some(sum), _)) => recursion(sum)
            case _ => true
          }

      case `?:`(_, t, f) =>
        recursion(t) && recursion(f)

      case `!`(_, sum) =>
        recursion(sum)

      case `[]`(_, sum) =>
        recursion(sum)

      case `(*)`(λ(Symbol(id)), params*)
          if stack.contains(id -> params.size) => true

      case `(*)`(λ(Symbol(id)), params*) =>
        val i = rec(id -> params.size)
        val sum = prog(i.abs-1)._2
        recursion(sum)(using id -> params.size :: stack)

      case _: `(*)` => ???
