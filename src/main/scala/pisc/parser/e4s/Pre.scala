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
package e4s

import io.circe.{ Codec, Json }

import emitter.shared.Meta.rateʹ

import Calculus.λ


enum Pre extends Positional with Bound with Free derives Codec.AsObject:

  case ν(override val line: Int,
         override val column: Int,
         override val bound: Names,
         override val free: Names,
         names: String*) // forcibly

  case τ(override val line: Int,
         override val column: Int,
         override val bound: Names,
         override val free: Names,
         override val agent: Option[String],
         override val label: Option[String],
         override val key: String,
         override val guard: Boolean,
         override val rate: Rate)
      extends Pre with Act

  case π(override val line: Int,
         override val column: Int,
         override val bound: Names,
         override val free: Names,
         channel: String,
         argument: Option[Json],
         parameter: Option[String],
         polarity: Boolean,
         override val agent: Option[String],
         override val label: Option[String],
         override val key: String,
         override val guard: Boolean,
         override val rate: Rate)
      extends Pre with Act

  case ::(override val line: Int,
          override val column: Int,
          override val bound: Names,
          override val free: Names,
          channel: String,
          operator: String,
          operands: List[String])


object Pre:

  import com.sksamuel.elastic4s.ElasticDsl.*
  import com.sksamuel.elastic4s.fields.*

  import Names.given


  def schema: Seq[ElasticField] =
    val default = Positional.schema ++ Bound.schema ++ Free.schema ++ Act.schema
    Seq(
      ObjectField(name = "τ", properties = default),
      ObjectField(name = "π",
                  properties = default
                            ++ Seq(keywordField("channel"),
                                   ObjectField(name = "argument", enabled = Some(false)),
                                   keywordField("parameter"),
                                   booleanField("polarity")))
    )


  extension (self: ast.Pre)

    def toPre: Pre =

      val (line, column) = self.location

      self match

        case it @ ast.Pre.ν(names*) =>
          ν(line, column, it.bound, it.free, names*)

        case it @ ast.Pre.τ(Some(rate), _) =>
          val (agent, label, key, guard) = Act(it.υidυ)
          τ(line, column, it.bound, it.free, agent, label, key, guard, Rate(rateʹ(rate)))

        case it @ ast.Pre.π(ch, arg, Some("ν"), Some(rate), _) =>
          val (agent, label, key, guard) = Act(it.υidυ)
          π(line, column, it.bound, it.free, ch.toString, Some(arg.toJson), Some(arg.toString), false, agent, label, key, guard, Rate(rateʹ(rate)))

        case it @ ast.Pre.π(ch, par, Some(""), Some(rate), _) =>
          val (agent, label, key, guard) = Act(it.υidυ)
          π(line, column, it.bound, it.free, ch.toString, None, Some(par.toString), true, agent, label, key, guard, Rate(rateʹ(rate)))

        case it @ ast.Pre.π(ch, λ(ops: List[`λ`]), Some(cons), _, _) =>
          ::(line, column, it.bound, it.free, ch.toString, cons, ops.map(_.asSymbol.name))

        case it @ ast.Pre.π(ch, arg, None, Some(rate), _) =>
          val (agent, label, key, guard) = Act(it.υidυ)
          π(line, column, it.bound, it.free, ch.toString, Some(arg.toJson), None, false, agent, label, key, guard, Rate(rateʹ(rate)))
