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

import io.circe.{ Codec, Decoder, Encoder, Json }
import io.circe.syntax.*

import com.sksamuel.elastic4s.Indexable

import com.sksamuel.elastic4s.ElasticDsl.*
import com.sksamuel.elastic4s.fields.*


package object e4s:

  object Bind:

    import AST.toAST

    given Encoder[ast.Bind] = _ match
      case (bind @ ast.AST.`(*)`(identifier, params*), sum) =>
        Json.obj("agent" -> bind.toAST.asJson
                ,"body" -> sum.toAST.asJson)

    given Encoder[List[ast.Bind]] = it => Json.arr(it.map(_.asJson)*)


  case class Document(prefixes: List[Pre], equations: Json) extends Indexable[Document] derives Codec.AsObject:
    override def json(self: Document): String =
      Json.obj("prefixes" -> Json.arr(self.prefixes.map(_.asJson)*),
               "equations" -> self.equations).spaces2

  object Document:

    import BioAmbients.{ Act => Action }
    import Calculus.mapreduce

    import Pre.toPre

    extension (self: ast.AST)
      def collect: Seq[Pre] =
        self.mapreduce {
          case ast.AST.`.`(_, prefixes*) => prefixes.filter { case Action(it) => it }.map(_.toPre)
          case ast.AST.!(_, _, Some(guard), _) => Seq(guard.toPre)
          case _ => Nil
        }(_ ++ _)

    def build(it: List[ast.Bind], equations: Json): Document =
      Document(it.map(_._2.collect).reduce(_ ++ _).toList, equations)

    def schema: Seq[ElasticField] =
      Seq(
        nestedField("prefixes").fields(Pre.schema),
        ObjectField(name = "equations", enabled = Some(false))
      )


  trait Positional:
    val line: Int
    val column: Int

  object Positional:
    def schema: Seq[ElasticField] = Seq(intField("line"), intField("column"))


  type Names = Set[String]

  object Names:

    given Conversion[BioAmbients.Names, Names] = _.map(_.name).toSet


  trait Act:
    val agent: Option[String]
    val label: Option[String]
    val key: String
    val guard: Boolean
    val rate: Rate

  object Act:

    def schema: Seq[ElasticField] =
      Seq(
        wildcardField("agent"),
        keywordField("label"),
        keywordField("key"),
        booleanField("guard"),
        ObjectField(name = "rate", properties = Rate.schema)
      )

    def apply(key: String): (Option[String], Option[String], String, Boolean) =
      key.split(",") match
        case Array(υidυ, _, _, label, _, agent, _) =>
          val at = label.indexOf('@')
          (Some(agent), Some(label.substring(0, at)), υidυ.stripPrefix("!"), υidυ.startsWith("!"))
        case Array(υidυ) =>
          (None, None, υidυ.stripPrefix("!"), υidυ.startsWith("!"))


  trait Bound:
    val bound: Names

  object Bound:
    def schema: Seq[ElasticField] = Seq(keywordField("bound"))


  trait Free:
    val free: Names

  object Free:
    def schema: Seq[ElasticField] = Seq(keywordField("free"))


  object `$`:
    def schema: Seq[ElasticField] = BioAmbients.`$`.values.map(_.toString).map(objectField(_)).toSeq

  object Cap:
    def schema: Seq[ElasticField] = BioAmbients.Cap.values.map(_.toString).map(objectField(_)).toSeq


  enum Rate derives Codec.AsObject:
    case ∞(weight: Long)
    case `ℝ⁺`(rate: BigDecimal)
    case ⊤(weight: Long)

  object Rate:

    def schema: Seq[ElasticField] =
      Seq(
        ObjectField(name = "∞", properties = Seq(longField("weight"))),
        ObjectField(name = "`ℝ⁺`", properties = Seq(ObjectField(name = "rate", enabled = Some(false)))),
        ObjectField(name = "⊤", properties = Seq(longField("weight")))
      )

    def apply(rate: String): Rate =
      rate.charAt(0) match
        case '∞' => ∞(rate.substring(2, rate.length - 1).toLong)
        case '⊤' => ⊤(rate.substring(2, rate.length - 1).toLong)
        case  _  => `ℝ⁺`(BigDecimal(rate.substring(3, rate.length - 1)))
