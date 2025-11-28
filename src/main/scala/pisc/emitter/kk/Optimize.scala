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
package emitter
package kk

import annotation.tailrec

import scala.collection.mutable.{ LinkedHashMap => Mapʹ }

import scala.meta.*
import dialects.Scala3

import parser.Calculus.`(*)`
import kk.Meta.*


object Optimize:

  final case class Opt(__1: Mapʹ[String, Mapʹ[String, List[String] | String | `(*)`]]):
    def _1 = __1.last._2

  extension (self: Defn.Def)

    def optimize1(using opt: Mapʹ[String, List[String] | String | `(*)`]): (Option[Defn.Def], Boolean) =

      self match

        case Defn.Def(_, Term.Name(name), _, _, Some(Type.Apply(Type.Name("Behavior"), Type.Name("Π") :: Nil)), body) =>

          { val it = opt.get(name); opt -= name; it } match

            case Some(names: List[String]) if names.nonEmpty =>

              val bodyʹ =

                body match

                  case Term.Block(stats) =>

                    Term.Block {

                      names.foldLeft(stats) { (stats, name) =>

                        @tailrec
                        def find(name: String, stats: List[Stat]): Term.Apply =

                          stats.last match

                            case Defn.Def(_, Term.Name(`name`), _, _, Some(Type.Apply(Type.Name("Behavior"), Type.Name("Π") :: Nil)), Term.Block(body)) =>

                              body.last match

                                case Term.Apply(Term.Select(Term.Name("Behaviors"), Term.Name("receive")),
                                            Term.PartialFunction(
                                              Case(Pat.Tuple(Pat.Given(Type.Apply(Type.Name("ActorContext"),
                                                                                  Type.Name("Π") :: Nil)) ::
                                                             Pat.Wildcard() :: Nil),
                                                   None,
                                                   Term.Block(stats)) :: Nil) :: Nil) =>

                                  stats.head match

                                    case Defn.Val(Nil, _, None,
                                                  Term.Apply(Term.Select(Term.Name("given_ActorContext_Π"),
                                                                         Term.Name("spawnAnonymous")), (it @ Term.Apply(Term.Name(identifier), _)) :: Nil)) =>
                                      opt(name) match
                                        case `(*)`(`identifier`, _, _*) => it

                                case Term.Apply(Term.Name(name), _) =>

                                  find(name, body.init)

                            case _ =>

                              find(name, stats.init)

                        def replace(stats: List[Stat], app: Term.Apply): List[Stat] =

                          def replaceʹ(stats: List[Stat]): Option[List[Stat]] =

                            if stats.isEmpty
                            then
                              None

                            else
                              stats.head match

                                case it @ Defn.Val(Nil, _, None,
                                                   Term.Apply(Term.Select(Term.Name("given_ActorContext_Π"),
                                                                          Term.Name("spawnAnonymous")), Term.Apply(Term.Name(`name`), _) :: _)) =>

                                  Some(it.copy(rhs = Term.Apply(Term.Select("given_ActorContext_Π", "spawnAnonymous"),
                                                                Term.ArgClause(app :: Nil))) :: stats.tail)

                                case it @ Term.If(Term.ApplyInfix(_, Term.Name("===="), _, _), Term.Block(t), Term.Block(f)) =>
                                  replaceʹ(stats.tail).map(it :: _)
                                    .orElse(replaceʹ(t).map { t => it.copy(thenp = Term.Block(t)) :: stats.tail })
                                    .orElse(replaceʹ(f).map { f => it.copy(elsep = Term.Block(f)) :: stats.tail })

                                case it =>
                                  replaceʹ(stats.tail).map(it :: _)

                          stats.last match

                            case it @ Term.Apply(Term.Select(Term.Name("Behaviors"), Term.Name("receive")),
                                                 Term.PartialFunction(
                                                   Case(Pat.Tuple(Pat.Given(Type.Apply(Type.Name("ActorContext"),
                                                                                       Type.Name("Π") :: Nil)) ::
                                                                  Pat.Extract(Term.Name("Right"), Pat.Var(Term.Name("it")) :: Nil) :: Nil), None, _) :: _) :: Nil) =>

                              stats.init.last match

                                case df @ Defn.Def(_, _, _, _, Some(Type.Apply(Type.Name("Behavior"), Type.Name("Π") :: Nil)), Term.Block(body)) =>

                                  stats.init.init :+ df.copy(body = Term.Block(replace(body, app))) :+ it

                            case Term.Apply(Term.Select(Term.Name("Behaviors"), Term.Name("receive")),
                                            Term.PartialFunction(
                                              Case(pat @ Pat.Tuple(Pat.Given(Type.Apply(Type.Name("ActorContext"),
                                                                                        Type.Name("Π") :: Nil)) :: _),
                                                   None,
                                                   (it: Term.Block)) :: Nil) :: Nil) =>

                              val itʹ = Term.Block(replaceʹ(it.stats).get)

                              stats.init :+ Term.Apply(Term.Select("Behaviors", "receive"),
                                                       Term.ArgClause(Term.PartialFunction(Case(pat, None, itʹ) :: Nil) :: Nil))

                            case it: Term.Apply =>
                              replace(stats.init, app) :+ it

                        replace(stats, find(name, stats.init))

                      }.flatMap {
                        case it: Defn.Def => it.optimize1._1
                        case it => Some(it)
                      }

                    }

              Some(self.copy(body = bodyʹ)) -> true

            case Some(nameʹ: String) =>
              opt -= nameʹ
              None -> true

            case Some(_: `(*)`) =>
              None -> true

            case _ =>

              var optimized = false

              val bodyʹ =

                body match

                  case Term.Block(stats) =>

                    val statsʹ = stats.flatMap {
                      case it: Defn.Def =>
                        val (df, opt) = it.optimize1
                        optimized ||= opt
                        df
                      case it => Some(it)
                    }

                    if optimized then Term.Block(statsʹ)
                    else body

                  case _ => body // cases sum

              ( if optimized
                then Some(self.copy(body = bodyʹ))
                else Some(self)
              ) -> optimized

        case _ => Some(self) -> false
