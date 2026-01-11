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
package fs2

import scala.meta.*
import dialects.Scala3

import parser.Calculus.`(*)`


object Meta extends emitter.shared.streams.Meta:

  def defn(body: Term): `(*)` => Defn.Def =
    case `(*)`("Main") =>
      Defn.Def(Nil,
               "Main", `String*`("args"), `: \\[F, Unit]`,
               body)
    case `(*)`(identifier, _params*) =>
      val params = _params.map(_.asSymbol.name)
      Defn.Def(Nil,
               identifier, `(…)`(params*), `: \\[F, Unit]`,
               body)


  def `String*`(* : String) =
    Member.ParamClauseGroup(
      Type.ParamClause(Nil),
      Term.ParamClause(Term.Param(Nil, *, Some(Type.Repeated(\\("String"))), None) :: Nil,
                       None) :: `(using String)(using %[F], /[F], \\[F])`
    ) :: Nil

  def `(…)`(* : String*) =
    Member.ParamClauseGroup(
      Type.ParamClause(Nil),
      Term.ParamClause(*
                        .map(Term.Param(Nil, _, Some(Type.Apply(\\("()"), Type.ArgClause(\\("F") :: Nil))), None))
                        .toList,
                       None) :: `(using String)(using %[F], /[F], \\[F])`
    ) :: Nil

  val `(using String)(using %[F], /[F], \\[F])` =
    Term.ParamClause(Term.Param(Mod.Using() :: Nil,
                                Name.Anonymous(), Some(\\("String")),
                                None) :: Nil
                    ,Some(Mod.Using())) ::
    Term.ParamClause(List("%", "/", "\\")
                       .map { it => Term.Param(Mod.Using() :: Nil,
                                               Name.Anonymous(), Some(Type.Apply(\\(it), Type.ArgClause(\\("F") :: Nil))),
                                               None)
                       }
                    ,Some(Mod.Using())) ::
    Nil


  def `List( *, … ).parSequence`(* : Term*): Term =
    *.flatMap {
      case Term.Select(Term.Name(`\\`), Term.Name("unit")) => None
      case it => Some(it)
    } match
      case Nil => \(Nil)
      case it => Term.Select(Term.Apply(\("πLs"), Term.ArgClause(it.toList)), "πparSequence")


  val `: String => \\[F, Unit]` =
    `: \\[F, Unit]`.map(Type.Function(Type.FuncParamClause(\\("String") :: Nil), _))

  def `\\.emit { def *(*: ()[F]): String => \\[F, Unit] = { implicit ^ => … }; * }`(* : (String, String), `…`: Term): Term =
    Term.Apply(Term.Select(\, "emit"),
               Term.ArgClause(
                 Term.Block(
                   Defn.Def(Nil,
                            *._1,
                            Member.ParamClauseGroup(Type.ParamClause(Nil),
                                                    Term.ParamClause(Term.Param(Nil,
                                                                                *._2,
                                                                                Some(Type.Apply(\\("()"), Type.ArgClause(\\("F") :: Nil))),
                                                                                None) :: Nil, None) :: Nil) :: Nil,
                            `: String => \\[F, Unit]`,
                            Term.Block(
                              Term.Function(
                                Term.ParamClause(Term.Param(Mod.Implicit() :: Nil,
                                                            "^",
                                                            None,
                                                            None) :: Nil, None),
                                `…`
                              ) :: Nil
                            )
                   ) :: \(*._1) :: Nil
                 ) :: Nil
               )
    )

  def `\\.emit { lazy val *: String => \\[F, Unit] = { implicit ^ => … }; * }`(* : String, `…`: Term): Term =
    Term.Apply(Term.Select(\, "emit"),
               Term.ArgClause(Term.Block(
                                Defn.Val(Mod.Lazy() :: Nil,
                                         `* <- …`(*) :: Nil,
                                         `: String => \\[F, Unit]`,
                                         Term.Block(
                                           Term.Function(
                                             Term.ParamClause(Term.Param(Mod.Implicit() :: Nil,
                                                                         "^",
                                                                         None,
                                                                         None) :: Nil, None),
                                             `…`
                                           ) :: Nil
                                         )
                                ) :: \(*) :: Nil
                              ) :: Nil
               )
    )


  private def `_ <- +`(parallelism: Int,
                       deferred: String,
                       cbarrier: String,
                       name: String,
                       remaining: String,
                       take: String,
                       offer: String,
                       replication: Term,
                       sum: List[Enumerator])
                      (using ^ : (Enumerator.Generator, Term.Name)): List[Enumerator] =
    val definition =
      Defn.Def(
        Nil,
        name,
        Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          Term.ParamClause(Term.Param(Nil, remaining, Some(\\("Int")), None)
                        :: Term.Param(Nil, take, Some(Type.Apply(\\("Option"), Type.ArgClause(Type.Apply(\\("Queue"), Type.ArgClause(\\("F") :: \\("Unit") :: Nil)) :: Nil))), None) :: Nil)
       :: Term.ParamClause(Term.Param(Mod.Using() :: Nil,
                                      Name.Anonymous(), Some(\\("String")),
                                      None) :: Nil) :: Nil
        ) :: Nil,
        `: \\[F, Unit]`,
        Term.If(Term.ApplyInfix(\(remaining), \("=="), Type.ArgClause(Nil), Term.ArgClause(Lit.Int(1) :: Nil)),
                `for * yield ()`(`_ <- *`(Term.Apply(replication, Term.ArgClause(\(deferred) :: \(cbarrier) :: \(take) :: \("None") :: Nil))) :: sum*),
                `for * yield ()`(`* <- Stream.eval(*)`(offer,
                                                       Term.Apply(Term.ApplyType(Term.Select("Queue", "bounded"),
                                                                                 Type.ArgClause(\\("F") :: \\("Unit") :: Nil)),
                                                                  Term.ArgClause(Lit.Int(1) :: Nil)))
                              :: ^._1
                              :: `_ <- *`(Term.Apply(Term.Select(
                                                       `for * yield ()`(`_ <- *`(Term.Apply(replication,
                                                                                            Term.ArgClause(\(deferred) :: \(cbarrier) :: \(take) :: Term.Apply(\("Some"), Term.ArgClause(\(offer) :: Nil)) :: Nil))) :: sum*),
                                                       "concurrently"),
                                                     Term.ArgClause(Term.Apply(Term.Apply(\(name),
                                                                                          Term.ArgClause(Term.ApplyInfix(\(remaining), \("-"), Type.ArgClause(Nil), Term.ArgClause(Lit.Int(1) :: Nil))
                                                                                                      :: Term.Apply(\("Some"), Term.ArgClause(\(offer) :: Nil)) :: Nil)),
                                                                               Term.ArgClause(^._2 :: Nil, Some(Mod.Using()))) :: Nil)))*)
        )
    )

    `* <- Stream.eval(*)`(deferred, `*[F]`("Deferred", \\("Boolean"))) ::
    `* <- Stream.eval(*)`(cbarrier, Term.Apply(`*[F]`("CyclicBarrier"), Term.ArgClause(Lit.Int(parallelism) :: Nil))) ::
    `* <- *`(name -> Term.Apply(Term.Select(\, "emit"), Term.ArgClause(Term.Block(definition :: \(name) :: Nil) :: Nil))) ::
    `_ <- *`(Term.Apply(\(name), Term.ArgClause(Lit.Int(parallelism) :: \("None") :: Nil))) :: Nil

  def `_ <- +`(parallelism: Int, replication: Term, sum: List[Enumerator])
              (using id: => String, ^ : (Enumerator.Generator, Term.Name)): List[Enumerator] =
    `_ <- +`(parallelism, id, id, id, id, id, id, replication, sum)


  private def `* <- +`(parameter: String,
                       parallelism: Int,
                       deferred: String,
                       cbarrier: String,
                       name: String,
                       remaining: String,
                       take: String,
                       offer: String,
                       replication: Term,
                       sum: List[Enumerator])
                      (using ^ : (Enumerator.Generator, Term.Name)): List[Enumerator] =
    val definition =
      Defn.Def(
        Nil,
        name,
        Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          Term.ParamClause(Term.Param(Nil, remaining, Some(\\("Int")), None)
                        :: Term.Param(Nil, take, Some(Type.Apply(\\("Option"), Type.ArgClause(Type.Apply(\\("Queue"), Type.ArgClause(\\("F") :: \\("Unit") :: Nil)) :: Nil))), None) :: Nil)
       :: Term.ParamClause(Term.Param(Mod.Using() :: Nil,
                                      Name.Anonymous(), Some(\\("String")),
                                      None) :: Nil) :: Nil
        ) :: Nil,
        `: \\[F, Unit]`,
        Term.If(Term.ApplyInfix(\(remaining), \("=="), Type.ArgClause(Nil), Term.ArgClause(Lit.Int(1) :: Nil)),
                `for * yield ()`(`* <- *`(parameter -> Term.Apply(replication, Term.ArgClause(\(deferred) :: \(cbarrier) :: \(take) :: \("None") :: Nil))) :: sum*),
                `for * yield ()`(`* <- Stream.eval(*)`(offer,
                                                       Term.Apply(Term.ApplyType(Term.Select("Queue", "bounded"),
                                                                                 Type.ArgClause(\\("F") :: \\("Unit") :: Nil)),
                                                                  Term.ArgClause(Lit.Int(1) :: Nil)))
                              :: ^._1
                              :: `_ <- *`(Term.Apply(Term.Select(
                                                       `for * yield ()`(`* <- *`(parameter -> Term.Apply(replication,
                                                                                                         Term.ArgClause(\(deferred) :: \(cbarrier) :: \(take) :: Term.Apply(\("Some"), Term.ArgClause(\(offer) :: Nil)) :: Nil))) :: sum*),
                                                       "concurrently"),
                                                     Term.ArgClause(Term.Apply(Term.Apply(\(name),
                                                                                          Term.ArgClause(Term.ApplyInfix(\(remaining), \("-"), Type.ArgClause(Nil), Term.ArgClause(Lit.Int(1) :: Nil))
                                                                                                      :: Term.Apply(\("Some"), Term.ArgClause(\(offer) :: Nil)) :: Nil)),
                                                                               Term.ArgClause(^._2 :: Nil, Some(Mod.Using()))) :: Nil)))*)
        )
    )

    `* <- Stream.eval(*)`(deferred, `*[F]`("Deferred", \\("Boolean"))) ::
    `* <- Stream.eval(*)`(cbarrier, Term.Apply(`*[F]`("CyclicBarrier"), Term.ArgClause(Lit.Int(parallelism) :: Nil))) ::
    `* <- *`(name -> Term.Apply(Term.Select(\, "emit"), Term.ArgClause(Term.Block(definition :: \(name) :: Nil) :: Nil))) ::
    `_ <- *`(Term.Apply(\(name), Term.ArgClause(Lit.Int(parallelism) :: \("None") :: Nil))) :: Nil

  def `* <- +`(parameter: String, parallelism: Int, replication: Term, sum: List[Enumerator])
              (using id: => String, ^ : (Enumerator.Generator, Term.Name)): List[Enumerator] =
    `* <- +`(parameter, parallelism, id, id, id, id, id, id, replication, sum)
