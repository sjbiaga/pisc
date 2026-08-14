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
package emitter
package zs

import scala.meta.*
import dialects.Scala3

import parser.Calculus.`(*)`


object Meta extends emitter.shared.effects.Meta:

  override protected lazy val \ = "ZStream"

  override protected lazy val \\ = "succeed"

  protected lazy val \\\ = "fromZIO"

  val `: ZStream[Any, Nothing, Unit]` =
    Some(Type.Apply(\\(\), Type.ArgClause(\\("Any") :: \\("Nothing") :: \\("Unit") :: Nil)))


  def defn(body: Term): `(*)` => Defn.Def =
    case `(*)`("Main") =>
      Defn.Def(Nil,
               "Main", `String*`("args"), `: ZStream[Any, Nothing, Unit]`,
               body)
    case `(*)`(identifier, _params*) =>
      val params = _params.map(_.asSymbol.name)
      Defn.Def(Nil,
               identifier, `(…)`(params*), `: ZStream[Any, Nothing, Unit]`,
               body)


  def `String*`(* : String) =
    Member.ParamClauseGroup(
      Type.ParamClause(Nil),
      Term.ParamClause(Term.Param(Nil, *, Some(Type.Repeated(\\("String"))), None) :: Nil,
                       None) :: `(using String)(using %, /, \\)`
    ) :: Nil

  def `(…)`(* : String*) =
    Member.ParamClauseGroup(
      Type.ParamClause(Nil),
      Term.ParamClause(*
                        .map(Term.Param(Nil, _, Some(\\("()")), None))
                        .toList,
                       None) :: `(using String)(using %, /, \\)`
    ) :: Nil

  val `(using String)(using %, /, \\)` =
    Term.ParamClause(Term.Param(Mod.Using() :: Nil,
                                Name.Anonymous(), Some(\\("String")),
                                None) :: Nil
                    ,Some(Mod.Using())) ::
    Term.ParamClause(List("%", "/", "\\")
                       .map { it => Term.Param(Mod.Using() :: Nil,
                                               Name.Anonymous(), Some(\\(it)),
                                               None)
                       }
                    ,Some(Mod.Using())) ::
    Nil



  def `* <- ZStream.fromZIO(*)`(* : (String, Term)): Enumerator.Generator =
    `* <- *`(*._1 -> Term.Apply(Term.Select(\, \\\), Term.ArgClause(*._2 :: Nil)))

  def `_ <- ZStream.fromZIO(*)`(* : Term): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), Term.Apply(Term.Select(\, \\\), Term.ArgClause(* :: Nil)))

  private val `ZStream.fromZIO`: Term => Boolean =
    case Term.Select(Term.Name(`\\`), Term.Name(`\\\\\\`)) => true
    case Term.Apply(it, _) => `ZStream.fromZIO`(it)
    case Term.ApplyType(it, _) => `ZStream.fromZIO`(it)
    case _ => false

  def `ZStream.fromZIO(…)`(`…`: List[Enumerator]): List[Enumerator] =
    `…`.map {
      case it @ Enumerator.Generator(_, rhs) if `ZStream.fromZIO`(rhs) => it
      case it: Enumerator.Generator => it.copy(rhs = Term.Apply(Term.Select(\, \\\), Term.ArgClause(it.rhs :: Nil)))
      case it => it
    }

  override def `_ <- *.acquire`(* : String): Enumerator =
    `ZStream.fromZIO(…)`(super.`_ <- *.acquire`(*)).head

  override def `_ <- *.release`(* : String): Enumerator =
    `ZStream.fromZIO(…)`(super.`_ <- *.release`(*)).head

  override def `* <- Semaphore(…)`(* : String, `…`: Int): Enumerator =
    `* <- ZStream.fromZIO(*)`(* -> Term.Apply(Term.ApplyType(\("Semaphore"), Type.ArgClause(\\("UIO") :: Nil)),
                                              Term.ArgClause(Lit.Int(`…`) :: Nil))).head


  def `List( *, … ).collectAllPar`(* : Term*): Term =
    *.flatMap {
      case Term.Select(Term.Name(`\\`), Term.Name("unit")) => None
      case it => Some(it)
    } match
      case Nil => \(Nil)
      case it => Term.Select(Term.Apply(\("πLs"), Term.ArgClause(it.toList)), "πcollectAllPar")

  def `List( *, … ).collectAllPar(…)`(* : Term*)(`…`: String): Term =
    *.flatMap {
      case Term.Select(Term.Name(`\\`), Term.Name("unit")) => None
      case it => Some(it)
    } match
      case Nil => \(Nil)
      case it => Term.Apply(Term.Select(Term.Apply(\("πLs"), Term.ArgClause(it.toList)), "πcollectAllPar"),
                            Term.ArgClause(`…` :: Nil))


  val `: String ?=> ZStream[Any, Nothing, Unit]` =
    `: ZStream[Any, Nothing, Unit]`.map(Type.ContextFunction(Type.FuncParamClause(\\("String") :: Nil), _))

  def `\\.\\\\ { def *(*: ()): String ?=> ZStream[Any, Nothing, Unit] = …; * }`(* : (String, String), `…`: Term): Term =
    Term.Apply(Term.Select(\, \\),
               Term.ArgClause(
                 Term.Block(
                   Defn.Def(Nil,
                            *._1,
                            Member.ParamClauseGroup(Type.ParamClause(Nil),
                                                    Term.ParamClause(Term.Param(Nil,
                                                                                *._2,
                                                                                Some(\\("()")),
                                                                                None) :: Nil, None) :: Nil) :: Nil,
                            `: String ?=> ZStream[Any, Nothing, Unit]`,
                            `…`
                   ) :: Term.Ascribe(\(*._1), \\("Π-Function1")) :: Nil
                 ) :: Nil
               )
    )

  def `\\.\\\\ { def *(): String ?=> ZStream[Any, Nothing, Unit] = …; * }`(* : String, `…`: Term): Term =
    Term.Apply(Term.Select(\, \\),
               Term.ArgClause(
                 Term.Block(
                   Defn.Def(Nil,
                            *,
                            Member.ParamClauseGroup(Type.ParamClause(Nil),
                                                    Term.ParamClause(Nil) :: Nil) :: Nil,
                            `: String ?=> ZStream[Any, Nothing, Unit]`,
                            `…`
                   ) :: Term.Ascribe(\(*), \\("Π-Function0")) :: Nil
                 ) :: Nil
               )
    )


  private def `given String = ^._2`(using ^ : (Enumerator.Generator, Term.Name)) =
    Enumerator.Val(Pat.Given(\\("String")), ^._2)

  private def `_ <- +`(parallelism: Int,
                       promise: String,
                       cbarrier: String,
                       name: String,
                       remaining: String,
                       acquire: String,
                       release: String,
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
                        :: Term.Param(Nil, acquire, Some(Type.Apply(\\("Option"), Type.ArgClause(Type.Apply(\\("Semaphore"), Type.ArgClause(\\("UIO") :: Nil)) :: Nil))), None) :: Nil)
       :: Term.ParamClause(Term.Param(Mod.Using() :: Nil,
                                      Name.Anonymous(), Some(\\("String")),
                                      None) :: Nil) :: Nil
        ) :: Nil,
        `: ZStream[Any, Nothing, Unit]`,
        `for * yield ()`(`* <- Semaphore(…)`(release, 0)
                      :: ^._1
                      :: `_ <- *`(Term.Apply(Term.Select(
                                               `for * yield ()`(`_ <- *`(Term.Apply(replication,
                                                                                    Term.ArgClause(\(promise) :: \(cbarrier) :: \(acquire) :: \(release) :: Nil))) :: `given String = ^._2` :: sum*),
                                               "drainFork"),
                                             Term.ArgClause(Term.If(Term.ApplyInfix(\(remaining), \("=="), Type.ArgClause(Nil), Term.ArgClause(Lit.Int(1) :: Nil)),
                                                                    Term.Select(\, "empty"),
                                                                    Term.Apply(Term.Apply(\(name),
                                                                                          Term.ArgClause(Term.ApplyInfix(\(remaining), \("-"), Type.ArgClause(Nil), Term.ArgClause(Lit.Int(1) :: Nil))
                                                                                                      :: Term.Apply(\("Some"), Term.ArgClause(\(release) :: Nil)) :: Nil)),
                                                                               Term.ArgClause(^._2 :: Nil, Some(Mod.Using())))) :: Nil)))*)
      )

    `* <- ZStream.fromZIO(*)`(promise, Term.ApplyType(Term.Select("Promise", "make"), Type.ArgClause(\\("Nothing") :: \\("Boolean") :: Nil))) ::
    `* <- ZStream.fromZIO(*)`(cbarrier, Term.Apply(Term.Select("CyclicBarrier", "make"), Term.ArgClause(Lit.Int(parallelism) :: Nil))) ::
    `* <- *`(name -> Term.Apply(Term.Select(\, \\), Term.ArgClause(Term.Block(definition :: \(name) :: Nil) :: Nil))) ::
    `_ <- *`(Term.Apply(\(name), Term.ArgClause(Lit.Int(parallelism) :: \("None") :: Nil))) :: Nil

  def `_ <- +`(parallelism: Int, replication: Term, sum: List[Enumerator])
              (using id: => String, ^ : (Enumerator.Generator, Term.Name)): List[Enumerator] =
    `_ <- +`(parallelism, id, id, id, id, id, id, replication, sum)


  private def `* <- +`(parameter: String,
                       parallelism: Int,
                       promise: String,
                       cbarrier: String,
                       name: String,
                       remaining: String,
                       acquire: String,
                       release: String,
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
                        :: Term.Param(Nil, acquire, Some(Type.Apply(\\("Option"), Type.ArgClause(Type.Apply(\\("Semaphore"), Type.ArgClause(\\("UIO") :: Nil)) :: Nil))), None) :: Nil)
       :: Term.ParamClause(Term.Param(Mod.Using() :: Nil,
                                      Name.Anonymous(), Some(\\("String")),
                                      None) :: Nil) :: Nil
        ) :: Nil,
        `: ZStream[Any, Nothing, Unit]`,
        `for * yield ()`(`* <- Semaphore(…)`(release, 0)
                      :: ^._1
                      :: `_ <- *`(Term.Apply(Term.Select(
                                               `for * yield ()`(`* <- *`(parameter -> Term.Apply(replication,
                                                                                                 Term.ArgClause(\(promise) :: \(cbarrier) :: \(acquire) :: \(release) :: Nil))) :: `given String = ^._2` :: sum*),
                                               "drainFork"),
                                             Term.ArgClause(Term.If(Term.ApplyInfix(\(remaining), \("=="), Type.ArgClause(Nil), Term.ArgClause(Lit.Int(1) :: Nil)),
                                                                    Term.Select(\, "empty"),
                                                                    Term.Apply(Term.Apply(\(name),
                                                                                          Term.ArgClause(Term.ApplyInfix(\(remaining), \("-"), Type.ArgClause(Nil), Term.ArgClause(Lit.Int(1) :: Nil))
                                                                                                      :: Term.Apply(\("Some"), Term.ArgClause(\(release) :: Nil)) :: Nil)),
                                                                               Term.ArgClause(^._2 :: Nil, Some(Mod.Using())))) :: Nil)))*)
      )

    `* <- ZStream.fromZIO(*)`(promise, Term.ApplyType(Term.Select("Promise", "make"), Type.ArgClause(\\("Nothing") :: \\("Boolean") :: Nil))) ::
    `* <- ZStream.fromZIO(*)`(cbarrier, Term.Apply(Term.Select("CyclicBarrier", "make"), Term.ArgClause(Lit.Int(parallelism) :: Nil))) ::
    `* <- *`(name -> Term.Apply(Term.Select(\, \\), Term.ArgClause(Term.Block(definition :: \(name) :: Nil) :: Nil))) ::
    `_ <- *`(Term.Apply(\(name), Term.ArgClause(Lit.Int(parallelism) :: \("None") :: Nil))) :: Nil

  def `* <- +`(parameter: String, parallelism: Int, replication: Term, sum: List[Enumerator])
              (using id: => String, ^ : (Enumerator.Generator, Term.Name)): List[Enumerator] =
    `* <- +`(parameter, parallelism, id, id, id, id, id, id, replication, sum)
