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
package generator

import java.util.UUID

import scala.meta._

import parser.Calculus._


object Program {

  def apply(bind: List[Bind]): List[String] =
    bind.map { case (bind, sum) => defn(bind, sum).toString }


  type Code = (Boolean, Option[String])
  //           ^^^^^^^  ^^^^^^^^^^^^^
  //           for-c.,  semaphore


  def defn(bind: `()`, sum: Sum): Defn.Def = {
    val identifier = bind.identifier.asSymbol.name
    val params = bind.params.map(_.asSymbol.name)

    Defn.Def(Nil,
             \(identifier), `()`(params: _*), `: IO[Unit]`,
             body(false -> None, sum).left.get)
  }


  def body(code: Code, node: AST): Either[Term.ForYield, List[Enumerator.Generator]] = {
    val (comprehension, semaphore) = code

    node match {

      // SUMMATION /////////////////////////////////////////////////////////////

      case it @ Sum(operand, _, _*) =>
        var * = List[Enumerator.Generator]()

        semaphore.map(* :+= `_ <- *.acquire`(_))

        val cs = false -> Some(uuid)

        * :+= `* <- Semaphore[IO](1)`(cs._2.get)

        * :+= `_ <- IO.race ( *, … )`(body(cs, operand).left.get ->
                                      body(cs, Sum(it.choices.tail)).left.get)

        if (comprehension)
          Right(*)
        else
          Left(`for * yield ()`(* : _*))

      case Sum(operand, _*) =>
        body(code, operand)

      case _: Sum => ???

      ///////////////////////////////////////////////////////////// summation //


      // COMPOSITION ///////////////////////////////////////////////////////////

      case it @ Par(_, _, _*) =>
        var * = List[Enumerator.Generator]()

        semaphore.map(* :+= `_ <- *.acquire`(_))

        val cs = false -> None

        val fy = it.components.foldLeft(List[Term.ForYield]())(_ :+ body(cs, _).left.get)

        * :+= `_ <- *`(`( *, … ).parMapN { (_, …) => }`(fy: _*))

        if (comprehension)
          Right(*)
        else
          Left(`for * yield ()`(* : _*))

      case Par(operand, _*) =>
        body(code, operand)

      case _: Par if comprehension =>
        Right(Nil)

      case _: Par =>
        var * = List[Enumerator.Generator]()

        * :+= `_ <- IO.unit`

        semaphore.map(* :+= `_ <- *.acquire`(_))

        Left(`for * yield ()`(* : _*))

      /////////////////////////////////////////////////////////// composition //


      // RESTRICTION | PREFIXES | (MIS)MATCH | IF THEN ELSE | REPLICATION //////

      case `ν`(Opd(Symbol(name))) =>
        Right(`* <- *`(name -> "ν"))

      case `τ` =>
        Right(`_ <- *`("τ"))


      case IO(Opd(Symbol(_)), par, true) if !par.isSymbol => ??? // not binding a name - caught by parser

      case IO(ch,  _, _) if !ch.isSymbol => ??? // not a channel name - caught by parser

      case IO(Opd(Symbol(ch)), Opd(Symbol(arg)), false) =>
        Right(`_ <- *`(s"$ch($arg)".parse[Term].get))

      case IO(Opd(Symbol(ch)), Opd(Expr(expr)), false) =>
        Right(`_ <- *`(s"$ch($expr)".parse[Term].get))

      case IO(Opd(Symbol(ch)), Opd(arg), false) =>
        Right(`_ <- *`(s"$ch($arg)".parse[Term].get))

      case IO(Opd(Symbol(ch)), Opd(Symbol(par)), true) =>
        Right(`* <- *`(par -> s"$ch()".parse[Term].get))


      case `[]`(((Opd(lhs), Opd(rhs)), mismatch), sum) =>
        val cs = false -> None

        if (mismatch)
          Right(`_ <- *`(`if * then IO.cede else …`(===(lhs -> rhs), body(cs, sum).left.get)))
        else
          Right(`_ <- *`(`if !* then IO.cede else …`(===(lhs -> rhs), body(cs, sum).left.get)))


      case `?:`(((Opd(lhs), Opd(rhs)), mismatch), t, f) =>
        val cs = false -> None

        if (mismatch)
          Right(`_ <- *`(`if !* then … else …`(===(lhs -> rhs), body(cs, f).left.get -> body(cs, t).left.get)))
        else
          Right(`_ <- *`(`if * then … else …`(===(lhs -> rhs), body(cs, t).left.get -> body(cs, f).left.get)))


      case `!`(sum) =>
        var * = List[Enumerator.Generator]()

        val cs = false -> None

        val name = uuid

        val it =
          `for * yield ()` {
            `_ <- *` {
              `( *, … ).parMapN { (_, …) => }`(
                body(cs, sum).left.get,
                `for * yield ()`(`_ <- IO.unit`, `_ <- *`(name))
              )
            }
          }

        * :+= `* <- *`("pi", `IO { lazy val *: IO[Unit] = …; * }`(name, it))
        * :+= `_ <- *`("pi")

        if (comprehension)
          Right(*)
        else
          Left(`for * yield ()`(* : _*))

      ////// restriction | prefixes | (mis)match | if then else | replication //


      // AGENT CALL ////////////////////////////////////////////////////////////

      case `()`(Opd(Symbol(identifier)), qual, params @ _*) =>
        var * = List[Enumerator.Generator]()

        semaphore.map(* :+= `_ <- *.acquire`(_))

        val args = params.map {
          case Opd(Symbol(name)) => name
          case Opd(value) =>
            value match {
              case it: BigDecimal => s"BigDecimal($it)"
              case it: String => s"$it"
              case Expr(it) => s"$it"
            }
        }

        if (qual.isEmpty)
          * :+= `_ <- *`(s"`$identifier`(${args.mkString(", ")})".parse[Term].get)
        else
          * :+= `_ <- *`(s"${qual.mkString(".")}.`π`.`$identifier`(${args.mkString(", ")})".parse[Term].get)

        if (comprehension)
          Right(*)
        else
          Left(`for * yield ()`(* : _*))

      case _: `()` => ??? // impossible by syntax

      //////////////////////////////////////////////////////////// agent call //


      // SEQUENCE //////////////////////////////////////////////////////////////
      // followed possibly either by agent call or another process expression //

      case Seq(ast, it @ _*) if it.isEmpty =>
        body(code, ast)

      case Seq(ast, it @ _*) =>
        var * = List[Enumerator.Generator]()

        * :+= `_ <- IO.unit`

        semaphore.map(* :+= `_ <- *.acquire`(_))

        val cs = true -> None

        * = (it :+ ast).foldLeft(*)(_ ++ body(cs, _).right.get)

        if (comprehension)
          Right(*)
        else
          Left(`for * yield ()`(* : _*))

      ////////////////////////////////////////////////////////////// sequence //

      case it => ???

    }

  }

  def uuid = UUID.randomUUID.toString

  def === : ((AnyRef, AnyRef)) => Term = {
    case (Symbol(x), Symbol(y)) => s"$x === $y".parse[Term].get
    case (Symbol(x), y: String) => s"$x === $y".parse[Term].get
    case (Symbol(x), Expr(y)) => s"$x === $y".parse[Term].get
    case (x: String, Symbol(y)) => s"$x === $y".parse[Term].get
    case (Expr(x), Symbol(y)) => s"$x === $y".parse[Term].get
    case (x: String, y: String) => s"$x === $y".parse[Term].get
    case (x: String, Expr(y)) => s"$x === $y".parse[Term].get
    case (Expr(x), y: String) => s"$x === $y".parse[Term].get
  }


  @inline implicit def \(* : Enumerator.Generator): List[Enumerator.Generator] = * :: Nil

  @inline implicit def \(* : String): Term.Name = Term.Name(*)


  def `()`(* : String*) =
    Member.ParamClauseGroup(
      Type.ParamClause(Nil), // []
      Term.ParamClause(*
                        .map(\(_))
                        .map(Term.Param(Nil, _, Some(Type.Name("()")), None))
                        .toList,
                       None) :: Nil
    ) :: Nil


  def `:`(name: String, clause: String): Option[Type.Apply] =
    Some(Type.Apply(Type.Name(name), Type.ArgClause(Type.Name(clause) :: Nil)))


  val `: IO[Unit]` = `:`("IO", "Unit")


  def `* <- …`(* : String*): Pat =
    if (*.size == 0)
      Pat.Wildcard()
    else if (*.size == 1)
      Pat.Var(\(*.head))
    else
      Pat.Tuple(*.map(\(_)).map(Pat.Var(_)).toList)


  def `* <- *`(* : (String, Term)): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(*._1), *._2)


  val `_ <- IO.unit` = `_ <- IO.*`("unit")


  def `_ <- *`(* : Term): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), *)


  def `_ <- IO.*`(* : String): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), Term.Select(\("IO"), \(*)))


  def `for * yield ()`(* : Enumerator.Generator*): Term.ForYield =
    Term.ForYield(*.toList, Lit.Unit())


  def `_ <- *.acquire`(* : String): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), Term.Select(\(*), \("acquire")))


  def `* <- Semaphore[IO](1)`(* : String): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(*),
                         Term.Apply(Term.ApplyType(\("Semaphore"),
                                                   Type.ArgClause(Type.Name("IO") :: Nil)),
                                    Term.ArgClause(Lit.Int(1) :: Nil, None)
                         )
    )


  def `_ <- IO.race ( *, … )`(* : (Term.ForYield, Term.ForYield)): Enumerator.Generator =
    `_ <- *`(Term.Apply(Term.Select(\("IO"), \("race")),
                        Term.ArgClause(List(*._1, *._2), None)))


  def `( *, … ).parMapN { (_, …) => }`(* : Term.ForYield*): Term =
    Term.Apply(
      Term.Select(Term.Tuple(*.toList), \("parMapN")),
      Term.ArgClause(Term.Block(
                       Term.Function(
                         Term.ParamClause(
                           List.fill(*.size)(Term.Param(Nil, Name.Placeholder(), None, None)),
                           None
                         ),
                         Term.Block(Nil)
                       ) :: Nil
                     ) :: Nil,
                     None
      )
    )


  def `if * then IO.cede else …`(* : Term, `…`: Term.ForYield): Term.If =
    Term.If(*, Term.Select(\("IO"), \("cede")), `…`, Nil)

  def `if !* then IO.cede else …`(* : Term, `…`: Term.ForYield): Term.If =
    Term.If(Term.ApplyUnary(\("!"), *), Term.Select(\("IO"), \("cede")), `…`, Nil)


  def `if * then … else …`(* : Term, `…`: (Term.ForYield, Term.ForYield)): Term.If =
    Term.If(*, `…`._1, `…`._2, Nil)

  def `if !* then … else …`(* : Term, `…`: (Term.ForYield, Term.ForYield)): Term.If =
    Term.If(Term.ApplyUnary(\("!"), *), `…`._1, `…`._2, Nil)


  def `IO { lazy val *: IO[Unit] = …; * }`(* : String, `…`: Term.ForYield): Term =
    Term.Apply(\("IO"),
               Term.ArgClause(Term.Block(
                                Defn.Val(Mod.Lazy() :: Nil,
                                         `* <- …`(*) :: Nil,
                                         `: IO[Unit]`,
                                         `…`
                                ) :: \(*) :: Nil
                              ) :: Nil,
                              None
               )
    )

}
