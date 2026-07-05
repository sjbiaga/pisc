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

package masc
package emitter
package ce

import scala.meta.*
import dialects.Scala3

import munit.FunSuite

import parser.Calculus.*
import Meta.{ \ => _, * }
import Program.emit
import ProgramSuite.*


class ProgramSuite extends FunSuite:

  // REPLICATION (INPUT) ///////////////////////////////////////////////////////

  /**
    * `! .(name).`
    *
    * @example {{{
    * for {
    *   _υ1υ  <- IO {
    *     def _υ1υ(name: `)(`): IO[Any] = if (!name) IO.cede
    *     else
    *       πLs(for {
    *         name <- ???
    *         _     <- _υ1υ(name)
    *       } yield ()).πparSequence
    *     _υ1υ
    *   }
    *   name <- ???
    *   _     <- _υ1υ(name)
    * } yield ()
    * }}}
    */
  test("replication - (input) guard - no parallelism & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, None, Some("name"), ∅())

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(_),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(Term.Param(Nil, Term.Name(_), Some(Type.Name(")(")), None) :: Nil),
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.If(Term.ApplyUnary(Term.Name("!"), Term.Name("name")),
                                                                   Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                   Term.Select(
                                                                     Term.Apply(
                                                                       Term.Name("πLs"),
                                                                       List(
                                                                         Term.ForYield(List(
                                                                                         Enumerator.Generator(Pat.Var(Term.Name("name")), _),
                                                                                         Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name") :: Nil))
                                                                                       ),
                                                                                       Lit.Unit())
                                                                       )
                                                                     ),
                                                                     Term.Name("πparSequence")
                                                                   )
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Var(Term.Name("name")), _),
                Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name") :: Nil))
      ) => true
    }
  }

  /**
    * `! 13* .(name).`
    *
    * @example {{{
    * for {
    *   _υ2υ  <- Semaphore[IO](13)
    *   _υ1υ  <- IO {
    *     def _υ1υ(name: `)(`): IO[Any] = if (!name) IO.cede
    *     else
    *       πLs(
    *         _υ2υ.release,
    *         for {
    *           name <- ???
    *           _     <- _υ2υ.acquire
    *           _     <- _υ1υ(name)
    *         } yield ()
    *       ).πparSequence
    *     _υ1υ
    *   }
    *   name <- ???
    *   _     <- _υ2υ.acquire
    *   _     <- _υ1υ(name)
    * } yield ()
    * }}}
    */
  test("replication - (input) guard - parallelism >= 0 & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, None, Some("name"), ∅())

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(_), Term.Apply(Term.ApplyType(Term.Name("Semaphore"),
                                                                           Type.Name(`\\`) :: Nil),
                                                            Lit.Int(13) :: Nil)),
                Enumerator.Generator(Pat.Var(_),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(Term.Param(Nil, Term.Name(_), Some(Type.Name(")(")), None) :: Nil),
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.If(Term.ApplyUnary(Term.Name("!"), Term.Name("name")),
                                                                   Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                   Term.Select(
                                                                     Term.Apply(
                                                                       Term.Name("πLs"),
                                                                       List(
                                                                         Term.Select(Term.Name(_), Term.Name("release")),
                                                                         Term.ForYield(List(
                                                                                         Enumerator.Generator(Pat.Var(Term.Name("name")), _),
                                                                                         Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                         Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name") :: Nil))
                                                                                       ),
                                                                                       Lit.Unit())
                                                                       )
                                                                     ),
                                                                     Term.Name("πparSequence")
                                                                   )
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Var(Term.Name("name")), _),
                Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name") :: Nil))
      ) => true
    }
  }

  /**
    * `! 13,seconds .(name).`
    *
    * @example {{{
    * for {
    *   _υ1υ  <- IO {
    *     def _υ1υ(name: `)(`): IO[Any] = if (!name) IO.cede
    *     else
    *       πLs(for {
    *         _     <- IO.sleep(13L.seconds)
    *         name <- ???
    *         _     <- _υ1υ(name)
    *       } yield ()).πparSequence
    *     _υ1υ
    *   }
    *   name <- ???
    *   _     <- _υ1υ(name)
    * } yield ()
    * }}}
    */
  test("replication - (input) guard - no parallelism & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, Some(13L->"seconds"), Some("name"), ∅())

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(_),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(Term.Param(Nil, Term.Name(_), Some(Type.Name(")(")), None) :: Nil),
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.If(Term.ApplyUnary(Term.Name("!"), Term.Name("name")),
                                                                   Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                   Term.Select(
                                                                     Term.Apply(
                                                                       Term.Name("πLs"),
                                                                       List(
                                                                         Term.ForYield(List(
                                                                                         Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                         Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                         Enumerator.Generator(Pat.Var(Term.Name("name")), _),
                                                                                         Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name") :: Nil))
                                                                                       ),
                                                                                       Lit.Unit())
                                                                       )
                                                                     ),
                                                                     Term.Name("πparSequence")
                                                                   )
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Var(Term.Name("name")), _),
                Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name") :: Nil))
      ) => true
    }
  }

  /**
    * `! 13* 13,seconds .(name).`
    *
    * @example {{{
    * for {
    *   _υ2υ  <- Semaphore[IO](13)
    *   _υ1υ  <- IO {
    *     def _υ1υ(name: `)(`): IO[Any] = if (!name) IO.cede
    *     else
    *       πLs(
    *         _υ2υ.release,
    *         for {
    *           _     <- IO.sleep(13L.seconds)
    *           name <- ???
    *           _     <- _υ2υ.acquire
    *           _     <- _υ1υ(name)
    *         } yield ()
    *       ).πparSequence
    *     _υ1υ
    *   }
    *   name <- ???
    *   _     <- _υ2υ.acquire
    *   _     <- _υ1υ(name)
    * } yield ()
    * }}}
    */
  test("replication - (input) guard - parallelism >= 0 & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, Some(13L->"seconds"), Some("name"), ∅())

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(_), Term.Apply(Term.ApplyType(Term.Name("Semaphore"),
                                                                           Type.Name(`\\`) :: Nil),
                                                            Lit.Int(13) :: Nil)),
                Enumerator.Generator(Pat.Var(_),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Def(Nil,
                                                           Term.Name(_),
                                                           Nil,
                                                           List(Term.Param(Nil, Term.Name(_), Some(Type.Name(")(")), None) :: Nil),
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.If(Term.ApplyUnary(Term.Name("!"), Term.Name("name")),
                                                                   Term.Select(Term.Name(`\\`), Term.Name("cede")),
                                                                   Term.Select(
                                                                     Term.Apply(
                                                                       Term.Name("πLs"),
                                                                       List(
                                                                         Term.Select(Term.Name(_), Term.Name("release")),
                                                                         Term.ForYield(List(
                                                                                         Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                         Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                         Enumerator.Generator(Pat.Var(Term.Name("name")), _),
                                                                                         Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                         Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name") :: Nil))
                                                                                       ),
                                                                                       Lit.Unit())
                                                                       )
                                                                     ),
                                                                     Term.Name("πparSequence")
                                                                   )
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Var(Term.Name("name")), _),
                Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Name(_), Term.Name("name") :: Nil))
      ) => true
    }
  }

  /////////////////////////////////////////////////////// replication (input) //

  // REPLICATION (UNGUARDED) ///////////////////////////////////////////////////

  /**
    * `!`
    *
    * @example {{{
    * for {
    *   _υ1υ <- IO {
    *     lazy val _υ1υ: IO[Any] = πLs(for {
    *       _ <- IO.unit
    *       _ <- _υ1υ
    *     } yield ()).πparSequence
    *     _υ1υ
    * } yield ()
    * }}}
    */
  test("replication - unguarded - no parallelism & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, None, None, ∅())

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(_),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Val(Mod.Lazy() :: Nil,
                                                           Pat.Var(Term.Name(_)) :: Nil,
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.Select(
                                                             Term.Apply(
                                                               Term.Name("πLs"),
                                                               List(
                                                                 Term.ForYield(List(
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(`\\`), Term.Name("unit"))),
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Name(_))
                                                                               ),
                                                                               Lit.Unit())
                                                               )
                                                             ),
                                                             Term.Name("πparSequence")
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(), Term.Name(_))
      ) => true
    }
  }

  /**
    * `! 13*`
    *
    * @example {{{
    * for {
    *   _υ2υ <- Semaphore[IO](13)
    *   _υ1υ <- IO {
    *     lazy val _υ1υ: IO[Any] = πLs(
    *       _υ2υ.release,
    *       for {
    *         _ <- IO.unit
    *         _ <- _υ2υ.acquire
    *         _ <- _υ1υ
    *       } yield ()
    *     ).πparSequence
    *     _υ1υ
    *   }
    *   _    <- _υ2υ.acquire
    *   _    <- _υ1υ
    * } yield ()
    * }}}
    */
  test("replication - unguarded - parallelism >= 0 & no pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, None, None, ∅())

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(_), Term.Apply(Term.ApplyType(Term.Name("Semaphore"),
                                                                           Type.Name(`\\`) :: Nil),
                                                            Lit.Int(13) :: Nil)),
                Enumerator.Generator(Pat.Var(_),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Val(Mod.Lazy() :: Nil,
                                                           Pat.Var(Term.Name(_)) :: Nil,
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.Select(
                                                             Term.Apply(
                                                               Term.Name("πLs"),
                                                               List(
                                                                 Term.Select(Term.Name(_), Term.Name("release")),
                                                                 Term.ForYield(List(
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(`\\`), Term.Name("unit"))),
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Name(_))
                                                                               ),
                                                                               Lit.Unit())
                                                               )
                                                             ),
                                                             Term.Name("πparSequence")
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                Enumerator.Generator(Pat.Wildcard(), Term.Name(_))
      ) => true
    }
  }

  /**
    * `! 13,seconds`
    *
    * @example {{{
    * for {
    *   _υ1υ <- IO {
    *     lazy val _υ1υ: IO[Any] = πLs(for {
    *       _ <- IO.unit
    *       _ <- IO.sleep(13L.seconds)
    *       _ <- _υ1υ
    *     } yield ()).πparSequence
    *     _υ1υ
    *   }
    *   _    <- _υ1υ
    * } yield ()
    * }}}
    */
  test("replication - unguarded - no parallelism & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(-1, Some(13L->"seconds"), None, ∅())

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(_),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Val(Mod.Lazy() :: Nil,
                                                           Pat.Var(Term.Name(_)) :: Nil,
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.Select(
                                                             Term.Apply(
                                                               Term.Name("πLs"),
                                                               List(
                                                                 Term.ForYield(List(
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(`\\`), Term.Name("unit"))),
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                 Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Name(_))
                                                                               ),
                                                                               Lit.Unit())
                                                               )
                                                             ),
                                                             Term.Name("πparSequence")
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(), Term.Name(_))
      ) => true
    }
  }

  /**
    * `! 13* 13,seconds`
    *
    * @example {{{
    * for {
    *   _υ2υ <- Semaphore[IO](13)
    *   _υ1υ <- IO {
    *     lazy val _υ1υ: IO[Any] = πLs(
    *       _υ2υ.release,
    *       for {
    *         _ <- IO.unit
    *         _ <- IO.sleep(13L.seconds)
    *         _ <- _υ2υ.acquire
    *         _ <- _υ1υ
    *       } yield ()
    *     ).πparSequence
    *     _υ1υ
    *   }
    *   _    <- _υ2υ.acquire
    *   _    <- _υ1υ
    * } yield ()
    * }}}
    */
  test("replication - unguarded - parallelism >= 0 & nonempty pace") {
    implicit def id: String = "υidυ"

    val `13` = `!`(13, Some(13L->"seconds"), None, ∅())

    //println(`13`.emit.map(_.structure))

    assertMatches(`13`.emit) {
      case List(Enumerator.Generator(Pat.Var(_), Term.Apply(Term.ApplyType(Term.Name("Semaphore"),
                                                                           Type.Name(`\\`) :: Nil),
                                                            Lit.Int(13) :: Nil)),
                Enumerator.Generator(Pat.Var(_),
                                     Term.Apply(Term.Name(`\\`),
                                                Term.Block(
                                                  Defn.Val(Mod.Lazy() :: Nil,
                                                           Pat.Var(Term.Name(_)) :: Nil,
                                                           Some(Type.Apply(Type.Name(`\\`), Type.Name("Any") :: Nil)),
                                                           Term.Select(
                                                             Term.Apply(
                                                               Term.Name("πLs"),
                                                               List(
                                                                 Term.Select(Term.Name(_), Term.Name("release")),
                                                                 Term.ForYield(List(
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(`\\`), Term.Name("unit"))),
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Select(Term.Name(`\\`), Term.Name("sleep")),
                                                                                                                                 Term.Select(Lit.Long(13L), Term.Name("seconds")) :: Nil)),
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                                                                                 Enumerator.Generator(Pat.Wildcard(), Term.Name(_))
                                                                               ),
                                                                               Lit.Unit())
                                                               )
                                                             ),
                                                             Term.Name("πparSequence")
                                                           )
                                                  ) :: Term.Name(_) :: Nil
                                                ) :: Nil)),
                Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name(_), Term.Name("acquire"))),
                Enumerator.Generator(Pat.Wildcard(), Term.Name(_))
      ) => true
    }
  }

  /////////////////////////////////////////////////// replication (unguarded) //


object ProgramSuite:

  val \ = "IO"
