package masc
package parser


package object ast:

  import AST.{ ∥, <>, !, `[]`, `go.`, `⟦⟧`, `{}`, `(*)` }

  type Bind = (`(*)`, ∥)

  type - = <> | ! | `[]` | `go.` | `⟦⟧` | `{}` | `(*)`


  trait Positional extends scala.util.parsing.input.Positional:
    val lc: Int => (Int, Int)

    final def location: (Int, Int) = lc(pos.column-1)


  object Cast:

    given [T <: Cap]: Conversion[Cap, T] = _.asInstanceOf[T]

    given [T <: Pre]: Conversion[Pre, T] = _.asInstanceOf[T]

    given [T <: AST]: Conversion[AST, T] = _.asInstanceOf[T]
