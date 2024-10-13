package masc

package object parser:

  import Calculus.{ `<>`, `!`, `[]`, `go.`, `⟦⟧`, `{}`, `(*)`, Pre, AST }

  type `-` = `<>` | `!` | `[]` | `go.` | `⟦⟧` | `{}` | `(*)`

  type `&` = Calculus.`|` | `-`

  type `Pre | AST` = Pre | AST

  type `name | process` = String | AST
