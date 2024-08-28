package masc

package object parser:

  import Calculus.{ ∅, `<>`, `!`, `[]`, `go.`, `(*)`, Pre, AST }

  type `-` = ∅.type | `<>` | `!` | `[]` | `go.` | `(*)`

  type `&` = Calculus.`|` | `-`

  type `Pre | AST` = Pre | AST
