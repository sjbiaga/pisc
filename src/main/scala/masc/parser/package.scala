package masc

package object parser:

  import Calculus.{ ∅, `<>`, `!`, `[]`, `(*)`, Pre, AST }

  type `-` = ∅.type | `<>` | `!` | `[]` | `(*)`

  type `&` = Calculus.`|` | `-`

  type `Pre | AST` = Pre | AST
