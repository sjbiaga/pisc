package masc

package object parser:

  import Calculus.{ ∅, `<>`, `!`, `[]`, `(*)` }

  type `-` = ∅.type | `<>` | `!` | `[]` | `(*)`

  type `&` = Calculus.`|` | `-`
