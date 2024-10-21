package pisc

package object parser:

  import Calculus.{ `+`, `?:`, `{}`, `(*)`, `!`, `⟦⟧`, π, τ, λ, Pre, AST }

  type `-` = `?:` | `{}` | `(*)` | `!` | `⟦⟧`

  type `&` = `+` | `-`

  type μ = π | τ

  type `Pre | AST` = Pre | AST

  type `name | process` = λ | AST
