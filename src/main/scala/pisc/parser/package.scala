package pisc

package object parser:

  import Calculus.{ `+`, `?:`, `{}`, `(*)`, `!`, `⟦⟧`, π, τ, λ, Pre, AST }

  import StochasticPi.{ Act, Sum }

  type `-` = `?:` | `{}` | `(*)` | `!` | `⟦⟧`

  type `&` = `+` | `-`

  type μ = π | τ

  type `Pre | AST` = Pre | AST

  type `Act | Sum` = Act | Sum

  type `name | process` = λ | AST
