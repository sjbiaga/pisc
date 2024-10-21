package pixc

package object parser:

  import Calculus.{ +, ?:, `{}`, `(*)`, !, `⟦⟧`, π, τ }

  type - = ?: | `{}` | `(*)` | ! | `⟦⟧`

  type & = + | -

  type μ = π | τ
