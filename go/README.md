Stochastic Pi-calculus in SCala aka sPISC ala RISC
==================================================

The π-calculus maps one to one on `Gala` monadic notation
"inside" the `Option` monad - which is not stack safe.

After code generation, the π-calculus "processes" could be
programmatically typed as `Gala` code, which further
transpiles to `Go`, which is very fast.


Program
-------

Agent identifiers (literals) start with uppercase, while
channel names start with lowercase.


Programs (examples)
---------------

The `examples` folder *must* have three sub-folders:

    ./examples/
       pisc/
       in/
       out/

The root project folder `go` contains four files: `spi.gala`, `spim.gala`, `import.gala.in`, and `main.gala.in`.

!!!Warning: do not delete them!!!

One can edit'em, though they're ready to generate a main program.

To get and run the examples, one can `source` the functions from `bin/spi.sh`.

To run an example, `cd` to `examples` and execute:

    ./examples $ spi -go ex.gala

To get the final source folder `ex` (from `out/ex.gala.out`), run:

    ./examples $ spio -go ex

To get the intermediary `in/ex.gala.in` file, execute the `pin` command in the `sbt` shell:

    sbt:Stochastic π-Calculus2Scala> spin -go ex

where `example/pisc/ex.pisc` contains the stochastic π-calculus source (equations binding
agents to process expressions).

Note that [Gala](https://github.com/martianoff/gala) must be installed and the command `gala`
available (perhaps `alias`ed). Also, do a `go clean -cache` from time to time.
