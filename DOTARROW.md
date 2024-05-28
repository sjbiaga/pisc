DotArrow
========

`DotArrow` is the codename for "_mobile code_". It is implemented in a very simplistic
fashion, for either `Scala` or `Haskell`.


DotArrow - `Scala`
------------------

It is possible for `Scala` in [`PISC`](https://github.com/sjbiaga/pisc) because
of three reasons:

- input actions allow for the received value to be applied a function before returning
  the actual result - so that an entire `Scala` source file passed as a `String` can be
  updated through `scala.sys.process._` facility to one modified as follows;

- the `IO` monad in the "mobile code" allows for a sequence of `flatMap`s to be chained
  and - if wanted - to be `IO.canceled` at any point between two `flatMap`s;

- [`Scalameta`](https://scalameta.org) allows for `Enumerator.Generator`s to be
  manipulated such that `IO.canceled` is inserted after each `flatMap` with a
  pattern variable with ascribed type.

Also, `Serializable` (`case`) `class`es allow for objects of type other than basic
types to be read and written using `ObjectInputStream` and `ObjectOutputStream`.

Alternatively, objects can be encoded to/decoded from `JSON` using the `Scala` library
[circe](https://circe.github.io/circe/).

The `bin/pi.sh` shell script has been added, respectively, two functions:

- `dotarrowCirce` and `dotarrowCirce2`;
- `dotarrowStream` and `dotarrowStream2`.

The first is used to insert `_ <- IO.canceled` cancellation points, while the second
is used to transform the `Scalameta` `AST` by dropping the first `Enumerator.Generator`
together with "its" `IO.canceled.`

The `examples/dotarrow` folder *must* have two sub-folders:

    ./examples/dotarrow/
       src/
       tmp/

The `Scala` source files go in the `./examples/dotarrow/` folder.

The (parent) root folder contains three files for "`stream`" serialization:

    ../dotarrow/stream/
       app.scala.in
       app2.scala.in
       bin.in

The first two are used in the two shell functions, `dotarrowStream`, respectively,
`dotarrowStream2`, while the third is copied initially as the object serialization
(temporary) binary file.

And three files for "`circe`" `JSON` serialization:

    ../dotarrow/circe/
       app.scala.in
       app2.scala.in
       json.in

The first two are used in the two shell functions, `dotarrowCirce`, respectively,
`dotarrowCirce2`, while the third is copied initially as the `JSON` serialization
(temporary) text file.

!!!Warning: do not delete them!!!

The `examples` folder contains a `dotarrow_stream_ex0.pisc` file. It contains embedded
`Scala` code that uses the `scala.sys.process._` facility to execute `Scala` code
as it is reduced to `_ <- IO { ... }.void`; the final value `196.0` can be seen.

To "execute" the "mobile code", parse the `.pisc` file:

    sbt:π-Calculus2Scala> run dotarrow_stream_ex0

Choose one example from the `examples/dotarrow/` folder, e.g., `ex3.scala`;
then - in a shell -, `cd` to `examples` folder, and run:

    ./examples $ pio dotarrow_stream_ex0
    ./examples $ pi dotarrow_stream_ex0.scala -- ex3

Note how "`ex3`" was passed as an argument to the `main` method in `dotarrow_stream_ex0.scala`.

Try the same, but using `circe`:

    sbt:π-Calculus2Scala> run dotarrow_circe_ex0
    ./examples $ pio dotarrow_circe_ex0
    ./examples $ pi dotarrow_circe_ex0.scala -- ex4

Or both:

    sbt:π-Calculus2Scala> run dotarrow_stream_circe_ex_3_4
    ./examples $ pio dotarrow_stream_circe_ex_3_4
    ./examples $ pi dotarrow_stream_circe_ex_3_4.scala

The `Scala` source files must have a strict format:

- the main object must be `object App extends IOApp.Simple`;

- the main method must be `override def run: IO[Unit]`;

- the body of the main method must be a `for-yield` comprehension;

- variable-bound generators of the `for-yield` must bind a single variable *and*
  be ascribed the type;

- names of variable-bound generators must not contain whitespace; their types may;

- the scope of pattern variables in definitions and generators is limited until
  the first variable-bound with ascribed type generator;

- "`if`" guards are impossible as "`withFilter`" lacks from the `IO` monad;

- the last `for-yield` statement must be `_ <- IO { ... }.void`;

- must not output to `scala.Console.err`;

- characters in the `Scala` source files are not supported;

- in addition, for `JSON` serialization with `circe`, the implicit
  `Encoder`s/`Decoder`s must be in the scope of the main method.


DotArrow - `Haskell`
--------------------

It is possible for `Haskell` in [`PISC`](https://github.com/sjbiaga/pisc) because
of three reasons - two as for `Scala` and third:

- [`TemplateHaskell`](https://hackage.haskell.org/package/template-haskell) allows
  for `BindS`s to be manipulated such that `exitFailure` is inserted after each
  `do`-statement with a pattern variable with type signature.

Also, values can be encoded to/decoded from `JSON` using the `Haskell` library
[aeson](https://hackage.haskell.org/package/aeson).

The `bin/pi.sh` shell script has been added two functions:

- `dotarrowAeson` and `dotarrowAeson2`.

To "execute" the "mobile code", parse the `.pisc` file:

    sbt:π-Calculus2Scala> run dotarrow_aeson_ex0

Choose one example from the `examples/dotarrow/` folder, e.g., `ex1/app/Main.hs`;
then - in a shell -, `cd` to `examples` folder, and run:

    ./examples $ pio dotarrow_aeson_ex0
    ./examples $ pi dotarrow_aeson_ex0.scala -- ex1

Note how "`ex1`" was passed as an argument to the `main` method in `dotarrow_aeson_ex0.scala`,
while the implied (see below) "`app/Main.hs`" was omitted.

[`Haskell Tool Stack`](https://hackage.haskell.org/package/stack) must be used and the
folders (e.g., "`ex1`") must have a `.cabal` build file; the command line launched
by `PISC` is, e.g.,

    stack run -- /path/to/pisc/examples/dotarrow/tmp.../app/Main.hs

from either two `dotarrow` - with "`stack-cabal`" setup - sub-folders:

    ./dotarrow/aeson
    ./dotarrow/aeson2

copied to, respectively:

    /path/to/pisc/examples/dotarrow/tmp.../tmp/aeson
    /path/to/pisc/examples/dotarrow/tmp.../tmp/aeson2

folders.

The `Haskell` source files must have a strict format:

- the executable source file must be `app/Main.hs` with module `Main`;

- among `import`s must be the following:

        import System.Exit (exitFailure)
        import System.IO (hPutStrLn, stderr)
        import Inp_gUgVwYdD8r
        import Out_gUgVwYdD8r

  where modules `Inp_gUgVwYdD8r` and `Out_gUgVwYdD8r` must not be modified;

- the main method signature `main :: IO ()` and declaration must be the last
  two declarations;

- the body of `main` must be a `do` notation;

- `do` statements that bind must pattern-match a single variable with type assertion;

- the last `do` statement must be `return ()`;

- must not output to `stderr`.
