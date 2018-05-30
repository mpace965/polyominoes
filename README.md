# Polyominoes

Generates the free, one-sided, and fixed [polyominoes](https://en.wikipedia.org/wiki/Polyomino#Algorithms_for_enumeration_of_fixed_polyominoes)
in Haskell. Using an Inductive Algorithm as described on the wikipedia page.

## Running

Built with [Haskell2010](https://www.haskell.org) and [Stack](https://docs.haskellstack.org). To run, clone the repository and run

```
$ stack build
$ stack exec Polyomino
```

the executable file is built to: `.stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/Polyomino/Polyomino` or a similar directory depending on your system and cabal version

## Documenting

To build the documentation

```
$ cabal configure --package-db=clear --package-db=global --package-db=$(stack path --snapshot-pkg-db) --package-db=$(stack path --local-pkg-db)
$ cabal haddock --executables
```

the documentation is then available at: `dist/doc/html/Polyomino/Polyomino/index.html`

## Sample Output

```
$ stack exec Polyomino
Generate polyominoes of length: 4
Polyominoes of type {free, one-sided, fixed}: one-sided
██░░░░░░
██░░░░░░
██░░░░░░
██░░░░░░

░░░░░░░░
██░░░░░░
██░░░░░░
████░░░░

░░░░░░░░
░░██░░░░
░░██░░░░
████░░░░

░░░░░░░░
██░░░░░░
████░░░░
██░░░░░░

░░░░░░░░
░░░░░░░░
████░░░░
████░░░░

░░░░░░░░
██░░░░░░
████░░░░
░░██░░░░

░░░░░░░░
░░░░░░░░
████░░░░
░░████░░
```
