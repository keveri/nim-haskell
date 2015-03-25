[![Build Status](https://travis-ci.org/keveri/nim-haskell.png)](https://travis-ci.org/keveri/nim-haskell)

# Simple Nim Game

[Nim](http://en.wikipedia.org/wiki/Nim) game in wikipedia.

## Running the game

Install dependencies and run.

```
$ cabal install --only-dependencies
$ cabal run
```

## Running tests

First make sure that all dependencies are installed:

```
$ cabal install --only-dependencies --enable-tests
```
### cabal

Only gives return code suggesting success/failure.

```
$ cabal test
```

### script

Script has a nice output.

```
$ sh run_tests.sh
```
