## aoc20-hs

Solutions for the [2020 Advent of Code](https://adventofcode.com/2020) problems in Haskell.

The algorithms are not really meant to be optimal, I'm just trying to get some practice in.

Run (e.g.) `cabal run d1` for the Day 1 answers.

------------------

## Things I've learnt along the way

- **Day 1** Basic file IO. A bit more about Cabal's system, including the `data-dir` entry.
- **Day 2** Using `Text` instead of `String`.
- **Day 3** The need to be careful with functions that return Int (vs Integer). The `cycle` function, by looking at other solutions.
- **Day 4** Functions in `Data.Text.Read`. The use of `Data.Map` to facilitate lookup of key-value pairs (post-refactoring). [Common stanzas in Cabal.](https://vrom911.github.io/blog/common-stanzas)
