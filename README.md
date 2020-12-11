## aoc20-hs

Solutions for the [2020 Advent of Code](https://adventofcode.com/2020) problems in Haskell.

The algorithms are not really meant to be optimal, I'm just trying to get some practice in.

Run (e.g.) `cabal run d1` for the Day 1 answers.

------------------

## Things I've learnt along the way

- **Day 1** Basic file IO. A bit more about Cabal's system, including the `data-dir` entry.
- **Day 2** Using `Text` instead of `String`.
- **Day 3** The need to be careful with functions that return Int (vs Integer). The `cycle` function, by looking at other solutions.
- **Day 4** Functions in `Data.Text.Read`. The use of `Data.Map` to facilitate lookup of key-value pairs (post-refactoring). [Common stanzas in Cabal.](https://vrom911.github.io/blog/common-stanzas) Basic parser usage (in particular `megaparsec`).
- **Day 5** was slightly easier. I could technically have gotten more parsing practice in.
- **Day 6** was nice and easy. I can feel my code getting a bit more organised. Glancing at some solutions online, it seems like this may have been a good opportunity to learn about `Data.Set`, so maybe I will do so if I find some free time tomorrow.
- **Day 7** Not much, I guess.
- **Day 8** Learnt a bit about efficient parsing, especially making full use of operators such as `<$` and `<*`. Tried out `Data.IntSet` and `Data.IntMap`, i.e.  versions of `Set` and `Map` which are optimised for `Int` keys.
- **Day 9** Just got through it as fast and as inefficiently as I could since I was really, really sleepy.
- **Day 10** [Dynamic programming.](https://www.geeksforgeeks.org/dynamic-programming/)
