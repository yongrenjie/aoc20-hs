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
- **Day 11** Re-learnt the hard way that in a list comprehension, the last variable changes the quickest.

```haskell
Prelude> [(x, y) | x <- [1..2], y <- "abc"]
[(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c')]
```

- **Day 12** Not much? The `foldl'` solution is so natural, I'm not sure what else I could be doing. Well, I learnt about the `ST` monad earlier in the day, so maybe that counts as learning something.
- **Day 13** Got spoiled on Reddit and found that this is a question about the [Chinese remainder theorem](https://en.wikipedia.org/wiki/Chinese_remainder_theorem). Just as well, though, because I wouldn't have managed to solve it otherwise. (I had an inefficient solution which worked on the test cases, but was too slow on the real input.)
- **Day 14** Revision of State monad. It took a long time, but I am really happy with the way this code came out!
- **Day 15** More State monad. This solution is really, really slow. It can be a lot faster with mutable arrays, but I don't really have the energy to do that today. Perhaps another time.
- **Day 16** Not much new Haskell, but learnt a bit about the problem structure. Also did it in VS Code. I think I'll go back to Vim.
- **Day 17** Not much from today's problem specifically. I'm quite pleased that I figured out how to not repeat code, though.
- **Day 18** Serious grammar and parsing practice, a lot harder than some of the other expression parsing exercises I've done!
- **Day 19** This was really hard. The naive solution of parsing each rule into a Parser () doesn't work, because the returned parsers are built from other parsers, which leads to an infinite loop. I considered lots and lots of options, including making all the parsers IO-based (didn't work), and threading an `IntMap` through each of them. Eventually I found that it sufficed to thread the rules through each parser, and it turns out that GHC does some automatic memoisation. I also struggled through the Oxford notes on memoisation / dynamic programming. I don't think I fully understand it yet, but we're getting there.
- (**Day 19, revisited**: I figured out the memoisation!! See comments in `d19.hs` for explanation.)
- **Day 20**: Basic matrix and vector operations using `Data.Matrix` and `Data.Vector`. Otherwise, though, this day's solution was very inelegant.
- **Day 21** was chill (thank goodness!)
- **Day 22** `Data.Sequence`. But otherwise this was quite straightforward, too. (I later changed it to use plain lists and it turns out that that's 2x faster, probably because the lists are quite short, so the asymptotic behaviour isn't so important.)
