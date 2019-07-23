<span>
<p align="center">
<img alt="Parsing With Haskell Parser Combinators" src="https://i.imgur.com/hKqlZrP.gif">
<br>
<sup></sup>
</p>
</span>

# Parsing With Haskell Parser Combinators

Need to parse something?
Never heard of a "parser combinator"?
Looking to learn some Haskell?
Awesome!
Below is everything you'll need to get up and parsing with Haskell parser combinators.
From here you can try tackling esoteric data serialization formats,
compiler front ends,
domain specific languages—you name it!

- [Building The Demos](#building-the-demos)
- [Running The Demos](#running-the-demos)
- [Parser Combinator](#parser-combinator)
- [Version Number](#version-number)
- [SRT](#srt)
- [Exercises](#exercises)

## Building The Demos

Included with this guide are two demo programs.

`version-number-parser` parses a file for a version number.
`srt-file-parser` parses a file for SRT subtitles.
Feel free to try them out with the files found in `test-input/`.

### Stack

Download the Haskell tool [Stack](https://docs.haskellstack.org/en/stable/README/)
and then run the following.

```bash
git clone https://github.com/lettier/parsing-with-haskell-parser-combinators
cd parsing-with-haskell-parser-combinators
stack build
```

### Cabal

If using Cabal, you can run the following.

```bash
git clone https://github.com/lettier/parsing-with-haskell-parser-combinators
cd parsing-with-haskell-parser-combinators
cabal sandbox init
cabal --require-sandbox build
cabal --require-sandbox install
```

## Running The Demos

After building the two demo programs, you can run them like so.

### Stack

To try the version number parser, run the following.

```bash
cd parsing-with-haskell-parser-combinators
stack exec -- version-number-parser
What is the version output file path?
test-input/gifcurry-version-output.txt
```

To try the SRT file parser, run the following.

```bash
cd parsing-with-haskell-parser-combinators
stack exec -- srt-file-parser
What is the SRT file path?
test-input/subtitles.srt
```

### Cabal

To try the version number parser, run the following.

```bash
cd parsing-with-haskell-parser-combinators
.cabal-sandbox/bin/version-number-parser
What is the version output file path?
test-input/gifcurry-version-output.txt
```

To try the SRT file parser, run the following.

```bash
cd parsing-with-haskell-parser-combinators
.cabal-sandbox/bin/srt-file-parser
What is the SRT file path?
test-input/subtitles.srt
```

## Parser Combinator

<span>
<p align="center">
<img alt="Parser Combinators" src="https://i.imgur.com/MLHPxhx.jpg">
<br>
<sup></sup>
</p>
</span>

One of the better ways to learn about the parsing strategy,
[parser combinator](https://en.wikipedia.org/wiki/Parser_combinator),
is to look at an implementation of one.

<blockquote>
<p align="right">
Parsers built using combinators are straightforward to construct, readable, modular, well-structured, and easily maintainable.
<br><br>
<sup>
—<a href="https://en.wikipedia.org/wiki/Parser_combinator">Parser combinator - Wikipedia</a>
</sup>
</p>
</blockquote>

### ReadP

Let's take a look under the hood of [ReadP](https://hackage.haskell.org/package/base-4.12.0.0/docs/Text-ParserCombinators-ReadP.html),
a parser combinator library found in base.
Since it is in base, you should already have it.

:bulb: Note, you may want to try out [Parsec](https://hackage.haskell.org/package/parsec) after getting familiar with ReadP.
It too is a parser combinator library that others prefer to ReadP.
As an added bonus, it is included in
[GHC's boot libraries](https://gitlab.haskell.org/ghc/ghc/wikis/commentary/libraries/version-history)
as of GHC version 8.4.1.

#### P Data Type

```haskell
-- (c) The University of Glasgow 2002

data P a
  = Get (Char -> P a)
  | Look (String -> P a)
  | Fail
  | Result a (P a)
  | Final [(a,String)]
  deriving Functor
```

We'll start with the `P` data type.
The `a` in `P a` is up to you (the library user) and can be whatever you'd like.
The compiler creates a functor instance automatically and there are hand-written instances for
applicative,
monad,
`MonadFail`,
and alternative.

:bulb: Note, for more on functors, applicatives, and monads, checkout
[Your easy guide to Monads, Applicatives, & Functors](https://medium.com/@lettier/your-easy-guide-to-monads-applicatives-functors-862048d61610).

`P` is a [sum type](https://en.wikipedia.org/wiki/Tagged_union) with five cases.

- `Get` consumes a single character from the input string and returns a new `P`.
- `Look` accepts a duplicate of the input string and returns a new `P`.
- `Fail` indicates the parser finished without a result.
- `Result` holds a possible parsing and another `P` case.
- `Final` is a list of two-tuples. The first tuple element is a possible parsing of the input
  and the second tuple element is the rest of the input string that wasn't consumed by `Get`.

#### Run

```haskell
-- (c) The University of Glasgow 2002

run :: P a -> ReadS a
run (Get f)      (c:s) = run (f c) s
run (Look f)     s     = run (f s) s
run (Result x p) s     = (x,s) : run p s
run (Final r)    _     = r
run _            _     = []
```

`run` is the heart of the ReadP parser.
It does all of the heavy lifting as it recursively runs through all of the parser states that we saw up above.
You can see that it takes a `P` and returns a `ReadS`.

```haskell
-- (c) The University of Glasgow 2002

type ReadS a = String -> [(a,String)]
```

`ReadS a` is a type alias for `String -> [(a,String)]`.
So whenever you see `ReadS a`, think `String -> [(a,String)]`.

```haskell
-- (c) The University of Glasgow 2002

run :: P a -> String -> [(a,String)]
run (Get f)      (c:s) = run (f c) s
run (Look f)     s     = run (f s) s
run (Result x p) s     = (x,s) : run p s
run (Final r)    _     = r
run _            _     = []
```

`run` pattern matches the different cases of `P`.

- If it's `Get`,
  it calls itself with a new `P` (returned by passing the function `f`, in `Get f`, the next character `c` in the input string)
  and the rest of the input string `s`.
- If it's `Look`,
  it calls itself with a new `P` (returned by passing the function `f`, in `Look f`,  the input string `s`)
  and the input string.
  Notice how `Look` doesn't consume any characters from the input string like `Get` does.
- If it's `Result`,
  it assembles a two-tuple—containing the parsed result and what's left of the input string—and
  prepends this to the result of a recursive call that runs with another `P` case and the input string.
- If it's `Final`, `run` returns a list of two-tuples containing parsed results and input string leftovers.
- For anything else, `run` returns an empty list.
  For example, if the case is `Fail`, `run` will return an empty list.

```haskell
> run (Get (\ a -> Get (\ b -> Result [a,b] Fail))) "12345"
[("12","345")]
```

ReadP doesn't expose `run` but if it did, you could call it like this.
The two `Get`s consume the `'1'` and `'2'`, leaving the `"345"` behind.

```haskell
> run (Get (\ a -> Get (\ b -> Result [a,b] Fail))) "12345"
> run (Get (\ b -> Result ['1',b] Fail)) "2345"
> run (Result ['1','2'] Fail) "345"
> (['1', '2'], "345") : run (Fail) "345"
> (['1', '2'], "345") : []
[("12","345")]
```

Running through each recursive call, you can see how we arrived at the final result.

```haskell
> run (Get (\ a -> Get (\ b -> Result [a,b] (Final [(['a','b'],"c")])))) "12345"
[("12","345"),("ab","c")]
```

Using `Final`, you can include a parsed result in the final list of two-tuples.

#### readP_to_S

```haskell
-- (c) The University of Glasgow 2002

   readP_to_S :: ReadP a -> ReadS a
-- readP_to_S :: ReadP a -> String -> [(a,String)]
   readP_to_S (R f) = run (f return)
```

While ReadP doesn't expose `run` directly, it does expose it via `readP_to_S`.
`readP_to_S` introduces a `newtype` called `ReadP`.
`readP_to_S` accepts a `ReadP a`, a string, and returns a list of two-tuples.

#### ReadP Newtype

<span>
<p align="center">
<img alt="ReadP Newtype" src="https://i.imgur.com/7WJPwLC.jpg">
<br>
<sup></sup>
</p>
</span>

```haskell
-- (c) The University of Glasgow 2002

newtype ReadP a = R (forall b . (a -> P b) -> P b)
```

Here's the definition of `ReadP a`.
There are instances for functor, applicative, monad, `MonadFail`, alternative, and `MonadPlus`.
The `R` constructor takes a function that takes another function and returns a `P`.
The accepted function takes whatever you chose for `a` and returns a `P`.

```haskell
-- (c) The University of Glasgow 2002

readP_to_S (R f) = run (f return)
```

Recall that `P` is a monad and `return`'s type is `a -> m a`.
So `f` is the `(a -> P b) -> Pb` function and `return` is the `(a -> P b)` function.
Ultimately, `run` gets the `P b` it expects.

```haskell
-- (c) The University of Glasgow 2002

readP_to_S (R f) inputString = run (f return) inputString
--               ^^^^^^^^^^^                  ^^^^^^^^^^^
```

It's left off in the source code but remember that `readP_to_S` and `run` expects an input string.

```haskell
-- (c) The University of Glasgow 2002

instance Functor ReadP where
  fmap h (R f) = R (\k -> f (k . h))
```

Here's the functor instance definition for `ReadP`.

```haskell
> readP_to_S (fmap toLower get) "ABC"
[('a',"BC")]

> readP_to_S (toLower <$> get) "ABC"
[('a',"BC")]
```

This allows us to do something like this.
`fmap` functor maps `toLower` over the functor `get` which equals `R Get`.
Recall that the type of `Get` is `(Char -> P a) -> P a` which the `ReadP` constructor (`R`) accepts.

```haskell
-- (c) The University of Glasgow 2002

fmap h       (R f  ) = R (\ k -> f   (k . h      ))
fmap toLower (R Get) = R (\ k -> Get (k . toLower))
```

Here you see the functor definition rewritten for the `fmap toLower get` example.

#### Applicative P Instance

Looking up above, how did `readP_to_S` return `[('a',"BC")]` when we only used `Get` which doesn't terminate `run`?
The answer lies in the applicative definition for `P`.

```haskell
-- (c) The University of Glasgow 2002

instance Applicative P where
  pure x = Result x Fail
  (<*>) = ap
```

`return` equals `pure` so we could rewrite `readP_to_S (R f) = run (f return)` to be `readP_to_S (R f) = run (f pure)`.
By using `return` or rather `pure`, `readP_to_S` sets `Result x Fail` as the final case `run` will encounter.
If reached,
`run` will terminate and we'll get our list of parsings.

```haskell
> readP_to_S (fmap toLower get) "ABC"

-- Use the functor instance to transform fmap toLower get.
> readP_to_S (R (\ k -> Get (k . toLower))) "ABC"

-- Call run which removes R.
> run ((\ k -> Get (k . toLower)) pure) "ABC"

-- Call function with pure to get rid of k.
> run (Get (pure . toLower)) "ABC"

-- Call run for Get case to get rid of Get.
> run ((pure . toLower) 'A') "BC"

-- Call toLower with 'A' to get rid of toLower.
> run (pure 'a') "BC"

-- Use the applicative instance to transform pure 'a'.
> run (Result 'a' Fail) "BC"

-- Call run for the Result case to get rid of Result.
> ('a', "BC") : run (Fail) "BC"

-- Call run for the Fail case to get rid of Fail.
> ('a', "BC") : []

-- Prepend.
[('a',"BC")]
```

Here you see the flow from `readP_to_S` to the parsed result.

#### Alternative P Instance

```haskell
-- (c) The University of Glasgow 2002

instance Alternative P where
  -- ...

  -- most common case: two gets are combined
  Get f1     <|> Get f2     = Get (\c -> f1 c <|> f2 c)

  -- results are delivered as soon as possible
  Result x p <|> q          = Result x (p <|> q)
  p          <|> Result x q = Result x (p <|> q)

  -- ...
```

The `Alternative` instance for `P` allows us to split the flow of the parser into a left and right path.
This comes in handy when the input can go none, one, or (more rarely) two of two ways.

```haskell
> readP_to_S ((get >>= \ a -> return a) <|> (get >> get >>= \ b -> return b)) "ABC"
[('A',"BC"),('B',"C")]
```

The `<|>` operator or function introduces a fork in the parser's flow.
The parser will travel through both the left and right paths.
The end result will contain all of the possible parsings that went left
and all of the possible parsings that went right.
If both paths fail, then the whole parser fails.

:bulb: Note, in other parser combinator implementations,
when using the `<|>` operator,
the parser will go left or right but not both.
If the left succeeds, the right is ignored.
The right is only processed if the left side fails.

```haskell
> readP_to_S ((get >>= \ a -> return [a]) <|> look <|> (get >> get >>= \a -> return [a])) "ABC"
[("ABC","ABC"),("A","BC"),("B","C")]
```

You can chain the `<|>` operator for however many options or alternatives there are.
The parser will return a possible parsing involving each.

#### ReadP Failure

```haskell
-- (c) The University of Glasgow 2002

instance Monad ReadP where
  fail _    = R (\_ -> Fail)
  R m >>= f = R (\k -> m (\a -> let R m' = f a in m' k))
```

Here is the `ReadP` monad instance.
Notice the definition for `fail`.

```haskell
> readP_to_S ((\ a b c -> [a,b,c]) <$> get <*> get <*> get) "ABC"
[("ABC","")]

> readP_to_S ((\ a b c -> [a,b,c]) <$> get <*> fail "" <*> get) "ABC"
[]

> readP_to_S (get >>= \ a -> get >>= \ b -> get >>= \ c -> return [a,b,c]) "ABC"
[("ABC","")]

> readP_to_S (get >>= \ a -> get >>= \ b -> fail "" >>= \ c -> return [a,b,c]) "ABC"
[]
```

You can cause an entire parser path to abort by calling `fail`.
Since ReadP doesn't provide a direct way to generate a `Result` or `Final` case,
the return value will be an empty list.
If the failed path is the only path, then the entire result will be an empty list.
Recall that when `run` matches `Fail`, it returns an empty list.

```haskell
-- (c) The University of Glasgow 2002

instance Alternative P where
  -- ...

  -- fail disappears
  Fail       <|> p          = p
  p          <|> Fail       = p

  -- ...
```

Going back to the alternative `P` instance,
you can see how a failure on either side (but not both) will not fail the whole parser.

```haskell
> readP_to_S (get >>= \ a -> get >>= \ b -> pfail >>= \ c -> return [a,b,c]) "ABC"
[]
```

Instead of using `fail`, ReadP provides `pfail` which allows you to generate a `Fail` case directly.

## Version Number

<span>
<p align="center">
<img alt="Version Number" src="https://i.imgur.com/mHnqDjf.jpg">
<br>
<sup></sup>
</p>
</span>

[Gifcurry](https://github.com/lettier/gifcurry),
the Haskell-built video editor for GIF makers, shells out to various different programs.
To ensure compatibility, it needs the version number for each of the programs it shells out to.
One of those programs is ImageMagick.

```bash
Version: ImageMagick 6.9.10-14 Q16 x86_64 2018-10-24 https://imagemagick.org
Copyright: © 1999-2018 ImageMagick Studio LLC
License: https://imagemagick.org/script/license.php
Features: Cipher DPC HDRI Modules OpenCL OpenMP
```

Here you see the output of `convert --version`.
How could you parse this to capture the 6, 9, 10, and 14?

Looking at the output,
we know the version number is a collection of numbers separated by either a period or a dash.
This definition covers the dates as well so we'll make sure that the first two numbers are separated by a period.
That way, if they put a date before the version number, we won't get the wrong result.

<span>
<p align="center">
<img alt="Version Number Parser" src="https://i.imgur.com/3hZDOpI.gif">
<br>
<sup></sup>
</p>
</span>

```txt
1. Consume zero or more characters that are not 0 through 9 and go to 2.
2. Consume zero or more characters that are 0 through 9, save this number, and go to 3.
3. Look at the rest of the input and go to 4.
4. If the input
    - is empty, go to 6.
    - starts with a period, go to 1.
    - starts with a dash
        - and you have exactly one number, go to 5.
        - and you have more than one number, go to 1.
    - doesn't start with a period or dash
        - and you have exactly one number, go to 5.
        - you have more than one number, go to 6.
5. Delete any saved numbers and go to 1.
6. Return the numbers found.
```

Before we dive into the code, here's the algorithm we'll be following.

### Building The Version Number Parser

```haskell
parseVersionNumber
  ::  [String]
  ->  ReadP [String]
parseVersionNumber
  nums
  = do
  _         <- parseNotNumber
  num       <- parseNumber
  let nums' = nums ++ [num]
  parseSeparator nums' parseVersionNumber
```

`parseVersionNumber` is the main parser combinator that parses an input string for a version number.
It accepts a list of strings and returns a list of strings in the context of the `ReadP` data type.
The accepted list of strings is not the input that gets parsed but rather the list of numbers found so far.
For the first function call, the list is empty since it hasn't parsed anything yet.

```haskell
parseVersionNumber
  nums
```

Starting from the top,
`parseVersionNumber` takes a list of strings which are the current list of numbers found so far.

```haskell
  _         <- parseNotNumber
```

`parseNotNumber` consumes everything that isn't a number from the input string.
Since we are not interested in the result, we discard it (`_ <-`).

```haskell
  num       <- parseNumber
  let nums' = nums ++ [num]
```

Next we consume everything that is a number and then add that to the list of numbers found so far.

```haskell
  parseSeparator nums' parseVersionNumber
```

After `parseVersionNumber` has processed the next number, it passes the list of numbers found and itself to `parseSeparator`.

#### Parsing The Separator

```haskell
parseSeparator
  ::  [String]
  ->  ([String] -> ReadP [String])
  ->  ReadP [String]
parseSeparator
  nums
  f
  = do
  next <- look
  case next of
    ""    -> return nums
    (c:_) ->
      case c of
        '.' -> f nums
        '-' -> if length nums == 1 then f [] else f nums
        _   -> if length nums == 1 then f [] else return nums
```

Here you see `parseSeparator`.

```haskell
  next <- look
  case next of
    ""    -> return nums
    (c:_) ->
```

`look` allows us to get what's left of the input string without consuming it.
If there's nothing left, it returns the numbers found.
However, if there is something left, it analyzes the first character.

```haskell
      case c of
        '.' -> f nums
        '-' -> if length nums == 1 then f [] else f nums
        _   -> if length nums == 1 then f [] else return nums
```

If the next character is a period, call `parseVersionNumber` again with the current list of numbers found.
If it's a dash and we have exactly one number, call `parseVersionNumber` with an empty list of numbers since it's a date.
If it's a dash and we don't have exactly one number, call `parseVersionNumber` with the list of numbers found so far.
Otherwise,
call `parseVersionNumber` with an empty list if we have exactly one number
or return the numbers found if we don't have exactly one number.

#### Parsing Non-numbers

```haskell
parseNotNumber
  ::  ReadP String
parseNotNumber
  =
  munch (not . isNumber)
```

`parseNotNumber` uses `munch` which `ReadP` provides.
`munch` is given the predicate `(not . isNumber)` which returns true for any character that isn't 0 through 9.

```haskell
munch :: (Char -> Bool) -> ReadP String
```

`munch` continuously calls `get` if the next character in the input string satisfies the predicate.
If it doesn't, `munch` returns the characters that did, if any.
Since it only uses `get`, munch always succeeds.

:bulb: Note, `parseNumber` is similar to `parseNotNumber`.
Instead of `not . isNumber`, the predicate is just `isNumber`.

#### Munch Versus Many

```haskell
parseNotNumber'
  ::  ReadP String
parseNotNumber'
  =
  many (satisfy (not . isNumber))
```

Instead of using `munch`,
you could write `parseNotNumber` like this,
using `many` and `satisfy`—both of which ReadP provides.
Looking at the type signature for `many`, it accepts a single parser combinator (`ReadP a`).
In this instance, it's being given the parser combinator `satisfy`.

```haskell
> readP_to_S (satisfy (not . isNumber)) "a"
[('a',"")]

> readP_to_S (satisfy (not . isNumber)) "1"
[]
```

`satisfy` takes a predicate and uses `get` to consume the next character.
If the accepted predicate returns true, `satisfy` returns the character.
Otherwise, `satisfy` calls `pfail` and fails.

```haskell
> readP_to_S (munch (not . isNumber)) "abc123"
[("abc","123")]

> readP_to_S (many (satisfy (not . isNumber))) "abc123"
[("","abc123"),("a","bc123"),("ab","c123"),("abc","123")]
```

Using `many` can give you unwanted results.
Ultimately, `many` introduces one or more `Result` cases.
Because of this, `many` always succeeds.

```haskell
> readP_to_S (many look) "abc123"
-- Runs forever.
```

`many` will run your parser until it fails or runs out of input.
If your parser never fails or never runs out of input, `many` will never return.

```haskell
> readP_to_S (many (get >>= \ a -> return (read (a : "") :: Int))) "12345"
[([],"12345"),([1],"2345"),([1,2],"345"),([1,2,3],"45"),([1,2,3,4],"5"),([1,2,3,4,5],"")]
```

For every index in the result,
the parsed result will be the outcome of having ran the parser index times on the entire input.

```haskell
> let parser        = get >>= \ a -> return (read (a : "") :: Int)
> let many' results = return results <|> (parser >>= \ result -> many' (results ++ [result]))
> readP_to_S (many' []) "12345"
[([],"12345"),([1],"2345"),([1,2],"345"),([1,2,3],"45"),([1,2,3,4],"5"),([1,2,3,4,5],"")]
```

Here's an alternate definition for `many`.
On the left side of `<|>`,
it returns the current parser results.
On the right side of `<|>`,
it runs the parser,
adds that result to the current parser results,
and calls itself with the updated results.
This has a cumulative sum type effect where index `i` is the parser result appended to the parser result at
`i - 1`,
`i - 2`,
...,
and `1`.

### Running The Version Number Parser

Now that we built the parser, let's run it.

```haskell
> let inputString =
>     "Some Program (C) 1234-56-78 All rights reserved.\n\
>     \Version: 12.345.6-7\n\
>     \License: Some open source license."
> readP_to_S (parseVersionNumber []) inputString
[(["12","345","6","7"],"\nLicense: Some open source license.")]
```

You can see it extracted the version number correctly even with the date coming before it.

## SRT

<span>
<p align="center">
<img alt="SRT" src="https://i.imgur.com/pMTs3AB.jpg">
<br>
<sup></sup>
</p>
</span>

Now let's parse something more complicated—SRT files.

For the release of
[Gifcurry](https://lettier.github.io/gifcurry)
six, I needed to parse
[SRT (SubRip Text) files](http://www.visualsubsync.org/help/srt).
SRT files contain subtitles that video processing programs use to display text on top of a video.
Typically this text is the dialog of a movie translated into various different languages.
By keeping the text separate from the video,
there only needs to be one video which saves time, storage space, and bandwidth.
The video software can swap out the text without having to swap out the video.
Contrast this with burning-in or hard-coding the subtitles where the text becomes a part of the image data that makes up the video.
In this case, you would need a video for each collection of subtitles.

<span>
<p align="center">
<img alt="Gifcurry" src="https://i.imgur.com/RUwM8eE.gif">
<br>
<sup>Inner Video © Blender Foundation | www.sintel.org</sup>
</p>
</span>

Gifcurry can take a SRT file and burn-in the subtitles for the video slice your select.

```txt
7
00:02:09,400 --> 00:02:13,800
What brings you to
the land of the gatekeepers?

8
00:02:15,000 --> 00:02:17,500
I'm searching for someone.

9
00:02:18,000 --> 00:02:22,200
Someone very dear?
A kindred spirit?
```

Here you see the English subtitles for
[Sintel](https://durian.blender.org/) (© Blender Foundation | www.sintel.org).

### SRT Format

<blockquote>
<p align="right">
SRT is perhaps the most basic of all subtitle formats.
<br><br>
<sup>
—<a href="https://matroska.org/technical/specs/subtitles/srt.html">SRT Subtitle | Matrosk</a>
</sup>
</p>
</blockquote>

The SRT file format consists of blocks, one for each subtitle, separated by an empty line.

```txt
2
```

At the top of the block is the index.
This determines the order of the subtitles.
Hopefully the subtitles are already in order and all of them have unique indexes but this may not be the case.

```txt
01:04:13,000 --> 02:01:01,640 X1:167 X2:267 Y1:33 Y2:63
```

After the index is the start time, end time, and an optional set of points specifying the rectangle the
subtitle text should go in.

```txt
01:04:13,000
```

The timestamp format is `hours:minutes:seconds,milliseconds`.

:bulb: Note the comma instead of the period separating the seconds from the milliseconds.

```txt
This is the actual subtitle
text. It can span multiple lines.
It may include formating
like <b>bold</b>, <i>italic</i>,
<u>underline</u>,
and <font color="#010101">font color</font>.
```

The third and last part of a block is the subtitle text.
It can span multiple lines and ends when there is an empty line.
The text can include formatting tags reminiscent of HTML.

### Building The SRT Parser

<span>
<p align="center">
<img alt="Parsing SRT" src="https://i.imgur.com/N1qlzd6.jpg">
<br>
<sup></sup>
</p>
</span>

```haskell
parseSrt
  ::  ReadP [SrtSubtitle]
parseSrt
  =
  manyTill parseBlock (skipSpaces >> eof)
```

`parseSrt` is the main parser combinator that handles everything.
It parses each block until it reaches the end of the file (`eof`) or input.
To be on the safe side,
there could be trailing whitespace between the last block and the end of the file.
To handle this, it parses zero or more characters of whitespace (`skipSpaces`) before parsing
the end of the file (`skipSpaces >> eof`).
If there is still input left by the time `eof` is reached, `eof` will fail and this will return nothing.
Therefore, it's important that `parseBlock` doesn't leave any thing but whitespace behind.

#### Building The SRT Block Parser

```haskell
parseBlock
  ::  ReadP SrtSubtitle
parseBlock
  = do
  i      <- parseIndex
  (s, e) <- parseTimestamps
  c      <- parseCoordinates
  t      <- parseTextLines
  return
    SrtSubtitle
      { index       = i
      , start       = s
      , end         = e
      , coordinates = c
      , taggedText  = t
      }
```

As we went over earlier, a block consists of an index, timestamps, possibly some coordinates, and some lines of text.
In this version of `parseBlock`, you see the more imperative do notation style with the record syntax.

```haskell
parseBlock'
  ::  ReadP SrtSubtitle
parseBlock'
  =
      SrtSubtitle
  <$> parseIndex
  <*> parseStartTimestamp
  <*> parseEndTimestamp
  <*> parseCoordinates
  <*> parseTextLines
```

Here's another way you could write `parseBlock`.
This is the applicative style.
Just be sure to get the order right.
For example, I could've accidentally mixed up the start and end timestamps.

#### Building The SRT Index Parser

<span>
<p align="center">
<img alt="Parsing The Index" src="https://i.imgur.com/bPF76DS.jpg">
<br>
<sup></sup>
</p>
</span>

```haskell
parseIndex
  ::  ReadP Int
parseIndex
  =
      skipSpaces
  >>  readInt <$> parseNumber
```

At the top of the block is the index.
Here you see `skipSpaces` again.
After skipping over whitespace,
it parses the input for numbers and converts it to an actual integer.

```haskell
readInt
  ::  String
  ->  Int
readInt
  =
  read
```

`readInt` looks like this.

```haskell
> read "123" :: Int
123
> read "1abc" :: Int
*** Exception: Prelude.read: no parse
```

Normally using `read` directly can be dangerous.
`read` may not be able to convert the input to the specified type.
However, `parseNumber` will only return the 10 numerical digit characters (`['0'..'9']`)
so using `read` directly becomes safe.

#### Building The SRT Timestamps Parser

<span>
<p align="center">
<img alt="Parsing The Timestamps" src="https://i.imgur.com/yI3o6NM.jpg">
<br>
<sup></sup>
</p>
</span>

Parsing the timestamps are a little more involved than parsing the index.

```haskell
parseTimestamps
  ::  ReadP (Timestamp, Timestamp)
parseTimestamps
  = do
  _   <- char '\n'
  s   <- parseTimestamp
  _   <- skipSpaces
  _   <- string "-->"
  _   <- skipSpaces
  e   <- parseTimestamp
  return (s, e)
```

This is the main combinator for parsing the timestamps.

`char` parses the character you give it or it fails.
If it fails then `parseTimestamps` fails, ultimately causing `parseSrt` to fail
so there must be a newline character after the index.

`string` is like `char` except instead of just one character, it
parses the string of characters you give it or it fails.

```haskell
parseStartTimestamp
  ::  ReadP Timestamp
parseStartTimestamp
  =
      char '\n'
  >>  parseTimestamp
```

`parseTimestamps` parses both timestamps,
but for the applicative style (`parseSrt'`),
we need a parser just for the start timestamp.

```haskell
parseEndTimestamp
  ::  ReadP Timestamp
parseEndTimestamp
  =
      skipSpaces
  >>  string "-->"
  >>  skipSpaces
  >>  parseTimestamp
```

This parses everything between the timestamps and returns the end timestamp.

```haskell
parseTimestamp
  ::  ReadP Timestamp
parseTimestamp
  = do
  h  <- parseNumber
  _  <- char ':'
  m  <- parseNumber
  _  <- char ':'
  s  <- parseNumber
  _  <- char ',' <|> char '.'
  m' <- parseNumber
  return
    Timestamp
      { hours        = readInt h
      , minutes      = readInt m
      , seconds      = readInt s
      , milliseconds = readInt m'
      }
```

This parses the four numbers that make up the timestamp.
The first three numbers are separated by a colon and the last one is separated by a comma.
To be more forgiving, however, we allow the possibility of there being a period instead of a comma.

```haskell
> readP_to_S (char '.' <|> char ',') "..."
[('.',"..")]

> readP_to_S (char '.' <|> char ',') ",.."
[(',',"..")]
```

:bulb: Note, when using `char` with `<|>`,
only one side can succeed (two `char` enter, one `char` leave)
since `char` consumes a single character and two characters cannot occupy the same space.

#### Building The SRT Coordinates Parser

<span>
<p align="center">
<img alt="Parsing The Coordinates" src="https://i.imgur.com/0mpO88C.jpg">
<br>
<sup></sup>
</p>
</span>

The coordinates are an optional part of the block but if included, will be on the same line as the timestamps.

```haskell
parseCoordinates
  ::  ReadP (Maybe SrtSubtitleCoordinates)
parseCoordinates
  =
  option Nothing $ do
    _  <- skipSpaces1
    x1 <- parseCoordinate 'x' 1
    _  <- skipSpaces1
    x2 <- parseCoordinate 'x' 2
    _  <- skipSpaces1
    y1 <- parseCoordinate 'y' 1
    _  <- skipSpaces1
    y2 <- parseCoordinate 'y' 2
    return
      $ Just
        SrtSubtitleCoordinates
          { x1 = readInt x1
          , x2 = readInt x2
          , y1 = readInt y1
          , y2 = readInt y2
          }
```

`option` takes two arguments.
The first argument is returned if the second argument, a parser, fails.
So if the coordinates parser fails, `parseCoordinates` will return `Nothing`.
Put another way, the coordinates parser failing does not cause the whole parser to fail.
This block will just have `Nothing` for its `coordinates` "field".

```haskell
parseCoordinate
  ::  Char
  ->  Int
  ->  ReadP String
parseCoordinate
  c
  n
  = do
  _  <- char (Data.Char.toUpper c) <|> char (Data.Char.toLower c)
  _  <- string $ show n ++ ":"
  parseNumber
```

This parser allows the coordinate labels to be in either uppercase or lowercase.
For example, `x1:1 X2:2 Y1:3 y2:4` would succeed.

#### Building The SRT Text Parser

<span>
<p align="center">
<img alt="Parsing The Text" src="https://i.imgur.com/vMuZsa1.jpg">
<br>
<sup></sup>
</p>
</span>

Parsing the text is the most involved portion due to the HTML-like tag formatting.

Tag parsing can be challenging—just ask anyone who parses them with a regular expression.
To make this easier on us—and for the user—we'll use a
[tag soup](https://en.wikipedia.org/wiki/Tag_soup)
kind of approach.
The parser will allow unclosed and/or wrongly nested tags.
It will also allow any tag and not just `b`, `u`, `i`, and `font`.

```haskell
parseTextLines
  ::  ReadP [TaggedText]
parseTextLines
  =
      char '\n'
  >>  (getTaggedText <$> manyTill parseAny parseEndOfTextLines)
```

We start out by matching on a newline character.
After that, we functor map or fmap (`<$>`) `getTaggedText` over the subtitle text characters until we reach the end of the text lines.

```haskell
parseEndOfTextLines
  ::  ReadP ()
parseEndOfTextLines
  =
  void (string "\n\n") <|> eof
```

We stop collecting characters (`parseAny`) when we reach two newline characters or the end of the file.
This signals the end of the block.

```haskell
getTaggedText
  ::  String
  ->  [TaggedText]
getTaggedText
  s
  =
  fst
    $ foldl
      folder
      ([], [])
      parsed
  where
```

`getTaggedText` folds through the parsed text from left to right, returning the accumulated tagged text.

```haskell
    parsed
      ::  [String]
    parsed
      =
      case readP_to_S (parseTaggedText []) s of
        []      -> [s]
        r@(_:_) -> (fst . last) r
```

`parsed` returns a list of one or more strings.
It attempts to parse the input text for tags.
If that fails, `parsed` returns the input string inside a list.
Otherwise, if `parseTaggedText` succeeds, `parse` returns the last possible parsing (`(fst . last) r`).

```haskell
    folder
      ::  ([TaggedText], [Tag])
      ->  String
      ->  ([TaggedText], [Tag])
    folder
      (tt, t)
      x
      | isTag x   = (tt, updateTags t x)
      | otherwise = (tt ++ [TaggedText { text = x, tags = t}], t)
```

As `folder` moves from left to right, over the parsed strings, it checks if the current string is a tag.
If it is a tag, it updates the current set of active tags (`t`).
Otherwise, it appends another tagged piece of text associated with the set of active tags.

```haskell
updateTags
  ::  [Tag]
  ->  String
  ->  [Tag]
updateTags
  tags
  x
  | isClosingTag x = remove compare' tags (makeTag x)
  | isOpeningTag x = add    compare' tags (makeTag x)
  | otherwise      = tags
  where
    compare'
      ::  Tag
      ->  Tag
      ->  Bool
    compare'
      a
      b
      =
      name a /= name b
```

`updateTags` updates the `tags` given by either removing or adding the given tag (`x`) depending on if it is a closing or opening tag.
If it is neither, it just returns the passed set of tags.
`add` will overwrite an existing tag if `tags` already has a tag by the same name.
You can see this in the `compare'` function given.

To keep the parser simple, if an opening tag `T` is found, `T` gets added to the list of tags
or overwrites an exiting `T` if already present.
If a corresponding closing `/T` is found, then `T` is removed from the list of tags, if present.
It doesn't matter if there is two or more `T`s in a row,
one or more `T`s without a closing `/T`,
and/or there's a closing `/T` without an opening `T`.

```haskell
makeTag
  ::  String
  ->  Tag
makeTag
  s
  =
  Tag
    { name       = getTagName       s
    , attributes = getTagAttributes s
    }
```

`makeTag` assembles a tag from the given string (`s`).
Each `Tag` has a name and zero or more attributes.

```haskell
parseTaggedText
  ::  [String]
  ->  ReadP [String]
parseTaggedText
  strings
  = do
  s <- look
  case s of
    "" -> return strings
    _  -> do
      r <- munch1 (/= '<') <++ parseClosingTag <++ parseOpeningTag
      parseTaggedText $ strings ++ [r]
```

`parseTaggedText` returns the input string broken up into pieces.
Each piece is either the text enclosed by tags, a closing tag, or an opening tag.
After it splits off a piece, it adds it to the other pieces and calls itself again.
If the remaining input string is empty, it returns the list of strings found.

```haskell
> readP_to_S (string "ab" <++ string "abc") "abcd"
[("ab","cd")]

> readP_to_S (string "ab" +++ string "abc") "abcd"
[("ab","cd"),("abc","d")]

> readP_to_S (string "ab" <|> string "abc") "abcd"
[("ab","cd"),("abc","d")]
```

The `<++` operator is left biased meaning that if the left side succeeds, it won't even bother with the right.
Recall that when we run the parser, we get a list of all the possible parsings.
All of these possible parsings are the result of the parser having traveled through all of the possible paths.
By using `<++`,
we receive the possible parsings from the left path and from the right path if and only if the left side failed.
If you'd like all of the possible parsings through the left and right side,
you can use the `+++` operator provided by `ReadP`.
`+++` is just `<|>` which we saw up above.

```haskell
parseOpeningTag
  ::  ReadP String
parseOpeningTag
  = do
  _ <- char '<'
  t <- munch1 (\ c -> c /= '/' && c /= '>')
  _ <- char '>'
  return $ "<" ++ t ++ ">"
```

An opening tag is an opening angle bracket, some text that doesn't include a forward slash, and the next immediate closing angle bracket.

```haskell
parseClosingTag
  ::  ReadP String
parseClosingTag
  = do
  _ <- char '<'
  _ <- char '/'
  t <- munch1 (/= '>')
  _ <- char '>'
  return $ "</" ++ t ++ ">"
```

A closing tag is an opening angle bracket, a forward slash, some text, and the next immediate closing angle bracket.

<span>
<p align="center">
<img alt="Parsing Tags" src="https://i.imgur.com/5HJWKPA.jpg">
<br>
<sup></sup>
</p>
</span>

```haskell
getTagAttributes
  ::  String
  ->  [TagAttribute]
getTagAttributes
  s
  =
  if isOpeningTag s
    then
      case readP_to_S (parseTagAttributes []) s of
        []    -> []
        (x:_) -> fst x
    else
      []
```

Opening tags can have attributes.
For example, `<font color="#101010">`.
Each attribute is a two-tuple, key-value pair.
In the above example, `color` would be the key and `#101010` would be the value.

```haskell
getTagName
  ::  String
  ->  String
getTagName
  s
  =
  case readP_to_S parseTagName s of
    []    -> ""
    (x:_) -> toLower' $ fst x
```

This returns the tag name in lowercase.

```haskell
parseTagName
  ::  ReadP String
parseTagName
  = do
  _ <- char '<'
  _ <- munch (== '/')
  _ <- skipSpaces
  n <- munch1 (\ c -> c /= ' ' && c /= '>')
  _ <- munch  (/= '>')
  _ <- char '>'
  return n
```

The tag name is the first string of non-whitespace characters
after the opening angle bracket,
a possible forward slash,
and some possible whitespace
and before some more whitespace
and/or the closing angle bracket.

```haskell
parseTagAttributes
  ::  [TagAttribute]
  ->  ReadP [TagAttribute]
parseTagAttributes
  tagAttributes
  = do
  s <- look
  case s of
    "" -> return tagAttributes
    _  -> do
      let h = head s
      case h of
        '>' -> return tagAttributes
        '<' -> trimTagname >> parseTagAttributes'
        _   -> parseTagAttributes'
  where
    parseTagAttributes'
      ::  ReadP [TagAttribute]
    parseTagAttributes'
      = do
      tagAttribute <- parseTagAttribute
      parseTagAttributes
        ( add
            (\ a b -> fst a /= fst b)
            tagAttributes
            tagAttribute
        )
```

`parseTagAttributes` recursively goes through the input string, collecting up the key-value pairs.
At the start of the tag (`<`), it first trims the tag name before tackling the attributes.
It stops parsing for attributes when it reaches the closing angle bracket (`>`).
If a tag happens to have duplicate attributes (based on the key),
`add` will ensure only the latest one remains in the list.

```haskell
trimTagname
  :: ReadP ()
trimTagname
  =
      char '<'
  >> skipSpaces
  >> munch1 (\ c -> c /= ' ' && c /= '>')
  >> return ()
```

This trims or discards the tag name.

```haskell
parseTagAttribute
  ::  ReadP TagAttribute
parseTagAttribute
  = do
  _ <- skipSpaces
  k <- munch1 (/= '=')
  _ <- string "=\""
  v <- munch1 (/= '\"')
  _ <- char '\"'
  _ <- skipSpaces
  return (toLower' k, v)
```

The attribute key is any string of non-whitespace characters before the equal sign.
The attribute value is any characters after the equal sign and double quote and before the next immediate double quote.

```haskell
isTag
  ::  String
  ->  Bool
isTag
  s
  =
  isOpeningTag s || isClosingTag s
```

A string is a tag if it is either an opening tag or a closing tag.

```haskell
isOpeningTag
  ::  String
  ->  Bool
isOpeningTag
  s
  =
  isPresent $ readP_to_S parseOpeningTag s
```

A string is an opening tag if the opening tag parser succeeds.

```haskell
isClosingTag
  ::  String
  ->  Bool
isClosingTag
  s
  =
  isPresent $ readP_to_S parseClosingTag s
```

A string is a closing tag if the closing tag parser succeeds.

### Running The SRT Parser

<span>
<p align="center">
<img alt="Parsed SRT Results" src="https://i.imgur.com/owAu628.jpg">
<br>
<sup></sup>
</p>
</span>

Now that we've assembled the parser, let's try it out.

```haskell
> let srt =
>       " 1\n\
>       \0:0:0,1 --> 0:1:0.2  x1:1 X2:3  y1:4 y2:10\n\
>       \<font color=\"red\" color=\"blue\">This is some <b><u><i>\n \
>       \subtitle \n\
>       \</u>text.</b>  "
> readP_to_S parseSrt srt
[([ SrtSubtitle
      { index = 1
      , start = Timestamp {hours = 0, minutes = 0, seconds = 0, milliseconds = 1}
      , end   = Timestamp {hours = 0, minutes = 1, seconds = 0, milliseconds = 2}
      , coordinates = Just (SrtSubtitleCoordinates {x1 = 1, x2 = 3, y1 = 4, y2 = 10})
      , taggedText =  [ TaggedText
                        { text = "This is some "
                        , tags = [ Tag {name = "font", attributes = [("color","blue")]}
                                 ]
                        }
                      , TaggedText
                          { text = "\n subtitle \n"
                          , tags = [ Tag {name = "font", attributes = [("color","blue")]}
                                   , Tag {name = "b",    attributes = []}
                                   , Tag {name = "u",    attributes = []}
                                   , Tag {name = "i",    attributes = []}
                                   ]
                          }
                      , TaggedText
                          { text = "text."
                          , tags = [ Tag {name = "font", attributes = [("color","blue")]}
                                   , Tag {name = "b",    attributes = []}
                                   , Tag {name = "i",    attributes = []}
                                   ]
                          }
                      , TaggedText
                          { text = "  "
                          , tags = [ Tag {name = "font", attributes = [("color","blue")]}
                                   , Tag {name = "i",    attributes = []}
                                   ]
                          }
                      ]
      }
  ]
, ""
)]
```

Here you see the result of parsing a test string.
Notice the errors in the test string like the use of a period instead of a comma or the duplicate tag attribute.

## Exercises

- Write a program that can convert an SRT file to a JSON file.
- Rewrite the version number parser using Parsec instead of ReadP.
- Rewrite the SRT parser using Parsec instead of ReadP.

## Copyright

(C) 2019 David Lettier
<br>
[lettier.com](https://www.lettier.com/)
