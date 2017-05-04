% Stanford CS240h Lab 1

You will write a simple function that performs [glob
matching](http://en.wikipedia.org/wiki/Glob_%28programming%29) Glob
matching is a simple form of pattern matching (i.e., regular
expressions but greatly reduced).

The function should have the type: 

        type GlobPattern = String

        matchGlob :: GlobPattern -> String -> Bool

Where the first argument will be the glob pattern and the second
argument will be the string to match on. The function should return
`True` if the string matches the glob.

You are encouraged to use only the base package of Haskell for this
lab. So hand roll a parser yourself.

We are providing a skeleton Cabal project to help get started,
download it from
[here](http://www.scs.stanford.edu/14sp-cs240h/labs/lab1.tar.gz).

## Due Date

Lab 1 should be submitted by the start of class (12:50pm) on *Tuesday,
April 8th*.

You have 48 hours of late days for the three labs. They are consumed
in 24 hour blocks and are used automatically. After they are used,
you'll have the maximum grade you can receive for a late lab reduced
by 25% each day.

## Glob Matching

Your glob matcher should handle any combination of the following
patterns:

* `'<char>'` -- matches a literal character (whatever `<char>` is).
  This extends to all characters except those that have a special
  meaning (special symbols).
* `'?'` -- matches any single character.
* `'*'` -- matches any string including the empty string.
* `"[<set match>]"` -- matches any character from the set match set,
  explained below.
* `"\<char>"` -- an escaped character, this matches a single literal
  character (`<char>`) where `<char>` can be any character,
  *including* the special symbols.

### Literal Matching

Initially parsing the glob pattern you treat all characters as literal
except for `?`, `*`, `\` and `[`. So the following glob patterns are
exact matchers:

* `"abcde"`
* `"a]b"`
* `"\a\b\c\d\e"` (equivalent to `"abcde"`)
* `"-adf]ai1"`

Note that we don't treat `]` as a special character unless we are
trying to close a set match.

### Escaped Literals

All special symbols can also be escaped (just as regular characters
can be), giving us the following glob patterns that are also exact
matches:

* `"\[a]"` -- matches the string `"[a]"`
* `"\*\*\?"` -- matches the string `"**?"`
* `"\\a\\"` -- matches the string `"\a\"`
* `"ab\*ba"` -- matches `"ab*ba"`
* `"ab\[ba"` -- matches `"ab[ba"`
* `"ab[a\]]ba"` -- matches `"ab]ba"` or `"ababa"`


### Set Match

When parsing a glob pattern, when you find an unescaped `'['` then you
need to parse the pattern from that point on as a set match. A range
group is closed by the first unescaped `']'`.

Please note that set matches can't nest, which means that once a set
match has started, the `'['` character should be treated as a literal,
not a special symbol any more. For example:

* `"[ab[c]"` -- matches a single character that is either `'a'`,
  `'b'`, `'c'` or `'['`.

A set match specifies a set of characters valid for matching the next
character in the string we're matching on. It matches one character
only.

Set matches work as follows:

* `"[abcd]"` -- matches a single character that is either `'a'`,
  `'b'`, `'c'` or `'d'`.
* `"[a-z]"` -- matches any character in the range of `'a'` to `'z'`.

This suggest then that set match be parsed as follows:

* `'<char>'` -- add a single literal character to the possible
  characters we can match on as long as `<char>` isn't a special
  symbol.
* `"<char_1>-<char_2>"` -- add all characters between the literal
  characters, `<char_1>` and `<char_2>`. We refer to these as
  *ranges* and specify them further below. A set match can contain
  multiple ranges.
* `']'` -- an unescaped `']'` closes the set match.
* `"\<char>"` -- an escaped character, this matches a single literal
  character (`<char>`) where `<char>` can be any character,
  *including* the set match special symbols.

Please note that if you end up with an empty set match, either because
it is just empty ("`[]`") or only contains empty ranges, then this
can't match anything in a string and so the whole glob pattern can't
match any string.

#### Ranges

Ranges have some subtle cases. The simple form is:
`"<char_1>-<char_2>"`, for example `"a-z"` adds all characters between
`'a'` to `'z'` to our set match. A set match can contain multiple
ranges.

The corner cases though are:

* A "`-`" as the first or last character of a set match should be
  treated as a literal `'-'`. So `"[---]"` would be valid as the first
  and last `'-'` are treated literally. So it's simply a range
  containing just `'-'`.
* A "`-`" following a range should be treated as a literal `'-'`. So
  `"[a-c-z]"` is a set match containing `'a', 'b', 'c', '-', 'z'`.

Ranges should simply use the Haskell built-in `Ord` instance for
`Char` for determining the range.

You may also end up with a range that is empty (i.e., `"[z-a]"`).

#### Examples

So for example:

* `"[-abc]"` -- matches a single character that is either '-', 'b', or
  'c'.
* `"[abc-]"` -- same as above.
* `"[--]"` -- matches a literal `-` character.
* `"[---]"` -- is a range but of only one character, `-`.
* `"[----]"` -- is a range (of just `-`) and a literal `-`, so it just
  matches a `-` character.
* `"[a-d-z]"` -- matches `a`, `b`, `c`, `d`, `-`, `z`.
* `"[z-a]"` -- empty range, mathces nothing.

## Cabal -- Build & Test Tool

Cabal is the standard build and packaging tool for Haskell. A starting
framework is provided for you. You can find the user guide for Cabal
[here](http://www.haskell.org/cabal/users-guide/developing-packages.html#test-suites).

## Provided Files

The files provided to get started are:

* globber.cabal -- specifies the build system.
* Globber.hs -- you should edit this file to implement a glob matcher
* TestGlobber.hs -- the test harness. You need to edit this and add
  your own tests!

## Building Lab 1

To get up and running (using Cabal), issue the following commands:

        cabal sandbox init

This will initiate a self-contained build environment where any
dependencies you need are installed locally in the current directory.
This helps avoid the Haskell equivalent of 'DLL Hell!'. If your
version of cabal is older such that it doesn't have the `sandbox`
command, then just proceed without it and it should all be fine.

Next, you want to build the lab. For that, issue the following
commands:

        cabal install --only-dependencies --enable-tests
        cabal configure --enable-tests
        cabal build

After that, you should also be able to run the test harness simply by
typing:

        cabal test

And you'll get some pretty output!

## Testing Lab 1

Some skeleton code for a test framework is provided in
`TestHarness.hs`. You'll need to edit it to add your own tests. The
test framework uses a Haskell package called
[hspec](http://hspec.github.io/). Please refer to it for documentation
on how to use it.

## Grading

While we strongly encourage you to take testing seriously and write a
comprehensive test suite, we are only going to grade you on your glob
matching implementation.

Grading will be just done on functionality but we will try to give
feedback on your coding style.

## Submitting

First, simply type:

        cabal sdist

This will generate a tar file of your code in `dist/globber.tar.gz`.

Then go to [upload.ghc.io](http://upload.ghc.io/) and submit your work
through the online form. You can resubmit as many times as you want up
until the deadline.

If you have any trouble submitting on-line, then please email the
staff mailing [list](mailto:cs240h-staff@scs.stanford.edu).

