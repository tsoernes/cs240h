% Stanford CS240h Lab 1

Please use this skeleton code as your starting point:
[lab1.tar.gz](http://www.scs.stanford.edu/16wi-cs240h/labs/lab1.tar.gz).

## Overview

You will write a simple Haskell version of the UNIX `tr` program
([POSIX](http://pubs.opengroup.org/onlinepubs/9699919799/utilities/tr.html)).

Your version will support two modes, translate or delete:

        tr string1 string2
        tr -d string1

Quoting from the OpenBSD man page:

> In the first synopsis form, the characters in string1 are translated into the
> characters in string2 where the first character in string1 is translated into
> the first character in string2 and so on.  If string1 is longer than string2,
> the last character found in string2 is duplicated until string1 is exhausted.
>
> In the second synopsis form, the characters in string1 are deleted from the
> input.

That is, we get the following output below:

        $ echo hello | tr eo oe
        $ holle

We only expect you to support ASCII, but given Haskell's default `Char` and
`String` type support unicode, it shouldn't be any extra effort to support
unicode.

### Literal Character Sets Only

We only expect you to support literal character sets. That is, arguments of the
form:

        tr abcd 1234
        tr ger uuu
        tr -a2 ---

You should not attempt to interpret `string1` or `string2` in any other way,
for example as a character range (`a-z`).

### Command Line Arguments

You should implement a very simple command line parser, if the first argument
is `"-d"`, then we are running in delete mode. Any other value for the first
argument is regarded as running in translate mode. There should always be just
two arguments.

Translate mode examples:

        tr ab ba
        tr -c aa
        tr d- bc

Delete mode examples:

        tr -d abc
        tr -d -d

### Corner Cases

Firstly, if `string2` is shorter than `string1`. While POSIX says this is
undefined, we take the common convention of repeating the final character of
`string2`, until it's length is equal to `string1`. That is, the arguments:

        tr abc x

Should be treated as equivalent to:

        tr abc xxx


Secondly, if `string1` contains repeated characters. This (as with POSIX) we
consider undefined and will not be testing. That is, arguments such as:

        tr aa bc

Will not be used in testing your solution.

Third and finally, if `string1` is shorter than `string2`, we just truncate
`string2` to be the same length as `string1`.

### Pure Function Interface

We provide a module `Tr.hs` that has the interface we want you to implement.
**Do not change this interface!** We will be using it for testing and expect
it to remain the same.

The interface is as follows:

~~~~ {.haskell}
type CharSet = String

tr :: CharSet -> Maybe CharSet -> String -> String
tr _inset _outset xs = xs
~~~~

We use `CharSet` simply as an alias for String (a list of `Char`'s) simply to
give a more descriptive type.

The first argument to the `tr` function is the set of characters to map from,
and the second argument is the set of characters to map to. The third argument
corresponds to `STDIN` and the return value corresponds to `STDOUT`.

The second argument uses a `Maybe CharSet` type to differentiate between
translate and delete mode. In translate mode it will be a `Just` value, while
in delete mode it will be a `Nothing` value. This is:


        translate mode: tr "eo" (Just "oe") "hello" -> "holle"
        delete mode: tr "e" Nothing "hello" -> "hllo"

It's up to you how to handle the first argument being the empty string, or the
second argument being `Just ""`, we will not be testing this edge case. We
would encourage you to keep the function exception free and complete and
enforce any argument parsing constraints in the `Main.hs` file before calling
the `tr` function.

## Questions

Please ask (early!) any qualifying questions about the specification on
[Piazza](https://piazza.com/stanford/winter2016/cs240h).

## Allowed Imports

You are encouraged to use only the base package of Haskell for this lab.
Although, packages such as [hspec](https://hackage.haskell.org/package/hspec)
and [QuickCheck](https://hackage.haskell.org/package/QuickCheck) should be used
for testing (but on your own for learning them, we'll cover QuickCheck later in
the course).

## Due Date

Lab 1 should be submitted by the start of class on Monday, January 11th.

You have 48 hours of late days for the three labs. They are consumed in 24 hour
blocks and are used automatically. After they are used, you'll have the maximum
grade you can receive for a late lab reduced by 25% each day.

## Stack -- Build & Test Tool

We are using the [stack](https://www.stackage.org/) build tool for this course.
Once getting the skeleton code, you should be able to run:

        stack setup
        stack build
        stack test
        stack exec tr-exe

We have provided an overview of Stack
[here](http://www.scs.stanford.edu/16wi-cs240h/labs/stack.html).

## Provided Files

The files provided to get started are:

* tr.cabal, stack.yaml -- specifies the build system.
* src/Tr.hs -- implement a pure tr translator/deleter here.
* app/Main.hs -- implement the command line interface to your pure function
  here (argument parsing, stdin/stdout handling).
* test/Spec.hs -- the test harness. You need to edit this and add your own
  tests! We provide two very simple ones.

PLEASE DON'T CHANGE THE INTERFACE OF THE `Tr.hs` module AS WE WILL EXPECT IT TO
BE THE SAME WHEN TESTING! DO NOT ADD ANY EXTRA SOURCE FILES EITHER.

## Testing Lab 1

Some skeleton code for a test framework is provided in `test/Spec.hs`. You'll
need to edit it to add your own tests. The test framework uses a Haskell
package called [hspec](http://hspec.github.io/). Please refer to it for
documentation on how to use it.

## Grading

While we strongly encourage you to take testing seriously and write a
comprehensive test suite, we are only going to grade you on your `tr` matching
implementation.

Grading will be done only on functionality but we will try to give feedback on
your coding style.

## Submitting

First, simply type:

        stack sdist

This will generate a tar file of your code. Please don't add any extra source
files without changing the `tr.cabal` file correspondingly. Otherwise, your
submission will be broken and missing files.

Then go to [upload.ghc.io](https://upload.ghc.io/) and submit your work through
the online form. You can resubmit as many times as you want up until the
deadline.

If you have any trouble submitting on-line, then please post on
[Piazza](https://piazza.com/stanford/winter2016/cs240h), or email the staff
mailing [list](mailto:cs240h-staff@scs.stanford.edu).

## Suggested Music

We suggest you listen to
[Cut Copy - Forest Through The Trees Mixtape](https://soundcloud.com/cuttersrecords/forest-through-the-trees-mixtape)
if needing music while programming.

