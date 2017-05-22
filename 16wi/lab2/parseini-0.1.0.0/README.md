% Stanford CS240h Lab 2

Please use this skeleton code as your starting point:
[lab2.tar.gz](http://www.scs.stanford.edu/16wi-cs240h/labs/lab2.tar.gz).

## Overview

You will write a parser for [git-style INI
files](https://git-scm.com/docs/git-config/2.1.4#_syntax), along with a
pretty-printer to reformat the parsed data.

**NOTE** (January 21, 5:00p): The above link has been updated with an older
and more clearly worded version of the git-config spec.

### INI file syntax

The above link gives a thorough description of the input file syntax. Please
note the following clarifications and slight deviations from the above spec:

- We *do not* expect you to implement either the deprecated
  `[section.subsection]` syntax or support for including other files.
  (You can implement support for including other files as a bonus; see below.)

- Your parser should support ASCII input files; UTF-8 is fun but not required.

- Variables can be multivalued; you should parse and retain all of the values listed
  in the input file. We do not require you to retain the exact ordering of values
  in the input file, but as noted below, your pretty-printer must be idempotent,
  so the ordering must be deterministic.

- Your parser should infer the [type](https://git-scm.com/docs/git-config#_values)
  of each value (note that multivalued variables can have a different type for
  each value). In particular,

    * Bool: `on`, `true`, and `yes` should all parse as `True`, while  `off`, `false`,
      and `no` should parse as `False`. As noted in the spec, this should be case
      insensitive. Two other notes:

        - `0` and `1` should be parsed as Ints rather than Bools, **despite what the
           spec says**.

        - As specified in the `git-config` man page, lines that contain only a key name
          and no `= value` should parse as a boolean with value `True`.

    * Int: Integers should have an optional sign (`+` or `-`) followed by one
      or more base-10 digits. Your parser should accept optional suffixes
      `k`, `M`, `G`, `T`, `P`, and `E`, indicating that the value should be
      scaled by $2^10$, $2^20$, etc.

    * Color: You do **not** need to implement the Color type described in newer
      versions of the git-config manpage.

    * String: everything else should be treated as a string. Be careful to follow
      the specification regarding escaping, internal vs. external whitespace,
      and quoting!

### Parsed representation

In the skeleton code linked above, we have provided several types and
signatures that specify the expected type of the parsed data. Please respect
this programming interface, or we will not be able to grade your lab!

### Pretty-printer

Once you have parsed the input file into an internal representation, the
final step is to pretty-print the resulting data using the INI syntax. Be sure
that your pretty-printer is idempotent! That is, running the pretty-printed
output through your parser and pretty-printer again should give the same
result. (Be careful of multivalued variables here!)

(The `bytestring` package exports a module called `Data.ByteString.Builder`
that you might find useful here.)

### Commandline interface

Your implementation should take an input file on STDIN and produce
pretty-printed output on STDOUT.

### Example inputs and outputs

In the skeleton code tarball you will find an `examples` directory. These are
example inputs and outputs to help you understand some of the edge cases of
the INI file spec. The example files come in pairs, e.g., `ex1In.txt` and
`ex1Out.txt`, representing an input and the corresponding pretty-printed
output. **It is not necessary to match the pretty-printed output exactly**;
for example, there are many equivalent representations of strings, or your
pretty-printer might insert whitespace differently. The purpose of these
examples is to help clear up specifics regarding parsing escapes, syntax
that must be supported, etc.

Please read the spec and examples carefully!

## Clarifications (updated January 22, 10:00a)

The following clarifications are based on Piazza questions. Please continue
posting requests for clarification as necessary!

In the below, where we list characters in single quotes, we mean the character
that you'd get if you typed exactly that string into ghci (so, e.g., the
backslash character is represented below by `'\\'`).

#### Spec oracle

To help you better understand the git-config specification, we've set up a
[web interface to our implementation](http://lab2oracle.phonon.net) that you
might find helpful.

#### Clarified specification

It turns out that recent edits to the git-config manpage and made its Syntax
section less useful. We've updated the link above to point to the
[git-config manpage from version 2.1.4](https://git-scm.com/docs/git-config/2.1.4#_syntax),
which is much more precise in its description of the syntax.

#### Double quotes

Double quoted strings can only appear inside string values and subsection
names. Thus, `key = "1024"` is a string, while `key = 1024` is an integer
(and `key = "True"` is a string, while `key = True` is a boolean).

Any character except backslash (`'\\'`) and double quote (`'\"'`) can appear
inside double quotes and is retained verbatim; backslash and double quote
must be escaped, and all valid escape sequences must be accepted (see below
for more on escape sequences).

Note that this includes `'#'` and `';'`. Inside double quotes, these do *not*
indicate the start of a comment.

Subsection names have special rules. See below for more information.

Unterminated double quotes should result in a parse error.

#### Subsection names

Subsection names are double quoted and may contain escape sequences. However,
newlines of any form (quoted, unquoted, and line continuations) are not
allowed inside subsection names, and should result in a parse error.

#### Escape sequences

There are several escape sequences that your parser should accept inside
subsection names and string values. However, the acceptable escape sequences
inside subsection names and string values are different (see above for more
information on subsection names).

The internal value stored in your `INIFile` should be the value that the
escape sequence maps to. For example, `\\n` maps to `\n`, so the latter
should be stored in you `INIFile`.

An escaped whitespace character (e.g., tab, newline, and backspace) in a string
value is never considered leading or trailing whitespace. So after parsing
`key = \\tasdf\\t` the corresponding value should be `\tasdf\t`.

Invalid escape sequences should result in a parse error. This includes
backslash followed by end-of-file.

#### Line continuations

A particularly confusing escape sequence seems to be line continuations.
There are several important points with regard to line continuations:

1. Line continuations can only appear in a *value*. A value begins *after* the
   `'='` on a line like `key = value`; thus, the `key` syntax (indicating a
   `True` bool) does *not* admit line continuations, nor can line continuations
   appear in comments, in section declarations, etc.

2. A line continuation is a special case of an escape sequence. The literal
   string `\\\n`, i.e., backslash followed by newline (ASCII `0x5C0A`)
   is an escape sequence that just means the empty string.

3. Line continuations result in an empty string, and leading and trailing
   whitespace in values is removed. Taken together, this means that if leading
   or trailing whitespace is broken by an escaped newline, any whitespace after
   the escaped newline is still considered leading or trailing whitespace.
   For an example of this, see `ex7In.txt` and `ex7Out.txt`.

#### Pretty printing

When pretty printing, the internal representation of your `INIFile` needs
to be converted to a format that can be parsed again.  This means that
special characters in string values must appear in double quotes and/or
be properly escaped.

You do *not* need to replicate line continuations in your pretty printed
output.

#### OverloadedStrings

Feel free to use the `OverloadedStrings` language feature.

### Bonus!

As a bonus, you can implement support for including other files, as described in the
[git-config man page](https://git-scm.com/docs/git-config#_includes).

Even if you decide not to implement this, *we encourage you to think about how you would
implement it*. This is a nice example of the kinds of design questions that often arise
in functional programming: on the one hand, we would like our parser to be pure, while
on the other hand file inclusion requires IO.

### Other information

Please ask (early!) any qualifying questions about the specification on
[Piazza](https://piazza.com/stanford/winter2016/cs240h).

You can use
[attoparsec](http://hackage.haskell.org/package/attoparsec) and
[bytestring](http://hackage.haskell.org/packages/bytestring)
for this lab (or you can roll your own parser if
you'd like). For testing, you should use
[hspec](https://hackage.haskell.org/package/hspec)
and/or [QuickCheck](https://hackage.haskell.org/package/QuickCheck).

A useful resource for learning more about parser combinator libraries like
attoparsec is chapter 16 of Real World Haskell,
[Using Parsec](http://book.realworldhaskell.org/read/using-parsec.html).
(This chapter covers Parsec, which is similar but not identical to attoparsec.
You will nevertheless find it a useful companion to the attoparsec
documentation.)

## Due Date

Lab 2 should be submitted by the start of class on January 27.

You have 48 hours of late days for the three labs. They are consumed in 24 hour
blocks and are used automatically. After they are used, you'll have the maximum
grade you can receive for a late lab reduced by 25% each day.

## Stack -- Build & Test Tool

We are using the [stack](https://www.stackage.org/) build tool for this course.
Once getting the skeleton code, you should be able to run:

        stack setup
        stack build
        stack test
        stack exec parseini-exe

We have provided an overview of Stack
[here](http://www.scs.stanford.edu/16wi-cs240h/labs/stack.html).

## Provided Files

The files provided to get started are:

* parseini.cabal, stack.yaml -- specifies the build system.

* src/ParseIni.hs -- implement your pure parser here

* src/PrettyPrintIni.hs -- implement your pretty-printer here

* app/Main.hs -- implement the command line interface to your pure functions
  here (argument parsing, stdin/stdout handling).

* test/Spec.hs -- the test harness. You need to edit this and add your own
  tests! We provide a few very simple ones.

**PLEASE DON'T CHANGE THE INTERFACE OF THE `ParseIni.hs` OR `PrettyPrint.hs`
MODULES, AS WE WILL EXPECT IT TO BE THE SAME WHEN TESTING!** (Do not add any
extra source files, either.)

## Testing Lab 2

Some skeleton code for a test framework is provided in `test/Spec.hs`. You'll
need to edit it to add your own tests. The test framework uses a Haskell
package called [hspec](http://hspec.github.io/). Please refer to it for
documentation on how to use it.

## Grading

We strongly encourage you to take testing seriously and write a comprehensive
test suite. Grading will be only on functionality, but we will try to give
feedback on your coding style.

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
[Piazza](https://piazza.com/stanford/winter2016/cs240h) or email the staff
mailing [list](mailto:cs240h-staff@scs.stanford.edu).

## Suggested Music

We suggest you listen to
[The Flaming Lips - The Soft Bulletin](https://www.youtube.com/watch?v=NgvlDOhRQOA)
if you need music while programming.
