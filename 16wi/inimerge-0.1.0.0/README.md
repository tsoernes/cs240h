% Stanford CS240h Lab 3

Please use this skeleton code as your starting point:
[lab3.tar.gz](http://www.scs.stanford.edu/16wi-cs240h/labs/lab3.tar.gz).

You only need to edit the `src/MergeIni.hs` file!

## Overview

In this lab, you will implement a git merge driver for INI files.  The
usage will be:

        inimerge [-w] headFile baseFile mergeFile

`headFile` corresponds to the latest version of the file.  `mergeFile`
consists of a file that you want to merge in.  `baseFile` is the
common ancestor of `headFile` and `mergeFile`.  By default your merge
driver will print the merged file to standard output (since this is
easiest for debugging).  However, the `-w` flag will cause it to
overwrite `headFile`, which is the behavior expected by git.  The
intended use will be to put the following in your `.git/config` file,
then use [gitattributes](http://git-scm.com/docs/gitattributes) to
assign this merge driver to INI files.

        [merge "inimerge"]
            name = INI file merge driver
            driver = inimerge -w %A %O %B
            recursive = text

## Strategy

The overall strategy will be as follows.  For any (section, key,
values) tuple that is the same in both the `headFile` and `mergeFile`,
use that version since the two files agree.  Otherwise, if `headFile`
agrees with `baseFile`, then assume that `mergeFile` has more recently
updated the file and use the values stored in `mergeFile`.
Conversely, if `mergeFile` and `baseFile` are identical, then use the
values from `headFile`.  Otherwise, you have a conflict.

If there is an update conflict, you will preserve all values.  Assume
that when a key has multiple values, the more recent values are added
earlier in the file.  Thus, you will date the list of values in
`headFile` and the list of values in `mergeFile` and find the longest
common suffix.  The merge file will contain first the unique prefix of
`mergeFile`, then the unique prefix of `headFile`, then the common
suffix of the two lists of values.

## The assignment

The assignment will consist of implementing a function in the file
`MergeIni.hs`.  The other files should be usable as-is, though you may
learn from reading them.

So as not to penalize people who did not do well in lab 2, we have
supplied you with an INI file parser and pretty-printer.  This parser
has a couple of differences from the lab 2 assignment.  Most
importantly, the `INIVal` type is just a strict bytestring, as we
don't care about parsing booleans and integers differently.  Second,
the parser we provided guarantees that when there is a duplicate key
in a section, the list of values preserves the order of the file (so
the head of the list is the value corresponding to the first
occurrence of the key in the file).

## Questions

Please ask (early!) any qualifying questions about the specification on
[Piazza](https://piazza.com/stanford/winter2016/cs240h).

## Due Date

Lab 3 should be submitted by the start of class on Wednesday, February 10th.

You have 48 hours of late days for the three labs. They are consumed in 24 hour
blocks and are used automatically. After they are used, you'll have the maximum
grade you can receive for a late lab reduced by 25% each day.

## Stack -- Build & Test Tool

We are using the [stack](https://www.stackage.org/) build tool for this course.
Once getting the skeleton code, you should be able to run:

        stack setup
        stack build
        stack test
        stack exec inimerge

We have provided an overview of Stack
[here](http://www.scs.stanford.edu/16wi-cs240h/labs/stack.html).

## Provided Files

You only need to edit the `src/MergeIni.hs` file!

The files provided to get started are:

* inimerge.cabal, stack.yaml, Setup.hs -- specifies the build system.
* app/IniMerge.hs -- implements the command line interface to your merger.
  Already done, no need to touch.
* src/MergeIni.hs -- implement your merge driver here!
* src/ParseIni.hs -- a INI parser implementation (from lab 2), no need to
  touch!
* src/Parser.hs -- a hand-written parser (rather than using parsec or
  attoparsec), no need to touch!
* src/PrettyPrintIni.hs -- a INI pretty-printer (from lab 2), no need to touch!
* test/Spec.hs -- the test harness. You need to edit this and add your own
  tests! We provide two very simple ones.

PLEASE DON'T CHANGE THE INTERFACE OF THE `MergeIni.hs` module AS WE WILL EXPECT
IT TO BE THE SAME WHEN TESTING! DO NOT ADD ANY EXTRA SOURCE FILES EITHER.

## Testing Lab 3

Some skeleton code for a test framework is provided in `test/Spec.hs`. You'll
need to edit it to add your own tests. The test framework uses a Haskell
package called
[test-framework](https://hackage.haskell.org/package/test-framework). Please
refer to it for documentation on how to use it.

## Grading

While we strongly encourage you to take testing seriously and write a
comprehensive test suite, we are only going to grade you on your `inimerge`
matching implementation.

Grading will be done only on functionality. We will provide an example solution
later to learn from. Come to office hours for more feedback.

## Submitting

First, simply type:

        stack sdist

This will generate a tar file of your code. Please don't add any extra source
files without changing the `inimerge.cabal` file correspondingly. Otherwise,
your submission will be broken and missing files.

Then go to [upload.ghc.io](https://upload.ghc.io/) and submit your work through
the online form. You can resubmit as many times as you want up until the
deadline.

If you have any trouble submitting on-line, then please post on
[Piazza](https://piazza.com/stanford/winter2016/cs240h), or email the staff
mailing [list](mailto:cs240h-staff@scs.stanford.edu).

## Suggested Music

We suggest listening to
[Newmangenerator](https://www.youtube.com/watch?v=3FrqDm12pVI).
if needing music while programming.

