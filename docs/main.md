
# Ririsu

## Overview

Ririsu is a small concatenative esoteric language that's perfect for
[code golfs][]. It's, in fact, even less readable than Perl!

[code golfs]: http://en.wikipedia.org/wiki/Code_golf


## Examples

### Sum all numbers of a sequence

```text
[123456789][li]|[+].
```

### Return all even numbers from a sequence

```text
[123456789][li]|[[2]i~%[0]i=]#
```

### Solving the [100 Doors problem][]

```text
[e]h[^[f]~[[1]i~-]~[2]i>!?],r[s^o=]|
```

[100 Doors problem]: http://rosettacode.org/wiki/100_doors


## Features

  - Higher-Order and First-Class functions;
  - Dynamic scoping;
  - Purity (within a Map/Fold);
  - Untyped to the core;
  - No I/O;


## Installing

Ririsu is implemented in [Erlang](http://www.erlang.org/), so you'll need that
to run anything. Once you've got that sorted out, just clone the repository:

```bash
$ git clone git://github.com/kurisuwhyte/ririsu
$ cd ririsu
$ make
```

## Running

Use `ririsu repl` to get a Read-Eval-Print-Loop.

You can evaluate code by running `ririsu <filename>` or 
`ririsu eval <string>`. You can also initialise the stack with the contents of
the STDIN, by prepending `-` to the command 
(e.g.: `echo "12" | ririsu - eval "hi~hi~ +"`) 


# Getting Started

## Warming up

So, now that you have a REPL we can try running some code. First, Ririsu is a
concatenative language, this means that everything works on top of a
stack. Some commands will push data on the stack, and some will use the top
values of the stack to do computation, and push the result back on the stack.

For example, addition would work like this:

```bash
ririsu> [1]i[2]i+
[3]
```

Where the previous operation is evaluated as follows:

```text
We start with an empty stack: []
"1" gets converted to an integer and pushed on the stack: [1]
"2" gets converted to an integer and pushed on the stack: [2, 1]
"+" operates on the top two values, and pushes the result back: 2 + 1 => [3]
```

> **Note:** Ririsu is untyped, so you can shoot yourself in the foot without realising you're shooting yourself in the foot, yay!


## Down the rabbit hole

Okay, if you ever heard about
[RPN](http://en.wikipedia.org/wiki/Reverse_Polish_notation) that was no
biggie. So let's get to the interesting stuff. In Ririsu you can use code as
data, and data as code. The stack is always what you use to pass arguments to
functions, and these arguments can be code (yes, yes, Higher-Order functions
and all that).

To treat code as data, you have to "Quote" it, so that it won't be executed
just yet:

```text
ririsu> [[1]i[2]i+]
[["[1]i[2]i+"]]
```

We can execute any list using the operation `$`:

```text
ririsu> $
[3]
```

Now things start getting interesting, since you can pass code around, and you
can evaluate it, it means we can do Higher-Order functions! Some of them are
already core-coded for your convenience, like Map:

```text
ririsu> [123456]
[["123456"], [3]]
ririsu> [li1+]|
[["234567"], [3]]
```

## Manipulate the stack, they said. It'll be fun, they said...

Sometimes you'll need to manipulate the stack so that the operation you're
applying gets arguments in the correct order. Operations are really just
functions that take in a Stack and return a new Stack — in Ririsu, though, you
also get **dynamic scoping**, since functions also take an environment.

Suppose you want to check if the square root of a value is an exact number. You
could do it by cloning items on the stack:

```text
ririsu> [5]is
[2.23606797749979]
ririsu> ^
[2.23606797749979,2.23606797749979]
ririsu> o
[2,2.23606797749979]
ririsu> =
[false]
```

Or you can use the dynamic scoping madness:

```text
ririsu> [[5]is]A@
[]  # just stored `square-root(5)` in `A`
ririsu> AAo=
[false]
```

> **Note:** Sadly, Erlang doesn't support unicode :(


## What else?

You can find the [Reference](reference.html) in this documentation. There's
also a handful of `examples/` laying around. The
[Concatenative Wiki](http://concatenative.org/wiki/view/Front%20Page) is an
amazing place to learn more about concatenative programming in general :)


# Misc

## Inspirations

  - [Brainfuck](http://esolangs.org/wiki/Brainfuck);
  - [Burlesque](http://mroman.ch/burlesque/);
  - [Factor](http://factorcode.org/);
  - [Haskell](http://www.haskell.org/haskellwiki/Haskell);


## Attributions

[Photo](http://www.flickr.com/photos/leradiateur/6093706876/) taken from
Flickr, licensed under CC.

This whole website is here thanks to [rstacruz](https://github.com/rstacruz/)'s
amazing [Flatdoc](https://github.com/rstacruz/flatdoc). Seriously guys, you
gotta check that out ;)


## Changelog

#### 0.3.0 — 29th August, 2013

First actual release.


## Boring stuff

Ririsu is licensed under the WTFPL.
