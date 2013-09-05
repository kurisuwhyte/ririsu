
# The Language

## Overview

Ririsu is a small concatenative esoteric language that's perfect for
[code golfs][]. It's, in fact, even less readable than Perl!

[code golfs]: http://en.wikipedia.org/wiki/Code_golf


## Examples

### Sum of numbers in a list

```text
[∘i]→[0]i[+]⚺
```

Run as: `echo 1 2 3 4 5 6 7 8 9 | ririsu - sum.ri`

### Even numbers in a list

```text
[∘i]→[[2]i⇄%[0]i=]⋈
```

Run as: `echo 1 2 3 4 5 6 7 8 9 | ririsu - even.ri`

### Solve the [100 Doors problem][]

```text
[100]iι[√↠⎨=]→
```

Run as: `ririsu modules/prelude.ri 100-doors.ri`

[100 Doors problem]: http://rosettacode.org/wiki/100_doors


## Features

  - Higher-Order and First-Class functions;
  - Dynamic scoping;
  - Purity;
  - Untyped to the core;
  - No I/O;


## Installing

Ririsu is implemented in [Elixir](http://www.elixir-lang.org/), so you'll need
that to run anything. Once you've got that sorted out, just clone the
repository:

```bash
$ git clone git://github.com/kurisuwhyte/ririsu
$ cd ririsu
$ mix escriptize
```

## Running

Use `ririsu repl` to get a Read-Eval-Print-Loop.

You can evaluate code by running `ririsu <filename>` or 
`ririsu eval <string>`. You can also initialise the stack with the contents of
the STDIN, by prepending `-` to the command 
(e.g.: `echo "12" | ririsu - eval "hi⇄hi⇄↓+"`) 


# A Tour of Ririsu

## Warming up

Now that you have a REPL, we can try interacting with some code. Ririsu is a
concatenative, stack-based language. This means that everything works on top of
a stack, where everything is basically a function taking a stack and returning
a new stack. Functions are represented by single unicode characters. Everything
is a function, including numbers and what not.

When Ririsu doesn't understand a command, it takes it to mean a function that
takes in a stack, and returns a new stack with the command at the top of
it. That is, if we have an empty stack `[]`, and evaluate the command `M`,
which is not understood by Ririsu, we get the new `["M"]` stack.

```bash
ririsu> 
[]
ririsu> M
"M"
```

There are a few unary, binary and n-ary operations built-in in Ririsu, which
provide means of manipulating the stack, code lists and values on the
stack. For example, the `+` operation is provided for addition, and the `i`
operation to convert a list to an integer.

```bash
ririsu> [1]i
[1]
ririsu> [2]i
[2, 1]
ririsu> +
[3]
```

> **Note:** Ririsu is untyped, so you can shoot yourself in the foot without
> realising you're shooting yourself in the foot, yay!


## Higher-Order Functions

Okay, if you ever heard about
[RPN](http://en.wikipedia.org/wiki/Reverse_Polish_notation) that was no
biggie. So let's get to the interesting stuff. In Ririsu you can use code as
data, and data as code. The stack is always what you use to pass arguments to
functions, and these arguments can be code too!

To treat code as data, you have to "Quote" it, so that it gets pushed onto the
stack as a list of commands rather than applied to the current stack. 

```text
ririsu> [[1]i[2]i+]
[["[1]i[2]i+"]]
```

At any moment we can apply a list to the rest of the stack using the `▶`
operation.

```text
ririsu> ▶
[3]
```

And since you can pass code around and apply it to the stack, you can do
higher-order functions! Some of them, like Map, are already provided in the
standard library for your convenience.

```text
ririsu> [123456]
[["123456"], [3]]
ririsu> [∘i[1]i+]→
[["234567"], [3]]
```


## Dynamic Scoping

Sometimes you'll need to manipulate the stack so that the operation you're
applying gets arguments in the correct order. Operations are really just
functions that take in a Stack and return a new Stack — in Ririsu, though, you
also get **dynamic scoping**, since functions also take an environment.

Suppose you want to check if the square root of a value is an exact number. You
could do it by cloning items on the stack.

```text
ririsu> [5]i√
[2.23606797749979]
ririsu> ↠
[2.23606797749979,2.23606797749979]
ririsu> ⎨
[2,2.23606797749979]
ririsu> =
[false]
```

Or you can use the dynamic scoping madness, by way of the `@` operation.

```text
ririsu> [[5]i√]A@
[]  # just stored `square-root(5)` in `A`
ririsu> AA⎨=
[false]
```


## What else?

You can find the [Reference](reference.html) in this documentation. There's
also a handful of `examples/` laying around. The
[Concatenative Wiki](http://concatenative.org/wiki/view/Front%20Page) is an
amazing place to learn more about concatenative programming in general :)


# Misc

## Inspirations

  - APL
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

#### 0.4.0 — 12th August, 2013

Full rewrite in Elixir with Unicode support.

#### 0.3.0 — 29th August, 2013

First actual release.


## Boring stuff

Ririsu is licensed under the WTFPL.
