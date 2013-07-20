Ririsu
======

A pure, untyped, parallel, distributed, stack-based data-processing esolang.


## Example

A quine:

```text

```

A program to return all even numbers of a sequence:

    [2%0=]#
    
A program to sum all numbers of a sequence:

    [+]\


## Installing

( ... )

## The Language

Ririsu is a stack-based language with one-character operators and unicode
support, so you can feel like you're programming in APL but not really.

### Basic operators

    ^                   x       Duplicates the top of the stack
    !                   x y     Swaps the head and second items of the stack
    @                   n c     Defines a new operator with [n]ame for [c]ode
    ~                   x       Evaluates X
                        x       Drops x
    +                   x y     arithmetic plus
    -                   x y     arithmetic minus
    /                   x y     arithmetic division
    *                   x y     arithmetic multiplication
    :                   x y     List consing
    =                   x y     Structural equality
    ¬                   x       logical not
    ∨                   x y     logical or
    ∧                   x y     logical and
    |                   f x     Maps x over f
    \                   f y x   Reduces x with f starting from y (foldr)
    #                   f x     Filters x with f
    [                           Begin quoting
    ]                           End quoting
    (anything else)             Gets pushed onto the stack



## Tests

( ... )


## Licence

WTFPL.
