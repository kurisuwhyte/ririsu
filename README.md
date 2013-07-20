Ririsu
======

A pure, untyped, parallel, distributed, stack-based data-processing esolang.


## Example

A quine:

```text

```

A program to return all even numbers of a sequence:

    [2~%0=]#
    
A program to sum all numbers of a sequence:

    [+].


## Running

    $ make
    $ echo "[dlrow olleh]" | ./bin/ririsu
    # [["h", "e", "l", "l", "o", " ", "w", "o", "r", "l", "d"]]


## The Language

Ririsu is a stack-based language with one-character operators:

### Stack manipulation

    ^ [A | _] -> [A A | _]                      Duplicates the top of the stack
    ~ [A B | _] -> [B A | _]                    Swaps the top of the stack
      [A | _] -> [_]                            Drops the top of the stack
                      
### Code and environment                      
                      
    @ [A [F] | _] -> [_]                        Defines A as an alias to F
    $ [A | _] -> [B]                            Evaluates A in the environment
    [                                           Enters quoting mode
    ]                                           Exits quoting mode

### Logical
                                                
    = [A B | _] -> [C | _]                      Structural equality
    > [A B | _] -> [C | _]                      True if A > B
    ! [A | _] -> [not A | _]                    Logical negation
    f                                           The constant false.

### Arithmetic
                                                
    + [A B | _] -> [C | _]                      Arithmetic addition
    - [A B | _] -> [C | _]                      Arithmetic subtraction
    * [A B | _] -> [C | _]                      Arithmetic multiplication
    / [A B | _] -> [C | _]                      Arithmetic division
    % [A B | _] -> [C | _]                      Modulo operation

### Lists
                                                
    : [A B | _] -> [[A | B] | _]                Cons
    & [A B | _] -> [A ++ B | _]                 Concatenation

### Branching

    ? [C [A:D] [B:E]| _] -> [D or E]            Evaluates A if C is true, else B

### Folding
    
    | [[F:A>B] [A] | _] -> [B | _]              Maps A over F
    # [[F:A>B] [A] | _] -> [[A] | _]            Filters A using F
    . [[F:A B>C] A [B] | _] -> [C | _]          Folds B using F starting from A
    , [[F:A>B] C | _] -> [[B] | _]              Unfolds a list until false.

### Others

Anything that doesn't match will just get pushed on the stack.


## Tests

( ... )


## Licence

WTFPL.
