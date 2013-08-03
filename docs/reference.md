# Stack manipulation

## `^` - duplicate
```hs
{D} [A | _] -> {D} [A A | _]
```

Duplicates the top of the stack.

## `~` - swap
```hs
{D} [A B | _] -> {D} [B A | _]
```

Swaps the top two items in the stack.

## ` ` - drop
```hs
{D} [A | _] -> [_]
```

Drops the top item off the stack.


# Code and environment                      

## `@` - define
```hs
{D} [A F | _] -> {D & A=F} [_]
```

Defines `A` as an alias for the code list `F`, within the current execution path.

## `$` - evaluate
```hs
{D | A=>B} [A | _] -> {D} [B]
```

Evaluates a piece of code in the current environment, and returns whatever it returns.

## `[` - begin quoting

Enters the quoting mode, where any character is treated as data rather than code, and promptly pushed onto the stack. Do note that quoting can be recursive: `[[1]]` is fine.

## `]` - end quoting

# Logical

## `=` - equals
```hs
{D} [A B | _] -> [bool | _]
```

Verifies if A and B are structurally the same value.

## `>` - greater than
```hs
{D} [A B | _] -> [bool | _]
```

Verifies if A is greater than B.

## `!` - not
```hs
{D} [A | _] -> [bool | _]
```

Negates the value of `A`.

## `f` - false

The constant `false`.

# Arithmetic

## `+` - addition
```hs
{D} [A B | _] -> [C | _]
```

## `-` - subtraction
```hs
{D} [A B | _] -> [C | _]
```

## `*` - multiplication
```hs
{D} [A B | _] -> [C | _]
```

## `/` - division
```hs
{D} [A B | _] -> [C | _]
```

## `%` - modulo
```hs
{D} [A B | _] -> [C | _]
```

# Lists

## `:` - Cons
```hs
{D} [A B | _] -> [A:B | _]
```

## `&` - concatenate
```hs
{D} [A B | _] -> [A ++ B | _]
```

# Branching

## `?` - either
```hs
{D} [A [B=>C] [D=>E] | _] -> [C or E]
```

Evaluates `A`. If it's `true`, evaluates and returns `B`, else evaluates and returns `D`.

# Folding

## `|` - map
```hs
{D} [[F::A=>B] [A] | _] -> [[B] | _]
```

Takes in a list of `A` and a function from `A` to `B`, then returns a list of `B`s.

## `#` - filter
```hs
{D} [[F::A=>bool] [A] | _] -> [[A] | _]
```

Takes in a list of `A` and a predicate that returns true for the items that should be kept, returns a list of those items.

## `.` - fold right
```hs
{D} [[F::A B=>C] B [A]] -> [C | _]
```

Right-associative fold.

## `,` - unfold
```hs
{D} [[F::A=>B] C | _] -> [[B] | _]
```

Constructs a list by recursively applying `F` to the previously computed value, until `false` is returned.
