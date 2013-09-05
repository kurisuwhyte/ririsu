# Basic combinators

## `↠` - Duplicate
```hs
[A | _] → [A A | _]
```

Duplicates the top of the stack.

## `⇄` - Swap
```hs
[A B | _] → [B A | _]
```

Swaps the two items at the top of the stack.

## `↓` - Drop
```hs
[A | _] → [_]
```

Drops the top item off the stack.

## `⊕` - Concatenate
```hs
[[A] [B] | _] → [[A B] | _]
```

Concatenates the top two quotations on the stack.

## `×` - Cons
```hs
[A [B] | _] → [[A B | _]
```

Cons the two top items on the stack.

## `∘` - Unit
```hs
[A | _] → [[A] | _]
```

Quotes the top of the stack.

## `▶` - I
```hs
[[A] | _] → [A | _]
```

Unquotes the 

