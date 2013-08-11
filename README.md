Ririsu
======

A pure, untyped, parallel, distributed, stack-based, dynamically-scoped
data-processing esolang.


## Example

A program to return all even numbers of a sequence:

    [123456789][∘i]→[[2]i⇄%[0]i=]⋈
    
A program to sum all numbers of a sequence:

    [123456789][∘i]→[0]i[+]⚺


## Running

> You'll need Erlang and Elixir correctly set-up in order to run Ririsu.

    $ git clone git://github.com/kurisuwhyte/ririsu
    $ cd ririsu
    $ mix escriptize
    $ ./bin/ririsu repl

(Or alternatively [download the binary](https://github.com/kurisuwhyte/ririsu/releases/download/0.4.1/ririsu))


## Documentation

Check it out on-line at http://kurisuwhyte.github.io/ririsu, or download the
gh-pages branch and open the `index.html` file.


## Tests

( ... )


## Licence

WTFPL.
