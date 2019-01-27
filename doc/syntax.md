Syntax
======

IExprs
------

Stahl uses a modified version of [SRFI49 IExprs](https://srfi.schemers.org/srfi-49/srfi-49.html).

The following examples serve as a quick introduction.

**TODO**: Get this under testing!

```
foo
; This is equivalent to foo

foo bar
; This is equivalent to (foo bar)

foo bar baz
; This is equivalent to (foo bar baz)

foo bar
  baz
; This is equivalent to (foo bar baz)

foo
  bar
; This is equivalent to (foo bar)

foo
  bar baz
; This is equivalent to (foo (bar baz))

foo
  bar
  baz
; This is equivalent to (foo bar baz)

foo
  bar
    baz
; This is equivalent to (foo (bar baz))

group
  foo
  bar
  baz
; This is equivalent to (foo bar baz)

group foo
  bar
  baz
; This is equivalent to (foo bar baz)

foo
  group 
    bar
    baz
; This is equivalent to (foo (bar baz))
```
