stahl
=====

[![Build Status](https://travis-ci.org/remexre/stahl.svg?branch=master)](https://travis-ci.org/remexre/stahl)

A dependently typed Lisp with algebraic effects.

Implementation Goals
--------------------

-	[ ] Typechecking for core language
-	[ ] Modules
-	[ ] ADTs
-	[ ] Recursion principles
-	[ ] CPDT arithmetic interpreter example
-	[ ] Interfaces/Typeclasses/Traits
-	[ ] Group theory in `std`
-	[ ] Effects
-	[ ] Basic IO
-	[ ] FFI
-	[ ] Fast interpreter
-	[ ] Compile to CAM
-	[ ] Compile to Forth
-	[ ] Compile to WebAssembly
	-	[ ] [Lucet](https://github.com/fastly/lucet) for native-code?
-	[ ] Elaborator scripts
-	[ ] Elaborator reflection
-	[ ] Macro support
-	[ ] Web framework (with heavy "cheating" as a runtime)
-	[ ] Compile to native code
-	[ ] Rewrite the world!

Language Goals
--------------

-	[ ] Prove soundness
	-	[ ] Prove `typeOf E T -> evalStep E E' -> typeOf E' T`
	-	[ ] Prove totality / strong normalization
-	[ ] Prove parametricity

License
-------

Licensed under either of

-	[Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0)
-	[MIT License](http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
