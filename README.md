# Cludge

This was an attempt to compile common lisp to javascript. This was
based on [SICL](https://github.com/robert-strandh/SICL) and the
compiler Cleavir. It turns out that compiling common lisp to run
externally is difficult because the spec largely assumes that the
compiler (and e.g. macros) have access to the run-time environment.
This means that the compiler essentially has to compile everything
twice—Once into javascript and once into an environment so we can run
it at compile time. And there are other problems, for example:

    (defmacro foo (x)
	  (let ((y x))
        `(values ,(lambda () (incf y))
		         ,(lambda () (decf y)))))

Somehow the compiler would need to generate code to put the cell for
`Y` somewhere and it would need to compile the two anonymous functions
(and have them reference the right cell). Thus we cannot use the host
environment as the compilation environment. And neither can we compile
once to JS and then run macros by running a JS interpreter on the
compiled JS without having some clever way of serialising closures
from the JS interpreter into correct JS source code.

## Dependencies

This depends on SICL however it also needs a (now obsolete) AST
interpreter which was formerly included in sicl. This is known to work
for SICL at commit `dd6015dbf9ad34063632ef505b597c38c03c0359`.

## Example

    CL-USER> (asdf:load-system :cludge)
    ;; lots of compiling...
	;; also cuurrently some errors with redefining constants. continue these
	CL-USER> (defparameter *my-env* (make-instance 'js-env:compilation-environment))
	;; Long wait
	CL-USER> (js-cleavir:compile '(+ 5 6) *my-env*)
	;; ommitted
	CL-USER> (js-syntax-writer:write-js-to-string * :whole t)
	"var l1,l2,l3,l4;var l5=Object.create(null),l6=symbol('+','COMMON-LISP',true);l2=l6.f;l3=5;l4=6;l1=l2(l5,l3,l4);return l1;"

cleaned up:

    var l1, l2, l3, l4;
	var l5 = Object.create(null), l6 = symbol('+', 'COMMON-LISP', true);
	l2 = l6.f; //the function slot of +
	l3 = 5;
	l4 = 6;
	l1 = l2(l5,l3,l4); // l5 is the dynamic environment
	return l1;

The compiler doesn't make any efforts to simplify the code produced so
it can be a bit verbose. It also assumes a small runtime, i.e. a
function `symbol(name, package, external)` for looking up symbols.

The compiler has support for some more tricky features of Common Lisp.
In particular, nonlocal control transfer (e.g. calling `(go foo)`
inside a function which closes over the tag `foo`). The compiler does
not use javascript's own closures to implement closures as javascript
variables have function-level scope rather than block scope. Also
there is a possibility in the intermediate representation of a having
a closed over function return a closure which itself may return a
closure of the first function, although I haven't figured out a way to
generate such a function.

Closures are implemented by binding functions to an object with
references to the closed-over variables, and then those functions may
access it from `this`.

## Other Bits

This module also contains a Common Lisp representation for Javascript
syntax as well as a writer for it (albeit not a pretty printer). This
allows for automatically picking JS variable names. This is in
`js-syntax.lisp`.

There is also an implementation of the relooper algorithm (from
emscriptem) in `relooper.lisp` and `relooper-classes.lisp`. After
trying a few times to make sense of the brief description of the
algorithm in the paper describing it, and failing to find the original
prototype javascript implementation, I gave up and translated the C++
in enscriptem to Lisp. Thus it insn't exactly idiomatic. The only
difference is that my implementation manipulates javascript in terms
of the `js-syntax` (i.e. symbols and lists instead of strings).

Finally note that the file `js-cleavir.lisp` contains an older version
of the compiler and broken implementation of relooper and that the
slightly-less-broken current implementation is mostly in
`js-cleavi2.lisp`.
