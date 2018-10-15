motivated by: https://github.com/urweb/urweb/issues/59

emacs indents are still broken for me!
- and there are other things we could do:
  - what is under point (e.g. figure out what is where)
  - go to def
  - list of top-level decls in the file (like a structure view in IDE)


https://github.com/xyproto/smlnj/blob/master/ml-yacc/examples/calc/

should be a good starting point
+ take the provided example calculator, ensure it builds and runs
  + how to parse the provided files???
+ teach the tool to spew out SOMETHING that it parsed
  + see the invocations below
  - except for in-place stuff
- bastardize the urweb sources to extract parser and lexer
  - this is probably the hardest part


model after ats-format, which is based on:
https://github.com/vmchale/atspkg/tree/master/language-ats/src/Language/ATS
- provides a parser that exactly constructs a concrete syntax tree
- implements a pretty-printer that folds over the tree and basically
  translates the input phrases into indentation/printing phrases

invocation?
--help
--version

urwebfmt <file> # probably outputs to stdout and stderr
urwebfmt -i <file> # modify a file in-place
cat file.ur | urwebfmt # takes stdin, outputs to stdout and stderr

# the mystery of too much elaboration of function declarations

this:

fun f x = x

turns into

val rec f = fn x => x

why and when??

* `eargp` rule, takes a `pterm` (preterm?)
* from `eargl2`, from `vali`

what is earg(p)? what is carg(p)?
- expression args and constructor args?
- eargl is then expression argument list
- eargp = expression argument pattern
- earga = `[SYMBOL]` or `[SYMBOL ::_]`

VAL pat eargl2 copt EQ eexp
e.g. val f e1 e2 e3 : t = exp

# using the new pretty-printing library

well, devise some rules and try them out.

will need at least 2 tests per language construct:
- one test when there is enough space
- another, when there is NOT enough space

so that's like, LOTS of tests!

# as of Oct 15, 2018

- still have to figure out how the new PP library works
- lackluster tests
