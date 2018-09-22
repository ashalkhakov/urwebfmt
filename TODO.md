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
