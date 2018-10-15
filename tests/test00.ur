(*
target language has no construct like this!
it is instead rewritten to:

val add = fn x => fn y => Basis.plus x y

so,
- fix: function declaration (make it more like the source syntax: the keyword [fun] <name> argslist = EXPRESSION-BODY)
- and stop rewriting of + to Basis.plus somehow
*)
fun add (x: int) (y: int) = x + y
fun sub (x: int) (y: int) = x - y
fun addsub2 x y = x + y - 2
