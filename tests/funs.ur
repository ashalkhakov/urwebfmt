fun app f x = f x
fun app' f = fn x => f x
fun twice f x = f (f x)
fun twice' = fn f => fn x => f (f x)

val t0 = app (fn x=>x+x) 1
val t1 = app' (fn x=>x+x) 1
val t2 = twice (fn x => x * 2) 2
val t3 = twice' (fn x => x * 2) 2
