fun f x = let val x = x in x end

fun g x =
    let
        val x = x+1
        val y = x*2
        val z = y/3
    in
        z
    end

val _ = f 0
val _ = g 0
