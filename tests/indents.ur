val x = 1
val y = 2

val z = x+y

val w =
    let
        val x = 0
    in
        x + x
    end

val r = let val x = 0 in x + x end

val r1 = let val x = 0 val y = 2 val z = 55555555 in x + y + 555555555555555555 end
