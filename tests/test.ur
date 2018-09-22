structure S = struct
    type t = int
    fun add (x: t) (y: t): t = x + y
end

fun help (): transaction page = return <xml>Hello!</xml>

fun help2 () =
    x <- help ();
    return <xml>{x}{x}{[S.add 2 5]}</xml>

open S

fun help3 (): transaction page = return <xml>{[add 2 5]}</xml>
