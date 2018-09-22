(* comments *)
(*
 * another one
 *
 *)
fun fn1 (): transaction xbody = return <xml>
  <!-- an xml comment -->

  <!-- another comment

  spanning lines
  -->
  </xml>

fun fn2 () = (* highly (* commented *) out! *) ()
