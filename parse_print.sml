(* parse_print.sml *)

structure ParsePrint : sig
	      val stdin_parse : unit -> unit
              val file_parse : (bool(*inplace*) * string) -> unit
          end =
struct

(*
 * We apply the functors generated from calc.lex and calc.grm to produce
 * the CalcParser structure.
 *)

  structure UrWebLrVals =
    UrwebLrValsFn(structure Token = LrParser.Token)

  structure UrWebLex =
    UrwebLexFn(structure Tokens = UrWebLrVals.Tokens)

  structure UrWebParser =
    Join(structure LrParser = LrParser
	 structure ParserData = UrWebLrVals.ParserData
	 structure Lex = UrWebLex)

(*
 * We need a function which given a lexer invokes the parser. The
 * function invoke does this.
 *)

  fun invoke lexstream =
      let
          fun print_error (s,i:int,_) =
	      TextIO.output(TextIO.stdErr,
			    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
      in UrWebParser.parse(0,lexstream, print_error,())
      end

(*
 * Finally, we need a driver function that reads one or more expressions
 * from the standard input. The function parse, shown below, does
 * this. It runs the calculator on the standard input and terminates when
 * an end-of-file is encountered.
 *)

  fun parse (filename : string) (reader : int -> string) outf =
      let val () = (ErrorMsg.resetErrors ();
                    ErrorMsg.resetPositioning filename;
                    UrWebLex.UserDeclarations.initialize ())
          val lexer = UrWebParser.makeLexer reader
	  val dummyEOF = UrWebLrVals.Tokens.EOF(0,0)
	  fun loop lexer =
	      let val (result,lexer) = invoke lexer
		  val (nextToken,lexer) = UrWebParser.Stream.get lexer
                  val pstr = SourcePrint.p_file result
                  val () = SourcePrint.renderToStream (outf, pstr)
                                                      (*
                  val comments = UrWebLex.UserDeclarations.getCommentsAsString ()
                  val () = if comments <> "" then (print "\n"; print ("comments: " ^ comments); print "\n") else ()
                                                      *)
                  val () = TextIO.output (outf, "\n") (* print also the newline at end of file *)
	      in if UrWebParser.sameToken(nextToken,dummyEOF) then ()
		  else loop lexer
	      end
      in loop lexer
      end

  fun stdin_parse () =
      parse "STDIN" (fn _ =>
                        case TextIO.inputLine TextIO.stdIn
                         of SOME s => s
                          | _ => "") TextIO.stdOut

  fun file_parse (inplace, name) =
      let val dev = TextIO.openIn name
      in parse name (fn i => TextIO.inputN (dev, i)) TextIO.stdOut
         before TextIO.closeIn dev
      end

end (* structure ParsePrint *)
