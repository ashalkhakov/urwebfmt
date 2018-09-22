structure Main = struct

exception FatalError of string

val versionString = "1.0.0"

fun usage prog =
    print ("Usage: " ^ prog ^ " [options]\n" ^
           prog ^ "\n" ^
           "\tPretty-print the code from STDIN, output to STDOUT\n" ^
           prog ^ " <file>\n" ^
           "Pretty-print the given file to STDOUT\n" ^
           prog ^ " -i <file>\n" ^
           "\tModify the provided file in-place\n" ^
           prog ^ " --help\n" ^
           "\tPrint usage information\n" ^
           prog ^ " --version\n" ^
           "\tPrint the version of the program\n")

fun version prog =
    print (prog ^ " version " ^ versionString ^ "\n")

fun do_file_pprint (inpl, arg) =
    ParsePrint.file_parse (inpl, arg)

fun do_stdin_pprint () = ParsePrint.stdin_parse ()

fun main (prog, args) =
    (let
        exception Args

        val inplace = ref false

        fun parseArgs nil = do_stdin_pprint ()
          | parseArgs ("--help" :: ts) = usage prog
          | parseArgs ("--version" :: ts) = version prog
          | parseArgs ("-i" :: ts) = (inplace := true; parseArgs ts)
          | parseArgs (arg :: ts) = (do_file_pprint (!inplace, arg))
(*          | parseArgs _ = (usage prog; raise Args) *)
    in
        parseArgs args
        handle Args => raise FatalError "Error parsing args. Use the --help option";
    (* do something *)
        OS.Process.success
    end)
    handle FatalError e => (print ("Fatal error:\n" ^ e ^ "\n"); OS.Process.failure)
end

(* for MLton *)
val _ = Main.main (CommandLine.name (), CommandLine.arguments ())
