(* REP! *)

open Backend

let read string =
  let lexbuf = Lexing.from_string string in
  Parse.main Lex.token lexbuf
;;

let _ =
  let stbl = makeSymtbl () in

    Stdlib.initLib stbl;
    Stdlib.loadLib stbl;

    let continue = ref true in

      while !continue do
	try
	  Printf.printf "%s:> " stbl.currentNamespaceName;
	  flush stdout;

	  print_endline (value2readablestr (evalList (read (read_line ())) stbl));

	  (*	  print_endline (value2str (evalList (p ()) stbl)); *)
	  flush stdout;
	with
	    UnboundVar( s ) -> Printf.printf "Symbol not found: %s\n" s
	  | TypeError( s ) ->  Printf.printf "Type error: %s\n" s
	  | NamespaceError( s ) -> Printf.printf "Namespace error: %s\n" s;
	  | RuntimeError( s ) -> Printf.printf "Runtime error: %s\n" s;

	  | Fubar( s ) -> Printf.printf "Oh snap!  %s  Integrity may be compromised.\n" s

	  | Lex.LexerError( s ) ->
	      Printf.printf "Lexer unhappy: %s\n" s
	  | Parsing.Parse_error -> print_endline "Parser unhappy."
	  | Failure( s ) -> Printf.printf "You fail: %s\n" s
	  | End_of_file -> (print_endline "Bye!"; continue := false)
          | MayhemException( e ) -> 
                Printf.printf "Mayhem exception: %s\n" (value2str e)


      done
;;
