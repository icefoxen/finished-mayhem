(* lex.mll
   A lexer for Mayhem


   Simon Heath
   6/3/2007
*)

{

open Parse
exception Eof
exception LexerError of string

let inComment = ref 0;;

let chrNum = ref 0;;
let lineNum = ref 0;;

let nl () =
  chrNum := 0;
  incr lineNum
;;

(* Abbreviation for the func that returns the string
   being lexed.
*)
let gs = Lexing.lexeme;;

(* Advances the position of the error-checking vars. *)
let adv lb =
  chrNum := !chrNum + (String.length (Lexing.lexeme lb));;

let str2float x =
   Scanf.sscanf x "%f" (fun x -> x)
;;

let str2int x =
   Scanf.sscanf x "%i" (fun x -> x)
;;
let str2char x =
   Scanf.sscanf x "%C" (fun x -> x) 
;;

let str2str x =
   Scanf.sscanf x "%S" (fun x -> x) 
;;

}


let idchar = 
        ['a'-'z' 'A'-'Z' '_' '+' '-' '*' '/' '%' '!' '$' '^' '&' '=' '<' '>']

let num = ['0'-'9']

let id = idchar (idchar | num)*

let inum =
   '-'?(['0'-'9']+|"0x"['0'-'9''a'-'f''A'-'F']+|"0o"['0'-'7']+)
let bnum =
   '-'?"0b"['0''1']+
let fnum =
   '-'?['0'-'9']+'.'['0'-'9']*
let chr =
   ("'"_"'") | ("'\\"(inum|bnum)"'") | ("'\\"("n"|"b"|"r"|"t"|"'"|"\\")"'")

let str = '"'([^'"' '\\']|'\\'_)*'"'

let ratio = inum "/" inum


rule token = parse
(*   ";"			{ adv lexbuf; lcomment lexbuf }
 | "/-"			{ adv lexbuf; incr inComment; bcomment lexbuf }

 | fnum			{ adv lexbuf; FLOAT( str2float (gs lexbuf) ) } *)
   str                  { adv lexbuf; STRING( str2str (gs lexbuf) ) }
 | chr                  { adv lexbuf; CHAR( str2char (gs lexbuf) ) }

 | ";" [^'\n']* '\n'             { adv lexbuf; COMMENT( gs lexbuf ) }

 | (inum|bnum)		{ adv lexbuf; INT( str2int (gs lexbuf) ) }
 | "\n"			{ nl (); token lexbuf }
 | [' ''\t']		{ adv lexbuf; token lexbuf }
 | "["                  { adv lexbuf; LBRACK }
 | "]"                  { adv lexbuf; RBRACK }
 | "{"                  { adv lexbuf; LBRACE }
 | "}"                  { adv lexbuf; RBRACE }
 | "("                  { adv lexbuf; LPAREN }
 | ")"                  { adv lexbuf; RPAREN }

 | ","                  { adv lexbuf; COMMA }
 | "->"                 { adv lexbuf; ARROW }
 | ":"                  { adv lexbuf; COLON }
 | "."                  { adv lexbuf; PERIOD }

 | "fun"                { adv lexbuf; FUN }
 | "defun"              { adv lexbuf; DEFUN }
 | "let"                { adv lexbuf; LET }
 | "in"                 { adv lexbuf; IN }
 | "end"                { adv lexbuf; END }
 | "namespace"          { adv lexbuf; NAMESPACE }

 | "struct"             { adv lexbuf; STRUCT }

 | "if"                 { adv lexbuf; IF }
 | "elif"               { adv lexbuf; ELIF }
 | "then"               { adv lexbuf; THEN }
 | "else"               { adv lexbuf; ELSE }
 | "`"                  { adv lexbuf; BACKQUOTE }
 | "Null"               { adv lexbuf; NULL }

 | "raise"              { adv lexbuf; RAISE }
 | "try"                { adv lexbuf; TRY }
 | "catch"              { adv lexbuf; CATCH }
 | "/-"			{ adv lexbuf; incr inComment; bcomment lexbuf }

 | id			{ adv lexbuf; let s = gs lexbuf in SYMBOL( s ) }
 | eof			{ EOF }
 | _			{ raise (LexerError( "Invalid token!" )) }

and bcomment = parse
   "/-"			{ adv lexbuf; incr inComment; 
                          bcomment lexbuf }
 | "-/"			{ adv lexbuf; decr inComment; 
                          if !inComment <= 0 then token lexbuf
			                    else bcomment lexbuf }
 | '\n'			{ nl (); bcomment lexbuf }
 | _			{ adv lexbuf; bcomment lexbuf } 


and lcomment = parse
(*   '\n'			{ nl (); token lexbuf }
 | _*'\n'			{ adv lexbuf; lcomment lexbuf }
*)
_*'\n'			{ nl ();  }
