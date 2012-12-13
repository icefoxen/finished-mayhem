(* stdlib.ml
   Standard library of necessary things.

*)

open Backend


let runtime message =
  raise (RuntimeError( message ))
;;

let typeerror message =
  raise (TypeError( message ))
;;

(* Arg handling and verification *)
let arg1 x =
  List.nth x 0
;;

let arg2 x =
  List.nth x 1
;;

let arg3 x =
  List.nth x 2
;;

let arg4 x =
  List.nth x 3
;;

let arg5 x =
  List.nth x 4
;;

let argn x i =
  List.nth x i
;;

let numArgs x = 
  List.length x
;;

let isInt = function
    Int( _ ) -> true
  | _ -> false
;;

let isSymbol = function
    Symbol( _ ) -> true
  | _ -> false
;;

let isTuple = function
    Tuple( _ ) -> true
  | _ -> false
;;

let isArray = function
    Array( _ ) -> true
  | _ -> false
;;

let isString = function
    String( _ ) -> true
  | _ -> false
;;

let isChar = function
    Char( _ ) -> true
  | _ -> false
;;

let isFunc = function
    Func( _ ) -> true
  | _ -> false
;;

let isBuiltin = function
    Builtin( _ ) -> true
  | _ -> false
;;

let isCallable x =
  (isFunc x) or (isBuiltin x)
;;


let extractValue = function
    Value( v ) -> v
  | _ -> Null
;;


let getInt = function
    Int( a ) -> a
  | _ -> runtime "Int expected!"
;;

let getChar = function
    Char( a ) -> a
  | _ -> runtime "Char expected!"

let getSymbol = function
    Symbol( a ) -> a
  | _ -> runtime "Symbol expected!"
;;

let getTuple = function
    Tuple( a ) -> a
  | _ -> runtime "Tuple expected!"
;;

let getArray = function
    Array( a ) -> a
  | _ -> runtime "Array expected!"
;;

let getString = function
    String( a ) -> a
  | _ -> runtime "String expected!"
;;



let sym2bool x =
  let b = getSymbol x in
    b = "true"
;;

let bool2sym b =
  if b then Symbol( "true" )
  else Symbol( "false" )
;;


(* Math and logic *)
let add x = 
  let vals = List.map getInt x in
    Int( List.fold_left (+) 0 vals )
;;

let sub x = 
  let vals = List.map getInt x in
    Int( List.fold_left (-) (List.hd vals) (List.tl vals) )
;;

let mul x = 
  let vals = List.map getInt x in
    Int( List.fold_left ( * ) 1 vals )
;;

let div x = 
  let vals = List.map getInt x in
    Int( List.fold_left (/) (List.hd vals) (List.tl vals) )
;;

let modulus x = 
  if (numArgs x) <> 2 then
    runtime "Mod expects 2 args!"
  else
    let a1 = getInt (arg1 x)
    and a2 = getInt (arg2 x) in
      Int( a1 mod a2 )
;;

let exp x =
  let a1 = getInt (arg1 x)
  and a2 = getInt (arg2 x) in
  let res = int_of_float ((float_of_int a1) ** (float_of_int a2)) in
    Int( res )
;;
      


(* XXX: Make these compare strings and such also! *)
let gt x =
  if (numArgs x) <> 2 then
    runtime "> expects 2 args!"
  else
    let a1 = getInt (arg1 x)
    and a2 = getInt (arg2 x) in
      bool2sym (a1 > a2)
;;


let lt x =
  if (numArgs x) <> 2 then
    runtime "< expects 2 args!"
  else
    let a1 = getInt (arg1 x)
    and a2 = getInt (arg2 x) in
      bool2sym (a1 < a2)
;;


let gte x =
  if (numArgs x) <> 2 then
    runtime ">= expects 2 args!"
  else
    let a1 = getInt (arg1 x)
    and a2 = getInt (arg2 x) in
      bool2sym (a1 >= a2)
;;


let lte x =
  if (numArgs x) <> 2 then
    runtime "<= expects 2 args!"
  else
    let a1 = getInt (arg1 x)
    and a2 = getInt (arg2 x) in
      bool2sym (a1 <= a2)
;;



let eq x =
  if (numArgs x) <> 2 then
    runtime "= expects 2 args!"
  else
    let a1 = (arg1 x)
    and a2 = (arg2 x) in
      bool2sym (a1 = a2)
;;

let neq x =
  if (numArgs x) <> 2 then
    runtime "/= expects 2 args!"
  else
    let a1 = (arg1 x)
    and a2 = (arg2 x) in
      bool2sym (a1 <> a2)
;;

let booland x =
  let a = arg1 x
  and b = arg2 x in
    bool2sym ((a = Symbol( "true" )) && (b = Symbol( "true" )))
;;

let boolor x =
  let a = arg1 x
  and b = arg2 x in
    bool2sym ((a = Symbol( "true" )) or (b = Symbol( "true" )))
;;

let boolnot x =
  let a = arg1 x in
    bool2sym (not (a = Symbol( "true" )))
;;

let boolxor x =
  let a = arg1 x
  and b = arg2 x in
  bool2sym 
    (((a = Symbol( "true" )) && (b = Symbol( "false" ))) or
	((a = Symbol( "false" )) && (b = Symbol( "true" ))))
;;

let binand x =
  ()
;;

let binor x = 
  ()
;;

let binnot x =
  ()
;;

let binxor x =
  ()
;;

let mhIsInt x =
  ()
;;

(* Sequences *)
(* You know, at this level we CAN actually do polymorphism... 
   We just can't change it in the language, which is still slightly horrid.

   Hrm.  Ad-hoc polymorphism, aka function overloading?  Actually,
   how IS that different from generic functions?  Well, inheritance.
   That allows subtyping.
*)
let seqSet x =
  let array = arg1 x
  and index = arg2 x
  and itm = arg3 x in
    match array with
	Array( arr ) ->
	  let idx = getInt index in
	    arr.(idx) <- (Value( itm )); Null
      | String( str ) ->
	  let idx = getInt index in
	    str.[idx] <- getChar itm; Null
      | _ -> Null

;;

let seqGet x =
  let array = arg1 x
  and index = arg2 x in
    match array with
	Array( arr ) ->
	  let idx = getInt index in
	    extractValue arr.(idx)
      | String( str ) ->
	  let idx = getInt index in
	    Char( str.[idx] )
      | _ -> typeerror "get expected a sequence!"
;;

let seqLength x =
  let array = arg1 x in
    match array with
	Array( arr ) ->
	  Int( Array.length arr )
      | String( str ) ->
	  Int( String.length str )
      | _ -> typeerror "length expected a sequence!"
;;
(*
let seqExpand x =
  let seq = arg1 x
  and len = arg2 x in
    match seq with
	Array( arr ) ->
;;
*)

let seqConcat x =
  let seq1 = arg1 x
  and seq2 = arg2 x in
    if isArray seq1 && isArray seq2 then
      Array( Array.append (getArray seq1) (getArray seq2) )
    else if isString seq1 && isString seq2 then
      String( (getString seq1) ^ (getString seq2) )
    else
      typeerror "concat expected 2 sequences of the same type!"
;;

let seqSlice x =
  let seq = arg1 x
  and start = getInt (arg2 x)
  and finish = getInt (arg3 x) in
  let len = finish - start in
    match seq with
	Array( arr ) -> Array( Array.sub arr start len )
      | String( str ) -> String( String.sub str start len )
      | _ -> typeerror "slice expected a sequence!"
;;


(*
let seqFold x =
   let seq = arg1 x
   and func = arg2 x
   and startval = arg3 x in
   match seq with
	Array( arr ) -> Array( Array.sub arr startval len )
;;
*)

let isSeq x =
  ()
;;



(* Arrays *)
let arrayMake x =
  match x with
      [Int( x )] -> Array( Array.create x (Value( Null )) )
    | _ -> Null
;;

let mhIsArray x =
  ()
;;


(* File I/O *)
(* Rather than make another file datatype, I am going to represent files
   as functions.  Because it'll be fun.

   The (openFile "filename" mode) function makes a file-function.
   A reading file-function takes the following args:
   (f 'read count)
   (f 'readLine)
   (f 'readChar)
   (f 'readInt)
   (f 'readByte)

   A writing file-function takes the following args:
   (f 'write itm)
   (f 'flush)

   All files take the following args:
   (f 'close)
   (f 'position)
   (f 'seek arg)
   (f 'length)
*)

let slurpFile f =
   let s = ref "" in
    try 
      while true do
	s := !s ^ (input_line f) ^ "\n"
      done;
      !s
    with
	End_of_file -> close_in f; !s
;;


let makeReadFile str =
  let stream = open_in str in
  let f = fun x ->
    let command = getSymbol (arg1 x) in
      try
	match command with
	    "read" ->
               String( (slurpFile stream) )
	  | "readLine" ->
	      String( (input_line stream) )
	  | "readChar" -> 
	      Char( (input_char stream) )
	  | "readInt" ->
	      Int( (input_binary_int stream) )
	  | "readByte" ->
	      Int( (input_byte stream) )

	  | "close" -> 
	      close_in stream;
	      Null;
	  | "position" ->
	      Int( pos_in stream )
	  | "seek" ->
	      let pos = getInt (arg2 x) in
		seek_in stream pos;
		Null
	  | "length" ->
	      Int( in_channel_length stream )
	  | x -> runtime ("Bad file command: " ^ x)
      with
	  Sys_error( "Bad file descriptor" ) ->
	    runtime "Operation on a closed file!"
  in
    Builtin( f )
;;

let makeWriteFile str =
  let stream = open_out str in
  let f = fun x ->
    let command = getSymbol (arg1 x) in
      try
	match command with
	    "write" ->
	      let itm = arg2 x in
		(match itm with
		    Int( x ) ->
		      output_binary_int stream x;
		      Null
		  | String( x ) ->
		      output_string stream x;
		      Null
		  | _ -> 
		      runtime "You can only write ints and strings to files!"
		)
	  | "flush" -> 
	      flush stream;
	      Null;
	  | "close" ->
	      close_out stream;
	      Null
	  | "position" -> 
	      Int( pos_out stream )
	  | "seek" -> 
	      let pos = getInt (arg2 x) in
		seek_out stream pos;
		Null
	  | x -> runtime ("Bad file command: " ^ x)
      with
	  Sys_error( "Bad file descriptor" ) ->
	    runtime "Operation on a closed file!"
  in
    Builtin( f )
;;




let makeFile x =
  let modesym = arg1 x in
  let str = getString (arg2 x) in
  if modesym = (Symbol( "read" )) then
    makeReadFile str
  else if modesym = (Symbol( "write")) then
    makeWriteFile str
  else
    runtime "Invalid file mode!"

;;


(* String handling *)

let char2str x =
  let c = getChar (arg1 x) in
    String( (String.make 1 c) )
;;

let int2char x =
  let c = getChar (arg1 x) in
    Int( int_of_char c )
;;

let char2int x =
  let i = getInt (arg1 x) in
    Char( char_of_int i )
;;

let str2int x =
  let i = getString (arg1 x) in
     Int( int_of_string i )
;;

let strMake x =
  let len = getInt (arg1 x)
  and init = getChar (arg2 x) in
    String( (String.make len init) )
    
;;

let mhIsString x =
  ()
;;


(* Other stuff *)
let evalString stbl x =
  let str = getString (arg1 x) in
  let lexbuf = Lexing.from_string str in
    evalList (Parse.main Lex.token lexbuf) stbl
;;

let loadFile stbl x =
  let str = getString (arg1 x) in
  let file = open_in str in
  let lexbuf = Lexing.from_channel file in
(*    print_endline "Lexing..."; *)
    let tree = (Parse.main Lex.token lexbuf) in
(*      Printf.printf "Parsing...  %d\n" (List.length tree); *)
  let res = evalList tree stbl in
(*    print_endline "Evaling..." *)
    close_in file;
(*    print_endline "Closing..."; *)
    res
;;

let toString x =
  String( value2str (arg1 x) )
;;

let toReadableString x =
  String( value2readablestr (arg1 x) )
;;

let print x =
  print_string (value2str (arg1 x));
  Null
;;

let display x=
  print_string (value2readablestr (arg1 x));
  Null
;;

let println x =
  print_endline (value2str (arg1 x));
  Null
;;

let mhIsTuple x =
  ()
;;

let mhIsSymbol x =
  ()
;;

let mhIsChar x =
  ()
;;


let hash x =
  let a = arg1 x in
    Int( Hashtbl.hash a )
;;


let mhExit x =
  let a = getInt (arg1 x) in
    exit a
;;



let initLib stbl =
  setVar "user" "+"   (Builtin( add )) stbl;
  setVar "user" "-"   (Builtin( sub )) stbl;
  setVar "user" "*"   (Builtin( mul )) stbl;
  setVar "user" "/"   (Builtin( div )) stbl;
  setVar "user" "mod" (Builtin( modulus )) stbl;
  setVar "user" "**"  (Builtin( exp )) stbl;

  setVar "user" ">"  (Builtin( gt )) stbl;
  setVar "user" "<"  (Builtin( lt )) stbl;
  setVar "user" ">=" (Builtin( gte )) stbl;
  setVar "user" "<=" (Builtin( lte )) stbl;
  setVar "user" "="  (Builtin( eq )) stbl;
  setVar "user" "/=" (Builtin( neq )) stbl;

  setVar "user" "and" (Builtin( booland )) stbl;
  setVar "user" "or"  (Builtin( boolor )) stbl;
  setVar "user" "not" (Builtin( boolnot )) stbl;
  setVar "user" "xor" (Builtin( boolxor )) stbl;

  setVar "user" "print"    (Builtin( print )) stbl;
  setVar "user" "println"  (Builtin( println )) stbl;
  setVar "user" "display"  (Builtin( display )) stbl;
  setVar "user" "toString" (Builtin( toString )) stbl;
  setVar "user" "toReadableString" (Builtin( toReadableString )) stbl;


  setVar "user" "get"    (Builtin( seqGet )) stbl;
  setVar "user" "set"    (Builtin( seqSet )) stbl;
  setVar "user" "length" (Builtin( seqLength )) stbl;
  setVar "user" "slice"  (Builtin( seqSlice )) stbl;
  setVar "user" "concat" (Builtin( seqConcat )) stbl;

  setVar "user" "array"  (Builtin( arrayMake )) stbl;

  setVar "user" "string"   (Builtin( strMake )) stbl;
  setVar "user" "char2int" (Builtin( char2int )) stbl;
  setVar "user" "int2char" (Builtin( int2char )) stbl;
  setVar "user" "char2str" (Builtin( char2str )) stbl;
  setVar "user" "str2int"  (Builtin( str2int )) stbl;

  setVar "user" "open" (Builtin( makeFile )) stbl;

  setVar "user" "eval" (Builtin( (evalString stbl) )) stbl;
  setVar "user" "load" (Builtin( (loadFile stbl) )) stbl;
  setVar "user" "hash" (Builtin( hash )) stbl;
  setVar "user" "exit" (Builtin( mhExit )) stbl;

;;

let loadLib stbl =
  ignore (loadFile stbl [(String( "mayhemlib.mh" ))])
;;
