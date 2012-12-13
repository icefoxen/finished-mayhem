(* 
   Mayhem backend

   Simon Heath
   28/9/2006
*)

exception Fubar of string;;
exception TypeError of string;;
exception UnboundVar of string;;
exception NamespaceError of string;;
exception RuntimeError of string



type syntree =
    Value of valuetype
  | Let of label * syntree
  | Apply of syntree * syntree list
  | Lambda of string list * syntree list
  | If of syntree * syntree list * syntree list

  | Try of syntree list * (string * syntree list) list
  | Raise of syntree

  | Structdecl of string * string list * syntree list
  | Structref of syntree * string

  | NamespaceChange of string
(*  | NamespaceImport of string *)

  | Comment of string

and label = string * string

and valuetype =
   Int of int
  | Symbol of string
  | Var of label
  | Tuple of syntree list
  | Array of syntree array
  | Char of char
  | String of string

  | Struct of (string, valuetype) Hashtbl.t

  (* Closures! They need to know their environment.  *)
  | Func of string list * syntree list * (string * valuetype) list
  | Builtin of (valuetype list -> valuetype)
  | Null
  | Undefined
;;

exception MayhemException of valuetype;;

type typedecl = 
  StructType of string * string list * (string * syntree) list
;;



(* We split the globals and stack up... *)
type symtable = {
    mutable stack : ((string, valuetype) Hashtbl.t) list;
    namespaces : (string, (string, valuetype) Hashtbl.t) Hashtbl.t;
    mutable currentNamespaceName : string;
    mutable globals : (string, valuetype) Hashtbl.t;
    typedecls : (string, typedecl) Hashtbl.t;
  }







let rec vallist2str lst =
  let rec loop lst accm =
    match lst with
	[] -> accm
      | hd :: tl ->
	  match hd with
	      Value( v ) ->
		loop tl (accm ^ " " ^ (value2str v))
	    | _ -> raise (Fubar( "Tuple is not awake!" ))
  in
    (loop lst "")

and struct2str tbl =
  Hashtbl.fold (fun str vl accm -> accm ^ str ^ ": " ^ (value2str vl) ^ "; ")
    tbl ""

and value2str = function
    Int( i ) -> Printf.sprintf "%d" i
  | Symbol( s ) -> "`" ^ s
  | Var( ns, s ) -> ns ^ ":" ^ s
  | Null -> "Null"
  | Tuple( x ) -> "[" ^ (vallist2str x) ^  "]"
  | Func( _ ) -> "<func>"
  | Builtin( _ ) -> "<builtin>"
  | Array( x ) -> "{" ^ (vallist2str (Array.to_list x)) ^ "}"
  | Char( x ) -> Printf.sprintf "'%c'" x
  | String( x ) -> x
  | Struct( vals ) -> "(" ^ struct2str vals ^ ")"
  | Undefined -> "Undefined"
;;


(* What a kludge... but sometimes we need to display strings with 
   the quotes, and sometimes we don't. *)
let rec vallist2readablestr lst =
  let rec loop lst accm =
    match lst with
	[] -> accm
      | hd :: tl ->
	  match hd with
	      Value( v ) ->
		loop tl (accm ^ (value2readablestr v) ^ " ")
	    | _ -> raise (Fubar( "Tuple is not awake again!" ))
  in
    (loop lst "")


and value2readablestr = function
    Int( i ) -> Printf.sprintf "%d" i
  | Symbol( s ) -> "`" ^ s
  | Var( ns, s ) -> ns ^ ":" ^ s
  | Null -> "Null"
  | Tuple( x ) -> "[" ^ (vallist2readablestr x) ^  "]"
  | Func( _ ) -> "<func>"
  | Builtin( _ ) -> "<builtin>"
  | Array( x ) -> "{" ^ (vallist2readablestr (Array.to_list x)) ^ "}"
  | Char( x ) -> Printf.sprintf "'%c'" x
  | String( x ) -> "\"" ^ x ^ "\""
  | Undefined -> "\"Undefined\""
  | Struct( vals ) -> "("^ struct2str vals ^ ")"
;;



let printBloodyStack stbl =
  let rec loop stack =
    if stack = [] then
      print_endline "Done printing stack\n"
    else (
	print_endline "New scope level";
      let currentScope = List.hd stack in
	Hashtbl.iter (fun name vl -> Printf.printf "Var: %s = %s\n" name (value2str vl);) currentScope;
	loop (List.tl stack)
      )
  in
    print_endline "Starting to print stack";
    loop stbl.stack;
;;

let addNamespace stbl name =
  Hashtbl.add stbl.namespaces name (Hashtbl.create 16);
;;

(* This will still preserve the current namespace until you leave it.
   I think this is the right behavior anyway.  ^_^
*)
let delNamespace stbl name =
  Hashtbl.remove stbl.namespaces name
;;


let setNamespace stbl name =
  try
    stbl.globals <- Hashtbl.find stbl.namespaces name;
    stbl.currentNamespaceName <- name
  with
      Not_found -> raise (NamespaceError( "Namespace '" ^ name ^ "' DNE!" ))
;;


let makeSymtbl () = 
  let defaultNamespace = Hashtbl.create 1 in
  let t = {
      stack = [];
      namespaces = Hashtbl.create 8;
      currentNamespaceName = "user";
      globals = defaultNamespace;
      typedecls = Hashtbl.create 8
    } in
    addNamespace t "user";
    setNamespace t "user";
    t
;;

let addType typedecl stbl =
  match typedecl with
      StructType( name, _, _ ) -> Hashtbl.add stbl.typedecls name typedecl
;;

let getType typename stbl =
  try
    Hashtbl.find stbl.typedecls typename
  with
     Not_found -> raise (TypeError( "Type " ^ typename ^ " does not exist!"))
;;




let pushScope stbl =
(*  Printf.printf "Pushing scope %d\n" ((List.length stbl) + 1); *)
  stbl.stack <- (Hashtbl.create 8) :: stbl.stack;
;;

(*
let addScope scope stbl =
  scope :: stbl
;;
*)

let popScope stbl =
  match stbl.stack with
      [] -> raise (Fubar( "I have no scope and I must bind." ))
    | hd :: tl -> stbl.stack <- tl
;;



let getLocal v symtbl = 
  let rec loop stack =
    match stack with
	[] ->
	  raise (UnboundVar( v ))
      | hd :: tl -> 
	  try
	    let res = Hashtbl.find hd v in
	      if res = Undefined then raise (UnboundVar( v ))
	      else res
	  with
	      Not_found -> 
		loop tl
  in
    loop symtbl.stack
;;

let currentNamespace = "";;

let getGlobal ns v symtbl =
  try
    if ns = currentNamespace then (
	let res = Hashtbl.find symtbl.globals v in
	  if res = Undefined then raise (UnboundVar( v ))
	  else res
      ) else (
	if Hashtbl.mem symtbl.namespaces ns then (
	    let namespace = Hashtbl.find symtbl.namespaces ns in
	    let res = Hashtbl.find namespace v in
	      if res = Undefined then raise (UnboundVar( v ))
	      else res
	  ) else (
	    raise (NamespaceError( "Namespace '" ^ ns ^ "' does not exist!" ));
	  )
      )
  with
      Not_found -> raise (UnboundVar( ns ^ ":" ^ v ))
;;


(* A bit of exception-wanking happens here to make the local
   and globals cooperate.
   I don't like it, but...  Oh well.
*)
let getVar v symtbl =
  match v with
      Var( ns, s ) ->
	(try
	    getLocal s symtbl
	  with
	      UnboundVar( _ ) -> getGlobal ns s symtbl)

    | _ -> raise (Fubar( "getVar: Var required!" ))

;;


let rec setVar namespace name value symtbl =
  (*  Printf.printf "Adding %s:%s\n" namespace name; *)
  if namespace = "" then
    if symtbl.stack = [] then (
	(*	print_endline "Adding global..."; *)
	Hashtbl.replace symtbl.globals name value
      ) else (
	(*	print_endline "Adding local..."; *)
	Hashtbl.add (List.hd symtbl.stack) name value
      )

  else (
      (*      print_endline "Sketchy!"; *)
      let ns = Hashtbl.find symtbl.namespaces namespace in
	Hashtbl.add ns name value
    )

and makeNewStruct givenvals structargs members tbl =
  (try
      let t = Hashtbl.create 8 in
	pushScope tbl;
	(* This is a lot like a function, really.  It's essentially
	   a constructor.  In fact, we use a lot of the same code as the apply
	   function.
	   First, we bind the given values to the args *)
	List.iter2 (fun nm vl ->
	  setVar currentNamespace nm vl tbl) 
	  structargs givenvals;

	let addMember member =
	  let name, tree = member in
	    Hashtbl.add t name (eval tree tbl)
	in
	  List.iter addMember members;

	  popScope tbl;
	  Struct( t )
    with
	Invalid_argument( "List.iter2" ) ->
 	  popScope tbl;
	  let str = Printf.sprintf 
	    "Invalid number of struct args, expected %d got %d\n" 
	    (List.length structargs) (List.length givenvals) in
	    raise (TypeError( str )))


(* Closures work.
   They may abruptly stop working if values become mutable, like this:

   defun accm x = 
   let v = ref x in
   fun = (set! v (add v 1))
   end

   let f = (accm 10) in
   (f) -> 11
   (f) -> 11, NOT 12.

   Since (set!) doesn't exist, it doesn't matter.  For now.
   But when we add objects by reference, we have to make sure it works.

   Basically, the purpose of this function is to find all the variables
   that a function refers to outside of it's own scope, and remember
   their current values in the function's closure environment.

   This entire function is begging to be refactored.  It's not actually
   that complicated, just written badly.
*)
and extractClosure (arglst : string list) funbody symtbl =
  (* A list of name/value pairs of bound vars *)
  let env = ref [] in 

  (* A list of lists of argument names.  Note we use it for local vars
     as well as args. *)
  let args = ref [] in

  let pushSym sym vl =
    env := (sym,vl) :: !env
      
  and pushArgs arglst =
    args := arglst :: !args
  and popArgs () =
    args := List.tl !args
  and isArg term =
    let rec loop itm arglst =
      if List.mem term itm then (
	  (*	  Printf.printf "%s is an arg!\n" term; *)
	  true
	)
      else if arglst = [] then (
	  (*	  Printf.printf "%s is not an arg!\n" term; *)
	  false
	) else
	loop (List.hd arglst) (List.tl arglst)
    in
      loop (List.hd !args) (List.tl !args)
  in

    pushArgs arglst;
    (*    Printf.printf "# of args: %d\n" (List.length arglst); *)
    (*    List.iter print_endline (List.hd !args); *)
    
    (* This does the actual work of tree-walking the function and
       seeing what variables are referred to, then sticking them in
       the closure.
    *)
    let rec extractTerm = function
	Value( t ) ->
	  (match t with
	      Var( namespace, name ) ->
		if isArg name then (* we're cool, the var has been declared *)
		  ()
		    (*		  Printf.printf "Is arg: %s\n" name *)
		else ( (* it's from outside the local scope *)
		    (*Printf.printf "Term extracted: %s\n" name;  *)

		    (* And if the var doesn't actually exist in the
		       enclosing scope, getVar will explode here. Good.  *)
		    pushSym name (getVar t symtbl);
		  )
	    | Tuple( itms ) -> extractAllTerms itms
	    | _ -> ())

      | Let( (ns,var), term ) -> 
	  pushArgs [var];
	  extractTerm term
      | Apply( func, args ) -> extractAllTerms args


      | Lambda( a, body ) -> 
	  pushArgs a;
          extractAllTerms body;
	  popArgs ();

      | If( cond, i, e ) -> 
            extractTerm cond;
            extractAllTerms i;
            extractAllTerms e;


      | Structref( varterm, name ) ->
            extractTerm varterm;

      | Structdecl( name, args, body ) -> ()

      | Raise( term ) -> extractTerm term;

      | Try( body, catchlist ) -> 
            extractAllTerms body;
            List.iter (fun pair ->
               let str,body = pair in
               pushArgs [str]; 
               extractAllTerms body;
               popArgs();) catchlist;

      | Comment( _ ) -> ()

      | NamespaceChange( _ ) -> ()

   and extractAllTerms lst = List.iter extractTerm lst
   in
      extractAllTerms funbody;
      !env


and eval tree tbl =
  match tree with
      Value( t ) -> 
	(match t with
	    Var( _ ) ->
	      getVar t tbl
	  | Tuple( lst ) ->
	      let res = List.map (fun x -> Value( eval x tbl )) lst in
		Tuple( res )
	  | Array( arr ) -> 
	      let res = Array.map (fun x -> Value( eval x tbl )) arr in
		Array( res )
	  | _ -> t)


    | Let( var, term ) ->
	let (namespace, name) = var in
	let result = eval term tbl in
	  setVar namespace name result tbl;
	  Null



    | Apply( func, arglst ) ->
	let f = eval func tbl
	and a = List.map (fun x -> eval x tbl) arglst in
	  apply f a tbl

    | Lambda( args, body ) ->
	let env = extractClosure args body tbl in
	  (*	  print_endline "Schloooorp!"; *)
	  Func( args, body, env )


    | If( cond, ifpart, elsepart ) ->
	let c = eval cond tbl in
	  if c = Symbol( "true" ) then
	    evalList ifpart tbl
	  else if c = Symbol( "false" ) then
	    evalList elsepart tbl
	  else
	    raise (TypeError( "If's only take 'true and 'false!" ))

    | NamespaceChange( newns ) -> 
	(try
	    setNamespace tbl newns;
	  Null
	  with 
	      (* Namespace doesn't exist, make it *)
	      NamespaceError( _ ) -> 
		addNamespace tbl newns; setNamespace tbl newns; Null)

    | Structdecl( name, args, vars ) -> 
	let getNameAndExpr = function
	    Let( label, syntree ) -> ((snd label), syntree)
	  | _ -> raise (Fubar( "Wark!  The impossible happened!"))
	in
	let members = List.map getNameAndExpr vars in

(*	  addType (StructType( name, args, members )) tbl; *)

	  let init arglst =
	    makeNewStruct arglst args members tbl
	  in
	  let initFunc = Builtin( init ) in
	    setVar currentNamespace name initFunc tbl;
	    Null

    | Structref( var, name ) ->
	let term = eval var tbl in
	  (match term with
	      Struct( fields ) ->
		(try
		    Hashtbl.find fields name
		  with
		      Not_found -> raise (RuntimeError( 
			  "Struct does not have field " ^ name )))
	    | x -> 
		raise (TypeError( "Expected a struct, got " ^ (value2str x))))

    | Raise( term ) ->
          let value = eval term tbl in
          raise (MayhemException( value )); 

    | Try( term, catchthing ) -> 
          (try (evalList term tbl) 
          with MayhemException( exnthing ) -> 
             let arg, body = (List.hd catchthing) in 
             pushScope tbl;
             setVar currentNamespace arg exnthing tbl;
             let res = evalList body tbl in
             popScope tbl;
             res
          )
	    
    | Comment( _ ) -> Null



and apply f args tbl =
  match f with
      Func( formals, body, env ) ->
	(try
	    (*	    Printf.printf "Scope is %d\n" (List.length tbl.stack); *)
	    pushScope tbl;

	  (* Add args *)
	  (*	  print_endline "Adding args...\n";  *)
	  List.iter2 (fun nm vl ->
	    (*	    Printf.printf "Var %s = %s\n" nm (value2str vl); *)
	    setVar currentNamespace nm vl tbl) 
	    formals args;
	  (* Add closure environment *)
	  (*	  print_endline "Adding closure...";  *)
	  List.iter 
	    (fun x -> let sym, vl = x in 
			(*			Printf.printf "Closure %s = %s\n" sym (value2str vl); *)
			setVar currentNamespace sym vl tbl) 
	    env;

	  (*	  print_endline "Evaluating body..."; *)
	  (*	  printBloodyStack tbl; *)
	  let r = evalList body tbl in
	    popScope tbl;
	    r
	  with 
	      Invalid_argument( "List.iter2" ) -> 
		popScope tbl;
		let str = Printf.sprintf 
		  "Invalid number of args, expected %d got %d\n" 
		  (List.length formals) (List.length args) in
		  raise (TypeError( str )))

    | Builtin( func ) ->
	func args
    | _ -> raise (TypeError( "apply: Function required!" ))




and evalList lst stbl =
  let lst = (List.rev (List.map (fun x -> eval x stbl) lst)) in
    if lst = [] then
      Null
    else
      List.hd lst
;;

(* I really do not need decls as a separate syntax tree componant.
   I CAN just make the syntax disallow putting them in other things.
*)
