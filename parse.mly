%{
(* parse.mly
   Parser for... something.


*)

open Backend


%}

%token EOF
%token LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE BACKQUOTE
%token COLON COMMA ARROW EQUAL PERIOD
%token STRUCT TRY CATCH RAISE
%token FUN QUOTE DEFUN LET IN ELSE END IF THEN ELIF NAMESPACE
%token NULL
%token <int> INT
%token <string> SYMBOL
%token <char> CHAR
%token <string> COMMENT
%token <string> STRING
/*%token <float> FLOAT
%token <bool> BOOLEAN
*/

%type <Backend.syntree list> main
%start main

%%

main:  
	  /* EMPTY */ 
	  	{[]}
	| decllist
	  	{$1}
	;

decllist:
          decl
                { [$1] }
        | decl decllist
                { $1 :: $2 }
        ;

decl:
          expr
                { $1 }
        | structdecl
                { $1 }
        ;

structdecl:
          STRUCT SYMBOL COLON letexprlist END
                { Structdecl( $2, [], $4 ) }
        | STRUCT SYMBOL arglist COLON letexprlist END
                { Structdecl( $2, $3, $5 ) }
        ;

letexprlist:
          letexpr
                { [$1] }
        | letexpr letexprlist
                { $1 :: $2 }
        ;


exprlist:
          expr
                { [$1] }
        | expr exprlist
                { $1 :: $2 }
        ;

/* XXX: Comments! */
expr:
          value
                { Value( $1 ) }
        | letexpr
                { $1 }
        | defunexpr
                { $1 }
        | funcallexpr
                { $1 }
        | lambdaexpr
                { $1 }
        | ifexpr
                { $1 }
        | COMMENT
                { Comment( $1 ) }
        | namespaceexpr
                { $1 }
        | structref
                { $1 }
        | raiseexpr
                { $1 }
        | tryexpr
                { $1 }
        ;

value:
          INT
                { Int( $1 ) }
        | CHAR
                { Char( $1 ) }
        | STRING
                { String( $1 ) }
        | BACKQUOTE SYMBOL
                { Symbol( $2 ) }
        | varexpr
                { Var( $1 ) }
        | LBRACK RBRACK
                { Tuple( [] ) }
        | LBRACK exprlist RBRACK
                { Tuple( $2 ) }
        | LBRACE RBRACE
                { Array( [||] ) }
        | LBRACE exprlist RBRACE
                { Array( (Array.of_list $2) ) }
        | NULL
                { Null }
        ;


varexpr:
          SYMBOL
                { ( "", $1 ) }
/* No namespaces yet.
        | SYMBOL PERIOD SYMBOL
                { ( $1, $3 ) }
*/
        ;

funcallexpr:
          LPAREN expr exprlist RPAREN
                { Apply( $2, $3 ) }
        | LPAREN expr RPAREN
                { Apply( $2, [] ) }
        ;

letexpr:
          varexpr COLON expr
                { Let( $1, $3 ) }
        ;

defunexpr:
          DEFUN varexpr arglist COLON exprlist END
                { Let( $2, Lambda( $3, $5 )) }
        | DEFUN varexpr COLON exprlist END
                { Let( $2, Lambda( [], $4 )) }
        ;

arglist:
          SYMBOL
                { [$1] }
        | SYMBOL arglist
                { $1 :: $2 }
        ;

lambdaexpr:
          FUN arglist COLON exprlist END
                { Lambda( $2, $4 ) }
        | FUN COLON exprlist END
                { Lambda( [], $3 ) }
        ;


ifexpr:
          IF expr THEN exprlist END
                { If( $2, $4, [] ) }
        | IF expr THEN exprlist ELSE exprlist END
                { If( $2, $4, $6 ) }
        | IF expr THEN exprlist eliflist END
                { If( $2, $4, [$5] ) }
        ;

elifpart:
          ELIF expr THEN exprlist
                { ($2,$4) }
        ;

eliflist:
          elifpart
                { let t, b = $1 in If( t, b, [] )}
        | elifpart ELSE exprlist
                { let t, b = $1 in If( t, b, $3 )}
        | elifpart eliflist
                { let t, b = $1 in If( t, b, [$2] )}
        ;


namespaceexpr:
          NAMESPACE SYMBOL
                { NamespaceChange( $2 ) }
        ;

structref:
          PERIOD expr PERIOD SYMBOL
                { Structref( $2, $4 ) }
        ;

raiseexpr:
          RAISE expr
                { Raise( $2 ) }
        ;

tryexpr:
          tryblock catchlist END
                { Try( $1, $2 ) }
        ;

tryblock:
          TRY exprlist
                { $2 }  
        ;

catchlist:
          catchblock
          { [$1] } 
         /*| catchblock catchlist
                { $1 :: $2 }
*/
        ;
catchblock:
          CATCH SYMBOL COLON exprlist
          { ( $2, $4 ) }
        ;
%%

