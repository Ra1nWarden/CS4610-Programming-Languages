/*Tokens*/

%token CLASS ELSE IF FI IN INHERITS ISVOID LEFT LOOP POOL THEN WHILE CASE ESAC NEW OF NOT TRUE FALSE AT TILDE PLUS MINUS TIMES DIVIDE LBRACE RBRACE LPAREN RPAREN SEMI COLON COMMA LARROW RARROW DOT LT LE EQUALS TYPE IDENTIFIER STRING INTEGER

/*Associativity and Precedence*/

%right IN
%right LARROW
%left NOT not
%nonassoc LE LT EQUALS
%left PLUS MINUS
%left TIMES DIVIDE
%left ISVOID isvoid
%left TILDE tilde
%left AT
%left DOT 
%left RPAREN
%right LPAREN lparen

%start file

/*Context free grammars*/

%%

file
	: program EOF {return $1;}
	;

program
	: class SEMI {$$ = [$1];}
	| class SEMI program {$$ = $3; $$.unshift($1);}
	;

class
	: CLASS type INHERITS type LBRACE feature_s RBRACE { var feats = $6;
	  	     	      	   	  	    	     if (feats == null) {
							     	feats = [];
							     }
							     $$ = { name: $2,
								    superclass: $4,
								    features: feats		   
								    }; }
	| CLASS type LBRACE feature_s RBRACE { var feats = $4;
	  	     	    	      	       if (feats == null) {
					       	  feats = [];
					       }
					       $$ = { name: $2,
	  	     	    	      	       	      superclass: null,
	  	     	    	      	       	      features: feats
						    };}
	;

feature_s
	: { $$ = null;}
	| feature_p { $$ = $1; }
	;

feature_p
	: feature SEMI { $$ = [$1];}
	| feature SEMI feature_p { $$ = $3;
	  	       		   $$.unshift($1);}
	;

feature
	: identifier LPAREN formal_option RPAREN COLON type LBRACE expr RBRACE {    var formallist = $3;
	  	     	    		  	       	    	   	       	    if($3 == null) {
										    	  formallist = [];
	  	     	    		  	       	    	   	       	    }
	  	     	    		  	       	    	   	       	    $$ = {
	  	     	    	   	    	   	      	     	  	      	  name: $1,
											  kind: "method",
											  type: $6,
											  formals: formallist,
											  body: $8
	  	     	    	   	    	   	      	     	  	     };
										 }
	| identifier COLON type larrow_option { if($4 != null) {
	  	     	   		      	      $$ = { name: $1,
	  	     	   		      	       	 kind: "attribute_init",
	  	     	   		      	       	 type: $3,
						       	 init: $4
	  	     	   		      	       };
						} else {
						  	$$ = { name: $1,
	  	     	   		      	       kind: "attribute_no_init",
	  	     	   		      	       type: $3
	  	     	   		      	       };
						}}
	;

larrow_option
	: { $$ = null;}
	| LARROW expr { $$ = $2;}
	;

formal_option
	: { $$ = null;}
	| formal formal_s { if ($2 != null) {
	  	 	       $$ = $2;
			       $$.unshift($1);
	  	 	    } else {
			       $$ = [$1];
			    }} 
	;

formal
	: identifier COLON type { $$ = {name: $1,
	  	     	   	       	type: $3
				       };}
	;

formal_s
	: {$$ = null;}
	| formal_p { $$ = $1;}
	;

formal_p
	: COMMA formal { $$ = [$2];}
	| COMMA formal formal_p { $$ = $3; $$.unshift($2);}
	;

expr	
	: identifier LARROW expr { $$ = {name: "assign", lineno: $1['lineno'], var: $1, rhs: $3};}
	| expr AT type DOT identifier LPAREN expr_option RPAREN { var arguments = $7;
	       	       	   	      	     		 	  if (arguments == null) {
								      arguments = [];
								  }
	       	       	   	      	     		 	  $$ = {name: "static_dispatch", lineno: $1['lineno'], type: $3, e: $1, method: $5, args: arguments};}
	| expr DOT identifier LPAREN expr_option RPAREN {  var arguments = $5;
	  	     	    		       	 if(arguments == null) {
						     arguments = [];
						 }
						 $$ = {name: "dynamic_dispatch", lineno: $1['lineno'], e: $1, method: $3, args: arguments};}
	| identifier LPAREN expr_option RPAREN { var arguments = $3;
	  	     	    		       	 if(arguments == null) {
						     arguments = [];
						 }
						 $$ = {name: "self_dispatch", lineno: $1['lineno'], method: $1, args: arguments};}
	| if expr THEN expr ELSE expr FI { $$ = {name: "if", lineno: $1, predicate: $2, then: $4, else: $6}; }
	| while expr LOOP expr POOL { $$ = {name: "while", lineno: $1, predicate: $2, body: $4};}
	| lbrace expr_p RBRACE { $$ = {name: "block", lineno: $1, body: $2}; }
	| let identifier COLON type larrow_option id_type_expr_s IN expr { var binding = $6; 
	      		       	    		  		    	       if (binding == null) {
									       	  binding = [];
									       }
	      		       	    		  		    	       if($5 == null) {
									       	     binding.unshift({
										      	name: "let_binding_no_init",
											variable: $2,
											type: $4
										      });
										     $$ = {
	      		       	    		  		    	       	      name: "let",
										      lineno: $1,
										      expr: $8,
										      bindings: binding };
										  } else {
										     binding.unshift({
										      	name: "let_binding_init",
											variable: $2,
											type: $4,
											value: $5
										      });
										    $$ = {
										       name: "let",
										       lineno: $1,
										       expr: $8,
										       bindings: binding };
									          }}
	| case expr OF id_type_expr_p ESAC { $$ = {
						  name: "case",
						  lineno: $1,
						  case: $2,
						  elements: $4
						  };}
	| new type { $$ = {name: "new", lineno: $1, class: $2};}
	| isvoid expr { $$ = {name: "isvoid", lineno: $1, e: $2};}
	| expr PLUS expr { $$ = {name: "plus", lineno: $1['lineno'], x: $1, y: $3};}
	| expr MINUS expr { $$ = {name: "minus", lineno: $1['lineno'], x: $1, y: $3};}
	| expr TIMES expr { $$ = {name: "times", lineno: $1['lineno'], x: $1, y: $3};}
	| expr DIVIDE expr { $$ = {name: "divide", lineno: $1['lineno'], x: $1, y:$3};}
	| tilde expr { $$ = {name: "negate", lineno: $1, x: $2};}
	| expr LT expr { $$ = {name: "lt", lineno: $1['lineno'], x: $1, y: $3};}
	| expr LE expr { $$ = {name: "le", lineno: $1['lineno'], x: $1, y: $3};}
	| expr EQUALS expr { $$ = {name: "eq", lineno: $1['lineno'], x: $1, y: $3};}
	| not expr { $$ = {name: "not", lineno: $1, x: $2};}
	| lparen expr RPAREN { $$ = {name: "bracket", lineno: $1, value: $2};}
	| identifier { $$ = {name: "identifier", lineno: $1['lineno'], variable: $1};}
	| integer { $$ = {name: "integer", lineno: $1['lineno'], value: $1['name']};}
	| string { $$ = {name: "string", lineno: $1['lineno'], value: $1['name']};}
	| true { $$ = {name: "true", lineno: $1};}
	| false { $$ = {name: "false", lineno: $1};}
	;

expr_option
	: { $$ = null;}
	| expr expr_s { if($2 != null) { 
	       	      	  $$ = $2;
			  $$.unshift($1);
			} else {
			  $$ = [$1];
			}
			}
	;

expr_s
	: { $$ = null;}
	| expr_s_p { $$ = $1;}
	;

expr_s_p
	: COMMA expr { $$ = [$2];}
	| COMMA expr expr_s_p { $$ = $3; $$.unshift($2);}
	;

expr_p
	: expr SEMI { $$ = [$1];}
	| expr SEMI expr_p { $$ = $3; $$.unshift($1);}
	;

id_type_expr_s
	: { $$ = null;}
	| id_type_expr_s_p {$$ = $1;}
	;

id_type_expr_s_p
	: COMMA identifier COLON type larrow_option { 
	  		   	      		    		   if ($5 != null) {
								      $$ = [{
								      	 name: "let_binding_init",
									 variable: $2,
									 type: $4,
									 value: $5
								      }];
								   } else {
								     $$ = [{
								      	 name: "let_binding_no_init",
									 variable: $2,
									 type: $4
								      }];
								   }
								}
	| COMMA identifier COLON type larrow_option id_type_expr_s_p { 
	  		   	      		    		   if ($5 != null) {
								      $$ = $6;
								      $$.unshift({
								      	 name: "let_binding_init",
									 variable: $2,
									 type: $4,
									 value: $5
								      });
								   } else {
								     $$ = $6;
								     $$.unshift({
								      	 name: "let_binding_no_init",
									 variable: $2,
									 type: $4
								      });
								   }
								}  
	;

id_type_expr_p
	: identifier COLON type RARROW expr SEMI { $$ = [{variable: $1, type: $3, body: $5}];}
	| identifier COLON type RARROW expr SEMI id_type_expr_p { $$ = $7; $$.unshift({variable: $1, type: $3, body: $5});}
	;

/*The following non-terminals are created for the purpose of tracking expression line numbers*/

type
	: TYPE { $$ = {lineno: yylineno, name: yytext};}
	;

identifier
	: IDENTIFIER { $$ = {lineno: yylineno, name: yytext};}
	;

integer
	: INTEGER { $$ = {lineno: yylineno, name: yytext};}
	;

if
	: IF { $$ = yylineno;}
	;

while
	: WHILE { $$ = yylineno;}
	;

lbrace
	: LBRACE { $$ = yylineno;}
	;

let
	: LET { $$ = yylineno;}
	;

case
	: CASE { $$ = yylineno;}
	;

new
	: NEW { $$ = yylineno;}
	;

isvoid
	: ISVOID { $$ = yylineno;}
	;

tilde
	: TILDE { $$ = yylineno;}
	;

not
	: NOT { $$ = yylineno;}
	;

lparen
	: LPAREN { $$ = yylineno;}
	;

string
	: STRING { $$ = {lineno: yylineno, name: yytext};}
	;

true
	: TRUE { $$ = yylineno;}
	;

false
	: FALSE { $$ = yylineno;}
	;