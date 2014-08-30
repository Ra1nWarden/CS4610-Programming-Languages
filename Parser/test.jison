%token PLUS MINUS INT
%left PLUS MINUS
%start program
%%
program: exp EOF { return $1; }
       ;
exp: exp PLUS exp { $$ = "plus_node" + "\n"+"lineno: " + yylineno +"\n"+ $1+"\n" + $3 + "yytext is " + $1 + " and " +$3 ; }
   | exp MINUS exp { $$ = "minus_node" + "\n" +"lineno: " + yylineno + "\n" + $1 +"\n"+ $3 + "yytext is " +"\nminus" +  yytext; }
   | INT { $$ = $1; }
   ;
