var cl_lex = [
    ['INT', 1, "11"] ,
    ['PLUS', 2, "1"],
    ['INT', 2, "22"],
    ['MINUS', 4, "2" ] ,
    ['INT', 5, "33"] ,
    ['EOF' ] ,
]
var token_count = 0
var parser = require("./test").parser;
parser.lexer = {
    lex : function() {
	var cl_lex_entry =
	    cl_lex[token_count++] ;
	var token = cl_lex_entry[0] ;
	var lexeme = cl_lex_entry[2] ;
	parser.lexer.yytext = lexeme ;
	parser.lexer.yylineno = cl_lex_entry[1];
	return token;
    },
    setInput : function(str) { }
}
var final_ast = parser.parse("");
console.log(final_ast);
