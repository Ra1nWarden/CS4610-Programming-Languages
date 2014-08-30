var fs = require('fs')

var lexfile = "test.cl-lex"



var output = fs.createWriteStream(, {encoding: 'utf8'})

output.write("test\n");
output.write("another test");
output.end();
