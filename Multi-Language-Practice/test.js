var readline = require('readline');

var rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false
});

rl.on('line', function(dest) {
    rl.on('line', function(source) {
	process.stdout.write("dest is: " + dest + " source is: " + source + '\n');
    });
});
