var readline = require('readline');

var rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false
});

var lines = new Array();

rl.on('line', function(each_line) {
    lines.push(each_line);
});

var connections = {};
var dest, source;
var sortedList = new Array();

rl.on('close', function() {
    for(var i = 0; i < lines.length; i += 2) {
	dest = lines[i];
	source = lines[i+1];
	if(connections.hasOwnProperty(dest)) {
	    connections[dest].push(source);
	}
	else {
	    connections[dest] = [source];
	}
	if(!connections.hasOwnProperty(source)) {
	    connections[source] = new Array();
	}
    }
    var nodeCount = 0;
    for(var eachNode in connections) {
	nodeCount = nodeCount + 1;
    }
    var cycle = true;
    while(sortedList.length != nodeCount) {
	var nextNode = "";
	for(var eachNode in connections) {
	    if(connections[eachNode].length == 0) {
		cycle = false;
		if(nextNode == "") {
		    nextNode = eachNode;
		}
		else if(nextNode > eachNode) {
		    nextNode = eachNode;
		}
	    }
	}
	if(cycle) {
	    process.stdout.write("cycle\n");
	    process.exit(0);
	}
	else {
	    delete connections[nextNode];
	    for(var eachNode in connections) {
		var eachSourceList = connections[eachNode];
		var indexForRemoval = eachSourceList.indexOf(nextNode);
		if(indexForRemoval > -1) {
		    eachSourceList.splice(indexForRemoval, 1);
		}
	    }
	    sortedList.push(nextNode);
	}
	cycle = true;
    }
    sortedList.forEach(function(sortedNode) {
	process.stdout.write(sortedNode + '\n');
    });
});
