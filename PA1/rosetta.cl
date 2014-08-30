class List inherits IO {
	isNil() : Bool { true };

	head() : String { { abort(); ""; } };
	tail() : List { { abort(); self; } };
	cons(i : String) : List {
		(new Cons).init(i, self)
	};
    
	remove(i : String) : List {
		if self.isNil() then self else
		if self.head() = i then self.tail()
		else
			(new Cons).init(self.head(), self.tail().remove(i))
		fi fi
	};
	 
	size() : Int {
		if self.isNil() then 0
		else
			1 + self.tail().size()
		fi
	};
};


class Cons inherits List {
	car : String;
	cdr : List;
	isNil() : Bool { false };
	head() : String { car };
	tail() : List { cdr };
	init(i : String, rest : List) : List {
		{
			car <- i;
			cdr <- rest;
			self;
		}
	};

};

class Connection inherits IO {
	key : String;
	value : List;
	dest() : String { key };
	sources() : List { value };
	init(s : String) : Connection {
		{
			key <- s;
			value <- new List;
			self;
		}
	};
	initWithSource(s : String, d : String) : Connection {
		{
			key <- d;
			value <- (new List).cons(s);
			self;
		}
	};
	addSource(s : String) : Connection {
		{
			value <- value.cons(s);
			self;
		}
	};
	removeSource(s : String) : Connection {
		{
			value <- value.remove(s);
			self;
		}
	};
	zeroSource() : Bool {
		value.size() = 0
	};
};

class Connections inherits IO {
	isNil() : Bool { true };
	head() : Connection { {  abort(); new Connection; } };	
	tail() : Connections { {  abort(); self;  } };
	cons(c : Connection) : Connections {
		(new ConnectionsList).init(c, self)
	};
	hasZero() : Bool {
		if self.isNil() then false else 
		if self.head().zeroSource() then true else
			self.tail().hasZero()
		fi fi
	};
	minSource() : String {
		let result : String <- "",
	            connects : Connections <- self
		in {	
		while not connects.isNil() loop
		{
		  if connects.head().zeroSource() then
			if result = "" then result <- connects.head().dest() else
			if connects.head().dest() < result then result <- connects.head().dest()
			else result <- result
			fi fi
		  else result <- result
		  fi;
		  connects <- connects.tail();
		}   
		pool;
		result;
		}
	};
	removeNode(s : String) : Connections {
		{
		if self.isNil() then self else
		if self.head().dest() = s then self.tail().removeNode(s) else
		(new ConnectionsList).init(self.head().removeSource(s), self.tail().removeNode(s))
		fi fi;
		}
	};
	containDest(s : String) : Bool {
		if self.isNil() then false else
		if self.head().dest() = s then true else
		self.tail().containDest(s)
		fi fi
	};
	addSourceToDest(s : String, d : String) : Connections {
		{
		if self.head().dest() = d then (new ConnectionsList).init(self.head().addSource(s), self.tail()) else
		(new ConnectionsList).init(self.head(), self.tail().addSourceToDest(s, d))
		fi;
		}
	};
	addConnection(s : String, d : String) : Connections {
		let result : Connections in
		{
		if self.containDest(d) then result <- self.addSourceToDest(s, d) else
		result <- self.cons((new Connection).initWithSource(s, d))
		fi;
		if self.containDest(s) then result else
		result <- result.cons((new Connection).init(s))
		fi;
		result;
		}
	};
	size() : Int {
		if self.isNil() then 0
		else 1 + self.tail().size()
		fi
	};
};

class ConnectionsList inherits Connections {
	car : Connection;
	cdr : Connections;
	isNil() : Bool { false };
	head() : Connection { car };
	tail() : Connections { cdr };

	init(c : Connection, rest : Connections) : Connections {
		{
			car <- c;
			cdr <- rest;
			self;
		}
	};

};


class Main inherits IO {
	sortedlist : List <- new List;
	connects : Connections <- new Connections;
	cycle : Bool <- false;
	print_list(l : List) : Object {
		if l.isNil() then out_string("")
			else {
				print_list(l.tail());
				out_string(l.head());
				out_string("\n");
			}

		fi
	};
	main() : Object {
		let 
			dst : String <- in_string (),
			src : String,
			done : Bool <- false,
			totalCount : Int,
			nextNode : String 	
		in
		{	
			if dst = "" then done <- true
			else done <- false
			fi;
			while not done loop {
				src <- in_string ();
				connects <- connects.addConnection(src, dst);	
				dst <- in_string ();
				if dst = "" then done <- true
				else done <- false
				fi;
			}
			pool;
			totalCount <- connects.size();
			while connects.hasZero() loop {
				nextNode <- connects.minSource();
				sortedlist <- sortedlist.cons(nextNode);
				connects <- connects.removeNode(nextNode);
			} pool ;
			if sortedlist.size() = totalCount then cycle <- false
			else cycle <- true
			fi;
			if cycle then out_string("cycle\n")
			else print_list(sortedlist)
			fi;
		}
	};
};
