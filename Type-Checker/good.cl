class A {
      val : SELF_TYPE;
      x : Int <- 3;
      a() : Int {
      	  2
      };
      returnSelf() : SELF_TYPE {
      		   self
      };
};

class B inherits A {
      y : String <- "fefe";
      a() : Int {
      	  3
      };
      b() : String {
      	  y
      };
};

class Main inherits IO {
      bvar : B <- new B;
      main() : Int {
      	     { 
	       if true then "else" else 3 fi;
	       while true loop 323 pool;
	       let x : Int <- 3 in x;     	     
	     }
      };
      in_int() : Int {
      	       3
      };
};