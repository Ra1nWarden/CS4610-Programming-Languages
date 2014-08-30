class Main inherits IO {
      main() : Int {
      	     recursive_call(1005)
      };
      recursive_call(x : Int) : Int {
      		       if x = 1 then 3 else recursive_call(x - 1) fi
      };
};