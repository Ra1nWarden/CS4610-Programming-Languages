class Main inherits IO {
      x : Int <- 1;
      getVal(a : Int, b : Int) : Int {
      	       a + b
      };
      main() : Object {
      	     out_int(getVal(x <- x - 5, let y : Int <- x +10 in y))
      };
};