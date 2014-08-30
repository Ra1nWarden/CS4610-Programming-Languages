class Bazz inherits IO {

     i : Int <- 3;

     h : Object <- printi();

     printi() : Int { { out_int(i); 0; } };

};

class Main {
      a : Bazz <- new Bazz;
      main() : Int {
      	     2
      };
};