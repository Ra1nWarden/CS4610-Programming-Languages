class A {
      compare(another : SELF_TYPE) : Bool {
      		      true
      };	  
};

class Main inherits A {
      main() : Bool {
      	     let one : A, two : A in two.compare(one)
      };
};