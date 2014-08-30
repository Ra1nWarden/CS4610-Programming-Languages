class A {
      comp(x : A) : SELF_TYPE {
      	     x
      };
};

class B inherits A {
      get2() : Int {
      	     2
      };
};

class Main {
      main() : Int {
      	     let one : A, two : B in one.comp(two).get2()
      };
};