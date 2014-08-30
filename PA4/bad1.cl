class A {
      method1() : Int {
           3
      };
};

class B inherits A {
      method1() : Int {
           2
      };
};

class Main {
      main() : Int {
      	     let test : A in test@B.method1()
      };
};