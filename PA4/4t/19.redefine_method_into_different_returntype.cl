class A {
      giveme() : String {
      	       "123"
      };
};

class B inherits A {
      giveme() : Int {
      	       123
      };       	 
};

class Main {
      main() : Int {
      	     let result : B in result.giveme()
      };
};