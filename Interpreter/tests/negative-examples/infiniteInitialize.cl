class Main inherits IO {
  main() : Object { 
    new A
  } ;
} ; 


class B {
  x : A <- new A;
};

class A {
  x : B <- new B;

};
