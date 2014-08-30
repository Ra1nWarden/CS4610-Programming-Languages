class Main inherits IO {
  main() : Object {
    out_string("Hello, world.\n")
  } ;
} ;


class B {
  x : A <- new A;
};

class A {
  x : B <- new B;

};
