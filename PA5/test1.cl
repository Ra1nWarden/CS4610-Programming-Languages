class A {
  b : B <- new B;
};

class B {
  a : A <- new A;
};

class Main {
  a : A <- new A;
  main() : Object {
      self
  };
};
