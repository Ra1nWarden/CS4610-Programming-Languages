class C {};

class A inherits C {
  foo() : A {
    new A
  };
};

class B inherits A {};

class Main inherits IO {
  x : B <- new B;
  main() : Object {
    case x of a : A => out_int(10);
              b : B => out_int(20);
              c : C => out_int(30);
    esac
  };
};
